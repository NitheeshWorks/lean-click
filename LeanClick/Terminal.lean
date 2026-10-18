import LeanClick.APIs
import Std.Data.HashSet
import Std.Data.HashMap

/-!
# Terminal: the one effectful module

Everything that touches the outside world: reading environment variables,
looking at the filesystem for `Path`/`File` checks, prompting on standard
input, writing to standard output and error, opening `File` values, and the
exit code. The parser it drives is the pure `Command.parseLevel`; this
module only gathers its inputs (`Env`) and acts on its outputs.

A group's callback runs after the group is parsed and before its subcommand
is parsed, as in click's `Group.invoke`.

Not proved; exercised by the conformance runner and the example.
-/

namespace LeanClick.Terminal

open Text

variable {σ : Type}

/-- The monad callbacks run in. -/
abbrev ClickM := ExceptT ClickError IO

/-- A callback: receives the context, returns the object handed to the
subcommand's context. -/
abbrev Callback (σ : Type) := Context σ → ClickM σ

/-- A command whose callbacks run in `IO`. -/
abbrev App (σ : Type) := Command (Callback σ)

instance {σ : Type} : Inhabited (Callback σ) := ⟨fun c => pure c.obj⟩

/-- `click.command` with an `IO` callback. -/
def App.command {σ : Type} (spec : CommandSpec) (callback : Callback σ := fun c => pure c.obj) : App σ :=
  .mk spec false callback []

/-- `click.group` with an `IO` callback. -/
def App.group {σ : Type} (spec : CommandSpec) (subcommands : List (App σ))
    (callback : Callback σ := fun c => pure c.obj) : App σ :=
  .mk spec true callback subcommands

/-- `click.echo`. -/
def echo (msg : String) (err : Bool := false) (nl : Bool := true) : IO Unit := do
  let h ← if err then IO.getStderr else IO.getStdout
  h.putStr (if nl then msg ++ "\n" else msg)
  h.flush

/-- `ctx.exit(code)`. -/
def exit (code : Nat := 0) : ClickM σ := throw (.exit code)

/-- `ctx.abort()`. -/
def abort : ClickM σ := throw .abort

/-- `ctx.fail(message)`: a usage error in the context. -/
def fail (ctx : Context σ) (message : String) : ClickM α :=
  throw (.usage ⟨.usage message, some ⟨ctx.commandPath, ctx.usage, some "--help"⟩⟩)

/-- Read one line; `none` at end of input. -/
def readLine : IO (Option String) := do
  let stdin ← IO.getStdin
  let line ← stdin.getLine
  if line.isEmpty then return none
  return some (rstripNewlines (String.ofList (line.toList.filter (· != '\r'))))

/-- `click.confirm(text, default)`; `none` means abort (end of input). -/
def confirm (text : String) (default : Option Bool := some false) (fuel : Nat := 100) :
    IO (Option Bool) := do
  let suffix := match default with
    | some true => "[Y/n]"
    | some false => "[y/N]"
    | none => "[y/n]"
  let rec loop : Nat → IO (Option Bool)
    | 0 => pure none
    | f + 1 => do
      echo s!"{text} {suffix}: " (nl := false)
      match ← readLine with
      | none => echo ""; pure none
      | some ans =>
        let a := lower (strip ans)
        if a == "y" || a == "yes" then pure (some true)
        else if a == "n" || a == "no" then pure (some false)
        else if a.isEmpty && default.isSome then pure default
        else do echo "Error: invalid input" (err := true); loop f
  loop fuel

/-- `click.prompt` for a parameter: shows the default and the choices,
re-asks until the value converts, and asks twice with a confirmation
prompt. `none` means abort. Input is not hidden (`hide_input` is deviation
`dev.hide-input`: Lean core has no portable way to turn off terminal echo). -/
def promptParam (fs : String → Option FileInfo) (p : Param) (dflt : Option String)
    (fuel : Nat := 100) : IO (Option String) := do
  let ps := p.prompt.getD { text := p.name }
  if p.decl.isBoolFlag then
    let d := dflt.bind strToBool
    match ← confirm ps.text d with
    | some b => return some (if b then "y" else "n")
    | none => return none
  let choices := match p.type with
    | .choice cs _ => if p.showChoices then s!" ({", ".intercalate cs})" else ""
    | _ => ""
  let showD := p.showDefault != .no
  let shown := match dflt with
    | some d => if showD then s!" [{d}]" else ""
    | none => ""
  let text := ps.text ++ choices ++ shown ++ ": "
  let rec ask : Nat → IO (Option String)
    | 0 => pure none
    | f + 1 => do
      echo text (nl := false)
      match ← readLine with
      | none => echo ""; pure none
      | some "" => match dflt with
        | some d => pure (some d)
        | none => ask f
      | some v =>
        match convert fs p.type v with
        | .error msg => do echo s!"Error: {msg}" (err := true); ask f
        | .ok _ => pure (some v)
  let rec loop : Nat → IO (Option String)
    | 0 => pure none
    | f + 1 => do
      match ← ask fuel with
      | none => pure none
      | some v =>
        if !ps.confirmation then pure (some v)
        else do
          echo "Repeat for confirmation: " (nl := false)
          match ← readLine with
          | none => echo ""; pure none
          | some w =>
            if v == w then pure (some v)
            else do echo "Error: The two entered values do not match." (err := true); loop f
  loop fuel

/-- What the filesystem says about a path. Permission bits are not
available through Lean core, so readability, writability and
executability are assumed. -/
def statPath (s : String) : IO (Option FileInfo) := do
  try
    let md ← System.FilePath.metadata s
    return some { isFile := md.type == .file, isDir := md.type == .dir }
  catch _ => return none

/-- Every string the parser could hand to a `Path`/`File` conversion at
this level: tokens, values after `=`, attached short-option values,
environment values and their pieces, and defaults. -/
def pathCandidates (ps : List Param) (argv : List String) (envVals : List String) : List String :=
  let fromTok := fun (t : String) =>
    let cs := t.toList
    let eq := match (splitEq t).2 with | some v => [v] | none => []
    -- attached short-option values (`-ofile`) only for option-looking
    -- tokens of reasonable length, so the view stays linear in argv
    let attached := if looksLikeOption t && cs.length ≤ 256 then
        (List.range (cs.length - 1)).map fun i => String.ofList (cs.drop (i + 2))
      else []
    t :: eq ++ attached
  let defaults := ps.flatMap fun p => p.default.getD []
  let envPieces := envVals.flatMap fun v => v :: splitWs v ++ splitChar v ':'
  let all := argv.flatMap fromTok ++ envPieces ++ defaults
  -- deduplicate with a hash set (linear) instead of `eraseDups` (quadratic)
  let (_, out) := all.foldl (fun (acc : Std.HashSet String × Array String) c =>
    if acc.1.contains c then acc else (acc.1.insert c, acc.2.push c)) ({}, #[])
  out.toList

/-- The terminal width click would use: `COLUMNS` (else 80), clamped to
80, minus 2, at least 50. -/
def terminalWidth : IO Nat := do
  let cols := ((← IO.getEnv "COLUMNS").bind String.toNat?).getD 80
  return max (min cols 80 - 2) 50

/-- Build the `Env` one level of parsing reads. -/
def levelEnv (cmd : App σ) (s : Settings) (argv : List String)
    (answers : List ((String × String) × String)) : IO Env := do
  let ps := cmd.allParams s
  let names := (ps.flatMap (envNames s)).eraseDups
  let mut vars : List (String × String) := []
  for n in names do
    if let some v ← IO.getEnv n then vars := vars ++ [(n, v)]
  let needsFs := ps.any fun p => match p.type with | .path _ | .file _ => true | _ => false
  let mut table : Std.HashMap String FileInfo := {}
  if needsFs then
    for c in pathCandidates ps argv (vars.map (·.2)) do
      if let some info ← statPath c then table := table.insert c info
  let tbl := table
  return { vars, fs := fun p => tbl.get? p, answers, width := s.width }

/-- Parse one level, prompting for missing values until the parser has
every answer it asked for. `none` means the user aborted a prompt. -/
def parseInteractive (cmd : App σ) (s : Settings) (path : String) (argv : List String) :
    Nat → List ((String × String) × String) → IO (Option (Except Stop (Level (Callback σ))))
  | 0, _ => pure (some (.error (.error ⟨.usage "Too many prompts.", none⟩)))
  | f + 1, answers => do
    let env ← levelEnv cmd s argv answers
    match cmd.parseLevel s path argv env with
    | .error (.prompt p d) =>
      match ← promptParam env.fs p d with
      | some a => parseInteractive cmd s path argv f (answers ++ [((path, p.name), a)])
      | none => pure none
    | r => pure (some r)

/-- Run an invocation level by level: parse, run the callback, descend. -/
def runAux (prog : String) :
    Nat → App σ → Option Settings → String → String → List String →
    Option (Context σ) → σ → IO Termination
  | 0, _, _, _, _, _, _, _ => pure (.halted (.error ⟨.usage "Command nesting exceeds the argument count.", none⟩))
  | fuel + 1, cmd, parent, infoName, path, argv, parentCtx, obj => do
    let s := cmd.settings parent infoName
    match ← parseInteractive cmd s path argv ((cmd.allParams s).length + 1) [] with
    | none => pure (.raised .abort)
    | some (.error st) =>
      match st.toHalt cmd s path prog with
      | .prompt _ _ _ => pure (.raised .abort)
      | h => pure (.halted h)
    | some (.ok lv) =>
      let ctx : Context σ :=
        { infoName, commandPath := path, params := lv.args, parent := parentCtx, obj,
          invokedSubcommand := lv.sub.map (·.2.1),
          help := cmd.getHelp s path, usage := cmd.getUsage s path }
      match ← (cmd.callback ctx).run with
      | .error e => pure (.raised e)
      | .ok obj' =>
        match lv.sub with
        | none => pure .ok
        | some (c, name, rest) =>
          runAux prog fuel c (some s) name (cmd.childPath s path name) rest (some ctx) obj'

/-- Print how an invocation ended. -/
def report : Termination → IO Unit
  | .ok => pure ()
  | .halted (.error e) => echo e.render (err := true) (nl := false)
  | .halted (.help t) => echo t
  | .halted (.message t) => echo t
  | .halted (.prompt _ _ _) => echo "Aborted!" (err := true)
  | .raised e => echo e.render (err := true) (nl := false)

/-- `Command.main`: run with `argv`, report, and return the exit code. -/
def main (cmd : App σ) (init : σ) (argv : List String) (prog : String := cmd.name)
    (defaultMap : Option DefaultMap := none) : IO UInt32 := do
  let width ← terminalWidth
  let t ← try
      runAux prog (argv.length + 1) cmd (some (rootParent prog width defaultMap)) prog prog argv none init
    catch e => pure (.raised (.failure (toString e)))
  report t
  return UInt32.ofNat t.exitCode

/-- `hello()` in click: run the command with the process arguments. -/
def _root_.LeanClick.Command.run {σ : Type} (cmd : App σ) (init : σ) (argv : List String) : IO UInt32 :=
  Terminal.main cmd init argv

/-- Open a `File` value for reading and return its contents (`-` is
standard input). -/
def readFileArg (f : FileArg) : ClickM String := do
  if f.name == "-" then
    let stdin ← IO.getStdin
    stdin.readToEnd
  else
    match ← (IO.FS.readFile f.name).toBaseIO with
    | .ok c => pure c
    | .error e => throw (.fileError f.name (toString e))

/-- Write to a `File` value (`-` is standard output). -/
def writeFileArg (f : FileArg) (contents : String) : ClickM Unit := do
  if f.name == "-" then echo contents (nl := false)
  else
    let act : IO Unit :=
      if f.mode.toList.contains 'a' then do
        let h ← IO.FS.Handle.mk f.name .append
        h.putStr contents
      else IO.FS.writeFile f.name contents
    match ← act.toBaseIO with
    | .ok () => pure ()
    | .error e => throw (.fileError f.name (toString e))

end LeanClick.Terminal
