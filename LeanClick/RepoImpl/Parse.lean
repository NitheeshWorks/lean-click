import LeanClick.RepoImpl.Resolve

/-!
# Parsing a command, dispatching a group

`Command.parseWith` is click's `Command.parse_args`: scan, distribute
positionals, process every parameter (eager ones first, then in invocation
order, then the rest; the first failure in that order is the one
reported), and reject extra arguments. It reads the environment only through
the function `envOf` it is given; `Command.parseArgs` supplies it from an
`Env`. That split is what `parse_deterministic` is stated against.

`Command.parseLevel` adds group dispatch: the first subcommand whose name
matches the next token. `Command.dispatch` runs the levels to the end, which
is what tests use; `LeanClick.Terminal` runs them one at a time so a group's
callback runs before its subcommand is parsed, as in click.
-/

namespace LeanClick

open Text

variable {σ : Type}

/-- `iter_params_for_processing`: eager parameters first, then by first
appearance in the invocation order, ties (and absent ones) in declaration
order. -/
def processingOrder (ps : List Param) (invoked : List String) : List Param :=
  let idx := fun (p : Param) => (invoked.idxOf? p.name).getD invoked.length
  let key := fun (p : Param) => (if p.eager.isSome then 0 else invoked.length + 1) + idx p
  sortBy key ps

/-- An argument's command-line raw value from its slot. -/
def argSlotRaw (p : Param) : ArgRaw → Except UsageError (Option Raw)
  | .single none => .ok none
  | .single (some s) => .ok (some (.tokens [s]))
  | .tuple vs =>
    if vs.all Option.isNone then .ok none
    else if vs.any Option.isNone then
      .error ⟨.badArgumentUsage s!"Argument {pyRepr p.name} takes {vs.length} values.", none⟩
    else .ok (some (.tokens (vs.filterMap id)))
  | .star [] => .ok none
  | .star vs => .ok (some (.tokens vs))

def Param.argNargs (p : Param) : ArgNargs :=
  match p.decl with
  | .argument n => n
  | .option _ _ => .exactly 1

/-- `_process_args_for_args`: the command-line raw value of every argument,
and the leftover tokens. -/
def argRaws (args : List Param) (positional : List String) :
    Except UsageError (List (String × Option Raw) × List String) := do
  let (slots, rest) := unpack (args.map Param.argNargs) positional
  let raws ← (args.zip slots).mapM fun (p, sl) => do
    let r ← argSlotRaw p sl
    return (p.name, r)
  return (raws, rest)

/-- The "Got unexpected extra argument(s)" message. -/
def extraArgsMsg (xs : List String) : String :=
  if xs.length == 1 then s!"Got unexpected extra argument ({" ".intercalate xs})"
  else s!"Got unexpected extra arguments ({" ".intercalate xs})"

def Stop.withCtx (c : CtxInfo) : Stop → Stop
  | .error e => .error { e with ctx := some c }
  | s => s

/-- The command-line raw value of a parameter after scanning. -/
def rawOf (sc : Scanned) (araws : List (String × Option Raw)) (p : Param) : Option Raw :=
  if p.isOption then cmdRaw sc.events p else (araws.lookup p.name).join

/-- Process every parameter: first in processing order (to find the stop
click would report first), then in declaration order for the result. -/
def processAll (order ps : List Param) (resolve : Param → Except Stop Entry) :
    Except Stop (List Entry) :=
  match order.mapM resolve with
  | .error s => .error s
  | .ok _ => ps.mapM resolve

/-- Resolve one parameter of a scanned command line. -/
def resolveWith (fs : String → Option FileInfo) (answers : List ((String × String) × String))
    (path : String) (info : CtxInfo) (sc : Scanned) (araws : List (String × Option Raw))
    (envOf : Param → Option String) (p : Param) : Except Stop Entry :=
  match resolveParam fs (answers.lookup (path, p.name)) (rawOf sc araws p) (envOf p) p with
  | .error st => .error (st.withCtx info)
  | .ok e => .ok e

/-- click's `Command.parse_args`, reading the environment only through
`envOf`. -/
def Command.parseWith (cmd : Command σ) (s : Settings) (path : String) (argv : List String)
    (fs : String → Option FileInfo) (answers : List ((String × String) × String))
    (envOf : Param → Option String) : Except Stop ParsedArgs :=
  let info := cmd.ctxInfo s path
  let fail := fun (e : UsageError) => Stop.error { e with ctx := some info }
  if argv.isEmpty && cmd.noArgsIsHelp then .error (fail ⟨.noArgsIsHelp (cmd.getHelp s path), none⟩)
  else
  match scan (cmd.scanCfg s) argv with
  | .error e => .error (fail e)
  | .ok sc =>
    let ps := cmd.allParams s
    let args := ps.filter Param.isArgument
    match argRaws args (sc.largs ++ sc.rargs) with
    | .error e => .error (fail e)
    | .ok (araws, leftover) =>
      let order := processingOrder ps (sc.events.map (·.dest) ++ args.map (·.name))
      match processAll order ps (resolveWith fs answers path info sc araws envOf) with
      | .error st => .error st
      | .ok entries =>
        if !leftover.isEmpty && !cmd.allowExtraArgs then .error (fail ⟨.usage (extraArgsMsg leftover), none⟩)
        else .ok ⟨entries, leftover⟩

/-- `parseWith` with the environment taken from an `Env`. -/
def Command.parseArgs (cmd : Command σ) (s : Settings) (path : String) (argv : List String)
    (env : Env) : Except Stop ParsedArgs :=
  cmd.parseWith s path argv env.fs env.answers (envString env.get s)

/-- One level of an invocation: the parsed arguments, and for a group the
selected subcommand with the name it was invoked as and its arguments. -/
structure Level (σ : Type) where
  args : ParsedArgs
  sub : Option (Command σ × String × List String)

/-- `Group.resolve_command`: the first subcommand with the name. -/
def Command.findSub (cmd : Command σ) (name : String) : Option (Command σ) :=
  cmd.subcommands.find? (fun c => c.name == name)

/-- Parse one level; a group also selects its subcommand. -/
def Command.parseLevel (cmd : Command σ) (s : Settings) (path : String) (argv : List String)
    (env : Env) : Except Stop (Level σ) :=
  match cmd.parseArgs s path argv env with
  | .error st => .error st
  | .ok pa =>
    if !cmd.isGroup then .ok ⟨pa, none⟩
    else
      let info := cmd.ctxInfo s path
      let fail := fun (e : UsageError) => Stop.error { e with ctx := some info }
      match pa.extra with
      | [] =>
        if cmd.spec.invokeWithoutCommand then .ok ⟨pa, none⟩
        else .error (fail ⟨.usage "Missing command.", none⟩)
      | name :: rest =>
        match cmd.findSub name with
        | some c => .ok ⟨{ pa with extra := [] }, some (c, name, rest)⟩
        | none =>
          let names := cmd.subcommands.map (·.name)
          let nsc := fail ⟨.noSuchCommand name (closeMatches name names), none⟩
          if (splitOpt name).1.isEmpty then .error nsc
          else match scan (cmd.scanCfg s) (name :: rest) with
            | .error e => .error (fail e)
            | .ok _ => .error nsc

/-- Why an invocation stopped before running every callback. -/
inductive Halt where
  | error (e : UsageError)
  | help (text : String)
  | message (text : String)
  | prompt (path : String) (p : Param) (defaultShown : Option String)

/-- Substitute `%(prog)s` and `%(version)s`. -/
def versionMessage (prog version template : String) : String :=
  (template.replace "%(prog)s" prog).replace "%(version)s" version

/-- A stop, rendered in the context it happened in. -/
def Stop.toHalt (cmd : Command σ) (s : Settings) (path prog : String) : Stop → Halt
  | .error e => .error e
  | .eager .help => .help (cmd.getHelp s path)
  | .eager (.version v t) => .message (versionMessage prog v t)
  | .prompt p d => .prompt path p d

/-- One parsed level of an invocation chain. -/
structure Frame (σ : Type) where
  cmd : Command σ
  infoName : String
  path : String
  settings : Settings
  args : ParsedArgs
  invokedSub : Option String

/-- The levels parsed, root first, and why parsing stopped, if it did. -/
structure Dispatch (σ : Type) where
  frames : List (Frame σ)
  halt : Option Halt

def dispatchAux (prog : String) (env : Env) :
    Nat → Command σ → Option Settings → String → String → List String → Dispatch σ
  | 0, _, _, _, _, _ => ⟨[], some (.error ⟨.usage "Command nesting exceeds the argument count.", none⟩)⟩
  | fuel + 1, cmd, parent, infoName, path, argv =>
    let s := cmd.settings parent infoName
    match cmd.parseLevel s path argv env with
    | .error st => ⟨[], some (st.toHalt cmd s path prog)⟩
    | .ok lv =>
      match lv.sub with
      | none => ⟨[⟨cmd, infoName, path, s, lv.args, none⟩], none⟩
      | some (c, name, rest) =>
        let d := dispatchAux prog env fuel c (some s) name (cmd.childPath s path name) rest
        ⟨⟨cmd, infoName, path, s, lv.args, some name⟩ :: d.frames, d.halt⟩

/-- Parse a whole invocation, every level, without running callbacks. -/
def Command.dispatch (cmd : Command σ) (prog : String) (argv : List String) (env : Env) : Dispatch σ :=
  dispatchAux prog env (argv.length + 1) cmd (some { width := env.width }) prog prog argv

/-! ## Messages and exit codes -/

/-- `format_message`. -/
def UsageError.message (e : UsageError) : String :=
  match e.kind with
  | .noSuchOption name ps =>
    s!"No such option {pyRepr name}." ++ (if ps.isEmpty then "" else " " ++ formatPossibilities ps)
  | .noSuchCommand name ps =>
    s!"No such command {pyRepr name}." ++ (if ps.isEmpty then "" else " " ++ formatPossibilities ps)
  | .badOptionUsage _ m => m
  | .badArgumentUsage m => m
  | .badParameter (some h) m => s!"Invalid value for {h}: {m}"
  | .badParameter none m => s!"Invalid value: {m}"
  | .missingParameter ty hint extra =>
    s!"Missing {ty}{if hint.isEmpty then "" else " " ++ hint}.{if extra.isEmpty then "" else " " ++ extra}"
  | .usage m => m
  | .noArgsIsHelp h => h

/-- `UsageError.show`: what goes to standard error. -/
def UsageError.render (e : UsageError) : String :=
  match e.kind with
  | .noArgsIsHelp h => h ++ "\n"
  | _ =>
    let pre := match e.ctx with
      | some c =>
        c.usage ++ "\n" ++
        (match c.helpHint with
         | some h => s!"Try '{c.commandPath} {h}' for help.\n"
         | none => "") ++ "\n"
      | none => ""
    pre ++ "Error: " ++ e.message ++ "\n"

/-- What a raised error prints. -/
def ClickError.render : ClickError → String
  | .usage e => e.render
  | .failure m => s!"Error: {m}\n"
  | .fileError f h => s!"Error: Could not open file {pyRepr f}: {h}\n"
  | .abort => "Aborted!\n"
  | .exit _ => ""

/-- How an invocation ended. -/
inductive Termination where
  | ok
  | halted (h : Halt)
  | raised (e : ClickError)

/-- The process exit code. -/
def Termination.exitCode : Termination → Nat
  | .ok => 0
  | .halted (.error _) => 2
  | .halted (.help _) => 0
  | .halted (.message _) => 0
  | .halted (.prompt _ _ _) => 1
  | .raised (.usage _) => 2
  | .raised (.failure _) => 1
  | .raised (.fileError _ _) => 1
  | .raised .abort => 1
  | .raised (.exit n) => n % 256

/-- An ending decided by the framework rather than by `ctx.exit(n)`. -/
def Termination.isFramework : Termination → Bool
  | .raised (.exit _) => false
  | _ => true

end LeanClick
