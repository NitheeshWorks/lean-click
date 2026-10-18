import LeanClick.APIs
import LeanClick.RepoImpl.Complete
import LeanClick.RepoImpl.Style
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

/-- What a command runs: its callback, and for a group that resolves
names itself (click's `Group.get_command` override, as the `aliases`
example does) the resolver. The resolver sees the group's context and the
unknown name, and returns the name of a subcommand, `none`, or fails
(`ctx.fail`). -/
structure Handlers (σ : Type) where
  run : Callback σ := fun c => pure c.obj
  resolve : Option (Context σ → String → ClickM (Option String)) := none

instance {σ : Type} : Inhabited (Handlers σ) := ⟨{}⟩

instance {σ : Type} : Coe (Callback σ) (Handlers σ) := ⟨fun f => { run := f }⟩

/-- A command whose callbacks run in `IO`. -/
abbrev App (σ : Type) := Command (Handlers σ)

/-- `click.command` with an `IO` callback. -/
def App.command {σ : Type} (spec : CommandSpec) (callback : Callback σ := fun c => pure c.obj) : App σ :=
  .mk spec false { run := callback } []

/-- `click.group` with an `IO` callback. -/
def App.group {σ : Type} (spec : CommandSpec) (subcommands : List (App σ))
    (callback : Callback σ := fun c => pure c.obj)
    (resolve : Option (Context σ → String → ClickM (Option String)) := none)
    (_noNestedChain : spec.chain = false ∨ subcommands.all (fun c => !c.isGroup) = true := by
      first | exact Or.inl rfl | decide | fail "It is not possible to add a group to another group that is in chain mode.") :
    App σ :=
  .mk spec true { run := callback, resolve } subcommands

/-- Does the stream show to a terminal? (click's `isatty`) -/
def isTty (err : Bool := false) : IO Bool := do
  let h ← if err then IO.getStderr else IO.getStdout
  h.isTty

/-- `click.echo`: write a line; ANSI styles are removed when the output is
not a terminal, unless `color` says otherwise (click's
`should_strip_ansi`). -/
def echo (msg : String) (err : Bool := false) (nl : Bool := true) (color : Option Bool := none) :
    IO Unit := do
  let h ← if err then IO.getStderr else IO.getStdout
  let keep ← match color with
    | some c => pure c
    | none => h.isTty
  let msg := if keep then msg else unstyle msg
  h.putStr (if nl then msg ++ "\n" else msg)
  h.flush

/-- `click.secho`: `echo (style ...)`. -/
def secho (msg : String) (fg : Option Color := none) (bg : Option Color := none)
    (bold : Option Bool := none) (underline : Option Bool := none) (err : Bool := false)
    (nl : Bool := true) : IO Unit :=
  echo (style msg fg bg bold (underline := underline)) err nl

/-- `click.format_filename`: how a file name is shown. -/
def formatFilename (name : String) : String := name

/-- `click.clear`: clear the screen (only on a terminal). -/
def clear : IO Unit := do
  if ← isTty then
    let h ← IO.getStdout
    h.putStr "\x1b[2J\x1b[1;1H"
    h.flush

/-- Read one key from the terminal (`click.getchar`), via `/dev/tty` in raw
mode, as click does on POSIX. -/
def getchar (echoIt : Bool := false) : IO String := do
  let tty ← IO.FS.Handle.mk "/dev/tty" .read
  let _ ← IO.Process.output { cmd := "stty", args := #["-F", "/dev/tty", "raw", "-echo"] }
  let bytes ← tty.read 32
  let _ ← IO.Process.output { cmd := "stty", args := #["-F", "/dev/tty", "sane"] }
  let ch := (String.fromUTF8? bytes).getD ""
  if echoIt then echo ch (nl := false)
  return ch

/-- `click.pause`: wait for a key, only when stdin and stdout are terminals. -/
def pause (info : String := "Press any key to continue...") : IO Unit := do
  let stdin ← IO.getStdin
  if (← stdin.isTty) && (← isTty) then
    echo info (nl := false)
    let _ ← getchar
    echo ""

/-- `click.echo_via_pager`: through `$PAGER` (else `less`) on a terminal,
straight to stdout otherwise. -/
def echoViaPager (text : String) : IO Unit := do
  if !(← isTty) then
    echo text
    return
  let pager := (← IO.getEnv "PAGER").getD "less"
  let child ← IO.Process.spawn { cmd := "sh", args := #["-c", pager], stdin := .piped }
  let (stdin, child) ← child.takeStdin
  stdin.putStr (text ++ "\n")
  stdin.flush
  let _ ← child.wait

/-- `click.launch`: open a URL or file with the desktop's handler
(`xdg-open`, or `open` on macOS); `locate` opens the containing folder. -/
def launch (url : String) (locate : Bool := false) : IO UInt32 := do
  let target := if locate then ((System.FilePath.mk url).parent.map (·.toString)).getD "." else url
  let opener := if System.Platform.isOSX then "open" else "xdg-open"
  try
    let child ← IO.Process.spawn { cmd := opener, args := #[target] }
    child.wait
  catch _ => return 127

/-- The editor `click.edit` runs: `editor`, else `$VISUAL`, else `$EDITOR`,
else the first of `editor`, `nano`, `vim` on the path, else `vi`
(click's `Editor.get_editor`). -/
def getEditor (editor : Option String := none) : IO String := do
  if let some e := editor then return e
  if let some e ← IO.getEnv "VISUAL" then return e
  if let some e ← IO.getEnv "EDITOR" then return e
  let path := ((← IO.getEnv "PATH").getD "").splitOn ":"
  for e in ["editor", "nano", "vim"] do
    for d in path do
      if !d.isEmpty && (← (System.FilePath.mk d / e).pathExists) then return e
  return "vi"

/-- `click.edit(text)`: write the text (with a final newline) to a
temporary file, run the editor on it, and return what it holds, or `none`
when the editor did not save (`require_save`). The editor command is split
the way a POSIX shell splits words, without a shell (`shlex.split`); a
failing editor is `ClickException("<editor>: Editing failed")`. -/
def edit (text : String := "") (editor : Option String := none) (requireSave : Bool := true)
    (extension : String := ".txt") : IO (Option String) := do
  let editor ← getEditor editor
  let text := if !text.isEmpty && !text.endsWith "\n" then text ++ "\n" else text
  let (h, tmp) ← IO.FS.createTempFile
  h.putStr text
  h.flush
  let path := System.FilePath.mk (tmp.toString ++ extension)
  IO.FS.rename tmp path
  try
    let before := (← path.metadata).modified
    let argv := splitArgString editor ++ [path.toString]
    let code ← try
        let child ← IO.Process.spawn { cmd := argv.headD editor, args := (argv.drop 1).toArray }
        child.wait
      catch e => throw (IO.userError s!"{editor}: Editing failed: {e}")
    if code != 0 then throw (IO.userError s!"{editor}: Editing failed")
    if requireSave && (← path.metadata).modified == before then return none
    return some ((← IO.FS.readFile path).replace "\r\n" "\n")
  finally
    try IO.FS.removeFile path catch _ => pure ()

/-- click's `progressbar(...)` settings (`termui.progressbar`'s defaults). -/
structure ProgressBar (α : Type) where
  items : List α
  length : Option Nat := some items.length
  label : String := ""
  hidden : Bool := false
  showEta : Bool := true
  showPercent : Option Bool := none
  showPos : Bool := false
  itemShowFunc : Option (Option α → Option String) := none
  fillChar : String := "#"
  emptyChar : String := "-"
  barTemplate : String := "%(label)s  [%(bar)s]  %(info)s"
  infoSep : String := "  "
  /-- `0` fills the terminal width. -/
  width : Nat := 36
  updateMinSteps : Nat := 1

/-- The running state of a bar (click's `ProgressBar` attributes). -/
structure BarState (α : Type) where
  pos : Float := 0
  avg : List Float := []
  start : Float
  lastEta : Float
  etaKnown : Bool := false
  finished : Bool := false
  maxWidth : Option Nat := none
  current : Option α := none
  lastLine : Option String := none
  completed : Float := 0
  width : Nat

/-- A progress bar in use (inside `withProgressbar`). -/
structure Bar (α : Type) where
  cfg : ProgressBar α
  state : IO.Ref (BarState α)
  tty : Bool

/-- `click.progressbar(iterable, ...)`. -/
def progressbar {α : Type} (items : List α) (label : String := "") (hidden : Bool := false)
    (showEta : Bool := true) (showPercent : Option Bool := none) (showPos : Bool := false)
    (itemShowFunc : Option (Option α → Option String) := none) (fillChar : String := "#")
    (emptyChar : String := "-") (barTemplate : String := "%(label)s  [%(bar)s]  %(info)s")
    (infoSep : String := "  ") (width : Nat := 36) (updateMinSteps : Nat := 1) : ProgressBar α :=
  { items, label, hidden, showEta, showPercent, showPos, itemShowFunc, fillChar, emptyChar,
    barTemplate, infoSep, width, updateMinSteps }

/-- `click.progressbar(length=n, ...)`: the items are `0, ..., n - 1`. -/
def progressbarLength (length : Nat) (label : String := "") (hidden : Bool := false)
    (showEta : Bool := true) (showPercent : Option Bool := none) (showPos : Bool := false)
    (fillChar : String := "#") (emptyChar : String := "-")
    (barTemplate : String := "%(label)s  [%(bar)s]  %(info)s") (infoSep : String := "  ")
    (width : Nat := 36) (updateMinSteps : Nat := 1) : ProgressBar Nat :=
  { items := List.range length, length := some length, label, hidden, showEta, showPercent,
    showPos, fillChar, emptyChar, barTemplate, infoSep, width, updateMinSteps }

namespace Bar
variable {α : Type}

private def now : IO Float := do return (← IO.monoNanosNow).toFloat / 1e9

private def pct (b : Bar α) (st : BarState α) : Float :=
  if st.finished then 1.0
  else
    let len := (b.cfg.length.getD 1).toFloat
    min (st.pos / (if len == 0 then 1 else len)) 1.0

private def two (n : Nat) : String := if n < 10 then s!"0{n}" else toString n

private def formatEta (b : Bar α) (st : BarState α) : String :=
  if !st.etaKnown then "" else
  let tpi := if st.avg.isEmpty then 0 else st.avg.foldl (· + ·) 0 / st.avg.length.toFloat
  let eta := match b.cfg.length with
    | some l => if st.finished then 0 else tpi * (l.toFloat - st.pos)
    | none => 0
  let t := eta.toUInt64.toNat
  let (s, t) := (t % 60, t / 60)
  let (m, t) := (t % 60, t / 60)
  let (h, d) := (t % 24, t / 24)
  if d > 0 then s!"{d}d {two h}:{two m}:{two s}" else s!"{two h}:{two m}:{two s}"

private def formatPos (b : Bar α) (st : BarState α) : String :=
  let p := if st.pos == st.pos.floor then toString st.pos.toUInt64 else toString st.pos
  match b.cfg.length with
  | some l => s!"{p}/{l}"
  | none => p

private def formatBar (b : Bar α) (st : BarState α) : String :=
  let rep := fun (n : Nat) (c : String) => String.join (List.replicate n c)
  match b.cfg.length with
  | some _ =>
    let n := (pct b st * st.width.toFloat).toUInt64.toNat
    rep n b.cfg.fillChar ++ rep (st.width - n) b.cfg.emptyChar
  | none => if st.finished then rep st.width b.cfg.fillChar else rep (max st.width 1) b.cfg.emptyChar

private def formatLine (b : Bar α) (st : BarState α) : String :=
  let showPercent := match b.cfg.showPercent, b.cfg.length with
    | none, some _ => !b.cfg.showPos
    | p, _ => p.getD false
  let pctS := let n := (pct b st * 100).toUInt64.toNat
    let s := toString n
    String.ofList (List.replicate (3 - min 3 s.length) ' ') ++ s ++ "%"
  let bits := (if b.cfg.showPos then [formatPos b st] else []) ++ (if showPercent then [pctS] else []) ++
    (if b.cfg.showEta && st.etaKnown && !st.finished then [formatEta b st] else []) ++
    (match b.cfg.itemShowFunc with
      | some f => (f st.current).toList
      | none => [])
  let line := ((b.cfg.barTemplate.replace "%(label)s" b.cfg.label).replace "%(bar)s" (formatBar b st)).replace
    "%(info)s" (b.cfg.infoSep.intercalate bits)
  String.ofList (line.toList.reverse.dropWhile Char.isWhitespace).reverse

/-- click's `render_progress`: off a terminal, the label once. -/
def render (b : Bar α) : IO Unit := do
  if b.cfg.hidden then return
  let st ← b.state.get
  if !b.tty then
    if st.lastLine != some b.cfg.label then
      b.state.set { st with lastLine := some b.cfg.label }
      echo b.cfg.label
    return
  let mut st := st
  let mut buf := ""
  if b.cfg.width == 0 then
    let clutter := (unstyle (formatLine b { st with width := 0 })).length
    let cols := ((← IO.getEnv "COLUMNS").bind String.toNat?).getD 80
    let newWidth := cols - clutter
    if newWidth < st.width && st.maxWidth.isSome then
      buf := buf ++ "\r" ++ String.ofList (List.replicate (st.maxWidth.getD 0) ' ')
      st := { st with maxWidth := some newWidth }
    st := { st with width := newWidth }
  let clearWidth := st.maxWidth.getD st.width
  let line := formatLine b st
  let len := (unstyle line).length
  if st.maxWidth.isNone || st.maxWidth.getD 0 < len then st := { st with maxWidth := some len }
  let out := buf ++ "\r" ++ line ++ String.ofList (List.replicate (clearWidth - len) ' ')
  if st.lastLine != some out then
    st := { st with lastLine := some out }
    echo out (nl := false)
  b.state.set st

private def makeStep (b : Bar α) (n : Float) : IO Unit := do
  let st ← b.state.get
  let pos := st.pos + n
  let finished := st.finished || (match b.cfg.length with | some l => pos ≥ l.toFloat | none => false)
  let t ← now
  if t - st.lastEta < 1.0 then
    b.state.set { st with pos, finished }
    return
  let step := if pos != 0 then (t - st.start) / pos else t - st.start
  b.state.set { st with pos, finished, lastEta := t, avg := st.avg.drop (st.avg.length - 6) ++ [step],
                        etaKnown := b.cfg.length.isSome }

/-- `bar.update(n_steps, current_item)`. -/
def update (b : Bar α) (n : Float) (item : Option α := none) : IO Unit := do
  b.state.modify fun st => { st with current := item.orElse fun _ => st.current, completed := st.completed + n }
  let st ← b.state.get
  if st.completed ≥ b.cfg.updateMinSteps.toFloat then
    makeStep b st.completed
    render b
    b.state.modify fun st => { st with completed := 0 }

/-- Iterating a bar yields its items and advances it after each one. -/
instance : ForIn IO (Bar α) α where
  forIn b init f := do
    let mut acc := init
    for x in b.cfg.items do
      if b.tty then
        b.state.modify fun st => { st with current := some x }
        if (← b.state.get).completed == 0 then render b
      match ← f x acc with
      | .done a => return a
      | .yield a =>
        acc := a
        if b.tty then update b 1
    return acc

end Bar

/-- `with click.progressbar(...) as bar:`: draw the bar, run the body,
then finish the line. -/
def withProgressbar {α β : Type} (cfg : ProgressBar α) (body : Bar α → IO β) : IO β := do
  let t ← Bar.now
  let tty ← isTty
  let state ← IO.mkRef ({ start := t, lastEta := t, width := cfg.width } : BarState α)
  let b : Bar α := { cfg, state, tty }
  b.render
  let r ← body b
  let st ← state.get
  if st.completed != 0 then
    Bar.makeStep b st.completed
    state.modify fun st => { st with completed := 0 }
    b.render
  if !cfg.hidden && tty then echo "" 
  return r

/-- `ctx.exit(code)`. -/
def exit (code : Nat := 0) : ClickM σ := throw (.exit code)

/-- `ctx.abort()`. -/
def abort : ClickM σ := throw .abort

/-- `ctx.fail(message)`: a usage error in the context. -/
def fail (ctx : Context σ) (message : String) : ClickM α :=
  throw (.usage ⟨.usage message, some ⟨ctx.commandPath, ctx.usage, some "--help"⟩⟩)

/-- `click.BadParameter(message, param_hint=...)` raised from a callback
or a command body; the running command's context is attached when it
reaches `main`, as click's `augment_usage_errors` does. -/
def badParameter (message : String) (paramHint : List String := []) : ClickError :=
  let hint := if paramHint.isEmpty then none else some (" / ".intercalate (paramHint.map pyRepr))
  .usage ⟨.badParameter hint message, none⟩

/-- `click.UsageError(message)` raised from a callback or a command body. -/
def usageError (message : String) : ClickError := .usage ⟨.usage message, none⟩

/-- A usage error raised without a context gets the running command's. -/
def _root_.LeanClick.ClickError.inContext (info : CtxInfo) : ClickError → ClickError
  | .usage ⟨k, none⟩ => .usage ⟨k, some info⟩
  | e => e

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
      | none => pure none
      | some ans =>
        let a := lower (strip ans)
        if a == "y" || a == "yes" then pure (some true)
        else if a == "n" || a == "no" then pure (some false)
        else if a.isEmpty && default.isSome then pure default
        else do echo "Error: invalid input"; loop f
  loop fuel

/-- click's `_mask_hidden_input`: a hidden answer is not echoed in an
error message, neither quoted (`repr`) nor bare between non-word
characters. -/
def maskHiddenInput (message value : String) : String :=
  let mask := "'***'"
  let message := message.replace (pyRepr value) mask
  if value.isEmpty then message else
  let word := fun (c : Char) => c.isAlphanum || c == '_'
  let v := value.toList
  let rec go : Nat → List Char → Option Char → List Char → List Char
    | 0, rest, _, acc => acc.reverse ++ rest
    | _, [], _, acc => acc.reverse
    | f + 1, c :: rest, prev, acc =>
      let cs := c :: rest
      if cs.take v.length == v && !(prev.map word).getD false &&
          !((cs.drop v.length).head?.map word).getD false then
        go f (cs.drop v.length) (v.getLast?) (mask.toList.reverse ++ acc)
      else go f rest (some c) (c :: acc)
  String.ofList (go (message.length + 1) message.toList none [])

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
  -- click's `value_proc`, the parameter's `process_value`: the type's
  -- conversion, then a custom type's and the callback's checks; a
  -- `BadParameter` re-asks, an abort ends the prompt
  let valueProc := fun (v : String) => (match convert fs p.type v with
    | .error msg => .error (some msg)
    | .ok c => match p.checks with
      | none => .ok ()
      | some f => match f (ParamType.erase p.type c) with
        | .error (.bad msg) => .error (some msg)
        | .error .abort => .error none
        | .ok _ => .ok () : Except (Option String) Unit)
  let read := fun (prompt : String) => do
    echo prompt (nl := false)
    match ← readLine with
    | none => do
      if ps.hideInput then echo "" (err := true)
      pure none
    | some v => pure (some v)
  -- the first answer: an empty one takes the default, or asks again
  let rec ask : Nat → IO (Option String)
    | 0 => pure none
    | f + 1 => do
      match ← read text with
      | none => pure none
      | some "" => match dflt with
        | some d => pure (some d)
        | none => ask f
      | some v => pure (some v)
  -- the confirmation: asked again while empty, unless both are empty
  let confirmText := s!"{ps.confirmationText.getD "Repeat for confirmation"}: "
  let rec again (v : String) : Nat → IO (Option String)
    | 0 => pure none
    | f + 1 => do
      match ← read confirmText with
      | none => pure none
      | some w => if !w.isEmpty || v.isEmpty then pure (some w) else again v f
  let rec loop : Nat → IO (Option String)
    | 0 => pure none
    | f + 1 => do
      match ← ask fuel with
      | none => pure none
      | some v =>
        match valueProc v with
        | .error none => pure none
        | .error (some msg) => do
          echo s!"Error: {if ps.hideInput then maskHiddenInput msg v else msg}"
          loop f
        | .ok () =>
          if !ps.confirmation then pure (some v)
          else match ← again v fuel with
            | none => pure none
            | some w =>
              if v == w then pure (some v)
              else do echo "Error: The two entered values do not match."; loop f
  loop fuel

/-- `os.path.normpath` of an absolute path: drop `.` and empty segments,
fold `..`. -/
def normalizePath (p : String) : String :=
  let segs := (p.splitOn "/").filter (fun x => !x.isEmpty && x != ".")
  let folded := segs.foldl (fun acc x => if x == ".." then acc.dropLast else acc ++ [x]) []
  "/" ++ "/".intercalate folded

/-- What the filesystem says about a path. Permission bits are not
available through Lean core, so readability, writability and
executability are assumed. -/
def statPath (s : String) (resolve : Bool := false) : IO (Option FileInfo) := do
  try
    let md ← System.FilePath.metadata s
    let real ← if resolve then (do return some (← IO.FS.realPath s).toString) else pure none
    return some { isFile := md.type == .file, isDir := md.type == .dir, realPath := real }
  catch _ =>
    if resolve then
      -- `os.path.realpath` of a missing path: made absolute and normalised
      let cwd ← IO.currentDir
      return some { isFile := false, isDir := false, exists_ := false,
                    realPath := some (normalizePath (if s.startsWith "/" then s else cwd.toString ++ "/" ++ s)) }
    else return none

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
  -- a chained group parses its subcommands in the same step, so read their
  -- variables too
  let subPs : List (Settings × Param) := if cmd.spec.chain then
      cmd.subcommands.flatMap fun c => let cs := c.settings (some s) c.name; (c.allParams cs).map (cs, ·)
    else []
  let ps := cmd.allParams s ++ subPs.map (·.2)
  let names := ((cmd.allParams s).flatMap (envNames s) ++ subPs.flatMap fun (cs, p) => envNames cs p).eraseDups
  let mut vars : List (String × String) := []
  for n in names do
    if let some v ← IO.getEnv n then vars := vars ++ [(n, v)]
  let needsFs := ps.any fun p => match p.type with | .path _ | .file _ => true | _ => false
  let resolve := ps.any fun p => match p.type with | .path sp => sp.resolvePath | _ => false
  let mut table : Std.HashMap String FileInfo := {}
  if needsFs then
    for c in pathCandidates ps argv (vars.map (·.2)) do
      if let some info ← statPath c resolve then table := table.insert c info
  let tbl := table
  return { vars, fs := fun p => tbl.get? p, answers, width := s.width }

/-- Parse one level, prompting for missing values until the parser has
every answer it asked for. `none` means the user aborted a prompt. -/
def parseInteractive (cmd : App σ) (s : Settings) (path : String) (argv : List String)
    (alias : String → Option String) :
    Nat → List ((String × String) × String) → IO (Option (Except Stop (Level (Handlers σ))))
  | 0, _ => pure (some (.error (.error ⟨.usage "Too many prompts.", none⟩)))
  | f + 1, answers => do
    let env ← levelEnv cmd s argv answers
    match cmd.parseLevelWith alias s path argv env with
    | .error (.prompt ppath p d) =>
      match ← promptParam env.fs p d with
      | some a => parseInteractive cmd s path argv alias f (answers ++ [((ppath, p.name), a)])
      | none => pure none
    | r => pure (some r)

/-- Parse a level, asking a group's resolver (a custom `get_command`) for
each subcommand name the group does not know. -/
def parseResolving (cmd : App σ) (s : Settings) (path infoName : String) (argv : List String)
    (parentCtx : Option (Context σ)) (obj : σ) (budget : Nat) :
    Nat → List (String × String) → IO (Except ClickError (Option (Except Stop (Level (Handlers σ)))))
  | 0, _ => pure (.ok (some (.error (.error ⟨.usage "Too many aliases.", none⟩))))
  | f + 1, aliases => do
    let alias := fun n => aliases.lookup n
    let r ← parseInteractive cmd s path argv alias budget []
    match r, cmd.callback.resolve with
    | some (.error (.error { kind := .noSuchCommand name _, ctx := _ })), some resolve =>
      let env ← levelEnv cmd s argv []
      match cmd.parseArgs s path argv env with
      | .error _ => pure (.ok r)
      | .ok pa =>
        let gctx : Context σ :=
          { infoName, commandPath := path, params := pa, parent := parentCtx, obj,
            help := cmd.getHelp s path, usage := cmd.getUsage s path }
        match ← (resolve gctx name).run with
        | .error e => pure (.error e)
        | .ok none => pure (.ok r)
        | .ok (some real) => parseResolving cmd s path infoName argv parentCtx obj budget f (aliases ++ [(name, real)])
    | _, _ => pure (.ok r)

/-- Run an invocation level by level: parse, run the callback, descend. -/
def runAux (prog : String) :
    Nat → App σ → Option Settings → String → String → List String →
    Option (Context σ) → σ → IO Termination
  | 0, _, _, _, _, _, _, _ => pure (.halted (.error ⟨.usage "Command nesting exceeds the argument count.", none⟩))
  | fuel + 1, cmd, parent, infoName, path, argv, parentCtx, obj => do
    let s := cmd.settings parent infoName
    let budget := (cmd.allParams s).length + 1 +
      (if cmd.spec.chain then argv.length * (cmd.subcommands.foldl (fun m c => max m c.spec.params.length) 0 + 1) else 0)
    match ← parseResolving cmd s path infoName argv parentCtx obj budget (argv.length + 1) [] with
    | .error e => pure (.raised e)
    | .ok none => pure (.raised .abort)
    | .ok (some (.error st)) =>
      match st.toHalt cmd s path prog with
      | .prompt _ _ _ => pure (.raised .abort)
      | h => pure (.halted h)
    | .ok (some (.ok lv)) =>
      let ctx : Context σ :=
        { infoName, commandPath := path, params := lv.args, parent := parentCtx, obj,
          invokedSubcommand := match lv.sub with
            | some (_, name, _) => some name
            | none => if lv.chained.isEmpty then none else some "*",
          help := cmd.getHelp s path, usage := cmd.getUsage s path }
      match ← (cmd.callback.run ctx).run with
      | .error e => pure (.raised (e.inContext (cmd.ctxInfo s path)))
      | .ok obj' =>
        match lv.sub with
        | none =>
          -- a chained group: every subcommand, in order, under the group's context
          let rec runChain : List (App σ × String × Settings × String × ParsedArgs) → IO Termination
            | [] => pure .ok
            | (c, name, cs, cpath, pa) :: more => do
              let cctx : Context σ :=
                { infoName := name, commandPath := cpath, params := pa, parent := some ctx, obj := obj',
                  help := c.getHelp cs cpath, usage := c.getUsage cs cpath }
              match ← (c.callback.run cctx).run with
              | .error e => pure (.raised (e.inContext (c.ctxInfo cs cpath)))
              | .ok _ => runChain more
          runChain lv.chained
        | some (c, name, rest) =>
          runAux prog fuel c (some s) name (cmd.childPath s path name) rest (some ctx) obj'

/-- Print how an invocation ended. -/
def report : Termination → IO Unit
  | .ok => pure ()
  | .halted (.error e) => echo e.render (err := true) (nl := false)
  | .halted (.help t) => echo t
  | .halted (.message t) => echo t
  | .halted (.prompt _ _ _) => echo "Aborted!" (err := true)
  | .halted .abort => echo "Aborted!" (err := true)
  | .raised e => echo e.render (err := true) (nl := false)

/-- The whole process environment, in order (`os.environ`). Lean core
reads single variables only, so this reads `/proc/self/environ`, and runs
`env -0` where that does not exist. -/
def environ : IO (List (String × String)) := do
  let raw ← try IO.FS.readBinFile "/proc/self/environ"
    catch _ => do
      let out ← IO.Process.output { cmd := "env", args := #["-0"] }
      pure out.stdout.toUTF8
  let text := (String.fromUTF8? raw).getD ""
  return (text.splitOn "\x00").filterMap fun kv =>
    match kv.splitOn "=" with
    | k :: rest => if k.isEmpty then none else some (k, "=".intercalate rest)
    | [] => none

/-- Shell completion (`_PROG_COMPLETE=bash_source|bash_complete|...`):
`some code` when the variable asked for it. -/
def shellComplete (cmd : App σ) (prog : String) : IO (Option UInt32) := do
  match ← IO.getEnv (completeVar prog) with
  | none => return none
  | some instr =>
    let (shell, what) := match instr.splitOn "_" with
      | sh :: rest => (sh, "_".intercalate rest)
      | [] => ("", "")
    if what == "source" then
      match completionScript shell prog with
      | some script => echo script (nl := false); return some 0
      | none => return some 1
    else if what == "complete" && (shell == "bash" || shell == "zsh" || shell == "fish") then
      let words := splitArgString ((← IO.getEnv "COMP_WORDS").getD "")
      let cwordVar := (← IO.getEnv "COMP_CWORD").getD ""
      let (args, incomplete) :=
        if shell == "fish" then
          let inc := if cwordVar.isEmpty then "" else (splitArgString cwordVar).headD ""
          let args := words.drop 1
          let args := if !inc.isEmpty && args.getLast? == some inc then args.dropLast else args
          (args, inc)
        else
          let cword := cwordVar.toNat?.getD 0
          ((words.drop 1).take (cword - 1), words.getD cword "")
      let items := cmd.complete prog args incomplete
        { vars := ← environ, cwd := (← IO.currentDir).toString, width := ← terminalWidth }
      echo ("\n".intercalate (items.map (CompletionItem.format shell)))
      return some 0
    else return some 1

/-- `Command.main`: run with `argv`, report, and return the exit code. -/
def main (cmd : App σ) (init : σ) (argv : List String) (prog : String := cmd.name)
    (defaultMap : Option DefaultMap := none) : IO UInt32 := do
  if let some code ← shellComplete cmd prog then return code
  let width ← terminalWidth
  let t ← try
      runAux prog (argv.length + 1) cmd (some (rootParent prog width defaultMap)) prog prog argv none init
    catch e => pure (.raised (.failure (toString e)))
  report t
  return UInt32.ofNat t.exitCode

/-- `hello()` in click: run the command with the process arguments. -/
def _root_.LeanClick.Command.run {σ : Type} (cmd : App σ) (init : σ) (argv : List String)
    (prog : String := cmd.name) : IO UInt32 :=
  Terminal.main cmd init argv prog

/-- Open a `File` value as a stream (`-` is standard input, or standard
output for a writing mode), as click's `File.convert` does; the handle
reads and writes bytes (`h.read 1024`, `h.write chunk`). -/
def openFile (f : FileArg) : ClickM IO.FS.Stream := do
  let writing := f.mode.toList.any (fun c => c == 'w' || c == 'a' || c == 'x')
  if f.name == "-" then
    return ← if writing then IO.getStdout else IO.getStdin
  let mode : IO.FS.Mode :=
    if f.mode.toList.contains 'a' then .append
    else if f.mode.toList.contains 'x' then .writeNew
    else if writing then .write
    else if f.mode.toList.contains '+' then .readWrite
    else .read
  match ← (IO.FS.Handle.mk f.name mode).toBaseIO with
  | .ok h => pure (IO.FS.Stream.ofHandle h)
  | .error e => throw (.fileError f.name (toString e))

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
