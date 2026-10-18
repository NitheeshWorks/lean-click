import LeanClick.RepoImpl.Command

/-!
# Help and usage

The help page is rendered from `Command.allParams`, the same table the
scanner matches against (the CORBA `Console` idea: a command cannot document
one set of options and accept another). The layout follows click's
`HelpFormatter`: usage line, help text, positional arguments (when any is
documented), options, commands (for a group), epilog.

The page is assembled by plain concatenation (`concatS`) so that
`help_lists_every_param` can be proved by locating each row.
-/

namespace LeanClick

open Text

variable {σ : Type}

/-- Concatenate a list of strings (structural, for proofs). -/
def concatS : List String → String
  | [] => ""
  | s :: ss => s ++ concatS ss

/-- Python's `inspect.cleandoc`. -/
def cleandoc (doc : String) : String :=
  let lines := (expandTabs doc).splitOn "\n"
  let blank := fun (l : String) => l.toList.all Char.isWhitespace
  let indentOf := fun (l : String) => (l.toList.takeWhile (· == ' ')).length
  let rest := lines.drop 1
  let margins := (rest.filter (!blank ·)).map indentOf
  let margin := margins.foldl min (margins.headD 0)
  let first := (lines.headD "").toList.dropWhile Char.isWhitespace
  let rest := rest.map fun l => String.ofList (l.toList.drop margin)
  let all := String.ofList first :: rest
  let all := all.dropWhile blank
  let all := (all.reverse.dropWhile blank).reverse
  "\n".intercalate all

/-- `HelpFormatter.write_usage` at indent 0. -/
def renderUsage (width : Nat) (prog args : String) : String :=
  let pfx := "Usage: "
  let usagePrefix := pfx ++ prog ++ " "
  if args.isEmpty then pfx ++ prog ++ "\n"
  else if width ≥ usagePrefix.length + 20 then
    fill width usagePrefix (spaces usagePrefix.length) (expandTabs args) ++ "\n"
  else
    let ind := spaces (pfx.length + 4)
    usagePrefix ++ "\n" ++ fill width ind ind (expandTabs args) ++ "\n"

/-- What follows the term in a definition-list row: the padding (or a
line break when the term is too wide) and the wrapped description. -/
def rowTail (width indent firstCol : Nat) (row : String × String) : String :=
  let (first, second) := row
  if second.isEmpty then "\n"
  else
    let sep := if first.length + 2 ≤ firstCol then spaces (firstCol - first.length)
      else "\n" ++ spaces (firstCol + indent)
    let textWidth := max (width - firstCol - 2) 10
    let lines := splitLines (wrapParagraphs textWidth "" "" second)
    match lines with
    | [] => sep ++ "\n"
    | l :: ls => sep ++ l ++ "\n" ++ concatS (ls.map fun x => spaces (firstCol + indent) ++ x ++ "\n")

/-- One definition-list row (`write_dl`): the term verbatim at the
indent, then the wrapped description. -/
def renderRow (width indent firstCol : Nat) (row : String × String) : String :=
  spaces indent ++ row.1 ++ rowTail width indent firstCol row

/-- `write_dl` with `col_max=30`, `col_spacing=2`. -/
def renderDl (width indent : Nat) (rows : List (String × String)) : String :=
  let w := rows.foldl (fun m r => max m r.1.length) 0
  let firstCol := min w 30 + 2
  concatS (rows.map (renderRow width indent firstCol))

/-- A section: blank line, heading, rows indented by 2. -/
def renderSection (width : Nat) (name : String) (rows : List (String × String)) : String :=
  if rows.isEmpty then "" else "\n" ++ name ++ ":\n" ++ renderDl width 2 rows

/-- Whether to show the default of an option, and the text to show. -/
def Param.defaultText (p : Param) (s : Settings) : Option String :=
  let (show_, isStr) := match p.showDefault with
    | .text _ => (true, true)
    | .yes => (true, false)
    | .no => (false, false)
    | .auto => (s.showDefault.getD false, false)
  if isStr then
    match p.showDefault with
    | .text t => some ("(" ++ t ++ ")")
    | _ => none
  else if !show_ then none
  else
    match p.decl, p.default with
    | .option _ (.boolFlag sec), d =>
      let v := match d with
        | some [x] => (strToBool x).getD false
        | _ => false
      if !sec.isEmpty then
        let o := if v then p.primaryOpts.headD "" else sec.headD ""
        some (splitOpt o).2
      else if v then some "True" else none
    | .option _ .count, none => some "0"
    | _, none => none
    | _, some ds =>
      -- click shows `str()` of the converted default; fall back to the raw
      -- text when it does not convert (an invalid default still shows)
      let ds := ds.map fun d => match convert (fun _ => none) p.type d with
        | .ok v => p.type.show v
        | .error _ => d
      if p.multiNargs || (match p.decl with | .option _ (.value _ true) => true | .option _ (.constFlag _ true) => true | _ => false)
      then some (", ".intercalate ds)
      else match ds with
        | [""] => some "\"\""
        | [x] => some x
        | xs => some (", ".intercalate xs)

/-- The `[...]` extras of an option's help (`get_help_extra`). -/
def Param.helpExtra (p : Param) (s : Settings) : List String :=
  let env := if p.showEnvvar then
      let vs := if p.envvar.isEmpty then
          match s.autoEnvvarPrefix with
          | some pre => if p.allowFromAutoenv then [pre ++ "_" ++ upper p.name] else []
          | none => []
        else p.envvar
      if vs.isEmpty then [] else ["env var: " ++ ", ".intercalate vs]
    else []
  let dflt := match p.defaultText s with
    | some d => ["default: " ++ d]
    | none => []
  -- an unbounded range has nothing to say (click prints `x<=None`, UP-023)
  let range := match p.type, p.decl with
    | .intRange r, .option _ .count => if r.min == some 0 && r.max == none then [] else [r.describe]
    | .intRange r, _ => if r.min.isNone && r.max.isNone then [] else [r.describe]
    | .floatRange r, _ => if r.min.isNone && r.max.isNone then [] else [r.describe]
    | _, _ => []
  let req := if p.required then ["required"] else []
  env ++ dflt ++ range ++ req

/-- `Option.get_help_record`; `none` for a hidden option. -/
def Param.optionRecord (p : Param) (s : Settings) : Option (String × String) :=
  if p.hidden then none else
  let help := p.help.getD ""
  let extra := p.helpExtra s
  let help := if extra.isEmpty then help
    else
      let e := "[" ++ "; ".intercalate extra ++ "]"
      if help.isEmpty then e else help ++ "  " ++ e
  some (p.helpSpec, help)

/-- The rows of the Options section. -/
def Command.optionRows (cmd : Command σ) (s : Settings) : List (String × String) :=
  (cmd.allParams s).filterMap fun p => if p.isOption then p.optionRecord s else none

/-- The rows of the Positional arguments section (shown only when some
argument has help). -/
def Command.argumentRows (cmd : Command σ) (s : Settings) : List (String × String) :=
  let args := (cmd.allParams s).filter Param.isArgument
  if args.any (fun p => p.help.isSome) then args.map fun p => (p.helpSpec, p.help.getD "")
  else []

/-- `get_short_help_str`. -/
def Command.shortHelpStr (cmd : Command σ) (limit : Nat) : String :=
  match cmd.spec.shortHelp with
  | some t => strip (cleandoc t)
  | none => match cmd.spec.help with
    | some h => strip (shortHelp h limit)
    | none => ""

/-- The rows of the Commands section of a group. -/
def Command.commandRows (cmd : Command σ) (s : Settings) : List (String × String) :=
  if !cmd.isGroup then [] else
  let subs := cmd.sortedSubcommands.filter (fun c => !c.spec.hidden)
  let w := subs.foldl (fun m c => max m c.name.length) 0
  let limit := s.width - 6 - w
  subs.map fun c => (c.name, c.shortHelpStr limit)

/-- The help text paragraph. -/
def Command.helpTextPart (cmd : Command σ) (s : Settings) : String :=
  let text := match cmd.spec.help with
    | some h => ((cleandoc h).splitOn "\x0c").headD ""
    | none => ""
  if text.isEmpty then "" else "\n" ++ wrapParagraphs s.width "  " "  " text ++ "\n"

def Command.epilogPart (cmd : Command σ) (s : Settings) : String :=
  match cmd.spec.epilog with
  | some e => let t := cleandoc e; if t.isEmpty then "" else "\n" ++ wrapParagraphs s.width "  " "  " t ++ "\n"
  | none => ""

/-- The usage line of a command at a command path (`get_usage` buffer). -/
def Command.usageText (cmd : Command σ) (s : Settings) (path : String) : String :=
  renderUsage s.width path (" ".intercalate (cmd.usagePieces s))

/-- `get_usage`. -/
def Command.getUsage (cmd : Command σ) (s : Settings) (path : String) : String :=
  rstripNewlines (cmd.usageText s path)

/-- The help page buffer (`format_help`). -/
def Command.formatHelp (cmd : Command σ) (s : Settings) (path : String) : String :=
  cmd.usageText s path ++ cmd.helpTextPart s ++
  renderSection s.width "Positional arguments" (cmd.argumentRows s) ++
  renderSection s.width "Options" (cmd.optionRows s) ++
  renderSection s.width "Commands" (cmd.commandRows s) ++
  cmd.epilogPart s

/-- `get_help`: the page with trailing newlines removed. -/
def Command.getHelp (cmd : Command σ) (s : Settings) (path : String) : String :=
  rstripNewlines (cmd.formatHelp s path)

/-- The context information a usage error is rendered with. -/
def Command.ctxInfo (cmd : Command σ) (s : Settings) (path : String) : CtxInfo where
  commandPath := path
  usage := cmd.getUsage s path
  helpHint := if cmd.spec.addHelpOption then
      (cmd.helpNames s).foldl (fun (b : Option String) n => match b with
        | some m => if n.length > m.length then some n else some m
        | none => some n) none
    else none

end LeanClick
