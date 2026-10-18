import LeanClick.RepoImpl.Parse

/-!
# Building commands

Smart constructors in click's decorator vocabulary. Declarations are parsed
the way `click.option` parses them: `"-p", "--port"` are spellings, a bare
identifier is the name, a `--reload` / `--no-reload` pair (one declaration, slash separated) declares a boolean flag
with its negative spelling, and the name otherwise comes from the longest
prefixed spelling (`--root-path` names `root_path`).
-/

namespace LeanClick

open Text

/-- Is the declaration a Python identifier (then it is the name)? -/
def isIdentifier (s : String) : Bool :=
  match s.toList with
  | [] => false
  | c :: cs => (c.isAlpha || c == '_') && cs.all (fun d => d.isAlphanum || d == '_')

/-- `Option._parse_decls`: the name, the primary spellings and the
secondary (`/`-separated) spellings. -/
def parseDecls (decls : List String) : String × List String × List String :=
  let step := fun (st : Option String × List String × List String × List (String × String)) (d : String) =>
    let (name, opts, sec, possible) := st
    if isIdentifier d then (some d, opts, sec, possible)
    else
      -- `decl.split(split_char, 1)`, by structural recursion on the
      -- characters so the kernel can evaluate it (`click_command`'s `decide`)
      let splitCh := match d.toList with | '/' :: _ => ';' | _ => '/'
      let rec go : List Char → List Char × Option (List Char)
        | [] => ([], none)
        | c :: rest => if c == splitCh then ([], some rest) else
          let (a, b) := go rest
          (c :: a, b)
      match go d.toList with
      | (firstCs, some secondCs) =>
        let first := String.ofList ((firstCs.reverse.dropWhile Char.isWhitespace).reverse)
        let second := String.ofList (secondCs.dropWhile Char.isWhitespace)
        let (opts, possible) := if first.isEmpty then (opts, possible)
          else (opts ++ [first], possible ++ [splitOpt first])
        let sec := if second.isEmpty then sec else sec ++ [second]
        (name, opts, sec, possible)
      | _ => (name, opts ++ [d], sec, possible ++ [splitOpt d])
  let (name, opts, sec, possible) := decls.foldl step (none, [], [], [])
  let best := possible.foldl (fun (b : Option (String × String)) x => match b with
    | none => some x
    | some y => if x.1.length > y.1.length then some x else some y) none
  let derived := match best with
    | some (_, v) => lower (replaceChar v '-' '_')
    | none => ""
  (name.getD derived, opts, sec)

namespace Param

/-- A value option, `click.option(*decls, type=...)`. -/
def opt (decls : List String) (type : ParamType := .string) (nargs : Nat := 1)
    (multiple : Bool := false) : Param :=
  let (name, opts, _) := parseDecls decls
  { name, decl := .option opts (.value nargs multiple), type }

/-- A boolean flag, `click.option(..., is_flag=True)`; a slash-separated declaration such as
`--reload` / `--no-reload` gives the negative spelling. -/
def flag (decls : List String) : Param :=
  let (name, opts, sec) := parseDecls decls
  { name, decl := .option opts (.boolFlag sec), type := .bool }

/-- A flag that stores a constant (`flag_value`). -/
def flagValue (decls : List String) (const : String) (type : ParamType := .string)
    (multiple : Bool := false) : Param :=
  let (name, opts, _) := parseDecls decls
  { name, decl := .option opts (.constFlag const multiple), type }

/-- A counting flag, `click.option(..., count=True)`. -/
def count (decls : List String) (bounds : IntBounds := countBounds) : Param :=
  let (name, opts, _) := parseDecls decls
  { name, decl := .option opts .count, type := .intRange bounds }

/-- A positional argument, `click.argument(name, ...)`. Required unless it
has a default or takes `nargs=-1`, as in click. -/
def arg (decl : String) (type : ParamType := .string) (nargs : ArgNargs := .exactly 1) : Param :=
  let name := lower (replaceChar decl '-' '_')
  { name, decl := .argument nargs, type,
    required := match nargs with | .exactly n => n > 0 | .star => false }

/-- `click.version_option(version)`. -/
def versionOption (version : String) (decls : List String := ["--version"])
    (template : String := "%(prog)s, version %(version)s") : Param :=
  -- the spellings are taken as given (no `/` pairs), so a command's table
  -- stays computable by the kernel (`click_command`'s `decide`)
  { name := "__version__", decl := .option decls (.boolFlag []), type := .bool,
    help := some "Show the version and exit.", eager := some (.version version template),
    exposeValue := false, allowFromAutoenv := false }

def withHelp (p : Param) (h : String) : Param := { p with help := some h }
/-- Set the default. An argument with a default is no longer required. -/
def withDefault (p : Param) (d : String) : Param :=
  { p with default := some [d], required := if p.isArgument then false else p.required }
def withDefaults (p : Param) (ds : List String) : Param :=
  { p with default := some ds, required := if p.isArgument then false else p.required }
def withEnvvar (p : Param) (vs : List String) : Param := { p with envvar := vs }
def requiredParam (p : Param) (b : Bool := true) : Param := { p with required := b }
def withMetavar (p : Param) (m : String) : Param := { p with metavar := some m }
def hide (p : Param) : Param := { p with hidden := true }
def showDefaultParam (p : Param) (b : Bool := true) : Param := { p with showDefault := if b then .yes else .no }
def showEnvvarParam (p : Param) : Param := { p with showEnvvar := true }
def withPrompt (p : Param) (text : String) (confirmation : Bool := false) (hideInput : Bool := false)
    (required : Bool := true) : Param :=
  { p with prompt := some { text, confirmation, hideInput, required } }
/-- `prompt=True`: the prompt text is the capitalised name. -/
def withNamePrompt (p : Param) : Param :=
  p.withPrompt (promptTextOf p.name)

/-- `click.confirmation_option()`: a `--yes` flag that asks
`Do you want to continue?` when absent and aborts on no; its value is not
passed to the callback. The spellings are taken as given (no `/` pairs).
-/
def confirmationOption (decls : List String := ["--yes"]) (name : String := "yes")
    (help : String := "Confirm the action without prompting.")
    (prompt : String := "Do you want to continue?") : Param :=
  { name, decl := .option decls (.boolFlag []), type := .bool, help := some help,
    prompt := some { text := prompt }, exposeValue := false,
    callback := some fun v => match v with
      | .bool false => .error .abort
      | v => .ok v }

/-- `click.password_option()`: `--password`, prompted for (`Password`)
with hidden input and a confirmation prompt. -/
def passwordOption (decls : List String := ["--password"]) (name : String := "password")
    (help : String := "") : Param :=
  { name, decl := .option decls (.value 1 false), type := .string,
    help := if help.isEmpty then none else some help,
    prompt := some { text := promptTextOf name, confirmation := true, hideInput := true } }

end Param

namespace Command
variable {κ : Type}

/-- `click.command`. -/
def command [Inhabited κ] (spec : CommandSpec) (callback : κ := default) : Command κ :=
  .mk spec false callback []

/-- `click.group`. -/
def group [Inhabited κ] (spec : CommandSpec) (subcommands : List (Command κ))
    (callback : κ := default)
    (_noNestedChain : spec.chain = false ∨ subcommands.all (fun c => !c.isGroup) = true := by
      first | exact Or.inl rfl | decide | fail "It is not possible to add a group to another group that is in chain mode.") :
    Command κ :=
  .mk spec true callback subcommands

end Command

end LeanClick
