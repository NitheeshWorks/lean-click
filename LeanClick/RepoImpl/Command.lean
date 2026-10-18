import LeanClick.RepoImpl.Scan

/-!
# Commands: effective settings, the parameter table, metavars

`Command.allParams` is *the* table: the declared parameters plus the help
option. The scanner's spelling table, the processing loop, the usage line
and the help page are all computed from it.
-/

namespace LeanClick

open Text

variable {σ : Type}

/-- The subcommands by name, as `Group.list_commands` returns them. -/
def Command.sortedSubcommands (cmd : Command σ) : List (Command σ) :=
  let rec ins (c : Command σ) : List (Command σ) → List (Command σ)
    | [] => [c]
    | d :: ds => if decide (c.name ≤ d.name) then c :: d :: ds else d :: ins c ds
  cmd.subcommands.foldr (fun c acc => ins c acc) []

/-- The help option: an eager boolean flag that never exposes a value. -/
def helpParam (names : List String) : Param where
  name := "__help__"
  decl := .option names (.boolFlag [])
  type := .bool
  help := some "Show this message and exit."
  eager := some .help
  exposeValue := false
  allowFromAutoenv := false

/-- The help option names left after removing those a parameter already
uses (`get_help_option_names`). -/
def Command.helpNames (cmd : Command σ) (s : Settings) : List String :=
  s.helpOptionNames.filter fun n => !(cmd.spec.params.any fun p => p.allOpts.contains n)

/-- A parameter with its default taken from the context's `default_map`
when the map names it (click's `Context.lookup_default`). -/
def Param.withDefaultMap (dm : Option DefaultMap) (p : Param) : Param :=
  match dm.bind (·.value? p.name) with
  | some ds => { p with default := some ds, fromDefaultMap := true }
  | none => p

/-- The parameter table: declared parameters (with `default_map` defaults),
then the help option. -/
def Command.allParams (cmd : Command σ) (s : Settings) : List Param :=
  let names := cmd.helpNames s
  cmd.spec.params.map (Param.withDefaultMap s.defaultMap) ++
    (if cmd.spec.addHelpOption && !names.isEmpty then [helpParam names] else [])

/-- The settings of a context for `cmd`, invoked as `infoName`, under a
parent context with settings `parent` (click's `Context.__init__`
inheritance rules). -/
def Command.settings (cmd : Command σ) (parent : Option Settings) (infoName : String) : Settings :=
  let base := parent.getD {}
  let norm := fun (s : String) => replaceChar (upper s) '-' '_'
  let auto := match cmd.spec.autoEnvvarPrefix with
    | some p => some (norm p)
    | none => match parent with
      | some ps => ps.autoEnvvarPrefix.map fun p => norm (p ++ "_" ++ infoName)
      | none => none
  { helpOptionNames := cmd.spec.helpOptionNames.getD base.helpOptionNames
    autoEnvvarPrefix := auto
    showDefault := match cmd.spec.showDefault with
      | some b => some b
      | none => base.showDefault
    width := base.width
    defaultMap := match cmd.spec.defaultMap with
      | some m => some m
      | none => match parent with
        | some ps => ps.defaultMap.bind (·.sub? infoName)
        | none => none }

def Command.allowExtraArgs (cmd : Command σ) : Bool := cmd.spec.allowExtraArgs.getD cmd.isGroup
def Command.interspersed (cmd : Command σ) : Bool := cmd.spec.allowInterspersedArgs.getD (!cmd.isGroup)
def Command.noArgsIsHelp (cmd : Command σ) : Bool :=
  cmd.spec.noArgsIsHelp.getD (cmd.isGroup && !cmd.spec.invokeWithoutCommand)

/-- The scanner configuration of a command. -/
def Command.scanCfg (cmd : Command σ) (s : Settings) : ScanCfg where
  tbl := table (cmd.allParams s)
  interspersed := cmd.interspersed
  ignoreUnknown := cmd.spec.ignoreUnknownOptions
  prefixes := optPrefixes (table (cmd.allParams s))

/-! ## Metavars, hints, spellings in help -/

/-- `Choice.get_metavar`. -/
def Param.typeMetavar (p : Param) : Option String :=
  match p.type with
  | .choice cs cS =>
    let inner := if p.isOption && !p.showChoices then "TEXT"
      else "|".intercalate (cs.map (normChoice cS))
    if p.required && p.isArgument then some ("{" ++ inner ++ "}") else some ("[" ++ inner ++ "]")
  | .dateTime fs => some ("[" ++ "|".intercalate fs ++ "]")
  | _ => none

/-- Does the parameter take more than one value per occurrence? -/
def Param.multiNargs (p : Param) : Bool :=
  match p.decl with
  | .option _ (.value n _) => n != 1
  | .option _ _ => false
  | .argument (.exactly n) => n != 1
  | .argument .star => true

/-- `make_metavar`. -/
def Param.makeMetavar (p : Param) : String :=
  match p.metavar with
  | some m => m
  | none =>
    if p.isOption then
      (p.typeMetavar.getD (upper p.type.name)) ++ (if p.multiNargs then "..." else "")
    else
      let var := p.typeMetavar.getD (upper p.name)
      let bracketed := var.startsWith "[" && var.endsWith "]"
      let var := if !p.required && !bracketed then "[" ++ var ++ "]" else var
      var ++ (if p.multiNargs then "..." else "")

def Param.primaryOpts (p : Param) : List String :=
  match p.decl with
  | .option opts _ => opts
  | .argument _ => []

def Param.isFlagLike (p : Param) : Bool :=
  match p.decl with
  | .option _ (.value _ _) => false
  | .option _ (.optValue _) => false
  | .option _ _ => true
  | .argument _ => false

def Param.secondaryOpts (p : Param) : List String :=
  match p.decl with
  | .option _ (.boolFlag sec) => sec
  | _ => []

/-- How a list of environment variable names is printed (Python's `str`). -/
def showEnvvars (vs : List String) : String :=
  match vs with
  | [v] => v
  | _ => "[" ++ ", ".intercalate (vs.map pyRepr) ++ "]"

/-- `get_error_hint`. -/
def Param.errorHint (p : Param) : String :=
  if p.isOption then
    let base := " / ".intercalate (p.primaryOpts.map fun o => "'" ++ o ++ "'")
    if p.showEnvvar && !p.envvar.isEmpty then base ++ s!" (env var: '{showEnvvars p.envvar}')"
    else base
  else "'" ++ p.makeMetavar ++ "'"

/-- Insertion before the first element with a key at least as large. -/
def insertBy {α : Type} (key : α → Nat) (x : α) : List α → List α
  | [] => [x]
  | y :: ys => if key x ≤ key y then x :: y :: ys else y :: insertBy key x ys

/-- Stable sort by a natural-number key. -/
def sortBy {α : Type} (key : α → Nat) (xs : List α) : List α :=
  xs.foldr (fun x acc => insertBy key x acc) []

/-- Stable sort by prefix length, joined with `, ` (`join_options`). -/
def joinOptions (opts : List String) : String :=
  ", ".intercalate (sortBy (fun o => (splitOpt o).1.length) opts)

/-- `get_help_spec`: the left column of the help row. -/
def Param.helpSpec (p : Param) : String :=
  if p.isOption then
    let write := fun (opts : List String) =>
      joinOptions opts ++ (if p.isFlagLike then "" else " " ++ p.makeMetavar)
    let first := write p.primaryOpts
    -- click joins with `; ` when a spelling uses the `/` prefix (`/debug; /no-debug`)
    let slash := (p.primaryOpts ++ p.secondaryOpts).any fun o => (splitOpt o).1 == "/"
    if p.secondaryOpts.isEmpty then first
    else first ++ (if slash then "; " else " / ") ++ write p.secondaryOpts
  else p.makeMetavar

/-- `get_usage_pieces`. -/
def Param.usagePieces (p : Param) : List String :=
  if p.isArgument then [p.makeMetavar] else []

/-- The usage pieces after the command path (`collect_usage_pieces`). -/
def Command.usagePieces (cmd : Command σ) (s : Settings) : List String :=
  let opts := if cmd.spec.optionsMetavar.isEmpty then [] else [cmd.spec.optionsMetavar]
  let args := (cmd.allParams s).flatMap Param.usagePieces
  let sub := if cmd.isGroup then
      [cmd.spec.subcommandMetavar.getD
        (if cmd.spec.chain then
          (if cmd.spec.invokeWithoutCommand then "[COMMAND1] [ARGS]... [COMMAND2 [ARGS]...]..."
           else "COMMAND1 [ARGS]... [COMMAND2 [ARGS]...]...")
         else if cmd.spec.invokeWithoutCommand then "[COMMAND] [ARGS]..." else "COMMAND [ARGS]...")]
    else []
  opts ++ args ++ sub

/-- The command path of a subcommand context (`Context.command_path`):
the parent's path, the parent's argument metavars, then the name. -/
def Command.childPath (parent : Command σ) (s : Settings) (parentPath name : String) : String :=
  let pieces := (parent.allParams s).flatMap Param.usagePieces
  lstrip (" ".intercalate (parentPath :: pieces) ++ " " ++ name)

end LeanClick
