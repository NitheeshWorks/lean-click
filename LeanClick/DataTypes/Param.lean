import LeanClick.DataTypes.Types

/-!
# Parameters, commands, contexts, errors

The declarative table a command is built from. The parser, the help
renderer and the error messages all read this one table, which is what lets
`help_lists_every_param` and `undeclared_option_rejected` be stated against
the same data.

Option shapes are a sum, not a bag of booleans: a value-taking option, a
boolean flag (with optional `--no-x` spellings), a constant flag, or a count.
A count with `multiple` or a secondary spelling on a non-boolean flag, which
upstream rejects at construction time, cannot be written down at all.
-/

namespace LeanClick

/-- How many values an argument takes. `star` is click's `nargs=-1`. -/
inductive ArgNargs where
  | exactly (n : Nat)
  | star
  deriving DecidableEq, Repr, Inhabited

/-- What an option does when it appears on the command line. -/
inductive OptKind where
  /-- Takes `nargs` values; with `multiple`, every occurrence is kept. -/
  | value (nargs : Nat := 1) (multiple : Bool := false)
  /-- A boolean flag. `secondary` holds the `--no-x` spellings that store
  `false`. -/
  | boolFlag (secondary : List String := [])
  /-- A flag that stores a constant (click's `flag_value`). -/
  | constFlag (const : String) (multiple : Bool := false)
  /-- Counts its occurrences (`-vvv`). -/
  | count
  /-- Takes one value, which may be left out: then it is `fallback`
  (click's `flag_value` on an option with `is_flag=False`), or, without a
  fallback, the option prompts (`prompt_required=False`). A following
  option-looking token is not taken as the value. -/
  | optValue (fallback : Option String)
  deriving DecidableEq, Repr, Inhabited

/-- A parameter is an option (with its spellings) or a positional argument. -/
inductive Decl where
  | option (opts : List String) (kind : OptKind)
  | argument (nargs : ArgNargs := .exactly 1)
  deriving DecidableEq, Repr, Inhabited

def Decl.isBoolFlag : Decl → Bool
  | .option _ (.boolFlag _) => true
  | _ => false

def Decl.isCount : Decl → Bool
  | .option _ .count => true
  | _ => false

/-- An eager parameter that stops processing: `--help`, or a message such as
`--version`. `version` substitutes `%(prog)s` and `%(version)s` in the
template, as click's `version_option` does. -/
inductive Eager where
  | help
  | version (version : String) (template : String := "%(prog)s, version %(version)s")
  deriving DecidableEq, Repr, Inhabited

/-- Whether to show the default in help. `auto` inherits the context setting. -/
inductive ShowDefault where
  | auto
  | yes
  | no
  | text (s : String)
  deriving DecidableEq, Repr, Inhabited

/-- `show_default=True`/`False`, or a text shown in its place. -/
instance : Coe Bool ShowDefault := ⟨fun b => if b then .yes else .no⟩
instance : Coe String ShowDefault := ⟨.text⟩

/-- Interactive prompting for an option whose value is missing. -/
structure PromptSpec where
  text : String
  confirmation : Bool := false
  /-- The confirmation prompt's own text (`confirmation_prompt="..."`);
  click's default is `Repeat for confirmation`. -/
  confirmationText : Option String := none
  hideInput : Bool := false
  /-- click's `prompt_required`. -/
  required : Bool := true
  deriving DecidableEq, Repr, Inhabited

/-- click's `prompt=` and `confirmation_prompt=` arguments: off, on with
the default text (`True`), or a text. -/
inductive PromptText where
  | off
  | auto
  | text (s : String)
  deriving DecidableEq, Repr, Inhabited

instance : Coe String PromptText := ⟨.text⟩
instance : Coe Bool PromptText := ⟨fun b => if b then .auto else .off⟩
instance : Coe (Option String) PromptText := ⟨fun o => match o with | some s => .text s | none => .off⟩

/-- The text of a prompt, with `auto` being the given default. -/
def PromptText.resolve (p : PromptText) (auto : String) : Option String :=
  match p with
  | .off => none
  | .auto => some auto
  | .text s => some s

/-- `prompt=True`'s text: the name with `_` as spaces, capitalised
(Python's `str.capitalize`). -/
def promptTextOf (name : String) : String :=
  let t := String.ofList (name.toList.map fun c => if c == '_' then ' ' else c)
  match t.toList with
  | c :: r => String.ofList (c.toUpper :: r.map Char.toLower)
  | [] => t

/-- The prompt a parameter declares from `prompt=`, `confirmation_prompt=`
and `hide_input=`. -/
def PromptText.spec (name : String) (prompt confirmation : PromptText) (hideInput : Bool) :
    Option PromptSpec :=
  (prompt.resolve (promptTextOf name)).map fun text =>
    { text, hideInput, confirmation := confirmation != .off,
      confirmationText := match confirmation with | .text s => some s | _ => none }

/-- What the filesystem says about a path (for `Path`/`File` checks). -/
structure FileInfo where
  isFile : Bool
  isDir : Bool
  readable : Bool := true
  writable : Bool := true
  executable : Bool := false
  /-- The real path (`os.path.realpath`), when known. -/
  realPath : Option String := none
  /-- `false` for a path that does not exist (known only for its real path). -/
  exists_ : Bool := true
  deriving DecidableEq, Repr, Inhabited

/-- click's `CompletionItem`: the value, the kind of completion the shell
should do (`plain`, `file`, `dir`), and help text. -/
structure CompletionItem where
  value : String
  type : String := "plain"
  help : Option String := none
  deriving DecidableEq, Repr, Inhabited

instance : Coe String CompletionItem := ⟨fun v => { value := v }⟩

/-- What a parameter callback can do with a value besides accept it:
reject it (`BadParameter`) or abort the program (`ctx.abort()`, as
`confirmation_option` does). -/
inductive CallbackError where
  | bad (message : String)
  | abort
  deriving DecidableEq, Repr, Inhabited

instance : Coe String CallbackError := ⟨.bad⟩

/-- Everything outside `argv` the parser may read: environment variables, a
view of the filesystem, prompt answers already collected, and the terminal
width. `LeanClick.Terminal` builds it; the parser only reads it. -/
structure Env where
  vars : List (String × String) := []
  fs : String → Option FileInfo := fun _ => none
  /-- Prompt answers keyed by `(command path, parameter name)`. -/
  answers : List ((String × String) × String) := []
  width : Nat := 80
  /-- The working directory, for `resolve_path`. -/
  cwd : String := ""

/-- Look up an environment variable; the first binding wins. -/
def Env.get (e : Env) (k : String) : Option String :=
  (e.vars.find? (fun kv => kv.1 == k)).map (·.2)

/-- One parameter. Defaults are raw strings, converted through `type` like
any other source (click converts its defaults too); a multi-value default is
the flat token list, chunked by `nargs`.

Invariants: a boolean flag has type `bool`; a count has an `IntRange`
type (`IntRange(min=0)` unless declared otherwise). -/
structure Param where
  name : String
  decl : Decl
  type : ParamType := .string
  required : Bool := false
  default : Option (List String) := none
  envvar : List String := []
  help : Option String := none
  metavar : Option String := none
  hidden : Bool := false
  showDefault : ShowDefault := .auto
  showEnvvar : Bool := false
  showChoices : Bool := true
  prompt : Option PromptSpec := none
  eager : Option Eager := none
  exposeValue : Bool := true
  allowFromAutoenv : Bool := true
  /-- click's parameter `callback`: sees the converted value, may replace it
  or reject it with a `BadParameter` message. -/
  callback : Option (Value → Except CallbackError Value) := none
  /-- A custom type's conversion (a `click.ParamType` subclass's
  `convert`), run on the value the parser's own type produced and before
  `callback`; `.bad msg` is the type's `self.fail(msg)`. -/
  typeCheck : Option (Value → Except CallbackError Value) := none
  /-- A default computed when the command runs (click's callable
  `default=`); `Terminal` evaluates it before parsing. -/
  dynamicDefault : Option (IO (List String)) := none
  /-- click's `shell_complete=`: completions for the incomplete word. -/
  shellComplete : Option (Env → String → List CompletionItem) := none
  /-- The default came from the context's `default_map`. -/
  fromDefaultMap : Bool := false
  h_bool : decl.isBoolFlag = true → type = .bool := by
    first | decide | (simp [Decl.isBoolFlag]; done) | (intro h; simp_all [Decl.isBoolFlag])
  h_count : decl.isCount = true → type.isIntRange = true := by
    first | decide | (simp [Decl.isCount, ParamType.isIntRange]; done) | (intro h; simp_all [Decl.isCount, ParamType.isIntRange])

/-- What runs on a converted value: a custom type's conversion, then the
parameter's callback (click's `process_value` order). -/
def Param.checks (p : Param) : Option (Value → Except CallbackError Value) :=
  match p.typeCheck, p.callback with
  | none, c => c
  | some t, none => some t
  | some t, some c => some fun v => t v >>= c

/-- Where a parameter's value came from (click's `ParameterSource`), in
precedence order. -/
inductive Source where
  | commandline
  | environment
  | defaultMap
  | prompt
  | default
  deriving DecidableEq, Repr, Inhabited

/-- The shape a parameter's value has, determined by its declaration. -/
inductive Shape where
  | single
  | tuple (n : Nat)
  | star
  | many (n : Nat)
  deriving DecidableEq, Repr, Inhabited

/-- A parsed value of type `t`, with its refinements. -/
inductive PValue (t : ParamType) where
  | none
  | one (x : t.Carrier)
  | tuple (xs : List t.Carrier)
  | many (xss : List (List t.Carrier))

/-- A value fits a shape: a `tuple n` value has exactly `n` items, and every
occurrence of a `many n` value has exactly `n` items. -/
def PValue.Fits {t : ParamType} : PValue t → Shape → Prop
  | .none, _ => True
  | .one _, .single => True
  | .tuple xs, .tuple n => xs.length = n
  | .tuple _, .star => True
  | .many xss, .many n => ∀ xs ∈ xss, xs.length = n
  | _, _ => False

/-- The shape a parameter's value takes. A `Tuple` value is one value (a
product), however many tokens it takes. -/
def Param.shape (p : Param) : Shape :=
  match p.decl with
  | .option _ (.value n false) => if n = 1 ∨ p.type.isComposite then .single else .tuple n
  | .option _ (.value n true) => if p.type.isComposite then .many 1 else .many n
  | .option _ (.boolFlag _) => .single
  | .option _ (.constFlag _ false) => .single
  | .option _ (.constFlag _ true) => .many 1
  | .option _ .count => .single
  | .option _ (.optValue _) => .single
  | .argument (.exactly n) => if n = 1 ∨ p.type.isComposite then .single else .tuple n
  | .argument .star => .star

/-- One parsed parameter: the declaration it answers, its typed value, the
source, and the proof that the value has the declared shape. -/
structure Entry where
  param : Param
  value : PValue param.type
  source : Source
  h_fits : value.Fits param.shape
  /-- The value a parameter callback replaced the converted one with. -/
  override : Option Value

/-- The parser's result for one command: an entry per parameter in
declaration order, and the arguments left over (for a group, the subcommand
and its arguments). -/
structure ParsedArgs where
  entries : List Entry
  extra : List String
  deriving Inhabited

/-- What a usage error knows about the command it happened in, for
rendering. -/
structure CtxInfo where
  commandPath : String
  usage : String
  /-- The longest help option name, for the `Try '... --help'` hint. -/
  helpHint : Option String
  deriving DecidableEq, Repr, Inhabited

/-- The kinds of usage error, one per click exception class. -/
inductive ErrorKind where
  | noSuchOption (name : String) (possibilities : List String)
  | noSuchCommand (name : String) (possibilities : List String)
  | badOptionUsage (name : String) (message : String)
  | badArgumentUsage (message : String)
  /-- `hint` is the quoted parameter spelling, `message` the type's text. -/
  | badParameter (hint : Option String) (message : String)
  /-- `paramType` is `option` or `argument`; `extra` the type's
  missing-value message (choices). -/
  | missingParameter (paramType : String) (hint : String) (extra : String)
  /-- `ctx.fail(...)`: extra arguments, missing command. -/
  | usage (message : String)
  /-- `no_args_is_help`: the message is the help page. -/
  | noArgsIsHelp (help : String)
  deriving DecidableEq, Repr, Inhabited

/-- click's `UsageError`: exit code 2. -/
structure UsageError where
  kind : ErrorKind
  ctx : Option CtxInfo := none
  deriving DecidableEq, Repr, Inhabited

/-- Errors a running callback can raise (click's exception classes that
reach `main`). -/
inductive ClickError where
  | usage (e : UsageError)
  /-- `ClickException(message)`: exit code 1. -/
  | failure (message : String)
  /-- `FileError`: exit code 1. -/
  | fileError (filename : String) (hint : String)
  /-- `Abort`: prints `Aborted!`, exit code 1. -/
  | abort
  /-- `Exit(code)`. -/
  | exit (code : Nat)
  deriving DecidableEq, Repr, Inhabited

/-- A context: the parsed parameters of one command invocation, its parent,
and the user object (click's `ctx.obj`). -/
structure Context (σ : Type) where
  infoName : String
  commandPath : String
  params : ParsedArgs
  parent : Option (Context σ)
  obj : σ
  invokedSubcommand : Option String := none
  /-- The rendered help page and usage line, for `ctx.get_help()`. -/
  help : String := ""
  usage : String := ""

/-- click's `default_map`: defaults by parameter name, and nested maps by
subcommand name. -/
inductive DefaultMap where
  | node (values : List (String × List String)) (subs : List (String × DefaultMap))
  deriving Inhabited

def DefaultMap.value? : DefaultMap → String → Option (List String)
  | .node vs _, n => (vs.find? (·.1 == n)).map (·.2)

def DefaultMap.sub? : DefaultMap → String → Option DefaultMap
  | .node _ subs, n => (subs.find? (·.1 == n)).map (·.2)

/-- Settings of a command (click's `Command` attributes and the
`context_settings` that are not inherited). -/
structure CommandSpec where
  name : String
  params : List Param := []
  help : Option String := none
  shortHelp : Option String := none
  epilog : Option String := none
  optionsMetavar : String := "[OPTIONS]"
  addHelpOption : Bool := true
  /-- `none` inherits the parent context's names (default `["--help"]`). -/
  helpOptionNames : Option (List String) := none
  noArgsIsHelp : Option Bool := none
  hidden : Bool := false
  allowExtraArgs : Option Bool := none
  allowInterspersedArgs : Option Bool := none
  ignoreUnknownOptions : Bool := false
  autoEnvvarPrefix : Option String := none
  showDefault : Option Bool := none
  /-- `context_settings={"default_map": ...}`. -/
  defaultMap : Option DefaultMap := none
  /-- Group only. -/
  invokeWithoutCommand : Bool := false
  /-- Group only: `chain=True`, several subcommands in one invocation. -/
  chain : Bool := false
  /-- Group only. -/
  subcommandMetavar : Option String := none
  /-- `deprecated=True` (or a reason): a warning on invocation and a
  `(DEPRECATED)` label in help. -/
  deprecated : Bool := false
  deprecatedReason : Option String := none

/-- A command or a group. `κ` is the type of its callback: the parser never
looks at it, so the pure layers are generic in `κ`, and only
`LeanClick.Terminal` fixes it to an `IO` action (`Terminal.App`). -/
inductive Command (κ : Type) where
  | mk (spec : CommandSpec) (isGroup : Bool) (callback : κ) (subcommands : List (Command κ))

namespace Command
variable {κ : Type}
def spec : Command κ → CommandSpec | .mk s _ _ _ => s
def isGroup : Command κ → Bool | .mk _ g _ _ => g
def callback : Command κ → κ | .mk _ _ c _ => c
def subcommands : Command κ → List (Command κ) | .mk _ _ _ s => s
def name (c : Command κ) : String := c.spec.name
end Command

/-- Context settings inherited from the parent context. -/
structure Settings where
  helpOptionNames : List String := ["--help"]
  autoEnvvarPrefix : Option String := none
  showDefault : Option Bool := none
  width : Nat := 80
  defaultMap : Option DefaultMap := none
  deriving Inhabited

end LeanClick
