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

/-- Interactive prompting for an option whose value is missing. -/
structure PromptSpec where
  text : String
  confirmation : Bool := false
  hideInput : Bool := false
  /-- click's `prompt_required`. -/
  required : Bool := true
  deriving DecidableEq, Repr, Inhabited

/-- One parameter. Defaults are raw strings, converted through `type` like
any other source (click converts its defaults too); a multi-value default is
the flat token list, chunked by `nargs`.

Invariants: a boolean flag has type `bool`; a count has type
`IntRange(min=0)`. -/
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
  h_bool : decl.isBoolFlag = true → type = .bool := by
    first | decide | simp [Decl.isBoolFlag] | (intro h; simp_all [Decl.isBoolFlag])
  h_count : decl.isCount = true → type = .intRange countBounds := by
    first | decide | simp [Decl.isCount] | (intro h; simp_all [Decl.isCount])

/-- Where a parameter's value came from (click's `ParameterSource`), in
precedence order. -/
inductive Source where
  | commandline
  | environment
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

/-- The shape a parameter's value takes. -/
def Param.shape (p : Param) : Shape :=
  match p.decl with
  | .option _ (.value n false) => if n = 1 then .single else .tuple n
  | .option _ (.value n true) => .many n
  | .option _ (.boolFlag _) => .single
  | .option _ (.constFlag _ false) => .single
  | .option _ (.constFlag _ true) => .many 1
  | .option _ .count => .single
  | .argument (.exactly n) => if n = 1 then .single else .tuple n
  | .argument .star => .star

/-- One parsed parameter: the declaration it answers, its typed value, the
source, and the proof that the value has the declared shape. -/
structure Entry where
  param : Param
  value : PValue param.type
  source : Source
  h_fits : value.Fits param.shape

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

/-- What the filesystem says about a path (for `Path`/`File` checks). -/
structure FileInfo where
  isFile : Bool
  isDir : Bool
  readable : Bool := true
  writable : Bool := true
  executable : Bool := false
  deriving DecidableEq, Repr, Inhabited

/-- Everything outside `argv` the parser may read: environment variables, a
view of the filesystem, prompt answers already collected, and the terminal
width. `LeanClick.Terminal` builds it; the parser only reads it. -/
structure Env where
  vars : List (String × String) := []
  fs : String → Option FileInfo := fun _ => none
  /-- Prompt answers keyed by `(command path, parameter name)`. -/
  answers : List ((String × String) × String) := []
  width : Nat := 80

/-- Look up an environment variable; the first binding wins. -/
def Env.get (e : Env) (k : String) : Option String :=
  (e.vars.find? (fun kv => kv.1 == k)).map (·.2)

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
  /-- Group only. -/
  invokeWithoutCommand : Bool := false
  /-- Group only. -/
  subcommandMetavar : Option String := none

/-- A command or a group. The callback receives the context and returns the
object handed to the subcommand's context. -/
inductive Command (σ : Type) where
  | mk (spec : CommandSpec) (isGroup : Bool)
      (callback : Context σ → ExceptT ClickError IO σ) (subcommands : List (Command σ))

namespace Command
variable {σ : Type}
def spec : Command σ → CommandSpec | .mk s _ _ _ => s
def isGroup : Command σ → Bool | .mk _ g _ _ => g
def callback : Command σ → Context σ → ExceptT ClickError IO σ | .mk _ _ c _ => c
def subcommands : Command σ → List (Command σ) | .mk _ _ _ s => s
def name (c : Command σ) : String := c.spec.name
end Command

/-- Context settings inherited from the parent context. -/
structure Settings where
  helpOptionNames : List String := ["--help"]
  autoEnvvarPrefix : Option String := none
  showDefault : Option Bool := none
  width : Nat := 80
  deriving DecidableEq, Repr, Inhabited

end LeanClick
