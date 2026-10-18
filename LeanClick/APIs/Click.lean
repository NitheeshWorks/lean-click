import LeanClick.APIs.Values

/-!
# click's decorator vocabulary

`option`, `argument`, `command`, `group` and `versionOption` take click's
keyword arguments under click's names, so a click program translates line
for line:

```python
@click.command()
@click.option("--count", default=1, help="Number of greetings.")
@click.option("--name", prompt="Your name", help="The person to greet.")
def hello(count, name): ...
```

```lean
def hello : App Unit := command "hello" [
    option ["--count"] (default := 1) (help := "Number of greetings."),
    option ["--name"] (prompt := "Your name") (help := "The person to greet.")]
  fun ctx => ...
```

As in click, an option's type is inferred from its default when not given,
`is_flag`, a slash-separated `--x` / `--no-x` declaration or `flag_value` make a flag, and
`count` counts. Defaults are written as Lean literals; `DefaultArg` carries
them to the parser, which converts them through the type like click does.
-/

namespace LeanClick

open Text

/-- A default value as written in a declaration: a string, number or boolean
literal, or a list of them for `multiple`/`nargs`. -/
inductive DefaultArg where
  | unset
  | str (s : String)
  | int (n : Int)
  | float (text : String)
  | bool (b : Bool)
  | many (xs : List String)
  deriving Inhabited

instance : Coe String DefaultArg := ⟨.str⟩
instance : Coe Bool DefaultArg := ⟨.bool⟩
instance : Coe Int DefaultArg := ⟨.int⟩
instance : Coe (List String) DefaultArg := ⟨.many⟩
instance (n : Nat) : OfNat DefaultArg n := ⟨.int n⟩
instance : Neg DefaultArg := ⟨fun d => match d with
  | .int n => .int (-n)
  | .float t => .float ("-" ++ t)
  | d => d⟩
instance : OfScientific DefaultArg where
  ofScientific m s e :=
    let d : Dec := if s then ⟨m, e⟩ else ⟨(m : Int) * (10 : Int) ^ e, 0⟩
    .float (showDec d)

/-- The raw tokens a default stands for. -/
def DefaultArg.tokens : DefaultArg → Option (List String)
  | .unset => none
  | .str s => some [s]
  | .int n => some [toString n]
  | .float t => some [t]
  | .bool b => some [if b then "true" else "false"]
  | .many xs => some xs

/-- click's `convert_type(None, default)`: the type a default implies. -/
def DefaultArg.impliedType : DefaultArg → ParamType
  | .int _ => .int
  | .float _ => .float
  | .bool _ => .bool
  | _ => .string

/-! ## click's type constants and constructors -/

def STRING : ParamType := .string
def INT : ParamType := .int
def FLOAT : ParamType := .float
def BOOL : ParamType := .bool

def UUID : ParamType := .uuid

/-- `click.DateTime(formats=None)`. -/
def DateTime (formats : List String := ["%Y-%m-%d", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%d %H:%M:%S"]) : ParamType :=
  .dateTime formats

/-- `click.Choice(choices, case_sensitive=True)`. -/
def Choice (choices : List String) (caseSensitive : Bool := true) : ParamType :=
  .choice choices caseSensitive

/-- `click.Path(exists=False, file_okay=True, dir_okay=True, ...)`. -/
def Path (exists_ : Bool := false) (fileOkay : Bool := true) (dirOkay : Bool := true)
    (writable : Bool := false) (readable : Bool := true) (executable : Bool := false)
    (allowDash : Bool := false) : ParamType :=
  .path { exists_, fileOkay, dirOkay, writable, readable, executable, allowDash }

/-- `click.File(mode="r")`. -/
def File (mode : String := "r") (lazy : Option Bool := none) : ParamType := .file { mode, lazy }

/-- `click.IntRange(min=None, max=None, min_open=False, max_open=False, clamp=False)`. -/
def IntRange (min : Option Int := none) (max : Option Int := none) (minOpen : Bool := false)
    (maxOpen : Bool := false) (clamp : Bool := false) : ParamType :=
  .intRange { min, max, minOpen, maxOpen, clamp }

/-- `click.FloatRange(...)`; bounds are exact decimals (`⟨15, 1⟩` is 1.5). -/
def FloatRange (min : Option Dec := none) (max : Option Dec := none) (minOpen : Bool := false)
    (maxOpen : Bool := false) (clamp : Bool := false) : ParamType :=
  .floatRange { min, max, minOpen, maxOpen, clamp }

/-- `click.option(*param_decls, ...)`. -/
def option (decls : List String) (type : Option ParamType := none) (default : DefaultArg := .unset)
    (help : String := "") (required : Bool := false) (envvar : List String := [])
    (multiple : Bool := false) (nargs : Nat := 1) (isFlag : Bool := false)
    (flagValue : Option String := none) (count : Bool := false) (prompt : Option String := none)
    (confirmationPrompt : Bool := false) (hideInput : Bool := false) (showDefault : Bool := false)
    (showEnvvar : Bool := false) (showChoices : Bool := true) (hidden : Bool := false)
    (metavar : Option String := none) (isEager : Bool := false)
    (callback : Option (Value → Except String Value) := none)
    (shellComplete : Option (String → List String) := none) : Param :=
  let (_, _, sec) := parseDecls decls
  let base : Param :=
    if count then Param.count decls
    else match flagValue with
      | some c => Param.flagValue decls c (type.getD .string) multiple
      | none =>
        if isFlag || !sec.isEmpty || (type.isNone && match default with | .bool _ => true | _ => false)
        then Param.flag decls
        else Param.opt decls (type.getD default.impliedType) nargs multiple
  { base with
    default := default.tokens, help := if help.isEmpty then none else some help, required,
    envvar, hidden, metavar, showEnvvar, showChoices,
    showDefault := if showDefault then .yes else .auto,
    prompt := prompt.map fun t => { text := t, confirmation := confirmationPrompt, hideInput },
    eager := if isEager then some .help else none, callback, shellComplete }

/-- `click.argument(name, ...)`; `nargs := -1` takes any number. Required
unless it has a default or `nargs = -1`, as in click. -/
def argument (name : String) (type : Option ParamType := none) (default : DefaultArg := .unset)
    (nargs : Int := 1) (required : Option Bool := none) (envvar : List String := [])
    (help : String := "") (metavar : Option String := none)
    (callback : Option (Value → Except String Value) := none)
    (shellComplete : Option (String → List String) := none) : Param :=
  let n : ArgNargs := if nargs < 0 then .star else .exactly nargs.toNat
  let base := Param.arg name (type.getD default.impliedType) n
  { base with
    default := default.tokens, envvar, metavar, help := if help.isEmpty then none else some help,
    required := required.getD (base.required && default.tokens.isNone), callback, shellComplete }

/-- `click.version_option(version)`. -/
def versionOption (version : String) (message : String := "%(prog)s, version %(version)s")
    (decls : List String := ["--version"]) : Param :=
  Param.versionOption version decls message

/-- The context settings click takes as `context_settings`. -/
structure ContextSettings where
  helpOptionNames : Option (List String) := none
  autoEnvvarPrefix : Option String := none
  showDefault : Option Bool := none
  ignoreUnknownOptions : Bool := false
  allowExtraArgs : Option Bool := none
  allowInterspersedArgs : Option Bool := none
  defaultMap : Option DefaultMap := none

/-- `@click.command(name, ...)`: the parameters in decorator order (top to
bottom), then the callback. -/
def command {κ : Type} [Inhabited κ] (name : String) (params : List Param := [])
    (callback : κ := default) (help : String := "") (shortHelp : Option String := none) (epilog : Option String := none)
    (contextSettings : ContextSettings := {}) (addHelpOption : Bool := true)
    (noArgsIsHelp : Bool := false) (hidden : Bool := false) : Command κ :=
  .mk { name, params, help := if help.isEmpty then none else some help, shortHelp, epilog,
        addHelpOption, hidden, noArgsIsHelp := some noArgsIsHelp,
        helpOptionNames := contextSettings.helpOptionNames,
        autoEnvvarPrefix := contextSettings.autoEnvvarPrefix,
        showDefault := contextSettings.showDefault,
        ignoreUnknownOptions := contextSettings.ignoreUnknownOptions,
        allowExtraArgs := contextSettings.allowExtraArgs,
        allowInterspersedArgs := contextSettings.allowInterspersedArgs,
        defaultMap := contextSettings.defaultMap }
    false callback []

/-- `@click.group(name, ...)` with its subcommands. -/
def group {κ : Type} [Inhabited κ] (name : String) (params : List Param := [])
    (commands : List (Command κ) := []) (callback : κ := default) (help : String := "") (shortHelp : Option String := none)
    (epilog : Option String := none) (contextSettings : ContextSettings := {})
    (invokeWithoutCommand : Bool := false) (noArgsIsHelp : Option Bool := none)
    (hidden : Bool := false) (chain : Bool := false) : Command κ :=
  .mk { name, params, help := if help.isEmpty then none else some help, shortHelp, epilog, hidden,
        invokeWithoutCommand, noArgsIsHelp, chain,
        helpOptionNames := contextSettings.helpOptionNames,
        autoEnvvarPrefix := contextSettings.autoEnvvarPrefix,
        showDefault := contextSettings.showDefault,
        ignoreUnknownOptions := contextSettings.ignoreUnknownOptions,
        allowExtraArgs := contextSettings.allowExtraArgs,
        allowInterspersedArgs := contextSettings.allowInterspersedArgs,
        defaultMap := contextSettings.defaultMap }
    true callback commands

namespace Context
variable {σ : Type}

/-- `ctx.params[name]`, refinement erased. -/
def get (ctx : Context σ) (name : String) : Value := ctx.params.get name

/-- An integer parameter (0 when absent). -/
def int (ctx : Context σ) (name : String) : Int := (ctx.params.getInt? name).getD 0
/-- A string, path or choice parameter (`""` when absent). -/
def str (ctx : Context σ) (name : String) : String := (ctx.params.getString? name).getD ""
/-- An optional string parameter (`None` in click). -/
def str? (ctx : Context σ) (name : String) : Option String := ctx.params.getString? name
/-- A boolean parameter (`false` when absent). -/
def bool (ctx : Context σ) (name : String) : Bool := (ctx.params.getBool? name).getD false
/-- A multi-value parameter as strings. -/
def strs (ctx : Context σ) (name : String) : List String := ctx.params.getStrings name

end Context

end LeanClick
