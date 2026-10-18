import LeanClick

/-!
# Conformance cases

Each case re-expresses one upstream test (https://github.com/pallets/click,
`tests/`; and fastapi-cli for the `dev`/`run` shape) in Lean and names it, so
`docs/traceability.toml` can point at both. The upstream suite is the
behavioural specification; no upstream code is copied.

Upstream tests drive a command through `CliRunner` and inspect its output;
here the pure `Command.dispatch` is driven instead, and the case checks the
parsed values (what the upstream callback would echo), the rendered error or
help text, and the exit code.

Run with `lake exe conformance`.
-/

open LeanClick

structure Case where
  /-- Upstream test this mirrors, e.g. `tests/test_basic.py::test_int_option`. -/
  upstream : String
  name : String
  run : IO Bool

abbrev Cmd := Command Unit

def mkCmd (name : String) (params : List Param) (help : Option String := none) : Cmd :=
  Command.command { name, params, help }

/-- What a `CliRunner.invoke` observes: the output, the exit code, and the
parsed levels. -/
structure Run where
  out : String
  code : Nat
  frames : List (Frame Unit)

def run (c : Cmd) (argv : List String) (env : Env := {}) : Run :=
  let d := c.dispatch c.name argv env
  match d.halt with
  | none => ⟨"", 0, d.frames⟩
  | some (.error e) => ⟨e.render, 2, d.frames⟩
  | some (.help t) => ⟨t ++ "\n", 0, d.frames⟩
  | some (.message t) => ⟨t ++ "\n", 0, d.frames⟩
  | some (.prompt _ p _) => ⟨s!"PROMPT {p.name}", 1, d.frames⟩

/-- The value the innermost command received, rendered as click echoes it. -/
def Run.val (r : Run) (name : String) : String :=
  match r.frames.getLast? with
  | some f => (f.args.get name).render
  | none => "<no frame>"

def Run.src (r : Run) (name : String) : Option Source :=
  match r.frames.getLast? with
  | some f => f.args.source? name
  | none => none

def has (s sub : String) : Bool := (s.splitOn sub).length > 1

def lines (s : String) : List String := Text.splitLines s

def pc (upstream name : String) (b : Bool) : Case := ⟨upstream, name, pure b⟩

def envOf (vars : List (String × String)) : Env := { vars }

/-! ## tests/test_basic.py -/

def sOpt : Cmd := command "cli" [option ["--s"] (default := "no value")]
def iOpt : Cmd := command "cli" [option ["--i"] (default := 42)]
def fOpt : Cmd := command "cli" [option ["--f"] (default := 42.0)]
def onOff (d : Option String) : Cmd :=
  mkCmd "cli" [match d with | some x => (Param.flag ["--on/--off"]).withDefault x | none => Param.flag ["--on/--off"]]
def fFlag (d : Option String) : Cmd :=
  mkCmd "cli" [match d with | some x => (Param.flag ["--f"]).withDefault x | none => Param.flag ["--f"]]
def boolOpt : Cmd := mkCmd "cli" [Param.opt ["--flag"] .bool]
def methodChoice : Cmd := mkCmd "cli" [Param.opt ["--method"] (.choice ["foo", "bar", "baz"])]
def methodArg : Cmd := mkCmd "cli" [Param.arg "method" (.choice ["foo", "bar", "baz"])]
def basicGroup : Cmd :=
  Command.group { name := "cli", help := some "This is the root." }
    [mkCmd "subcommand" [] (some "This is a subcommand.")]

def basicCases : List Case := [
  pc "tests/test_basic.py::test_string_option" "string option default" ((run sOpt []).val "s" == "no value"),
  pc "tests/test_basic.py::test_string_option" "string option --s=42" ((run sOpt ["--s=42"]).val "s" == "42"),
  pc "tests/test_basic.py::test_string_option" "string option needs a value"
    (has (run sOpt ["--s"]).out "Error: Option '--s' requires an argument." && (run sOpt ["--s"]).code == 2),
  pc "tests/test_basic.py::test_string_option" "string option --s= is empty" ((run sOpt ["--s="]).val "s" == ""),
  pc "tests/test_basic.py::test_string_option" "string option unicode" ((run sOpt ["--s=☃"]).val "s" == "☃"),
  pc "tests/test_basic.py::test_int_option" "int option default" ((run iOpt []).val "i" == "42"),
  pc "tests/test_basic.py::test_int_option" "int option --i=23" ((run iOpt ["--i=23"]).val "i" == "23"),
  pc "tests/test_basic.py::test_int_option" "int option bad value"
    (has (run iOpt ["--i=x"]).out "Error: Invalid value for '--i': 'x' is not a valid integer."),
  pc "tests/test_basic.py::test_float_option" "float option default" ((run fOpt []).val "f" == "42.0"),
  pc "tests/test_basic.py::test_float_option" "float option --f=23.5" ((run fOpt ["--f=23.5"]).val "f" == "23.5"),
  pc "tests/test_basic.py::test_float_option" "float option bad value"
    (has (run fOpt ["--f=x"]).out "Error: Invalid value for '--f': 'x' is not a valid float."),
  pc "tests/test_basic.py::test_boolean_switch" "switch --on, default true" ((run (onOff (some "true")) ["--on"]).val "on" == "True"),
  pc "tests/test_basic.py::test_boolean_switch" "switch --on, default false" ((run (onOff (some "false")) ["--on"]).val "on" == "True"),
  pc "tests/test_basic.py::test_boolean_switch" "switch --on, unset" ((run (onOff none) ["--on"]).val "on" == "True"),
  pc "tests/test_basic.py::test_boolean_switch" "switch --off, default true" ((run (onOff (some "true")) ["--off"]).val "on" == "False"),
  pc "tests/test_basic.py::test_boolean_switch" "switch --off, unset" ((run (onOff none) ["--off"]).val "on" == "False"),
  pc "tests/test_basic.py::test_boolean_switch" "switch absent, default true" ((run (onOff (some "true")) []).val "on" == "True"),
  pc "tests/test_basic.py::test_boolean_switch" "switch absent, default false" ((run (onOff (some "false")) []).val "on" == "False"),
  pc "tests/test_basic.py::test_boolean_switch" "switch absent, unset" ((run (onOff none) []).val "on" == "False"),
  pc "tests/test_basic.py::test_boolean_flag" "flag default true, given" ((run (fFlag (some "true")) ["--f"]).val "f" == "True"),
  pc "tests/test_basic.py::test_boolean_flag" "flag default true, absent" ((run (fFlag (some "true")) []).val "f" == "True"),
  pc "tests/test_basic.py::test_boolean_flag" "flag default false, given" ((run (fFlag (some "false")) ["--f"]).val "f" == "True"),
  pc "tests/test_basic.py::test_boolean_flag" "flag default false, absent" ((run (fFlag (some "false")) []).val "f" == "False"),
  pc "tests/test_basic.py::test_boolean_conversion" "bool type accepts every true spelling"
    (["1", "true", "t", "yes", "y", "on", "True", "Yes", "On"].all fun v => (run boolOpt ["--flag", v]).val "flag" == "True"),
  pc "tests/test_basic.py::test_boolean_conversion" "bool type accepts every false spelling"
    (["0", "false", "f", "no", "n", "off", "False", "No", "Off"].all fun v => (run boolOpt ["--flag", v]).val "flag" == "False"),
  pc "tests/test_basic.py::test_choice_option" "choice option value" ((run methodChoice ["--method=foo"]).val "method" == "foo"),
  pc "tests/test_basic.py::test_choice_option" "choice option invalid"
    ((run methodChoice ["--method=meh"]).code == 2 &&
     has (run methodChoice ["--method=meh"]).out "Invalid value for '--method': 'meh' is not one of 'foo', 'bar', 'baz'."),
  pc "tests/test_basic.py::test_choice_option" "choice option metavar in help"
    (has (run methodChoice ["--help"]).out "--method [foo|bar|baz]"),
  pc "tests/test_basic.py::test_choice_argument" "choice argument value" ((run methodArg ["foo"]).val "method" == "foo"),
  pc "tests/test_basic.py::test_choice_argument" "choice argument invalid"
    ((run methodArg ["meh"]).code == 2 &&
     has (run methodArg ["meh"]).out "Invalid value for '{foo|bar|baz}': 'meh' is not one of 'foo', 'bar', 'baz'."),
  pc "tests/test_basic.py::test_choice_argument" "choice argument metavar in help"
    (has (run methodArg ["--help"]).out "{foo|bar|baz}"),
  pc "tests/test_basic.py::test_required_option" "required option missing"
    (let r := run (mkCmd "cli" [(Param.opt ["--foo"]).requiredParam]) []; r.code == 2 && has r.out "Missing option '--foo'"),
  pc "tests/test_basic.py::test_evaluation_order" "eager first, then invocation order, then declaration order"
    (let mk := fun (n : String) (eager : Bool) =>
        ({ (Param.flagValue [s!"--{n}"] n) with eager := if eager then some .help else none } : Param)
     let ps : List Param := [(Param.opt ["--missing"]).withDefault "missing",
       mk "eager-flag1" true, mk "eager-flag2" true, mk "eager-flag3" true,
       mk "normal-flag1" false, mk "normal-flag2" false, mk "normal-flag3" false]
     let inv := ["eager_flag2", "eager_flag1", "normal_flag2", "eager_flag3", "normal_flag3",
       "normal_flag3", "normal_flag1", "normal_flag1"]
     (processingOrder ps inv).map (·.name) ==
       ["eager_flag2", "eager_flag1", "eager_flag3", "normal_flag2", "normal_flag3", "normal_flag1", "missing"]),
  pc "tests/test_basic.py::test_hidden_option" "hidden option not in help"
    (let r := run (mkCmd "cli" [(Param.opt ["--nope"]).hide]) ["--help"]; r.code == 0 && !has r.out "--nope"),
  pc "tests/test_basic.py::test_hidden_command" "hidden command not in help"
    (let g : Cmd := Command.group { name := "cli" } [Command.command { name := "nope", hidden := true }]
     let r := run g ["--help"]; r.code == 0 && !has r.out "nope"),
  pc "tests/test_basic.py::test_summary_line" "summary line stops at the paragraph"
    (let g : Cmd := Command.group { name := "cli" }
        [mkCmd "cmd" [] (some "\n        Summary line without period\n\n        Here is a sentence. And here too.\n        ")]
     let r := run g ["--help"]; has r.out "Summary line without period" && !has r.out "Here is a sentence."),
  pc "tests/test_basic.py::test_help_invalid_default" "invalid default still shown in help"
    (let c := mkCmd "cli" [(Param.opt ["-a"] (.path { exists_ := true })).withDefault "not found" |>.showDefaultParam]
     let r := run c ["--help"]; r.code == 0 && has r.out "default: not found"),
  pc "tests/test_basic.py::test_basic_group" "group help lists the subcommand"
    (let r := run basicGroup ["--help"]
     r.code == 0 && has r.out "COMMAND [ARGS]..." && has r.out "This is the root" && has r.out "This is a subcommand."),
  pc "tests/test_basic.py::test_basic_group" "group dispatches to the subcommand"
    (let r := run basicGroup ["subcommand"]; r.code == 0 && (r.frames.map (·.path)) == ["cli", "cli subcommand"]),
  pc "tests/test_basic.py::test_group_from_list" "group built from a list"
    ((run (Command.group { name := "cli" } [mkCmd "sub" []]) ["sub"]).frames.length == 2)
]

/-! ## tests/test_options.py -/

def countCmd : Cmd := mkCmd "cli" [(Param.count ["-v"] { min := some 0, max := some 3 }).withHelp "Verbosity"]
def boundCount : Cmd := Command.command { name := "cli", params := [Param.opt ["--bound"], Param.opt ["--count"]] }
def msgCmd : Cmd := mkCmd "cli" [(Param.opt ["-m", "--message"] (multiple := true)).requiredParam]

def optionCases : List Case := [
  pc "tests/test_options.py::test_counting" "count -vvv" ((run countCmd ["-vvv"]).val "v" == "3"),
  pc "tests/test_options.py::test_counting" "count out of range"
    (has (run countCmd ["-vvvv"]).out "Invalid value for '-v': 4 is not in the range 0<=x<=3."),
  pc "tests/test_options.py::test_counting" "count absent is 0" ((run countCmd []).val "v" == "0"),
  pc "tests/test_options.py::test_counting" "count help row" (has (run countCmd ["--help"]).out "-v      Verbosity"),
  pc "tests/test_options.py::test_unknown_options" "unknown long option"
    (has (run (mkCmd "cli" []) ["--foo"]).out "No such option '--foo'."),
  pc "tests/test_options.py::test_unknown_options" "unknown short option"
    (has (run (mkCmd "cli" []) ["-f"]).out "No such option '-f'."),
  pc "tests/test_options.py::test_suggest_possible_options" "--cat suggests --count"
    (has (run boundCount ["--cat"]).out "Did you mean '--count'?"),
  pc "tests/test_options.py::test_suggest_possible_options" "--bounds suggests both"
    (has (run boundCount ["--bounds"]).out "(Did you mean one of: '--bound', '--count'?)"),
  pc "tests/test_options.py::test_suggest_possible_options" "--bount suggests both"
    (has (run boundCount ["--bount"]).out "(Did you mean one of: '--bound', '--count'?)"),
  pc "tests/test_options.py::test_multiple_required" "multiple values, attached short value"
    ((run msgCmd ["-m", "foo", "-mbar"]).val "message" == "(foo, bar)"),
  pc "tests/test_options.py::test_multiple_required" "multiple required missing"
    (has (run msgCmd []).out "Error: Missing option '-m' / '--message'."),
  pc "tests/test_options.py::test_nargs_tup_composite_mult" "multiple nargs=2 occurrences"
    ((run (mkCmd "copy" [Param.opt ["--item"] (nargs := 2) (multiple := true)]) ["--item", "peter", "1", "--item", "max", "2"]).val "item"
      == "((peter, 1), (max, 2))"),
  pc "tests/test_options.py::test_multiple_envvar" "multiple from auto envvar"
    (let c : Cmd := Command.command { name := "cmd", params := [Param.opt ["--arg"] (multiple := true)], autoEnvvarPrefix := some "TEST" }
     (run c [] (envOf [("TEST_ARG", "foo bar baz")])).val "arg" == "(foo, bar, baz)"),
  pc "tests/test_options.py::test_toupper_envvar_prefix" "auto prefix is upper-cased"
    (let c : Cmd := Command.command { name := "cmd", params := [Param.opt ["--arg"]], autoEnvvarPrefix := some "test" }
     (run c [] (envOf [("TEST_ARG", "foo")])).val "arg" == "foo"),
  pc "tests/test_options.py::test_nargs_envvar" "nargs=2 from envvar"
    (let c : Cmd := Command.command { name := "cmd", params := [Param.opt ["--arg"] (nargs := 2)], autoEnvvarPrefix := some "TEST" }
     (run c [] (envOf [("TEST_ARG", "foo bar")])).val "arg" == "(foo, bar)"),
  pc "tests/test_options.py::test_nargs_envvar" "nargs=2 multiple from envvar"
    (let c : Cmd := Command.command { name := "cmd", params := [Param.opt ["--arg"] (nargs := 2) (multiple := true)], autoEnvvarPrefix := some "TEST" }
     (run c [] (envOf [("TEST_ARG", "x 1 y 2")])).val "arg" == "((x, 1), (y, 2))"),
  pc "tests/test_options.py::test_empty_envvar" "empty envvar counts as unset"
    (let c := mkCmd "cli" [(Param.opt ["--mypath"] (.path { exists_ := true })).withEnvvar ["MYPATH"]]
     let r := run c [] (envOf [("MYPATH", "")]); r.code == 0 && r.val "mypath" == "None"),
  pc "tests/test_options.py::test_boolean_flag_envvar" "flag pair from envvar"
    (let c := mkCmd "cli" [(Param.flag ["--shout/--no-shout"]).withEnvvar ["SHOUT"]]
     (run c [] (envOf [("SHOUT", "True")])).val "shout" == "True" &&
     (run c [] (envOf [("SHOUT", "0")])).val "shout" == "False" &&
     (run c [] (envOf [("NO_SHOUT", "True")])).val "shout" == "False"),
  pc "tests/test_options.py::test_boolean_envvar_bad_values" "bad boolean envvar"
    (let c := mkCmd "cli" [(Param.flag ["--shout/--no-shout"]).withEnvvar ["SHOUT"]]
     ["tr ue", "10", "randomstring", "x:y"].all fun v =>
       let r := run c [] (envOf [("SHOUT", v)])
       r.code == 2 && has r.out s!"Invalid value for '--shout': {Text.pyRepr v} is not a valid boolean."),
  pc "tests/test_options.py::test_multiple_default_help" "multiple defaults shown joined"
    (let c := mkCmd "cmd" [(Param.opt ["--arg1"] (multiple := true)).withDefaults ["foo", "bar"] |>.showDefaultParam,
        (Param.opt ["--arg2"] .int (multiple := true)).withDefaults ["1", "2"] |>.showDefaultParam]
     let r := run c ["--help"]; has r.out "foo, bar" && has r.out "1, 2"),
  pc "tests/test_options.py::test_show_envvar" "show_envvar in help"
    (has (run (mkCmd "cmd" [(Param.opt ["--arg1"]).withEnvvar ["ARG1"] |>.showEnvvarParam]) ["--help"]).out "ARG1"),
  pc "tests/test_options.py::test_show_envvar_auto_prefix" "show_envvar with auto prefix"
    (let c : Cmd := Command.command { name := "cmd", params := [(Param.opt ["--arg1"]).showEnvvarParam], autoEnvvarPrefix := some "TEST" }
     has (run c ["--help"]).out "TEST_ARG1"),
  pc "tests/test_options.py::test_show_envvar_auto_prefix_dash_in_command" "auto prefix through a group"
    (let g : Cmd := Command.group { name := "cli", autoEnvvarPrefix := some "TEST" }
        [mkCmd "foo-bar" [(Param.opt ["--baz"]).showEnvvarParam]]
     has (run g ["foo-bar", "--help"]).out "TEST_FOO_BAR_BAZ"),
  pc "tests/test_options.py::test_missing_required_flag" "required flag pair missing"
    (has (run (mkCmd "cli" [(Param.flag ["--on/--off"]).requiredParam]) []).out "Error: Missing option '--on'."),
  pc "tests/test_options.py::test_missing_choice" "missing choice lists the choices"
    (let r := run (mkCmd "cmd" [(Param.opt ["--foo"] (.choice ["foo", "bar"])).requiredParam]) []
     r.code == 2 && has r.out "Error: Missing option '--foo'. Choose from:\n\tfoo,\n\tbar"),
  pc "tests/test_options.py::test_missing_envvar" "missing option with show_envvar hint"
    (has (run (mkCmd "cli" [(Param.opt ["--foo"]).withEnvvar ["bar"] |>.requiredParam]) []).out "Error: Missing option '--foo'." &&
     has (run (mkCmd "cli" [(Param.opt ["--foo"]).withEnvvar ["bar"] |>.requiredParam |>.showEnvvarParam]) []).out
       "Error: Missing option '--foo' (env var: 'bar')."),
  pc "tests/test_options.py::test_case_insensitive_choice" "case-insensitive choice returns the declared spelling"
    (let c := mkCmd "cmd" [Param.opt ["--foo"] (.choice ["Orange", "Apple"] false)]
     (run c ["--foo", "apple"]).val "foo" == "Apple" && (run c ["--foo", "oRANGe"]).val "foo" == "Orange"),
  pc "tests/test_options.py::test_case_insensitive_choice" "case-sensitive choice rejects other case"
    (let c := mkCmd "cmd" [Param.opt ["--foo"] (.choice ["Orange", "Apple"])]
     (run c ["--foo", "apple"]).code == 2 && (run c ["--foo", "Apple"]).code == 0),
  pc "tests/test_options.py::test_show_default_boolean_flag_name" "show default of a flag pair names the active spelling"
    (has (run (mkCmd "cli" [(Param.flag ["--cache/--no-cache"]).withDefault "false" |>.showDefaultParam |>.withHelp "Enable the cache."]) ["--help"]).out
      "[default: no-cache]"),
  pc "tests/test_options.py::test_show_default_string" "show_default as text"
    (has (run (mkCmd "cli" [{ (Param.opt ["--limit"]) with showDefault := .text "unlimited" }]) ["--help"]).out "(unlimited)"),
  pc "tests/test_options.py::test_show_default_with_empty_string" "empty default shown quoted"
    (has (run (mkCmd "cli" [(Param.opt ["--limit"]).withDefault "" |>.showDefaultParam]) ["--help"]).out "[default: \"\"]"),
  pc "tests/test_options.py::test_do_not_show_no_default" "no default, nothing shown"
    (!has (run (mkCmd "cli" [(Param.opt ["--limit"]).showDefaultParam]) ["--help"]).out "default:"),
  pc "tests/test_options.py::test_intrange_default_help_text" "range in help"
    (has (run (mkCmd "cli" [Param.opt ["--count"] (.intRange { min := some 1, max := some 32 })]) ["--help"]).out "[1<=x<=32]"),
  pc "tests/test_options.py::test_count_default_type_help" "count range not shown"
    (!has (run (mkCmd "cli" [Param.count ["--verbose"]]) ["--help"]).out "x>=0"),
  pc "tests/test_options.py::test_invalid_option" "option value taken even if it looks like an option"
    ((run (mkCmd "cli" [Param.opt ["--name"], Param.flag ["--other"]]) ["--name", "--other"]).val "name" == "--other"),
  pc "tests/test_options.py::test_aliases_for_flags" "secondary spelling of a pair"
    (let c := mkCmd "cli" [Param.flag ["--warnings/--no-warnings", "-w/-W"]]
     (run c ["-W"]).val "warnings" == "False" && (run c ["-w"]).val "warnings" == "True"),
  pc "tests/test_options.py::test_prefixes" "++ prefix options"
    (let c := mkCmd "cli" [(Param.flag ["++foo"]).withHelp "das foo", (Param.flag ["--bar"]).withHelp "das bar"]
     let r := run c ["++foo", "--bar"]
     r.val "foo" == "True" && r.val "bar" == "True" && has (run c ["--help"]).out "++foo   das foo"),
  pc "tests/test_options.py::test_winstyle_options" "slash options and the '; ' separator"
    (let c : Cmd := Command.command {
        name := "cmd", helpOptionNames := some ["/?"],
        params := [(Param.flag ["/debug;/no-debug"]).withHelp "Enables or disables debug mode."] }
     (run c ["/debug"]).val "debug" == "True" && (run c ["/no-debug"]).val "debug" == "False" &&
     (run c []).val "debug" == "False" &&
     has (run c ["/?"]).out "/debug; /no-debug  Enables or disables debug mode." &&
     has (run c ["/?"]).out "/?                 Show this message and exit."),
  pc "tests/test_options.py::test_legacy_options" "single-dash long option"
    (let c := mkCmd "cmd" [Param.opt ["-whatever"]]
     (run c ["-whatever", "42"]).val "whatever" == "42" && (run c ["-whatever=23"]).val "whatever" == "23"),
  pc "tests/test_options.py::test_show_default_precedence" "shown default is the converted value"
    (has (run (mkCmd "cli" [(Param.opt ["--f"] .float).withDefault "1e20" |>.showDefaultParam]) ["--help"]).out
      "[default: 1e+20]"),
  pc "tests/test_options.py::test_intrange_default_help_text" "unbounded range shows no range (UP-023)"
    (!has (run (mkCmd "cli" [(Param.opt ["--n"] (.intRange {})).withDefault "1" |>.showDefaultParam]) ["--help"]).out "None"),
  pc "tests/test_options.py::test_toupper_envvar_prefix" "help and version never read auto envvars (UP-011)"
    (let c : Cmd := Command.command {
        name := "cli", autoEnvvarPrefix := some "APP",
        params := [Param.versionOption "1.2", Param.opt ["--name"]] }
     let r := run c [] (envOf [("APP___VERSION__", "1"), ("APP___HELP__", "1"), ("APP_VERSION", "1")])
     r.code == 0 && r.out == "" && r.frames.length == 1),
  pc "tests/test_options.py::test_envvar_string_flag_value" "falsy envvar leaves a flag_value option unset (UP-009)"
    (let c := mkCmd "cli" [(Param.flagValue ["--upper"] "upper").withEnvvar ["MODE"]]
     (run c [] (envOf [("MODE", "upper")])).val "upper" == "upper" &&
     (run c [] (envOf [("MODE", "1")])).val "upper" == "upper" &&
     (run c [] (envOf [("MODE", "0")])).val "upper" == "None"),
  pc "tests/test_options.py::test_option_names" "name from the longest spelling"
    ((parseDecls ["-a", "--aggressive"]).1 == "aggressive" && (parseDecls ["--root-path"]).1 == "root_path" &&
     (parseDecls ["-a", "-b"]).1 == "a" && (parseDecls ["--x", "name"]).1 == "name")
]

/-! ## tests/test_arguments.py -/

def argCases : List Case := [
  pc "tests/test_arguments.py::test_nargs_star" "nargs=-1 and a fixed argument"
    (let c := mkCmd "copy" [Param.arg "src" (nargs := .star), Param.arg "dst"]
     let r := run c ["foo.txt", "bar.txt", "dir"]; r.val "src" == "(foo.txt, bar.txt)" && r.val "dst" == "dir"),
  pc "tests/test_arguments.py::test_nargs_tup" "nargs=2 argument"
    (let c := mkCmd "copy" [Param.arg "name", Param.arg "point" .int (nargs := .exactly 2)]
     let r := run c ["peter", "1", "2"]; r.val "name" == "peter" && r.val "point" == "(1, 2)"),
  pc "tests/test_arguments.py::test_nargs_err" "extra argument"
    (has (run (mkCmd "copy" [Param.arg "x"]) ["foo", "bar"]).out "Got unexpected extra argument (bar)"),
  pc "tests/test_arguments.py::test_nargs_err" "extra arguments"
    (has (run (mkCmd "copy" [Param.arg "x"]) ["foo", "bar", "baz"]).out "Got unexpected extra arguments (bar baz)"),
  pc "tests/test_arguments.py::test_missing_arg" "missing argument"
    (let r := run (mkCmd "cmd" [Param.arg "arg"]) []; r.code == 2 && has r.out "Missing argument 'ARG'."),
  pc "tests/test_arguments.py::test_nargs_star_ordering" "star argument in the middle"
    (let c := mkCmd "cmd" [Param.arg "a", Param.arg "b" (nargs := .star), Param.arg "c"]
     let r := run c ["a", "b", "c", "d"]; r.val "a" == "a" && r.val "b" == "(b, c)" && r.val "c" == "d"),
  pc "tests/test_arguments.py::test_nargs_specified_plus_star_ordering" "fixed, star, fixed with nargs=2"
    (let c := mkCmd "cmd" [Param.arg "a" (nargs := .star), Param.arg "b", Param.arg "c" (nargs := .exactly 2)]
     let r := run c ["a", "b", "c", "d", "e", "f"]
     r.val "a" == "(a, b, c)" && r.val "b" == "d" && r.val "c" == "(e, f)"),
  pc "tests/test_arguments.py::test_nargs_tup_composite" "partial nargs=2 argument is an error"
    (has (run (mkCmd "cmd" [Param.arg "item" (nargs := .exactly 2)]) ["a"]).out "Argument 'item' takes 2 values."),
  pc "tests/test_arguments.py::test_empty_nargs" "empty star argument"
    ((run (mkCmd "cmd" [Param.arg "arg" (nargs := .star)]) []).val "arg" == "()"),
  pc "tests/test_arguments.py::test_empty_nargs" "required star argument missing"
    (has (run (mkCmd "cmd" [(Param.arg "arg" (nargs := .star)).requiredParam]) []).out "Missing argument 'ARG...'"),
  pc "tests/test_arguments.py::test_missing_argument_string_cast" "argument default"
    ((run (mkCmd "cmd" [(Param.arg "arg").withDefault "x"]) []).val "arg" == "x"),
  pc "tests/test_arguments.py::test_nargs_envvar" "argument from envvar, split by nargs"
    (let c := mkCmd "cmd" [(Param.arg "arg" (nargs := .exactly 2)).withEnvvar ["X"]]
     (run c [] (envOf [("X", "a b")])).val "arg" == "(a, b)"),
  pc "tests/test_arguments.py::test_eat_options" "-- ends option processing"
    (let c := mkCmd "cmd" [Param.opt ["-f"], Param.arg "files" (nargs := .star)]
     let r := run c ["-f", "-x", "--", "foo", "-b"]; r.val "f" == "-x" && r.val "files" == "(foo, -b)"),
  pc "tests/test_arguments.py::test_path_allow_dash" "dash is a path when allowed"
    ((run (mkCmd "cmd" [Param.arg "input" (.path { exists_ := true, allowDash := true })]) ["-"]).val "input" == "-"),
  pc "tests/test_arguments.py::test_file_args" "file argument for reading must exist"
    (let c := mkCmd "cmd" [Param.arg "input" (.file {})]
     has (run c ["missing.txt"]).out "Invalid value for 'INPUT': 'missing.txt': No such file or directory" &&
     (run c ["-"]).val "input" == "-")
]

/-! ## tests/test_commands.py, tests/test_formatting.py, tests/test_context.py -/

def cmdCases : List Case := [
  pc "tests/test_commands.py::test_command_no_args_is_help" "no_args_is_help exits 2 with the help"
    (let r := run (Command.command { name := "cli", noArgsIsHelp := some true }) []
     r.code == 2 && has r.out "Show this message and exit."),
  pc "tests/test_commands.py::test_group_invoke_without_command" "group without subcommand prints help"
    (let r := run (Command.group { name := "cli" } [mkCmd "sub" []]) []; r.code == 2 && has r.out "Usage: cli"),
  pc "tests/test_commands.py::test_group_with_args" "missing command"
    (let g : Cmd := Command.group { name := "cli", params := [Param.arg "obj"] } [mkCmd "move" []]
     let r := run g ["obj1"]; r.code == 2 && has r.out "Error: Missing command."),
  pc "tests/test_commands.py::test_group_with_args" "group argument then subcommand"
    (let g : Cmd := Command.group { name := "cli", params := [Param.arg "obj"] } [mkCmd "move" []]
     let r := run g ["obj1", "move"]; r.code == 0 && (r.frames.map (·.path)) == ["cli", "cli OBJ move"]),
  pc "tests/test_commands.py::test_unknown_command" "unknown command"
    (has (run (Command.group { name := "cli" } []) ["unknown"]).out "No such command 'unknown'."),
  pc "tests/test_commands.py::test_unknown_command_suggestion" "did you mean a command"
    (let g : Cmd := Command.group { name := "cli" } [mkCmd "push" [], mkCmd "declare" [], mkCmd "refine" []]
     has (run g ["pause"]).out "Did you mean 'push'?" &&
     has (run g ["decline"]).out "(Did you mean one of: 'declare', 'refine'?)"),
  pc "tests/test_commands.py::test_group_first_match" "first subcommand of a name wins"
    (let g : Cmd := Command.group { name := "cli" } [mkCmd "a" [Param.opt ["--x"]], mkCmd "a" [Param.opt ["--y"]]]
     (run g ["a", "--x", "1"]).val "x" == "1"),
  pc "tests/test_formatting.py::test_basic_functionality" "help paragraphs and \\b blocks"
    (let c := mkCmd "cli" [] (some "First paragraph.\n\n        This is a very long second\n        paragraph and not correctly\n        wrapped but it will be rewrapped.\n\n        \x08\n        This is\n        a paragraph\n        without rewrapping.\n\n        \x08\n        1\n         2\n          3\n\n        And this is a paragraph\n        that will be rewrapped again.\n        ")
     lines (run c ["--help"] { width := 60 }).out == [
        "Usage: cli [OPTIONS]", "", "  First paragraph.", "",
        "  This is a very long second paragraph and not correctly",
        "  wrapped but it will be rewrapped.", "",
        "  This is", "  a paragraph", "  without rewrapping.", "",
        "  1", "   2", "    3", "",
        "  And this is a paragraph that will be rewrapped again.", "",
        "Options:", "  --help  Show this message and exit."]),
  pc "tests/test_formatting.py::test_wrapping_long_options_strings" "usage line wraps under the prefix"
    (let leaf := mkCmd "command" (["first", "second", "third", "fourth", "fifth", "sixth"].map (Param.arg ·)) (some "A command.")
     let g : Cmd := Command.group { name := "cli", help := some "Top level command" }
        [Command.group { name := "a-very-long", help := some "Second level" } [leaf]]
     lines (run g ["a-very-long", "command", "--help"] { width := 54 }).out == [
        "Usage: cli a-very-long command [OPTIONS] FIRST SECOND",
        "                               THIRD FOURTH FIFTH",
        "                               SIXTH", "", "  A command.", "",
        "Options:", "  --help  Show this message and exit."]),
  pc "tests/test_formatting.py::test_wrapping_long_command_name" "usage arguments move to the next line"
    (let leaf := mkCmd "command" (["first", "second", "third", "fourth", "fifth", "sixth"].map (Param.arg ·)) (some "A command.")
     let g : Cmd := Command.group { name := "cli", help := some "Top level command" }
        [Command.group { name := "a-very-very-very-long", help := some "Second level" } [leaf]]
     lines (run g ["a-very-very-very-long", "command", "--help"] { width := 54 }).out == [
        "Usage: cli a-very-very-very-long command ",
        "           [OPTIONS] FIRST SECOND THIRD FOURTH FIFTH",
        "           SIXTH", "", "  A command.", "",
        "Options:", "  --help  Show this message and exit."]),
  pc "tests/test_formatting.py::test_formatting_usage_error" "usage error layout"
    (let r := run (mkCmd "cmd" [Param.arg "arg"]) []
     r.code == 2 && lines r.out == ["Usage: cmd [OPTIONS] ARG", "Try 'cmd --help' for help.", "", "Error: Missing argument 'ARG'."]),
  pc "tests/test_formatting.py::test_formatting_usage_error_metavar_missing_arg" "metavar in missing-argument error"
    (lines (run (mkCmd "cmd" [(Param.arg "arg").withMetavar "metavar"]) []).out ==
      ["Usage: cmd [OPTIONS] metavar", "Try 'cmd --help' for help.", "", "Error: Missing argument 'metavar'."]),
  pc "tests/test_formatting.py::test_formatting_usage_error_metavar_bad_arg" "metavar in bad-value error"
    (lines (run (mkCmd "cmd" [(Param.arg "arg" .int).withMetavar "metavar"]) ["3.14"]).out ==
      ["Usage: cmd [OPTIONS] metavar", "Try 'cmd --help' for help.", "",
       "Error: Invalid value for 'metavar': '3.14' is not a valid integer."]),
  pc "tests/test_formatting.py::test_formatting_usage_error_nested" "nested usage error"
    (let g : Cmd := Command.group { name := "cmd" } [mkCmd "foo" [Param.arg "bar"]]
     lines (run g ["foo"]).out == ["Usage: cmd foo [OPTIONS] BAR", "Try 'cmd foo --help' for help.", "",
       "Error: Missing argument 'BAR'."]),
  pc "tests/test_formatting.py::test_formatting_usage_error_no_help" "no help option, no hint"
    (lines (run (Command.command { name := "cmd", params := [Param.arg "arg"], addHelpOption := false }) []).out ==
      ["Usage: cmd [OPTIONS] ARG", "", "Error: Missing argument 'ARG'."]),
  pc "tests/test_formatting.py::test_formatting_usage_custom_help" "custom help name in hint"
    (lines (run (Command.command { name := "cmd", params := [Param.arg "arg"], helpOptionNames := some ["--man"] }) []).out ==
      ["Usage: cmd [OPTIONS] ARG", "Try 'cmd --man' for help.", "", "Error: Missing argument 'ARG'."]),
  pc "tests/test_formatting.py::test_formatting_usage_error_help_hint" "shadowed help names"
    (let mk := fun (extra : List Param) => (Command.group { name := "cli", helpOptionNames := some ["-h", "--help"] }
        [mkCmd "foo" (Param.arg "required_arg" :: extra)] : Cmd)
     has (run (mk []) ["foo"]).out "Try 'cli foo --help' for help." &&
     has (run (mk [Param.opt ["--host", "-h"]]) ["foo"]).out "Try 'cli foo --help' for help." &&
     has (run (mk [Param.opt ["--help-file", "--help"]]) ["foo"]).out "Try 'cli foo -h' for help." &&
     !has (run (mk [Param.opt ["--host", "-h"], Param.opt ["--help-file", "--help"]]) ["foo"]).out "Try "),
  pc "tests/test_context.py::test_context_meta" "subcommand path includes the parent"
    (let g : Cmd := Command.group { name := "cli" } [mkCmd "sub" []]
     ((run g ["sub"]).frames.map (·.invokedSub)) == [some "sub", none]),
  pc "tests/test_basic.py::test_custom_version_option" "version option"
    (let c : Cmd := Command.command { name := "cli", params := [Param.versionOption "1.0"] }
     let r := run c ["--version"]; r.code == 0 && r.out == "cli, version 1.0\n"),
  pc "tests/test_basic.py::test_param_named_help" "help option wins over required arguments"
    ((run (mkCmd "cli" [Param.arg "x"]) ["--help"]).code == 0)
]

/-! ## fastapi-cli: the `dev` / `run` shape (typer on click) -/

def devCmd (name host : String) (reload : Bool) : Cmd :=
  command name (help := "Run a FastAPI app in development mode.") [
    argument "path" (type := Path) (required := false)
      (help := "A path to a Python file or package directory."),
    option ["--host"] (default := host) (help := "The host to serve on."),
    option ["--port"] (default := 8000) (envvar := ["PORT"]) (help := "The port to serve on."),
    option ["--reload/--no-reload"] (default := reload) (help := "Enable auto-reload."),
    option ["--root-path"] (default := ""),
    option ["--app"],
    option ["--entrypoint", "-e"],
    option ["--proxy-headers/--no-proxy-headers"] (default := true),
    option ["--verbose", "-v"] (isFlag := true)]

def fastapiCli : Cmd :=
  group "fastapi" (contextSettings := { helpOptionNames := some ["-h", "--help"] })
    [versionOption "0.0.0" (message := "FastAPI CLI version: %(version)s"),
     option ["--verbose/--no-verbose"]]
    [devCmd "dev" "127.0.0.1" true, devCmd "run" "0.0.0.0" false]

def fastapiCases : List Case := [
  pc "fastapi-cli tests/test_cli.py::test_dev" "dev defaults"
    (let r := run fastapiCli ["dev", "single_file_app.py"]
     r.code == 0 && r.val "path" == "single_file_app.py" && r.val "host" == "127.0.0.1" &&
     r.val "port" == "8000" && r.val "reload" == "True" && r.val "root_path" == "" && r.val "proxy_headers" == "True"),
  pc "fastapi-cli tests/test_cli.py::test_dev_args" "dev with every option"
    (let r := run fastapiCli ["dev", "single_file_app.py", "--host", "192.168.0.2", "--port", "8080",
        "--no-reload", "--root-path", "/api", "--app", "api", "--no-proxy-headers"]
     r.val "host" == "192.168.0.2" && r.val "port" == "8080" && r.val "reload" == "False" &&
     r.val "root_path" == "/api" && r.val "app" == "api" && r.val "proxy_headers" == "False"),
  pc "fastapi-cli tests/test_cli.py::test_run" "run defaults"
    (let r := run fastapiCli ["run", "single_file_app.py"]
     r.val "host" == "0.0.0.0" && r.val "reload" == "False"),
  pc "fastapi-cli tests/test_cli.py::test_dev_env_vars" "PORT from the environment"
    (let r := run fastapiCli ["dev", "single_file_app.py"] (envOf [("PORT", "8111")])
     r.val "port" == "8111" && r.src "port" == some .environment),
  pc "fastapi-cli tests/test_cli.py::test_dev_env_vars_and_args" "command line beats PORT"
    (let r := run fastapiCli ["dev", "single_file_app.py", "--port", "8080"] (envOf [("PORT", "8111")])
     r.val "port" == "8080" && r.src "port" == some .commandline),
  pc "fastapi-cli tests/test_cli.py::test_version" "--version"
    (let r := run fastapiCli ["--version"]; r.code == 0 && has r.out "FastAPI CLI version: 0.0.0"),
  pc "fastapi-cli tests/test_cli.py::test_dev_help" "dev -h lists the options"
    (let r := run fastapiCli ["dev", "-h"]
     r.code == 0 && has r.out "Usage: fastapi dev [OPTIONS] [PATH]" && has r.out "--reload / --no-reload" &&
     has r.out "--port INTEGER" && has r.out "-e, --entrypoint TEXT" && has r.out "-h, --help"),
  pc "fastapi-cli tests/test_cli.py::test_callback_help" "root help lists both commands"
    (let r := run fastapiCli ["--help"]; has r.out "dev" && has r.out "run" && has r.out "--version"),
  pc "fastapi-cli tests/test_cli.py::test_dev_bad_port" "bad port"
    (let r := run fastapiCli ["dev", "--port", "eighty"]
     r.code == 2 && has r.out "Invalid value for '--port': 'eighty' is not a valid integer.")
]

/-! ## Typed commands (`click_command`), typer's rules as in fastapi-cli -/

/-- Serve an app. -/
click_command serveT
    (path : Argument (Option String)) "A path."
    (host : String := "127.0.0.1") "The host."
    (port : Nat := 8000) "The port." [envvar := ["PORT"]]
    (reload : Bool := true) "Reload."
    (workers : Option Nat := none)
    (tag : List String := [])
    (level : LeanClick.IntIn 0 3 := ⟨1, by decide⟩)
    : IO Unit :=
  IO.println s!"{path} {host} {port} {reload} {workers} {tag} {level.val}"

def typedValues (argv : List String) (env : Env := {}) : IO (Option String) := do
  let d := serveT.dispatch "serve" argv env
  match d.frames.getLast?, d.halt with
  | some f, none =>
    let r ← (do
      let path ← ClickType.fetch (Option String) f.args "path"
      let host ← ClickType.fetch String f.args "host"
      let port ← ClickType.fetch Nat f.args "port"
      let reload ← ClickType.fetch Bool f.args "reload"
      let workers ← ClickType.fetch (Option Nat) f.args "workers"
      let tag ← ClickType.fetch (List String) f.args "tag"
      let level ← ClickType.fetch (IntIn 0 3) f.args "level"
      pure s!"{path} {host} {port} {reload} {workers} {tag} {level.val}" : Terminal.ClickM String).run
    pure (match r with | .ok s => some s | .error _ => none)
  | _, _ => pure none

def typedCases : List Case := [
  ⟨"fastapi-cli tests/test_cli.py::test_dev", "typed command values arrive typed", do
    pure ((← typedValues ["main.py", "--port", "9", "--no-reload", "--workers", "2", "--tag", "a", "--tag", "b", "--level", "3"])
      == some "(some main.py) 127.0.0.1 9 false (some 2) [a, b] 3")⟩,
  ⟨"fastapi-cli tests/test_cli.py::test_dev_env_vars", "typed command defaults and envvar", do
    pure ((← typedValues [] (envOf [("PORT", "8111")])) == some "none 127.0.0.1 8111 true none [] 1")⟩,
  pc "tests/test_options.py::test_counting" "typed refined range rejects out-of-range"
    (has (run (Command.mk serveT.spec false () []) ["--level", "5"]).out "5 is not in the range 0<=x<=3."),
  pc "fastapi-cli tests/test_cli.py::test_dev_help" "typed command help: option spellings from binder names"
    (has (helpPage serveT "serve") "--reload / --no-reload" && has (helpPage serveT "serve") "--workers INTEGER RANGE" && has (helpPage serveT "serve") "[PATH]  A path.")
]

def cases : List Case := basicCases ++ optionCases ++ argCases ++ cmdCases ++ fastapiCases ++ typedCases

def main : IO UInt32 := do
  let mut failures := 0
  for c in cases do
    let ok ← try c.run catch e => do IO.println s!"      {e}"; pure false
    IO.println s!"{if ok then "ok  " else "FAIL"}  {c.name}  [{c.upstream}]"
    unless ok do failures := failures + 1
  IO.println s!"{cases.length} cases, {failures} failing"
  return if failures == 0 then 0 else 1
