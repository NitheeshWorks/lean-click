import LeanClick
import Lean.Data.Json

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
  /-- stdout and stderr together, for value checks. -/
  out : String
  code : Nat
  frames : List (Frame Unit)
  stdout : String := ""
  stderr : String := ""

def run (c : Cmd) (argv : List String) (env : Env := {}) (defaultMap : Option DefaultMap := none) : Run :=
  let d := c.dispatch c.name argv env defaultMap
  match d.halt with
  | none => ⟨"", 0, d.frames, "", ""⟩
  | some (.error e) => ⟨e.render, 2, d.frames, "", e.render⟩
  | some (.help t) => ⟨t ++ "\n", 0, d.frames, t ++ "\n", ""⟩
  | some (.message t) => ⟨t ++ "\n", 0, d.frames, t ++ "\n", ""⟩
  | some (.prompt _ p _) => ⟨s!"PROMPT {p.name}", 1, d.frames, "", s!"PROMPT {p.name}"⟩
  | some .abort => ⟨"Aborted!\n", 1, d.frames, "", "Aborted!\n"⟩

/-- The value the innermost command received, rendered as click echoes it. -/
def Run.val (r : Run) (name : String) : String :=
  match r.frames.getLast? with
  | some f => (f.args.get name).render
  | none => "<no frame>"

def Run.src (r : Run) (name : String) : Option Source :=
  match r.frames.getLast? with
  | some f => f.args.source? name
  | none => none


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
  pc "tests/test_basic.py::test_string_option" "string option --s= is empty" ((run sOpt ["--s="]).val "s" == ""),
  pc "tests/test_basic.py::test_string_option" "string option unicode" ((run sOpt ["--s=☃"]).val "s" == "☃"),
  pc "tests/test_basic.py::test_int_option" "int option default" ((run iOpt []).val "i" == "42"),
  pc "tests/test_basic.py::test_int_option" "int option --i=23" ((run iOpt ["--i=23"]).val "i" == "23"),
  pc "tests/test_basic.py::test_float_option" "float option default" ((run fOpt []).val "f" == "42.0"),
  pc "tests/test_basic.py::test_float_option" "float option --f=23.5" ((run fOpt ["--f=23.5"]).val "f" == "23.5"),
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
  pc "tests/test_basic.py::test_choice_argument" "choice argument value" ((run methodArg ["foo"]).val "method" == "foo"),
  pc "tests/test_basic.py::test_uuid_option" "UUID option"
    (let c : Cmd := command "cli" [option ["--u"] (type := UUID) (default := "ba122011-349f-423b-873b-9d6a79c688ab")]
     (run c []).val "u" == "ba122011-349f-423b-873b-9d6a79c688ab" &&
     (run c ["--u=821592C1C50E49719CD6E89DC6832F86"]).val "u" == "821592c1-c50e-4971-9cd6-e89dc6832f86"),
  pc "tests/test_basic.py::test_datetime_option_default" "DateTime option, default formats"
    (let c : Cmd := command "cli" [option ["--start_date"] (type := DateTime)]
     (run c ["--start_date=2015-09-29"]).val "start_date" == "2015-09-29 00:00:00" &&
     (run c ["--start_date=2015-09-29T09:11:22"]).val "start_date" == "2015-09-29 09:11:22" &&
     (run c ["--start_date=2015-02-30"]).code == 2),
  pc "tests/test_basic.py::test_datetime_option_custom" "DateTime with custom format"
    ((run (command "cli" [option ["--start_date"] (type := DateTime ["%A %B %d, %Y"])] : Cmd)
      ["--start_date=Wednesday June 05, 2010"]).val "start_date" == "2010-06-05 00:00:00"),
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
  pc "tests/test_options.py::test_counting" "count absent is 0" ((run countCmd []).val "v" == "0"),
  pc "tests/test_options.py::test_multiple_required" "multiple values, attached short value"
    ((run msgCmd ["-m", "foo", "-mbar"]).val "message" == "(foo, bar)"),
  pc "tests/test_options.py::test_nargs_tup_composite_mult" "Tuple option, multiple"
    (let c : Cmd := command "copy" [option ["--item"] (type := Tuple [STRING, INT]) (multiple := true)]
     (run c ["--item", "peter", "1", "--item", "max", "2"]).val "item" == "((peter, 1), (max, 2))"),
  pc "tests/test_arguments.py::test_nargs_tup_composite" "Tuple argument and option, as click 8.5.0"
    (let c : Cmd := command "c" [option ["--item"] (type := Tuple [STRING, INT]) (multiple := true),
        option ["--pt"] (type := Tuple [INT, FLOAT]) (default := ["1", "2.5"]) (showDefault := true),
        argument "pair" (type := Tuple [STRING, INT])]
     let r := run c ["a", "1"]
     r.val "pair" == "(a, 1)" && r.val "pt" == "(1, 2.5)" && r.val "item" == "()" &&
     (run c ["--pt", "1", "2", "a", "3"]).val "pt" == "(1, 2.0)"),
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
  pc "tests/test_options.py::test_case_insensitive_choice" "case-insensitive choice returns the declared spelling"
    (let c := mkCmd "cmd" [Param.opt ["--foo"] (.choice ["Orange", "Apple"] false)]
     (run c ["--foo", "apple"]).val "foo" == "Apple" && (run c ["--foo", "oRANGe"]).val "foo" == "Orange"),
  pc "tests/test_options.py::test_case_insensitive_choice" "case-sensitive choice rejects other case"
    (let c := mkCmd "cmd" [Param.opt ["--foo"] (.choice ["Orange", "Apple"])]
     (run c ["--foo", "apple"]).code == 2 && (run c ["--foo", "Apple"]).code == 0),
  pc "tests/test_options.py::test_invalid_option" "option value taken even if it looks like an option"
    ((run (mkCmd "cli" [Param.opt ["--name"], Param.flag ["--other"]]) ["--name", "--other"]).val "name" == "--other"),
  pc "tests/test_options.py::test_aliases_for_flags" "secondary spelling of a pair"
    (let c := mkCmd "cli" [Param.flag ["--warnings/--no-warnings", "-w/-W"]]
     (run c ["-W"]).val "warnings" == "False" && (run c ["-w"]).val "warnings" == "True"),
  pc "tests/test_options.py::test_legacy_options" "single-dash long option"
    (let c := mkCmd "cmd" [Param.opt ["-whatever"]]
     (run c ["-whatever", "42"]).val "whatever" == "42" && (run c ["-whatever=23"]).val "whatever" == "23"),
  pc "tests/test_options.py::test_intrange_default_help_text" "unbounded range shows no range (UP-023)"
    ((run (mkCmd "cli" [(Param.opt ["--n"] (.intRange {})).withDefault "1" |>.showDefaultParam]) ["--help"]).stdout ==
      "Usage: cli [OPTIONS]\n\nOptions:\n  --n INTEGER RANGE  [default: 1]\n  --help             Show this message and exit.\n"),
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
  pc "tests/test_basic.py::test_evaluation_order" "parameter callback can replace the value"
    (let up : Value → Except CallbackError Value := fun v => match v with | .str s => .ok (.str s.toUpper) | v => .ok v
     (run (command "cmd" [option ["--name"] (callback := some up)] : Cmd) ["--name", "ann"]).val "name" == "ANN"),
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
  pc "tests/test_arguments.py::test_missing_arg" "missing argument"
    (let r := run (mkCmd "cmd" [Param.arg "arg"]) []; r.code == 2 && r.stderr == "Usage: cmd [OPTIONS] ARG\nTry 'cmd --help' for help.\n\nError: Missing argument 'ARG'.\n"),
  pc "tests/test_arguments.py::test_nargs_star_ordering" "star argument in the middle"
    (let c := mkCmd "cmd" [Param.arg "a", Param.arg "b" (nargs := .star), Param.arg "c"]
     let r := run c ["a", "b", "c", "d"]; r.val "a" == "a" && r.val "b" == "(b, c)" && r.val "c" == "d"),
  pc "tests/test_arguments.py::test_nargs_specified_plus_star_ordering" "fixed, star, fixed with nargs=2"
    (let c := mkCmd "cmd" [Param.arg "a" (nargs := .star), Param.arg "b", Param.arg "c" (nargs := .exactly 2)]
     let r := run c ["a", "b", "c", "d", "e", "f"]
     r.val "a" == "(a, b, c)" && r.val "b" == "d" && r.val "c" == "(e, f)"),
  pc "tests/test_arguments.py::test_empty_nargs" "empty star argument"
    ((run (mkCmd "cmd" [Param.arg "arg" (nargs := .star)]) []).val "arg" == "()"),
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
     (run c ["-"]).val "input" == "-")
]

/-! ## tests/test_commands.py, tests/test_formatting.py, tests/test_context.py -/

def cmdCases : List Case := [
  pc "tests/test_commands.py::test_group_with_args" "group argument then subcommand"
    (let g : Cmd := Command.group { name := "cli", params := [Param.arg "obj"] } [mkCmd "move" []]
     let r := run g ["obj1", "move"]; r.code == 0 && (r.frames.map (·.path)) == ["cli", "cli OBJ move"]),
  pc "tests/test_commands.py::test_default_maps" "default_map for a subcommand"
    (let g : Cmd := group "cli" [] [command "foo" [option ["--name"] (default := "normal")]]
     let r := run g ["foo"] (defaultMap := some (.node [] [("foo", .node [("name", ["changed"])] [])]))
     r.val "name" == "changed" && r.src "name" == some .defaultMap && (run g ["foo"]).val "name" == "normal"),
  pc "tests/test_defaults.py::test_flag_default_map" "default_map for a flag pair, in help too"
    (let g : Cmd := group "cli" [] [command "foo" [option ["--name/--no-name"] (isFlag := true) (showDefault := true) (help := "name flag")]]
     let dm : Option DefaultMap := some (.node [] [("foo", .node [("name", ["true"])] [])])
     (run g ["foo"]).val "name" == "False" && (run g ["foo"] (defaultMap := dm)).val "name" == "True"),
  pc "tests/test_commands.py::test_default_maps" "command line beats default_map"
    (let g : Cmd := group "cli" [] [command "foo" [option ["--name"] (default := "normal")]]
     (run g ["foo", "--name", "x"] (defaultMap := some (.node [] [("foo", .node [("name", ["changed"])] [])]))).val "name" == "x"),
  pc "tests/test_chain.py::test_basic_chaining" "chained group runs each command in order"
    (let g : Cmd := group "cli" [] [command "sdist" [], command "bdist" []] (chain := true)
     let r := run g ["bdist", "sdist", "bdist"]
     r.code == 0 && (r.frames.map (·.infoName)) == ["cli", "bdist", "sdist", "bdist"] &&
     (r.frames.head?.bind (·.invokedSub)) == some "*"),
  pc "tests/test_chain.py::test_chaining_with_options" "chained commands with options"
    (let g : Cmd := group "cli" [] [command "sdist" [option ["--format"]], command "bdist" [option ["--format"]]] (chain := true)
     let r := run g ["bdist", "--format=1", "sdist", "--format=2"]
     (r.frames.drop 1).map (fun f => (f.infoName, (f.args.get "format").render)) == [("bdist", "1"), ("sdist", "2")]),
  pc "tests/test_chain.py::test_chaining_with_arguments" "chained commands with arguments"
    (let g : Cmd := group "cli" [] [command "sdist" [argument "format"], command "bdist" [argument "format"]] (chain := true)
     let r := run g ["bdist", "1", "sdist", "2"]
     (r.frames.drop 1).map (fun f => (f.infoName, (f.args.get "format").render)) == [("bdist", "1"), ("sdist", "2")]),
  pc "tests/test_chain.py::test_group_arg_behavior" "a chained group with a required argument"
    (let g : Cmd := group "cli" [argument "arg"] [command "a"] (chain := true)
     let r := run g ["foo", "a"]
     r.code == 0 && (r.frames.map (·.infoName)) == ["cli", "a"] && (r.frames.head?.map (·.args.get "arg" |>.render)) == some "foo"),
  pc "tests/test_chain.py::test_group_chaining" "a group nested in a chained group is refused"
    (let c : Cmd := .mk { name := "cli", chain := true } true () [group "sub" [] [command "leaf"], command "other"]
     let r := run c ["sub", "leaf"]
     r.code == 2 && r.stderr == "Usage: cli [OPTIONS] COMMAND1 [ARGS]... [COMMAND2 [ARGS]...]...\nTry 'cli --help' for help.\n\nError: Found the group 'sub' as subcommand to another group  'cli' that is in chain mode. This is not supported.\n" &&
     (run c ["other"]).code == 0),
  pc "tests/test_commands.py::test_group_first_match" "first subcommand of a name wins"
    (let g : Cmd := Command.group { name := "cli" } [mkCmd "a" [Param.opt ["--x"]], mkCmd "a" [Param.opt ["--y"]]]
     (run g ["a", "--x", "1"]).val "x" == "1"),
  pc "tests/test_context.py::test_context_meta" "subcommand path includes the parent"
    (let g : Cmd := Command.group { name := "cli" } [mkCmd "sub" []]
     ((run g ["sub"]).frames.map (·.invokedSub)) == [some "sub", none]),
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
  pc "fastapi-cli tests/test_cli.py::test_dev_env_vars_and_args" "last command-line occurrence beats PORT"
    (let r := run fastapiCli ["dev", "--port", "1", "app.py", "--port=2"] (envOf [("PORT", "8111")])
     r.val "port" == "2" && r.src "port" == some .commandline),
  pc "fastapi-cli tests/test_cli.py::test_dev_env_vars" "PORT used when --port is absent from a longer argv"
    (let r := run fastapiCli ["dev", "app.py", "--host", "h", "--no-reload"] (envOf [("PORT", "8111")])
     r.val "port" == "8111" && r.src "port" == some .environment),
  pc "fastapi-cli tests/test_cli.py::test_dev" "default when neither is given"
    (let r := run fastapiCli ["dev", "app.py", "--host", "h"]
     r.val "port" == "8000" && r.src "port" == some .default),
  pc "fastapi-cli tests/test_cli.py::test_version" "--version"
    (let r := run fastapiCli ["--version"]; r.code == 0 && r.stdout == "FastAPI CLI version: 0.0.0\n"),
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
  let d := (serveT (σ := Unit)).dispatch "serve" argv env
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
    pure ((← typedValues [] (envOf [("PORT", "8111")])) == some "none 127.0.0.1 8111 true none [] 1")⟩
]

/-! ## tests/test_shell_completion.py -/

def words (c : Cmd) (args : List String) (inc : String) : List String :=
  (c.complete c.name args inc).map (·.value)

def completionCases : List Case := [
  pc "tests/test_shell_completion.py::test_command" "option names"
    (let c : Cmd := command "cli" [option ["-t", "--test"]]
     words c [] "" == [] && words c [] "-" == ["-t", "--test", "--help"] && words c [] "--" == ["--test", "--help"] &&
     words c [] "--t" == ["--test"] && words c ["-t", "a"] "-" == ["--help"]),
  pc "tests/test_shell_completion.py::test_group" "group commands and options"
    (let c : Cmd := group "cli" [option ["-a"]] [command "x", command "y"]
     words c [] "" == ["x", "y"] && words c [] "-" == ["-a", "--help"]),
  pc "tests/test_shell_completion.py::test_nested_group" "nested groups"
    (let c : Cmd := group "cli" [] [group "get" [] [group "full" [option ["--verbose"]] [command "data" [option ["-a"]]]]]
     words c [] "" == ["get"] && words c ["get"] "" == ["full"] && words c ["get", "full"] "" == ["data"] &&
     words c ["get", "full"] "-" == ["--verbose", "--help"] && words c ["get", "full", "data"] "" == [] &&
     words c ["get", "full", "data"] "-" == ["-a", "--help"]),
  pc "tests/test_shell_completion.py::test_group_command_same_option" "group and command share an option"
    (let c : Cmd := group "cli" [option ["-a"]] [command "x" [option ["-a"]]]
     words c [] "-" == ["-a", "--help"] && words c ["-a", "a"] "-" == ["--help"] &&
     words c ["-a", "a", "x"] "-" == ["-a", "--help"] && words c ["-a", "a", "x", "-a", "a"] "-" == ["--help"]),
  pc "tests/test_shell_completion.py::test_chained" "chained group completion"
    (let c : Cmd := .mk { name := "cli", chain := true } true () [command "set" [option ["-y"]], command "start", group "get" [] [command "full"]]
     words c [] "" == ["get", "set", "start"] && words c [] "s" == ["set", "start"] &&
     words c ["set", "start"] "" == ["get"] && words c ["get"] "" == ["full", "set", "start"] &&
     words c ["get", "full"] "" == ["set", "start"] && words c ["get"] "s" == ["set", "start"]),
  pc "tests/test_shell_completion.py::test_help_option" "help option completion"
    (let c : Cmd := group "cli" [] [command "with", command "no" (addHelpOption := false)]
     words c ["with"] "--" == ["--help"] && words c ["no"] "--" == []),
  pc "tests/test_shell_completion.py::test_argument_order" "arguments complete in order"
    (let c : Cmd := command "cli" [argument "plain", argument "c1" (type := Choice ["a1", "a2", "b"]),
        argument "c2" (type := Choice ["c1", "c2", "d"])]
     words c [] "" == [] && words c [] "a" == [] && words c ["x"] "a" == ["a1", "a2"] && words c ["x", "b"] "d" == ["d"]),
  pc "tests/test_shell_completion.py::test_argument_default" "arguments with defaults still complete"
    (let c : Cmd := command "cli" [argument "a" (type := Choice ["a"]) (default := "a"),
        argument "b" (type := Choice ["b"]) (default := "b")] (addHelpOption := false)
     words c [] "" == ["a"] && words c ["a"] "b" == ["b"] && words c ["x"] "b" == ["b"]),
  pc "tests/test_shell_completion.py::test_type_choice" "choice option values"
    (let c : Cmd := command "cli" [option ["-c"] (type := Choice ["a1", "a2", "b"])]
     words c ["-c"] "" == ["a1", "a2", "b"] && words c ["-c"] "a" == ["a1", "a2"] && words c ["-c"] "a2" == ["a2"]),
  pc "tests/test_shell_completion.py::test_choice_special_characters" "choices with special characters"
    (let c : Cmd := command "cli" [option ["-c"] (type := Choice ["!1", "!2", "+3"])]
     words c ["-c"] "" == ["!1", "!2", "+3"] && words c ["-c"] "!" == ["!1", "!2"] && words c ["-c"] "!2" == ["!2"]),
  pc "tests/test_shell_completion.py::test_choice_conflicting_prefix" "a declared prefix wins over a choice"
    (let c : Cmd := command "cli" [option ["-c"] (type := Choice ["!1", "!2", "+3"]), option ["+p"] (isFlag := true)]
     words c ["-c"] "" == ["!1", "!2", "+3"] && words c ["-c"] "+" == ["+p"]),
  pc "tests/test_shell_completion.py::test_option_count" "count options take no value"
    (let c : Cmd := command "cli" [option ["-c"] (count := true)]
     words c ["-c"] "" == [] && words c ["-c"] "-" == ["--help"]),
  pc "tests/test_shell_completion.py::test_path_types" "path and file completion kinds"
    (let k := fun (t : ParamType) => ((command "cli" [option ["-f"] (type := t)] : Cmd).complete "cli" ["-f"] "ab").map fun i => (i.value, i.type)
     k (File) == [("ab", "file")] && k (Path) == [("ab", "file")] && k (Path (fileOkay := false)) == [("ab", "dir")]),
  pc "tests/test_shell_completion.py::test_option_flag" "flag pair names, then the argument"
    (let c : Cmd := command "cli" [option ["--on/--off"], argument "a" (type := Choice ["a1", "a2", "b"])] (addHelpOption := false)
     words c [] "--" == ["--on", "--off"] && words c ["--on"] "a" == ["a1", "a2"]),
  pc "tests/test_shell_completion.py::test_flag_option_with_nargs_option" "nargs option after a flag"
    (let c : Cmd := command "cli" [argument "a" (type := Choice ["a1", "a2", "b"]), option ["--flag"] (isFlag := true),
        option ["-c"] (type := Choice ["p", "q"]) (nargs := 2)] (addHelpOption := false)
     words c ["a1", "--flag", "-c"] "" == ["p", "q"]),
  pc "tests/test_shell_completion.py::test_option_custom" "custom completion function"
    (let c : Cmd := command "cli" [argument "x", argument "y", argument "z" (shellComplete := some fun _ i => [i.toUpper])]
     words c ["a", "b"] "" == [""] && words c ["a", "b"] "c" == ["C"]),
  pc "tests/test_shell_completion.py::test_option_multiple" "multiple options are offered again"
    (let c : Cmd := command "type" [option ["-m"] (type := Choice ["a", "b"]) (multiple := true), option ["-f"]]
     words c ["-m"] "" == ["a", "b"] && (words c ["-m", "a"] "-").contains "-m" && words c ["-m", "a", "-m"] "" == ["a", "b"] &&
     !(words c ["-f", "f"] "-").contains "-f"),
  pc "tests/test_shell_completion.py::test_option_nargs" "nargs option values"
    (let c : Cmd := command "cli" [option ["-c"] (type := Choice ["a", "b"]) (nargs := 2)]
     words c ["-c"] "" == ["a", "b"] && words c ["-c", "a"] "" == ["a", "b"] && words c ["-c", "a", "b"] "" == []),
  pc "tests/test_shell_completion.py::test_argument_nargs" "nargs arguments"
    (let c : Cmd := command "cli" [argument "x" (type := Choice ["a", "b"]) (nargs := 2),
        argument "y" (type := Choice ["c", "d"]) (nargs := -1), option ["-z"]]
     words c [] "" == ["a", "b"] && words c ["a"] "" == ["a", "b"] && words c ["a", "b"] "" == ["c", "d"] &&
     words c ["a", "b", "c"] "" == ["c", "d"] && words c ["a", "-z", "1"] "" == ["a", "b"] &&
     words c ["a", "-z", "1", "b"] "" == ["c", "d"]),
  pc "tests/test_shell_completion.py::test_double_dash" "after --, words are values"
    (let c : Cmd := command "cli" [option ["--opt"], argument "name" (type := Choice ["name", "--", "-o", "--opt"])] (addHelpOption := false)
     words c [] "-" == ["--opt"] && words c ["value"] "-" == ["--opt"] &&
     words c [] "" == ["name", "--", "-o", "--opt"] && words c ["--"] "" == ["name", "--", "-o", "--opt"]),
  pc "tests/test_shell_completion.py::test_hidden" "hidden commands and options"
    (let c : Cmd := group "cli" [] [command "hidden" [option ["-a"], option ["-b"] (type := Choice ["a", "b"]) (hidden := true)]
        (addHelpOption := false) (hidden := true)]
     !(words c [] "").contains "hidden" && !(words c [] "hidden").contains "hidden" &&
     words c ["hidden"] "-" == ["-a"] && words c ["hidden", "-b"] "" == ["a", "b"]),
  pc "tests/test_shell_completion.py::test_full_complete" "bash and zsh output formats"
    (CompletionItem.format "bash" { value := "a" } == "plain,a" &&
     CompletionItem.format "zsh" { value := "a:b", help := some "h" } == "plain\na\\:b\nh" &&
     CompletionItem.format "zsh" { value := "a" } == "plain\na\n_" &&
     CompletionItem.format "fish" { value := "a", help := some "x\ty" } == "plain,a\tx y"),
  pc "tests/test_shell_completion.py::test_completion_item_data" "complete variable name"
    (completeVar "foo-bar" == "_FOO_BAR_COMPLETE" && (completionScript "bash" "foo-bar").isSome &&
     (completionScript "elvish" "foo-bar").isNone),
  pc "tests/test_parser.py::test_split_arg_string" "shell word splitting"
    (splitArgString "cli a 'b c' \"d e\" f\\ g" == ["cli", "a", "b c", "d e", "f g"] &&
     splitArgString "cli 'unterminated" == ["cli", "unterminated"])
]

/-! ## Golden cases: click 8.5.0's exact output

`tests/golden/generate.py` ran each scenario on click 8.5.0 and recorded the
exit code, stdout and stderr in `tests/golden/click.json`; every fixture
below is the twin of the click command of the same name there. The runner
replays each scenario and compares all three byte for byte. -/

def nopCmd (name : String) (params : List Param) (help : String := "") : Cmd := command name params (help := help)

def dataArgs6 : List Param := ["first", "second", "third", "fourth", "fifth", "sixth"].map (argument ·)

def devLike (name host : String) (reload : Bool) : Cmd :=
  command name (help := "Run a FastAPI app in development mode.") [
    argument "path" (type := Path) (required := some false),
    option ["--host"] (default := host) (help := "The host to serve on."),
    option ["--port"] (default := 8000) (envvar := ["PORT"]) (help := "The port to serve on."),
    option ["--reload/--no-reload"] (default := reload) (help := "Enable auto-reload."),
    option ["--root-path"] (default := ""),
    option ["--app"],
    option ["--entrypoint", "-e"],
    option ["--proxy-headers/--no-proxy-headers"] (default := true),
    option ["--verbose", "-v"] (isFlag := true)]

def hintGroup (extra : List Param) : Cmd :=
  group "cli" [] [nopCmd "foo" (argument "required_arg" :: extra)] (contextSettings := { helpOptionNames := some ["-h", "--help"] })

/-- Serve an app. -/
click_command serve
    (path : Argument (Option String)) "A path."
    (host : String := "127.0.0.1") "The host."
    (port : Nat := 8000) "The port." [envvar := ["PORT"]]
    (reload : Bool := true) "Reload."
    (workers : Option Nat := none)
    (tag : List String := [])
    (level : LeanClick.IntIn 0 3 := ⟨1, by decide⟩)
    : IO Unit :=
  IO.println s!"{path} {host} {port} {reload} {workers} {tag} {level.val}"

/-- One `default_map` leaf value. -/
def jsonLeaf (v : Lean.Json) : Option (List String) :=
  match v with
  | .str s => some [s]
  | .bool b => some [if b then "true" else "false"]
  | .num n => some [toString n]
  | .arr xs => some (xs.toList.filterMap fun (x : Lean.Json) => match x with
      | .str s => some s
      | .num n => some (toString n)
      | _ => none)
  | _ => none

/-- click's `default_map` from the JSON a scenario recorded (nesting depth
bounded by `fuel`). -/
def jsonDefaultMap : Nat → Lean.Json → Option DefaultMap
  | 0, _ => none
  | fuel + 1, .obj kvs =>
    let entries := kvs.toList
    let values := entries.filterMap fun (k, v) => (jsonLeaf v).map (k, ·)
    let subs := entries.filterMap fun (k, v) => match v with
      | .obj _ => (jsonDefaultMap fuel v).map (k, ·)
      | _ => none
    some (.node values subs)
  | _, _ => none

def goldenFixtures : List (String × Cmd) := [
  ("sopt", nopCmd "cli" [option ["--s"] (default := "no value")]),
  ("iopt", nopCmd "cli" [option ["--i"] (default := 42)]),
  ("fopt", nopCmd "cli" [option ["--f"] (default := 42.0)]),
  ("methodopt", nopCmd "cli" [option ["--method"] (type := Choice ["foo", "bar", "baz"])]),
  ("methodarg", nopCmd "cli" [argument "method" (type := Choice ["foo", "bar", "baz"])]),
  ("reqopt", nopCmd "cli" [option ["--foo"] (required := true)]),
  ("hiddenopt", nopCmd "cli" [option ["--nope"] (hidden := true)]),
  ("hiddencmd", group "cli" [] [command "nope" (hidden := true)]),
  ("summary", group "cli" [] [nopCmd "cmd" [] "\n        Summary line without period\n\n        Here is a sentence. And here too.\n        "]),
  ("invaliddefault", nopCmd "cli" [option ["-a"] (type := Path (exists_ := true)) (default := "not found") (showDefault := true)]),
  ("basicgroup", group "cli" [] [nopCmd "subcommand" [] "This is a subcommand."] (help := "This is the root.")),
  ("count", nopCmd "cli" [option ["-v"] (count := true) (help := "Verbosity") (type := IntRange (some 0) (some 3))]),
  ("empty", nopCmd "cli" []),
  ("boundcount", nopCmd "cli" [option ["--bound"], option ["--count"]]),
  ("msg", nopCmd "cli" [option ["-m", "--message"] (multiple := true) (required := true)]),
  ("shout", nopCmd "cli" [option ["--shout/--no-shout"] (envvar := ["SHOUT"])]),
  ("multidefault", nopCmd "cmd" [option ["--arg1"] (multiple := true) (default := ["foo", "bar"]) (showDefault := true),
    option ["--arg2"] (multiple := true) (default := ["1", "2"]) (type := INT) (showDefault := true)]),
  ("showenvvar", nopCmd "cmd" [option ["--arg1"] (envvar := ["ARG1"]) (showEnvvar := true)]),
  ("showenvvarauto", command "cmd" [option ["--arg1"] (showEnvvar := true)] (contextSettings := { autoEnvvarPrefix := some "TEST" })),
  ("dashgroup", group "cli" [] [command "foo-bar" [option ["--baz"] (showEnvvar := true)]] (contextSettings := { autoEnvvarPrefix := some "TEST" })),
  ("reqflag", nopCmd "cli" [option ["--on/--off"] (required := true)]),
  ("misschoice", nopCmd "cmd" [option ["--foo"] (type := Choice ["foo", "bar"]) (required := true)]),
  ("missenv1", nopCmd "cli" [option ["--foo"] (envvar := ["bar"]) (required := true)]),
  ("missenv2", nopCmd "cli" [option ["--foo"] (envvar := ["bar"]) (showEnvvar := true) (required := true)]),
  ("casechoice", nopCmd "cmd" [option ["--foo"] (type := Choice ["Orange", "Apple"] false)]),
  ("casechoice2", nopCmd "cmd" [option ["--foo"] (type := Choice ["Orange", "Apple"])]),
  ("flagdefault", nopCmd "cli" [option ["--cache/--no-cache"] (default := false) (showDefault := true) (help := "Enable the cache.")]),
  ("showdefaulttext", nopCmd "cli" [{ (option ["--limit"]) with showDefault := .text "unlimited" }]),
  ("emptydefault", nopCmd "cli" [option ["--limit"] (default := "") (showDefault := true)]),
  ("nodefault", nopCmd "cli" [option ["--limit"] (showDefault := true)]),
  ("intrange", nopCmd "cli" [option ["--count"] (type := IntRange (some 1) (some 32))]),
  ("countdefault", nopCmd "cli" [option ["--verbose"] (count := true)]),
  ("copyx", nopCmd "copy" [argument "x"]),
  ("missarg", nopCmd "cmd" [argument "arg"]),
  ("tupitem", nopCmd "cmd" [argument "item" (nargs := 2)]),
  ("star", nopCmd "cmd" [argument "arg" (nargs := -1) (required := some true)]),
  ("filearg", nopCmd "cmd" [argument "input" (type := File)]),
  ("noargshelp", command "cli" (noArgsIsHelp := true)),
  ("grpsub", group "cli" [] [command "sub"]),
  ("grpargs", group "cli" [argument "obj"] [command "move"]),
  ("unknowncmd", group "cli"),
  ("suggest", group "cli" [] [command "push", command "declare", command "refine"]),
  ("fmtbasic", nopCmd "cli" [] "First paragraph.\n\n        This is a very long second\n        paragraph and not correctly\n        wrapped but it will be rewrapped.\n\n        \x08\n        This is\n        a paragraph\n        without rewrapping.\n\n        \x08\n        1\n         2\n          3\n\n        And this is a paragraph\n        that will be rewrapped again.\n        "),
  ("longopts", group "cli" [] [group "a-very-long" [] [nopCmd "command" dataArgs6 "A command."] (help := "Second level")] (help := "Top level command")),
  ("longname", group "cli" [] [group "a-very-very-very-long" [] [nopCmd "command" dataArgs6 "A command."] (help := "Second level")] (help := "Top level command")),
  ("metavarmiss", nopCmd "cmd" [argument "arg" (metavar := some "metavar")]),
  ("metavarbad", nopCmd "cmd" [argument "arg" (type := INT) (metavar := some "metavar")]),
  ("nested", group "cmd" [] [nopCmd "foo" [argument "bar"]]),
  ("nohelp", command "cmd" [argument "arg"] (addHelpOption := false)),
  ("customhelp", command "cmd" [argument "arg"] (contextSettings := { helpOptionNames := some ["--man"] })),
  ("version", nopCmd "cli" [versionOption "1.0"]),
  ("prefixes", nopCmd "cli" [option ["++foo"] (isFlag := true) (help := "das foo"), option ["--bar"] (isFlag := true) (help := "das bar")]),
  ("winstyle", command "cmd" [option ["/debug;/no-debug"] (help := "Enables or disables debug mode.")] (contextSettings := { helpOptionNames := some ["/?"] })),
  ("float1e20", nopCmd "cli" [option ["--f"] (type := FLOAT) (default := "1e20") (showDefault := true)]),
  ("callbacks", nopCmd "c" [option ["--a"] (callback := some fun _ => .error "no"), option ["--b"] (type := INT)]),
  ("validation", nopCmd "cmd" [option ["--foo"] (default := 1) (callback := some fun v => match v with
      | .int n => if n < 0 then .error "Value needs to be positive" else .ok v
      | v => .ok v)]),
  ("chainhelp", group "cli" [] [nopCmd "sdist" [] "SDIST HELP", nopCmd "bdist" [] "BDIST HELP"] (help := "ROOT HELP") (chain := true)),
  ("chainunknown", group "cli" [] [command "a"] (chain := true)),
  ("uuid", nopCmd "cli" [option ["--u"] (type := UUID) (default := "ba122011-349f-423b-873b-9d6a79c688ab")]),
  ("datetime", nopCmd "cli" [option ["--start_date"] (type := DateTime)]),
  ("datetimearg", nopCmd "cli" [argument "when" (type := DateTime ["%Y-%m-%d"]) (required := some false)]),
  ("tuple", nopCmd "c" [option ["--item"] (type := Tuple [STRING, INT]) (multiple := true),
    option ["--pt"] (type := Tuple [INT, FLOAT]) (default := ["1", "2.5"]) (showDefault := true),
    argument "pair" (type := Tuple [STRING, INT])]),
  ("fastapi", group "fastapi" [option ["--verbose/--no-verbose"]] [devLike "dev" "127.0.0.1" true, devLike "run" "0.0.0.0" false]
    (contextSettings := { helpOptionNames := some ["-h", "--help"] })),
  ("hint1", hintGroup []), ("hint2", hintGroup [option ["--host", "-h"]]),
  ("hint3", hintGroup [option ["--help-file", "--help"]]),
  ("hint4", hintGroup [option ["--host", "-h"], option ["--help-file", "--help"]]),
  ("flagmap", group "cli" [] [nopCmd "foo" [option ["--name/--no-name"] (isFlag := true) (showDefault := true) (help := "name flag")]]),
  ("typedserve", Command.mk (serve (σ := Unit)).spec false () [])
]

def jsonStr (j : Lean.Json) (k : String) : String := (j.getObjValAs? String k).toOption.getD ""

/-- A whole-process scenario: run the example binary with the arguments and
stdin click's program got, compare exit code, stdout and stderr. -/
def processCase (prog : String) (j : Lean.Json) : Case :=
  let argv := ((j.getObjValAs? (List String) "argv").toOption).getD []
  let input := jsonStr j "input"
  let code := ((j.getObjValAs? Nat "code").toOption).getD 99
  let wantOut := jsonStr j "stdout"
  let wantErr := jsonStr j "stderr"
  -- the scenario's whole environment (nothing inherited), as the generator ran it
  let env : Array (String × Option String) := match j.getObjVal? "env" with
    | .ok (.obj kvs) => kvs.toArray.filterMap fun (k, v) => match v with | .str s => some (k, some s) | _ => none
    | _ => #[("COLUMNS", some "80")]
  let cwd := match j.getObjVal? "cwd" with | .ok (.str d) => some d | _ => none
  ⟨jsonStr j "upstream", s!"process {prog} {argv} (tests/golden/click.json)", do
    let root ← IO.currentDir
    let bin := root / ".lake" / "build" / "bin" / prog
    unless ← bin.pathExists do
      IO.println s!"      no program {bin} (lake build {prog})"
      return false
    let child ← IO.Process.spawn {
      cmd := (root / ".lake" / "build" / "bin" / prog).toString, args := argv.toArray,
      cwd := cwd.map (root / ·), stdin := .piped, stdout := .piped, stderr := .piped,
      env, inheritEnv := (j.getObjVal? "env").toOption.isNone }
    let (stdin, child) ← child.takeStdin
    stdin.putStr input
    stdin.flush
    let outTask ← IO.asTask child.stdout.readToEnd
    let errTask ← IO.asTask child.stderr.readToEnd
    let rc ← child.wait
    -- the repository root is written as <ROOT> in the recording
    let fix := fun (t : String) => t.replace root.toString "<ROOT>"
    let out := fix (← IO.ofExcept outTask.get)
    let err := fix (← IO.ofExcept errTask.get)
    let ok := rc.toNat == code && out == wantOut && err == wantErr
    unless ok do
      IO.println s!"      code {rc} want {code}"
      IO.println s!"      stdout {repr out}\n      want   {repr wantOut}"
      IO.println s!"      stderr {repr err}\n      want   {repr wantErr}"
    pure ok⟩

def goldenCases : IO (List Case) := do
  let text ← IO.FS.readFile "tests/golden/click.json"
  let some arr := (Lean.Json.parse text).toOption.bind (·.getArr?.toOption)
    | throw (IO.userError "tests/golden/click.json does not parse")
  return arr.toList.map fun j =>
    if let .ok (.str prog) := j.getObjVal? "process" then processCase prog j else
    let fx := jsonStr j "fixture"
    let argv := ((j.getObjValAs? (List String) "argv").toOption).getD []
    let envVars := match j.getObjVal? "env" with
      | .ok (.obj kvs) => kvs.toList.filterMap fun (k, v) => match v with | .str s => some (k, s) | _ => none
      | _ => []
    let width := ((j.getObjValAs? Nat "width").toOption).getD 80
    let code := ((j.getObjValAs? Nat "code").toOption).getD 99
    let wantOut := jsonStr j "stdout"
    let wantErr := jsonStr j "stderr"
    ⟨jsonStr j "upstream", s!"golden {fx} {argv} (tests/golden/click.json)", do
      match goldenFixtures.lookup fx with
      | none => IO.println s!"      no fixture {fx}"; pure false
      | some c =>
        let dm := match j.getObjVal? "default_map" with
          | .ok v => jsonDefaultMap 8 v
          | .error _ => none
        let r := run c argv { vars := envVars, width } dm
        let ok := r.code == code && r.stdout == wantOut && r.stderr == wantErr
        unless ok do
          IO.println s!"      code {r.code} want {code}"
          IO.println s!"      stdout {repr r.stdout}\n      want   {repr wantOut}"
          IO.println s!"      stderr {repr r.stderr}\n      want   {repr wantErr}"
        pure ok⟩

def cases : List Case := basicCases ++ optionCases ++ argCases ++ cmdCases ++ fastapiCases ++ typedCases ++ completionCases

def main : IO UInt32 := do
  let mut failures := 0
  let all := cases ++ (← goldenCases)
  for c in all do
    let ok ← try c.run catch e => do IO.println s!"      {e}"; pure false
    IO.println s!"{if ok then "ok  " else "FAIL"}  {c.name}  [{c.upstream}]"
    unless ok do failures := failures + 1
  IO.println s!"{all.length} cases, {failures} failing"
  return if failures == 0 then 0 else 1
