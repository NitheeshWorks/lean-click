# lean-click

[![Lean 4](https://img.shields.io/badge/built%20with-Lean%204-blue.svg)](https://lean-lang.org)
[![Proofs](https://img.shields.io/badge/proofs-machine%20checked-brightgreen.svg)](CONTRIBUTING.md#verification)

A clean-room port of [click](https://github.com/pallets/click) to Lean 4:
composable command-line interfaces whose help page is rendered from the same
table the parser is proved against.

## A Simple Example

click's own quickstart, translated:

```lean
import LeanClick
open LeanClick

/-- Simple program that greets NAME for a total of COUNT times. -/
click_command hello
    (count : Int := 1) "Number of greetings."
    (name : String) "The person to greet." [prompt := "Your name"]
    : IO Unit := do
  for _ in [0:count.toNat] do
    IO.println s!"Hello, {name}!"

def main (args : List String) : IO UInt32 := hello.run () args
```

```
$ lake exe hello --count=3
Your name: Click
Hello, Click!
Hello, Click!
Hello, Click!

$ lake exe hello --help
Usage: hello [OPTIONS]

  Simple program that greets NAME for a total of COUNT times.

Options:
  --count INTEGER  Number of greetings.
  --name TEXT      The person to greet.
  --help           Show this message and exit.
```

A command is a typed Lean function, as typer (and fastapi-cli) write click
commands: a binder with a default is an option (`root_path` is
`--root-path`), one without is an argument, a `Bool` with a default is a
`--x/--no-x` flag pair, `Option α` is optional, `List α` repeatable,
`IntIn lo hi` an `IntRange` whose values carry their bounds proof. The doc
comment is the help text; a string after a binder is its help; `[envvar :=
["PORT"], decls := ["--entrypoint", "-e"], prompt := "...", type := Path
(exists_ := true)]` passes click's keywords. The body receives typed values,
never strings looked up by name. `examples/FastapiCli.lean`
(`lake exe example`) is `fastapi dev` / `fastapi run` in this style.

The decorator vocabulary is there too, for groups and hand-built commands:
`command`, `group` (with `chain := true`), `option`, `argument`,
`versionOption`, the types `STRING` `INT` `FLOAT` `BOOL` `UUID` `DateTime`
`Tuple` `Choice` `Path` `File` `IntRange` `FloatRange`, parameter `callback`s,
`contextSettings` with `defaultMap`, and shell completion through
`_PROG_COMPLETE` (bash, zsh, fish), and click's keyword arguments (`default`, `help`, `required`,
`envvar`, `multiple`, `nargs`, `isFlag`, `flagValue`, `count`, `prompt`,
`showDefault`, `hidden`, `metavar`, `contextSettings`). Errors, help pages
and exit codes match click's text. The pure parser is
`LeanClick.parse cmd argv env`, whose result is refined by its
postcondition (`ParsedFor`).

## What is proved

Every theorem is machine-checked; CI fails on `sorry`, on `partial`, and on
any axiom beyond `propext`, `Quot.sound`, `Classical.choice`.

| In words | Lean |
|---|---|
| Parsing reads the environment only through the variables the command declares | `parse_env_local : (∀ n ∈ cmd.envNames s, e₁.get n = e₂.get n) → e₁.fs = e₂.fs → e₁.answers = e₂.answers → cmd.parseArgs s path argv e₁ = cmd.parseArgs s path argv e₂` |
| Every spelling a visible option accepts is on its `--help` page | `help_lists_every_param : p ∈ cmd.allParams s → p.isOption → ¬p.hidden → ∀ o ∈ p.allOpts, IsInfix o (cmd.formatHelp s path)` |
| An undeclared long option fails with `No such option` | `undeclared_option_rejected : … → cmd.parseArgs s path (pre ++ tok :: post) env = .error (.error ⟨.noSuchOption tok _, _⟩)` |
| A required parameter with no value gives `Missing option` / `Missing argument` | `required_missing_error : … → resolveParam fs ans none env p = .error (.error p.missingError)` |
| A successful parse leaves no required parameter missing | `required_present : cmd.parseArgs s path argv env = .ok r → ∀ e ∈ r.entries, e.param.required → e.value.isMissing = false` |
| A `nargs = n` value holds exactly `n` values | `nargs_arity : e.param.shape = .tuple n → e.value = .tuple xs → xs.length = n` |
| A `Tuple` value converts from exactly `arity` tokens and holds one value per position | `tuple_arity : convertComposite fs t xs = .ok y → xs.length = t.arity ∧ (t.components y).length = t.arity` |
| A choice converts iff a declared choice matches, to the first match | `choice_in_set : (∃ c, convert fs (.choice cs cS) s = .ok c ∧ c.val = v) ↔ cs.find? (fun c => normChoice cS c == normChoice cS s) = some v` |
| Over any sequence of a flag pair's spellings, the last one decides | `bool_flag_pairs : … → cmd.parseArgs s path argv env = .ok r → ∃ e ∈ r.entries, e.param = p ∧ e.source = .commandline ∧ e.toValue = .bool (decide (argv.getLast h ∈ opts))` |
| The command line beats the environment variable: an option given as `--o v` / `--o=v`, any number of times, holds the last occurrence's value | `commandline_beats_envvar : … → cmd.parseArgs s path (occTokens occs) env = .ok r → ∃ e ∈ r.entries, e.param = p ∧ e.source = .commandline ∧ ∃ x, convert env.fs p.type (occs.getLast h).2.1 = .ok x ∧ e.toValue = p.type.erase x` |
| The environment variable beats the default: an option absent from any argv takes its variable's converted value | `envvar_beats_default : … → Absent cmd s p argv → envString env.get s p = some w → cmd.parseArgs s path argv env = .ok r → ∃ e ∈ r.entries, e.param = p ∧ e.source = .environment ∧ ∃ x, convert env.fs p.type w = .ok x ∧ e.toValue = p.type.erase x` |
| The default applies when neither is given | `default_when_absent : … → Absent cmd s p argv → envString env.get s p = none → p.default = some [d] → cmd.parseArgs s path argv env = .ok r → ∃ e ∈ r.entries, e.param = p ∧ e.source = .default ∧ ∃ x, convert env.fs p.type d = .ok x ∧ e.toValue = p.type.erase x` |
| `LeanClick.parse` is `parseArgs` under the root settings, so all of the above apply to it | `parse_val : parse cmd argv env prog = .ok r → cmd.parseArgs (cmd.settings _ prog) prog argv env = .ok r.val` |
| What a callback reads is the converted value (or its callback's result) | `parse_get_eq_toValue : cmd.parseArgs … = .ok r → r.entry? n = some e → e.param.callback = none → r.get n = e.toValue` |
| A group runs the first subcommand with the name | `group_dispatch_first_match : cmd.parseLevel s path argv env = .ok lv → lv.sub = some (c, name, rest) → cmd.subcommands.find? (·.name == name) = some c` |
| Exit codes are bytes; click's own are 0, 1, 2 | `exit_code_range : t.exitCode < 256`, `framework_exit_codes : t.isFramework → t.exitCode ≤ 2` |

`Absent cmd s p argv` says no token of `argv` names one of `p`'s
spellings, as a long option or inside a short-option cluster.
`Command.Unambiguous` (no two parameters share a spelling) is proved by
`click_command` for every command it builds (`<name>.unambiguous`, by
`decide`), so users never discharge it. Choice values are `{c // c ∈ choices}` and range values carry their bounds
proof, by type. What is tested rather than proved (text wrapping, close-match
hints, number parsing, the `Terminal` module) is listed in
[CONTRIBUTING.md](CONTRIBUTING.md#verification) and `docs/traceability.toml`.

## Getting started

```
lake build            # library, proofs, tests, examples
lake exe props        # property tests (seeded; `lake exe props 42`)
lake exe conformance  # cases re-expressed from click's test suite
scripts/check.sh      # the whole CI gate, locally
```

Toolchain: `leanprover/lean4:v4.35.0`.

Part of the verified FastAPI stack: [lean-fastapi](https://github.com/NitheeshWorks/lean-fastapi)
builds `fastapi dev` / `fastapi run` on this repo. Depends on Lean core only.

## License

MIT, see [LICENSE](LICENSE). Upstream attribution is in [NOTICE](NOTICE).
