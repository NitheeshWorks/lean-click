# lean-click

[![Lean 4](https://img.shields.io/badge/built%20with-Lean%204-blue.svg)](https://lean-lang.org)
[![Proofs](https://img.shields.io/badge/proofs-machine%20checked-brightgreen.svg)](CONTRIBUTING.md#verification)

A clean-room port of [click](https://github.com/pallets/click) to Lean 4: a command-line parser whose help text is rendered from the same table the parser is proved against.

Part of the verified FastAPI stack: [lean-fastapi](https://github.com/NitheeshWorks/lean-fastapi)
and its dependencies, each ported as its own repo with its proofs. This one
depends on: none (Lean core only).

## What "verified" means here

* Public functions take the weakest precondition they need and return the
  strongest postcondition, as a refinement type `{r // Post r}` or an
  `Except` of one. Invariants live in types, not comments.
* Every theorem is machine-checked. CI fails on `sorry` and on any axiom
  beyond Lean's own `propext`, `Quot.sound` and `Classical.choice`.
* What is tested rather than proved is listed, with the reason, in
  [CONTRIBUTING.md](CONTRIBUTING.md#verification) and in
  `docs/traceability.toml`.

## Getting started

```
lake build            # library, proofs, and both test executables
lake exe props        # property tests (seeded; `lake exe props 42`)
lake exe conformance  # cases re-expressed from the upstream test suite
scripts/check.sh      # the whole CI gate, locally
```

Toolchain: `leanprover/lean4:v4.35.0` (pinned in `lean-toolchain`; `elan` installs it).

## Usage

```lean
import LeanClick
open LeanClick LeanClick.Terminal

def dev : App Unit := App.command {
    name := "dev", help := some "Run a FastAPI app in development mode.",
    params := [
      (Param.arg "path" (.path { exists_ := true })).requiredParam false,
      (Param.opt ["--host"]).withDefault "127.0.0.1",
      (Param.opt ["--port"] .int).withDefault "8000" |>.withEnvvar ["PORT"],
      (Param.flag ["--reload/--no-reload"]).withDefault "true"] }
  (fun ctx => do echo s!"port {ctx.params.getInt? "port"}")

def main (args : List String) : IO UInt32 := Terminal.main dev () args
```

The pure parser is `LeanClick.parse cmd argv env`, returning the parsed
arguments with their postcondition; `Command.dispatch` parses a whole group
invocation; `helpPage cmd` renders `--help`. See `examples/FastapiCli.lean`
(`lake exe example`).

## Status

Milestone 10 slice: options and arguments (string, int, float, bool, choice,
path, file, int/float ranges), `nargs`, `multiple`, `required`, defaults,
envvars and automatic envvar prefixes, boolean flag pairs, constant and count
flags, commands and groups with `Context`, help and usage rendering, click's
error messages and exit codes, `--help`/`--version`, prompts and
confirmation. Proved: the ten headline theorems listed in
[CONTRIBUTING.md](CONTRIBUTING.md#verification); tested: text wrapping,
close-match hints, number parsing and the `Terminal` module.

## License

MIT, see [LICENSE](LICENSE). Upstream attribution is in [NOTICE](NOTICE).
