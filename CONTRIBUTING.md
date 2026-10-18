# Contributing to lean-click

`README.md` covers usage. This file covers the architecture, the verification
rules, the repository layout and the conventions, and is the document to keep
current when the code changes.

## Architecture

`LeanClick/` is layered bottom-up; each layer imports only the ones below it, so a
proof at one layer rewrites with lemmas from beneath it instead of unfolding
to bytes:

| Directory | Holds | Proved? |
|---|---|---|
| `DataTypes/` | structures and inductives; invariants are fields | n/a |
| `RepoImpl/` | executable definitions; total, structural recursion | subject of proofs |
| `Specifications/` | the properties, in `Prop` | the contract |
| `Proofs/` | theorems; `Proofs/Verified.lean` bundles one `Verified<Layer>` per layer | yes |
| `APIs/` | the public surface: builders, accessors, and `LeanClick.parse`, which returns `{r // ParsedFor cmd s r}` | the refinement, by `parseArgs_parsedFor` |
| `Terminal.lean` | the only effectful module: environment, filesystem view, prompts, output, exit | no; tested |

Everything except `Terminal.lean` is a pure function. A `Command κ` is
generic in the type `κ` of its callbacks; the parser never inspects them, and
only `Terminal` fixes `κ` to an `IO` action (`Terminal.App σ`).

### Modules

| Module | Holds |
|---|---|
| `DataTypes/Types.lean` | `ParamType`, its refined `Carrier` (choice membership, range bounds), `Dec` |
| `DataTypes/Param.lean` | `Param`, `Decl`/`OptKind`, `Entry` (with `h_fits`), `Command`, `Context`, errors, `Env` |
| `RepoImpl/Text.lean` | `repr`, `textwrap`, `difflib`, Python number parsing |
| `RepoImpl/Convert.lean` | conversion of one token through a type |
| `RepoImpl/Scan.lean` | the port of `parser.py`: the option table, events, `unpack` |
| `RepoImpl/Command.lean` | `Command.allParams` (the table), settings, metavars, hints |
| `RepoImpl/Help.lean` | the help page and usage line from `allParams` |
| `RepoImpl/Resolve.lean` | one parameter: source precedence, prompt, conversion, required, eager |
| `RepoImpl/Parse.lean` | `parseWith`, group dispatch, messages, exit codes |
| `RepoImpl/Complete.lean` | shell completion: resolution, completions, formats, scripts |
| `APIs/Click.lean` | click's decorator vocabulary: `command`, `option`, `argument`, types |
| `Typed.lean` | `ClickType` and the `click_command` macro: commands as typed functions |
| `Specifications/Parse.lean` | the ten headline properties |
| `Proofs/*.lean` | their proofs; `Verified.lean` bundles `VerifiedParser`/`VerifiedHelp` | This is
load-bearing: a round-trip property of a function is an equation that
`omega` and `simp` attack directly; the same property of a procedure would
be a Hoare triple over a heap.

## Verification <a name="verification"></a>

* **Weak preconditions, strong postconditions.** A public function takes the
  minimal hypothesis its body needs (usually none; the refinement lives in the
  input type) and returns the strongest useful statement, typically a round
  trip or a membership, as `{r // Post r}` or `Except Err {r // Post r}`.
* **Refinement types are subtypes with `Decidable` predicates**, so the same
  predicate is checked at runtime (`if h : P x`) and used in proofs.
* **Totality.** No `partial def` or `unsafe` under `LeanClick/`. Byte parsers recurse
  on a `Nat` fuel equal to the remaining input, with a progress lemma per
  production.
* **`_ext` lemmas.** A decoder lemma holds on any continuation of the buffer and
  reports the final position, so field lemmas compose. Do not add a lemma that
  only holds at the end of a buffer; it is unusable one field later.
* **Numbers.** `Nat`/`Int`/decimal-as-rational in proofs. `Float` only at the
  boundary via `Float.toBits`. Never `bv_decide` or `native_decide`.
* **The gate.** `scripts/check.sh` runs what CI runs: build, `sorry` grep,
  `scripts/Axioms.lean` (every theorem under `LeanClick` reaches only `propext`,
  `Quot.sound`, `Classical.choice`), `lake exe props`, `lake exe conformance`,
  the traceability check.

### Proved

`parse_env_local`, `help_lists_every_param` (plus
`help_lists_every_argument`, `help_lists_every_subcommand`),
`undeclared_option_rejected`, `required_missing_error`, `required_present`,
`nargs_arity`, `choice_in_set`, `bool_flag_pairs`,
`commandline_beats_envvar`, `envvar_beats_default`, `default_when_absent`,
`group_dispatch_first_match`, `exit_code_range`, `framework_exit_codes`,
with their lemmas in `Proofs/` (`Proofs/Precedence.lean` holds the
command line > envvar > default theorems for arbitrary argv: the scanner
only emits an event for a parameter from a token that `Mentions` its
spelling, `scanLoop_events`). `parse_val` links `LeanClick.parse` to
`parseArgs`. The parse-level proofs share one step,
`parseArgs_entry` (the entry for a parameter is what `resolveParam` makes of
its scanned command line), and the help proofs one theory of string
containment (`IsInfix`, `concatS_infix`, `renderSection_infix`). The axiom gate's theorem count also includes the compiler's
`inj`/`sizeOf_spec` lemmas; the hand-written theorems are the `theorem`
declarations under `LeanClick/Proofs/`.

### Tested, not proved

Listed here with the reason, and in `docs/traceability.toml` with
`status = "tested"`.

* Text wrapping (`Text.wrap`, `wrapParagraphs`): a port of Python's
  `textwrap` greedy filler with hyphen breaking; its byte-exact layout is a
  property of the algorithm, not of the parser, and is checked against the
  upstream formatting tests and `Props.wrapWidth`/`wrapKeepsText`.
* "Did you mean" hints (`Text.closeMatches`): `difflib` ratios; the choice of
  suggestions only affects the message text (`Props.closeSubset`).
* Python number parsing and printing (`parseInt`, `parseDec`, `showDec`):
  round-trip property tests.
* `Terminal.lean`: prompts, confirmation, the filesystem view, the exit code
  reporting. Effectful; exercised by `examples/FastapiCli.lean`.

### Deviations from upstream

Recorded as `status = "deviation"` rows in `docs/traceability.toml`:
`inf`/`nan` floats rejected and huge exponents kept exact; `hide_input` does
not hide input; path permission checks assumed true; `nargs=0` arguments
(UP-024) and options; falsy envvar on a `flag_value` option leaves it unset
(UP-009); unbounded ranges show no range text (UP-023); `--help`/`--version`
never read automatic environment variables (UP-011); parameters sharing a
name are not arbitrated; boolean flags have no explicit `None` default; an
option-looking unknown subcommand is re-scanned rather than re-parsed.
The shell completion scripts are this port's own.
Excluded, with reasons in `docs/traceability.toml`: `click.Tuple`
(heterogeneous composites), `token_normalize_func`, group result
callbacks, Windows console support, custom `ParamType` classes,
deprecation warnings, `prompt_required=False`, lazy subcommand loading.

Implemented beyond the first slice: chained groups (`group ... (chain :=
true)`), `default_map` (`ContextSettings.defaultMap`, source
`DEFAULT_MAP`), the `UUID` and `DateTime` types (refined by
`uuidCanonical` and `Timestamp.valid`), parameter callbacks
(`callback :=`), and shell completion (`_PROG_COMPLETE`, bash/zsh/fish,
`RepoImpl/Complete.lean`).

### Complexity

The scanner accumulates events and positionals in reverse and reverses
once at the end, so it is linear in the number of argv tokens.
`Terminal.pathCandidates` is linear in the total argv length (suffixes are
taken only for option-looking tokens of at most 256 characters, and
deduplication uses a hash set).

## Tests

* `tests/Props.lean`: seeded property tests, no dependencies, under a second.
* `tests/Conformance.lean`: cases re-expressed from the upstream suite; each
  names the upstream test (`tests/test_x.py::test_y`).
* `tests/pbt/`: a separate Lake package, the one place an external dependency
  (`plausible`) is taken.

## Traceability

`docs/traceability.toml` links upstream behaviour to the Lean symbol, theorem
and test that realise it. Add a row when behaviour is newly implemented,
tested-only, excluded, or deviates. CI fails if a registered symbol does not
resolve.

## Conventions

* Lean 4 `leanprover/lean4:v4.35.0`; no Mathlib; the core library depends only on Lean
  core and the sibling `lean-*` repos listed in `lakefile.toml`, pinned by
  commit in `lake-manifest.json`.
* Choose the definition that suits the automation over a prettier one with a
  harder proof.
* No upstream code is copied; the upstream test suite and docs are the spec.
