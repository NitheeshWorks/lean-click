<!-- Port of click's examples/README. -->
# Click Examples

This folder contains click's examples, ported one to one to Lean: the
same programs, names and layout as click's `examples/` folder.
`MANIFEST.toml` maps every upstream file to its port, or says why it is
left out. Each program is a `lean_exe` in `lakefile.toml`, built by
`lake build`; its printed output is compared byte for byte with click
8.5.0 running the Python original (`tests/golden/click.json`, replayed by
`lake exe conformance`).

| Example | What it shows | Port |
|---|---|---|
| [aliases](aliases/) | command aliases from a config file, and abbreviations | `aliases/aliases.lean` |
| [colors](colors/) | colored output, stripped when not a terminal | `colors/colors.lean` |
| [completion](completion/) | shell completion, custom completions | `completion/completion.lean` |
| [complex](complex/) | plugin subcommands, an environment object, `auto_envvar_prefix` | `complex/complex/cli.lean` |
| imagepipe | chained image processing commands | not ported: needs Pillow (see `MANIFEST.toml`) |
| [inout](inout/) | `File` arguments, `-` for stdin and stdout | `inout/inout.lean` |
| [naval](naval/) | nested groups, the docopt Naval Fate example | `naval/naval.lean` |
| [repo](repo/) | a git-like tool: context objects, prompts, confirmation, the editor | `repo/repo.lean` |
| [termui](termui/) | the terminal UI helpers: styles, pager, progress bars, editor | `termui/termui.lean` |
| [validation](validation/) | a validating callback, a custom type, checks in the command | `validation/validation.lean` |

Run one with `lake build naval && .lake/build/bin/naval --help`.

Where the port differs from the Python, the port's first comment says so:
`complex` lists its plugin modules statically (Lean programs are linked
statically, so there is no `__import__`), and `aliases` loads its config
in the resolver and the group body (parameter callbacks are pure here),
and writes the config as text (upstream's binary-mode write fails, UP-042).

`docs/` holds the code of click's README and docs pages, one Lean file
per page (`docs/quickstart.lean` for `docs/quickstart.md`, and so on),
each block under its number on the page; the `docs-<page>` programs
replay the pages' runs against click 8.5.0.
