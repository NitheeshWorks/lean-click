<!-- Port of click's examples/complex/README. -->
# complex

complex is an example of building very complex cli applications that load
subcommands from a plugin folder and other things.

All the commands are implemented as plugins in `complex/commands/`. A module
named `cmd_foo` shows up as the `foo` command, and its `cli` is the nested
command. Python finds the plugins at run time; a Lean program is linked
statically, so `complex/cli.lean` imports the plugin modules and lists them.

Usage:

```
lake build complex
.lake/build/bin/complex --help
```
