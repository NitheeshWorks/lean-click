<!-- Port of click's examples/aliases/README. -->
# aliases

aliases is a fairly advanced example that shows how to implement command
aliases with Click. Its group resolves names itself (the `resolve` handler,
click's `Group.get_command` override).

It supports both aliases read from a config file as well as automatic
abbreviations.

The aliases from the config are read from the `aliases.ini` file. Try
`aliases st` and `aliases ci`!

Usage:

```
lake build aliases
.lake/build/bin/aliases --help
```
