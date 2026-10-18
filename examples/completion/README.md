<!-- Port of click's examples/completion/README. -->
# completion

Demonstrates Click's shell completion support.

```bash
lake build completion
export PATH="$PWD/.lake/build/bin:$PATH"
```

For Bash:

```bash
eval "$(_COMPLETION_COMPLETE=bash_source completion)"
```

For Zsh:

```zsh
eval "$(_COMPLETION_COMPLETE=zsh_source completion)"
```

For Fish:

```fish
eval (env _COMPLETION_COMPLETE=fish_source completion)
```

Now press tab (maybe twice) after typing something to see completions.

```
$ completion <TAB> <TAB>
$ completion gr <TAB> <TAB>
```
