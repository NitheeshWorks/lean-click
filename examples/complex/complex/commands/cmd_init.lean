import examples.complex.complex.environment

/-!
Port of click's `examples/complex/complex/commands/cmd_init.py`: the
`init` plugin command.
-/

open LeanClick LeanClick.Terminal

namespace Complex.Commands.CmdInit

/-- Initializes a repository. -/
click_command cli [name := "init", shortHelp := some "Initializes a repo."]
    (path : Option String) [type := some (Path (resolvePath := true))]
    (ctx : Pass Environment)
    : IO Unit := do
  let path := path.getD ctx.home
  ctx.log s!"Initialized the repository in {formatFilename path}"

end Complex.Commands.CmdInit
