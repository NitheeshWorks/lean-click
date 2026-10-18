import examples.complex.complex.environment

/-!
Port of click's `examples/complex/complex/commands/cmd_status.py`: the
`status` plugin command.
-/

open LeanClick LeanClick.Terminal

namespace Complex.Commands.CmdStatus

/-- Shows file changes in the current working directory. -/
click_command cli [name := "status", shortHelp := some "Shows file changes."]
    (ctx : Pass Environment)
    : IO Unit := do
  ctx.log "Changed files: none"
  ctx.vlog "bla bla bla, debug info"

end Complex.Commands.CmdStatus
