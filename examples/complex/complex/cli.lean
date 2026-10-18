import examples.complex.complex.environment
import examples.complex.complex.commands.cmd_init
import examples.complex.complex.commands.cmd_status

/-!
Port of click's `examples/complex/complex/cli.py`: a command line
interface whose subcommands are plugins, the `cmd_<name>` modules of
`complex.commands`.

Python finds the plugins at run time (`os.listdir` and `__import__`).
Lean programs are linked statically, so the plugin modules are imported
here and listed in `commands`, which is what `list_commands` and
`get_command` return in Python (sorted by name).
-/

open LeanClick LeanClick.Terminal

namespace Complex

/-- The plugins of `complex.commands`. -/
def commands : List (App Environment) := [Commands.CmdInit.cli, Commands.CmdStatus.cli]

/-- A complex command line interface. -/
click_group cli [commands := commands, contextSettings := { autoEnvvarPrefix := some "COMPLEX" }]
    (home : Option String) "Changes the folder to operate on."
      [decls := ["--home"], type := some (Path (exists_ := true) (fileOkay := false) (resolvePath := true))]
    (verbose : Bool := false) "Enables verbose mode." [decls := ["-v", "--verbose"]]
    (ctx : Pass Environment)
    : IO Environment := do
  return { ctx with verbose, home := home.getD ctx.home }

end Complex

def main (args : List String) : IO UInt32 := do
  let cwd ← IO.currentDir
  Complex.cli.run { home := cwd.toString } args (prog := "complex")
