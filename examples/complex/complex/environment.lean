import LeanClick

/-!
Part of the port of click's `examples/complex/complex/cli.py`: its
`Environment` object and logging helpers, in a module of their own so the
command modules can import them while `cli.lean` imports the commands
(Python breaks that cycle with a lazy `__import__`).
-/

open LeanClick LeanClick.Terminal

namespace Complex

structure Environment where
  verbose : Bool := false
  home : String
  deriving Inhabited

/-- Logs a message to stderr. -/
def Environment.log (_env : Environment) (msg : String) : IO Unit :=
  echo msg (err := true)

/-- Logs a message to stderr only if verbose is enabled. -/
def Environment.vlog (env : Environment) (msg : String) : IO Unit := do
  if env.verbose then env.log msg

end Complex
