import LeanClick

/-!
The program behind each `examples/docs/<page>.lean`: it runs one of the
page's commands, picked by name, the way the page's `click:run` blocks
invoke them (`tests/golden/click.json` records click 8.5.0 doing the same).

    docs-<page> <command> <prog name> <args>...

A command is named `<variable>@<block>`: the Python variable the page
binds it to and the block that defines it.
-/

namespace LeanClick.Docs

/-- A page's commands: each runs with a program name and arguments. -/
abbrev Programs := List (String × (String → List String → IO UInt32))

/-- A command run as `cmd(prog_name=prog)` with the given object. -/
def run {σ : Type} (cmd : Terminal.App σ) (init : σ) : String → List String → IO UInt32 :=
  fun prog args => cmd.run init args (prog := prog)

/-- `os.path.abspath`, which the pages use. -/
def abspath (p : String) : IO String := do
  if p.startsWith "/" then return Terminal.normalizePath p
  return Terminal.normalizePath s!"{← IO.currentDir}/{p}"

def main (programs : Programs) : List String → IO UInt32
  | name :: prog :: args => do
    match programs.lookup name with
    | some run => run prog args
    | none =>
      IO.eprintln s!"no command {name}; this page has {", ".intercalate (programs.map (·.1))}"
      return 2
  | _ => do
    IO.eprintln "usage: docs-<page> <command> <prog name> <args>..."
    return 2

end LeanClick.Docs
