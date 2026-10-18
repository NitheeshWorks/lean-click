import examples.docs.runner

/-!
Port of click's `docs/exceptions.md`: its code blocks, in page order, each
under its block number `docs/exceptions.md#block-<n>`
(`tests/golden/docblocks.py`). The `click:run` blocks are the golden runs
in `tests/golden/click.json`, replayed through `programs`. Blocks left out
are listed with their reasons in `examples/MANIFEST.toml`.
-/

open LeanClick LeanClick.Terminal Text

namespace Docs.Exceptions

-- block 1: `make_context` is the pure parse (`LeanClick.parse`: the parsed
-- values, or how the invocation halts); running the command with them is
-- `Command.invoke`
def b1 {σ : Type} (command : App σ) (obj : σ) : ClickM Unit := do
  match LeanClick.parse command ["args", "go", "here"] (prog := "command-name") with
  | .ok _ => command.invoke obj ["args", "go", "here"] (prog := "command-name")
  | .error h => report (.halted h)

-- block 2: `standalone_mode=False`: errors reach the caller
def b2 {σ : Type} (command : App σ) (obj : σ) : ClickM Unit :=
  command.invoke obj ["command-name", "args", "go", "here"]

-- block 3: handling them as `main` would
def b3 {σ : Type} (command : App σ) (obj : σ) : IO UInt32 := do
  match ← (command.invoke obj ["command-name", "args", "go", "here"]).run with
  | .ok () => return 0
  | .error .abort =>
    echo "Bye!" (err := true)
    return 1
  | .error e =>
    e.show
    return UInt32.ofNat e.exitCode

-- block 4: a group without arguments shows its help
namespace B4
click_command printer [name := "printer"] (this : Option String := none) : IO Unit := do
  if let some t := this then if !t.isEmpty then echo t
click_group printer_group [name := "printer_group", commands := [printer]] : IO Unit := pure ()
end B4

-- block 7: ... unless `no_args_is_help=False`
namespace B7
click_command printer [name := "printer"] (this : Option String := none) : IO Unit := do
  if let some t := this then if !t.isEmpty then echo t
click_group printer_group [name := "printer_group", noArgsIsHelp := some false, commands := [printer]] :
    IO Unit := pure ()
end B7

open Docs in
def programs : Docs.Programs :=
  [("printer_group@4", run B4.printer_group ()), ("printer_group@7", run B7.printer_group ())]

end Docs.Exceptions

def main (args : List String) : IO UInt32 := Docs.main Docs.Exceptions.programs args
