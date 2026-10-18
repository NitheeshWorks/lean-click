import examples.docs.runner

/-!
Port of click's `docs/index.md`: its code blocks, in page order, each under
its block number `docs/index.md#block-<n>` (`tests/golden/docblocks.py`).
The `click:run` blocks are the golden runs in `tests/golden/click.json`,
replayed through `programs`.
-/

open LeanClick LeanClick.Terminal Text

namespace Docs.Index

-- block 1 (`if __name__ == '__main__': hello()` is `run`)
namespace B1
/-- Simple program that greets NAME for a total of COUNT times. -/
click_command hello
    (count : Int := 1) "Number of greetings."
    (name : String) "The person to greet." [prompt := "Your name"]
    : IO Unit := do
  for _ in [0:count.toNat] do echo s!"Hello {name}!"
end B1

def programs : Docs.Programs := [("hello@1", Docs.run B1.hello ())]

end Docs.Index

def main (args : List String) : IO UInt32 := Docs.main Docs.Index.programs args
