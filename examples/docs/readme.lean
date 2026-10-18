import examples.docs.runner

/-!
Port of click's `README.md`: its one code block, `README.md#block-1`
(`tests/golden/docblocks.py`). The page has no `click:run` blocks;
`tests/golden/generate.py` runs the program, and `tests/Conformance.lean`
replays the runs through `programs`.
-/

open LeanClick LeanClick.Terminal Text

namespace Docs.Readme

-- block 1 (`if __name__ == '__main__': hello()` is `run`)
namespace B1
/-- Simple program that greets NAME for a total of COUNT times. -/
click_command hello
    (count : Int := 1) "Number of greetings."
    (name : String) "The person to greet." [prompt := "Your name"]
    : IO Unit := do
  for _ in [0:count.toNat] do echo s!"Hello, {name}!"
end B1

def programs : Docs.Programs := [("hello@1", Docs.run B1.hello ())]

end Docs.Readme

def main (args : List String) : IO UInt32 := Docs.main Docs.Readme.programs args
