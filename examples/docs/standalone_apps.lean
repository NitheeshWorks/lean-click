import examples.docs.runner

/-!
Port of click's `docs/standalone-apps.md`: its code blocks, in page order,
each under its block number `docs/standalone-apps.md#block-<n>`
(`tests/golden/docblocks.py`). The page has no `click:run` blocks;
`tests/golden/generate.py` runs block 1's program, and
`tests/Conformance.lean` replays the runs through `programs`. Block 2, the
packaging entry script, is the `docs-standalone-apps` executable in
`lakefile.toml` (`examples/MANIFEST.toml`).
-/

open LeanClick LeanClick.Terminal Text

namespace Docs.StandaloneApps

-- block 1 (hello_cli/app.py)
namespace B1
/-- Greet someone by NAME (default: World). -/
click_command main_command [name := "main"] (name : Argument String := "World") (count : Int := 1) "Number of times to greet." :
    IO Unit := do
  for _ in [0:count.toNat] do echo s!"Hello, {name}!"
end B1

def programs : Docs.Programs := [("main@1", Docs.run B1.main_command ())]

end Docs.StandaloneApps

def main (args : List String) : IO UInt32 := Docs.main Docs.StandaloneApps.programs args
