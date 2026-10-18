import examples.docs.runner

/-!
Port of click's `docs/parameters.md`: its code blocks, in page order, each
under its block number `docs/parameters.md#block-<n>`
(`tests/golden/docblocks.py`). The `click:run` blocks are the golden runs
in `tests/golden/click.json`, replayed through `programs`.
-/

open LeanClick LeanClick.Terminal Text

namespace Docs.Parameters

-- block 1
namespace B1
/-- Bake the RECIPE, with or without gluten. -/
click_command bake
    (recipe : String)
    (gluten_free : Bool := false) [decls := ["-g", "--gluten-free/--no-gluten-free"]]
    : IO Unit := do
  let note := if gluten_free then "no gluten" else "with gluten"
  echo s!"{recipe}: {note}"
end B1

def programs : Docs.Programs := [("bake@1", Docs.run B1.bake ())]

end Docs.Parameters

def main (args : List String) : IO UInt32 := Docs.main Docs.Parameters.programs args
