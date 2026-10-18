import LeanClick

/-!
Port of click's `docs/entry-points.md`: its code block, under its block
number `docs/entry-points.md#block-1` (`tests/golden/docblocks.py`). The
page is about packaging; a Lean program's entry point is its `main`.
-/

open LeanClick LeanClick.Terminal

namespace Docs.EntryPoints

-- block 1
/-- Prints a greeting. -/
click_command cli : IO Unit := echo "Hello, World!"

end Docs.EntryPoints

def main (args : List String) : IO UInt32 := Docs.EntryPoints.cli.run () args
