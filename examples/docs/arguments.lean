import examples.docs.runner

/-!
Port of click's `docs/arguments.md`: its code blocks, in page order, each
under its block number `docs/arguments.md#block-<n>`
(`tests/golden/docblocks.py`). The `click:run` blocks are the golden runs
in `tests/golden/click.json`, replayed through `programs`.
-/

open LeanClick LeanClick.Terminal Text

namespace Docs.Arguments

-- block 1
namespace B1
/-- Print FILENAME. -/
click_command touch (filename : String) : IO Unit := echo filename
end B1

-- block 3
namespace B3
/-- Move file SRC to DST. -/
click_command copy (src : String) (dsts : List String) : IO Unit := do
  for destination in dsts do echo s!"Copy {src} to folder {destination}"
end B3

-- block 5
namespace B5
/-- Print all FILES file names. -/
click_command touch (files : List String) [type := some (Path)] : IO Unit := do
  for filename in files do echo filename
end B5

-- block 7
namespace B7
/-- Print all FILES file names. -/
click_command touch [contextSettings := { ignoreUnknownOptions := true }]
    (files : List String) [type := some (Path)] : IO Unit := do
  for filename in files do echo filename
end B7

-- block 9
namespace B9
/-- Print value of SRC environment variable. -/
click_command echo (src : FileArg) [envvar := ["SRC"], type := some (File "r")] : ClickM Unit := do
  Terminal.echo (← readFileArg src)
end B9

-- block 11
namespace B11
/-- Print value of SRC environment variable. -/
click_command echo (src : FileArg) [envvar := ["SRC", "SRC_2"], type := some (File "r")] : ClickM Unit := do
  Terminal.echo (← readFileArg src)
end B11

open Docs in
def programs : Docs.Programs :=
  [("touch@1", run B1.touch ()), ("copy@3", run B3.copy ()), ("touch@5", run B5.touch ()),
   ("touch@7", run B7.touch ()), ("echo@9", run B9.echo ()), ("echo@11", run B11.echo ())]

end Docs.Arguments

def main (args : List String) : IO UInt32 := Docs.main Docs.Arguments.programs args
