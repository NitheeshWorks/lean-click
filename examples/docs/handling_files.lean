import examples.docs.runner

/-!
Port of click's `docs/handling-files.md`: its code blocks, in page order,
each under its block number `docs/handling-files.md#block-<n>`
(`tests/golden/docblocks.py`). The `click:run` blocks are the golden runs
in `tests/golden/click.json`, replayed through `programs`.
-/

open LeanClick LeanClick.Terminal Text

namespace Docs.HandlingFiles

-- block 1
namespace B1
/-- Copy contents of INPUT to OUTPUT. -/
click_command inout
    (input : FileArg) [type := some (File "rb")]
    (output : FileArg) [type := some (File "wb")]
    : ClickM Unit := do
  let input ← input.stream
  let output ← output.stream
  repeat
    let chunk ← input.read 1024
    if chunk.isEmpty then break
    output.write chunk
end B1

-- block 3
namespace B3
/-- Print FILENAME if the file exists. -/
click_command touch (filename : String) [type := some (Path (exists_ := true))] : IO Unit :=
  echo (formatFilename filename)
end B3

open Docs in
def programs : Docs.Programs := [("inout@1", run B1.inout ()), ("touch@3", run B3.touch ())]

end Docs.HandlingFiles

def main (args : List String) : IO UInt32 := Docs.main Docs.HandlingFiles.programs args
