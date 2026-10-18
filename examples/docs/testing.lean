import examples.docs.runner

/-!
Port of click's `docs/testing.md`: its code blocks, in page order, each
under its block number `docs/testing.md#block-<n>`
(`tests/golden/docblocks.py`). The page's apps are ported here; its
`CliRunner` tests are the golden runs `docs/testing.md#block-<n>` in
`tests/golden/click.json` (`tests/golden/generate.py` runs each app as the
test does), which `tests/Conformance.lean` replays through `programs`.
-/

open LeanClick LeanClick.Terminal Text

namespace Docs.Testing

-- block 1 (hello.py)
namespace B1
click_command hello (name : String) : IO Unit := echo s!"Hello {name}!"
end B1

-- block 3 (sync.py)
namespace B3
click_command sync : IO Unit := echo "Syncing"

click_group cli [commands := [sync]] (debug : Bool := false) : IO Unit :=
  echo s!"Debug mode is {if debug then "on" else "off"}"
end B3

-- block 5 (sync.py, without the option)
namespace B5
click_command sync : IO Unit := echo "Syncing"

click_group cli [commands := [sync]] : IO Unit := pure ()
end B5

-- block 7 (cat.py)
namespace B7
click_command cat (f : FileArg) [type := some (File)] : ClickM Unit := do
  echo (← readFileArg f)
end B7

-- block 9 (prompt.py)
namespace B9
click_command prompt (foo : String) [prompt := true] : IO Unit := echo s!"foo={foo}"
end B9

open Docs in
def programs : Docs.Programs :=
  [("hello@1", run B1.hello ()), ("cli@3", run B3.cli ()), ("cli@5", run B5.cli ()), ("cat@7", run B7.cat ()),
   ("prompt@9", run B9.prompt ())]

end Docs.Testing

def main (args : List String) : IO UInt32 := Docs.main Docs.Testing.programs args
