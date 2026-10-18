import examples.docs.runner

/-!
Port of click's `docs/faqs.md`: its code blocks, in page order, each under
its block number `docs/faqs.md#block-<n>` (`tests/golden/docblocks.py`).
The page has no `click:run` blocks; `tests/golden/generate.py` runs blocks
5, 7 and 8 as processes, replayed through `programs`. Blocks left out are
listed with their reasons in `examples/MANIFEST.toml`.
-/

open LeanClick LeanClick.Terminal Text

namespace Docs.Faqs

-- block 1: the page leaves the conversion unwritten (`...`)
namespace B1
opaque nasaDateToIso : String → String

click_command nasa_date_to_iso_command [name := "nasa_date_to_iso"] (datestr : String) : IO Unit :=
  echo (nasaDateToIso datestr)
end B1

-- blocks 2 and 3: calling the command from code, without parsing: a typed
-- command is a function
namespace B2
def callIt : IO Unit := B1.nasa_date_to_iso_command.main "2017-090"
end B2

-- block 5: prompting from the body
namespace B5
click_command convert : ClickM Unit := do
  let source ← Terminal.prompt String "File to convert" (type := some (Path (exists_ := true)))
  echo s!"Converting {source}"
end B5

-- block 7: the same as an argument, which shell completion can complete
namespace B7
click_command convert (source : String) [type := some (Path (exists_ := true))] : IO Unit :=
  echo s!"Converting {source}"
end B7

-- block 8
namespace B8
def completeDirs (_env : Env) (incomplete : String) : List CompletionItem :=
  ["Documents", "Downloads", "Pictures"].map fun name => { value := incomplete ++ name }

click_command copy
    (src : Option String := none) [type := some (Path)]
    (dest : Option String := none) [shellComplete := some completeDirs]
    : IO Unit := echo s!"Copy from {src.getD "None"} to {dest.getD "None"}"
end B8

open Docs in
def programs : Docs.Programs :=
  [("convert@5", run B5.convert ()), ("convert@7", run B7.convert ()), ("copy@8", run B8.copy ())]

end Docs.Faqs

def main (args : List String) : IO UInt32 := Docs.main Docs.Faqs.programs args
