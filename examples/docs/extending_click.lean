import examples.docs.runner

/-!
Port of click's `docs/extending-click.md`: its code blocks, in page order,
each under its block number `docs/extending-click.md#block-<n>`
(`tests/golden/docblocks.py`). Blocks left out are listed with their
reasons in `examples/MANIFEST.toml`.
-/

open LeanClick LeanClick.Terminal Text

namespace Docs.ExtendingClick

-- block 3: `AliasedGroup.get_command`, the group's resolver: a name that
-- is not a command resolves to the one command it abbreviates.
-- (`resolve_command` returning the full name is what the parser does.)
namespace B3
def aliasedGroup (names : List String) {σ : Type} (ctx : Context σ) (cmdName : String) :
    ClickM (Option String) := do
  let found := names.filter (·.startsWith cmdName)
  match found with
  | [] => return none
  | [m] => return some m
  | _ => fail ctx s!"Too many matches: {", ".intercalate (sortStrings found)}"
end B3

-- block 4
namespace B4
click_command push : IO Unit := pure ()
click_command pop : IO Unit := pure ()
click_group cli [commands := [push, pop], resolve := B3.aliasedGroup ["push", "pop"]] : IO Unit := pure ()
end B4

def programs : Docs.Programs := [("cli@4", Docs.run B4.cli ())]

end Docs.ExtendingClick

def main (args : List String) : IO UInt32 := Docs.main Docs.ExtendingClick.programs args
