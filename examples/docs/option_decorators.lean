import examples.docs.runner

/-!
Port of click's `docs/option-decorators.md`: its code blocks, in page
order, each under its block number `docs/option-decorators.md#block-<n>`
(`tests/golden/docblocks.py`). The `click:run` blocks are the golden runs
in `tests/golden/click.json`, replayed through `programs`, except block 2:
a hidden prompt reads the terminal, not a pipe.
-/

open LeanClick LeanClick.Terminal Text

namespace Docs.OptionDecorators

/-- `codecs.encode(s, 'rot13')`. -/
def rot13 (s : String) : String :=
  s.map fun c =>
    let rot := fun (base : Char) => Char.ofNat ((c.toNat - base.toNat + 13) % 26 + base.toNat)
    if 'a' ≤ c && c ≤ 'z' then rot 'a' else if 'A' ≤ c && c ≤ 'Z' then rot 'A' else c

-- block 1
namespace B1
click_command encode (password : String) [prompt := true, hideInput := true, confirmationPrompt := true] :
    IO Unit := echo s!"encoded: {rot13 password}"
end B1

-- block 3
namespace B3
click_command encrypt (password : String) [password := true] : IO Unit :=
  echo s!"encoded: to {rot13 password}"
end B3

-- block 4: a callback that aborts
namespace B4
def abortIfFalse : Value → Except CallbackError Value
  | .bool false => .error .abort
  | v => .ok v

click_command dropdb [params := [option ["--yes"] (isFlag := some true) (callback := some abortIfFalse)
    (exposeValue := false) (prompt := "Are you sure you want to drop the db?")]] : IO Unit :=
  echo "Dropped all tables!"
end B4

-- block 6: the same with `confirmation_option`
namespace B6
click_command dropdb [params := [confirmationOption (prompt := "Are you sure you want to drop the db?")]] :
    IO Unit := echo "Dropped all tables!"
end B6

open Docs in
def programs : Docs.Programs := [("dropdb@4", run B4.dropdb ()), ("dropdb@6", run B6.dropdb ())]

end Docs.OptionDecorators

def main (args : List String) : IO UInt32 := Docs.main Docs.OptionDecorators.programs args
