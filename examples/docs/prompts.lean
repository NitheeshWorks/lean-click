import examples.docs.runner

/-!
Port of click's `docs/prompts.md`: its code blocks, in page order, each
under its block number `docs/prompts.md#block-<n>`
(`tests/golden/docblocks.py`). The `click:run` blocks are the golden runs
in `tests/golden/click.json`, replayed through `programs`.
-/

open LeanClick LeanClick.Terminal Text

namespace Docs.Prompts

-- block 1
namespace B1
click_command hello (name : String) [prompt := true] : IO Unit := echo s!"Hello {name}!"
end B1

-- block 3
namespace B3
click_command hello (name : String) [prompt := "Your name please"] : IO Unit := echo s!"Hello {name}!"
end B3

-- block 5
def b5 : ClickM Int := Terminal.prompt Int "Please enter a valid integer"

-- block 6: the default's type is the value's type
def b6 : ClickM Dec := Terminal.prompt Dec "Please enter a number" (default := some 42.0)

-- block 7
namespace B7
click_command hello (name : String := "Default") [prompt := true, promptRequired := false] : IO Unit :=
  echo s!"Hello {name}!"
end B7

-- block 9
def b9 : ClickM Unit := do
  if ← confirm "Do you want to continue?" then
    echo "Well done!"

-- block 10
def b10 : ClickM Unit := discard <| confirm "Do you want to continue?" (abort := true)

/-- `os.environ.get("USER", "")`. -/
def currentUser : IO String := return (← IO.getEnv "USER").getD ""

-- block 11
namespace B11
click_command hello (username : String) [prompt := true, dynamicDefault := some currentUser] : IO Unit :=
  echo s!"Hello, {username}!"
end B11

-- block 12
namespace B12
click_command hello (username : String)
    [prompt := true, dynamicDefault := some currentUser, showDefault := "current user"] : IO Unit :=
  echo s!"Hello, {username}!"
end B12

open Docs in
def programs : Docs.Programs :=
  [("hello@1", run B1.hello ()), ("hello@3", run B3.hello ()), ("hello@7", run B7.hello ()),
   ("hello@12", run B12.hello ())]

end Docs.Prompts

def main (args : List String) : IO UInt32 := Docs.main Docs.Prompts.programs args
