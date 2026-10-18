import examples.docs.runner

/-!
Port of click's `docs/quickstart.md`: its code blocks, in page order, each
under the block number `docs/quickstart.md#block-<n>`
(`tests/golden/docblocks.py` numbers them). The `click:run` blocks are the
golden runs in `tests/golden/click.json`, replayed through `programs`.
-/

open LeanClick LeanClick.Terminal

namespace Docs.Quickstart

-- block 1
namespace B1
click_command hello : IO Unit := echo "Hello World!"
end B1

-- block 2: `if __name__ == '__main__': hello()` is `B1.hello.run`, as in
-- `programs` below. Blocks 3 and 4 are `click:run` blocks.

-- block 5: `cli.add_command(...)` is the group's `commands`
namespace B5
click_command initdb : IO Unit := echo "Initialized the database"
click_command dropdb : IO Unit := echo "Dropped the database"
click_group cli [commands := [initdb, dropdb]] : IO Unit := pure ()
end B5

-- block 6: `@cli.command()` is the group's `commands` too
namespace B6
click_command initdb : IO Unit := echo "Initialized the database"
click_command dropdb : IO Unit := echo "Dropped the database"
click_group cli [commands := [initdb, dropdb]] : IO Unit := pure ()
end B6

-- block 7: `if __name__ == '__main__': cli()` is `B6.cli.run`.

-- block 8
namespace B8
click_command greet : IO Unit := echo "Hello, World!"
end B8

-- block 9
namespace B9
click_group group [commands := [B8.greet]] : IO Unit := pure ()
end B9

-- block 10
namespace B10
click_command hello
    (count : Int := 1) "number of greetings"
    (name : String)
    : IO Unit := do
  for _ in [0:count.toNat] do
    echo s!"Hello {name}!"
end B10

-- block 11 is a `click:run` block.

open Docs in
def programs : Docs.Programs := [("hello@1", run B1.hello ()), ("hello@10", run B10.hello ())]

end Docs.Quickstart

def main (args : List String) : IO UInt32 := Docs.main Docs.Quickstart.programs args
