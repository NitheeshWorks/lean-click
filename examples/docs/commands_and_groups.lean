import examples.docs.runner

/-!
Port of click's `docs/commands-and-groups.md`: its code blocks, in page
order, each under its block number `docs/commands-and-groups.md#block-<n>`
(`tests/golden/docblocks.py`). The `click:run` blocks are the golden runs
in `tests/golden/click.json`, replayed through `programs`. Blocks left out
are listed with their reasons in `examples/MANIFEST.toml`.
-/

open LeanClick LeanClick.Terminal Text

namespace Docs.CommandsAndGroups

-- block 1
namespace B1
click_command hello (count : Int := 1) : IO Unit := do
  for _ in [0:count.toNat] do echo "Hello!"
end B1

-- block 3: the command's name
namespace B3
click_command hello [name := "say-hello"] (count : Int := 1) : IO Unit := do
  for _ in [0:count.toNat] do echo "Hello!"
end B3

-- block 5: a deprecated command
namespace B5
click_command hello [name := "say-hello", deprecated := true] (count : Int := 1) : IO Unit := do
  for _ in [0:count.toNat] do echo "Hello!"
end B5

-- block 7: the group's callback runs before its subcommand
namespace B7
click_command hello [name := "say-hello"] (count : Int := 1) : IO Unit := do
  for _ in [0:count.toNat] do echo "Hello!"
click_group greeting [commands := [hello]] : IO Unit := echo "Starting greeting ..."
end B7

-- block 10: the group's name
namespace B10
click_command hello [name := "say-hello"] (count : Int := 1) : IO Unit := do
  for _ in [0:count.toNat] do echo "Hello!"
click_group greeting [name := "greet-someone", commands := [hello]] : IO Unit := echo "Starting greeting ..."
end B10

-- block 12
namespace B12
click_command sync : IO Unit := echo "The subcommand"
click_group cli [invokeWithoutCommand := true, commands := [sync]] (ctx : Context Unit) : IO Unit := do
  match ctx.invokedSubcommand with
  | none => echo "I was invoked without subcommand"
  | some sub => echo s!"I am about to invoke {sub}"
end B12

-- block 14
namespace B14
click_command hello (count : Int := 1) : IO Unit := do
  for _ in [0:count.toNat] do echo "Hello!"
click_command goodbye (count : Int := 1) : IO Unit := do
  for _ in [0:count.toNat] do echo "Goodbye!"
click_group greeting [commands := [hello, goodbye]] : IO Unit := pure ()
end B14

-- block 16: a group in a group
namespace B16
click_command initdb : IO Unit := echo "Initialized the database"
click_command dropdb : IO Unit := echo "Dropped the database"
click_group session [commands := [initdb, dropdb]] : IO Unit := echo "Starting session"
click_group cli [commands := [session]] : IO Unit := pure ()
end B16

-- block 18: `cli.add_command(dropdb)` is the group's `commands`
namespace B18
click_command initdb : IO Unit := echo "Initialized the database"
click_command dropdb : IO Unit := echo "Dropped the database"
click_group cli [commands := [initdb, dropdb]] : IO Unit := pure ()
end B18

-- block 20: `greet(auto_envvar_prefix='GREETER')` is `run`'s
-- `autoEnvvarPrefix`, as in `programs`
namespace B20
click_command greet (username : Option String := none) : IO Unit := echo s!"Hello {username.getD "None"}!"
end B20

-- block 22
namespace B22
click_command greet (username : Option String := none) : IO Unit := echo s!"Hello {username.getD "None"}!"
click_group cli [commands := [greet]] (debug : Bool := false) : IO Unit :=
  echo s!"Debug mode is {if debug then "on" else "off"}"
end B22

-- block 24: the context is passed to what needs it, not looked up
-- (`click.get_current_context()`)
def getCurrentCommandName {σ : Type} (ctx : Context σ) : String := ctx.infoName

-- block 25: a thread that runs with the context (`with ctx:`)
def spawnThread {σ : Type} (ctx : Context σ) (func : Context σ → IO Unit) : IO (Task (Except IO.Error Unit)) :=
  IO.asTask (func ctx)

-- block 26: comparing parameter sources, in click's `ParameterSource` order
namespace B26
def portSource (ctx : Context Unit) : IO Unit := do
  let some source := ctx.params.source? "port" | return
  -- True if the value was explicitly set (command line, prompt, or env var).
  if source < .defaultMap then pure ()
  -- True if the value came from any kind of default.
  if source ≥ .defaultMap then pure ()
end B26

-- block 27
namespace B27
click_command cli (port : Argument Int := 8080) [envvar := ["PORT"]] (ctx : Context Unit) : IO Unit := do
  let source := ctx.params.source? "port"
  echo s!"Port came from {(source.map Source.name).getD "None"}"
end B27

open Docs in
def programs : Docs.Programs :=
  [("hello@1", run B1.hello ()), ("hello@3", run B3.hello ()), ("hello@5", run B5.hello ()),
   ("greeting@7", run B7.greeting ()), ("greeting@10", run B10.greeting ()), ("cli@12", run B12.cli ()),
   ("greeting@14", run B14.greeting ()), ("cli@16", run B16.cli ()), ("cli@18", run B18.cli ()),
   ("greet@20+{\"auto_envvar_prefix\": \"GREETER\"}",
     fun prog args => B20.greet.run () args (prog := prog) (autoEnvvarPrefix := some "GREETER")),
   ("cli@22+{\"auto_envvar_prefix\": \"GREETER\"}",
     fun prog args => B22.cli.run () args (prog := prog) (autoEnvvarPrefix := some "GREETER")),
   ("cli@27", run B27.cli ())]

end Docs.CommandsAndGroups

def main (args : List String) : IO UInt32 := Docs.main Docs.CommandsAndGroups.programs args
