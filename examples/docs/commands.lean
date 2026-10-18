import examples.docs.runner
import Std.Data.HashMap

/-!
Port of click's `docs/commands.md`: its code blocks, in page order, each
under its block number `docs/commands.md#block-<n>`
(`tests/golden/docblocks.py`). The `click:run` blocks are the golden runs
in `tests/golden/click.json`, replayed through `programs`. Blocks left out
are listed with their reasons in `examples/MANIFEST.toml`.
-/

open LeanClick LeanClick.Terminal Text

namespace Docs.Commands

-- block 1
namespace B1
click_command sync : IO Unit := echo "Syncing"
click_group cli [commands := [sync]] (debug : Bool := false) : IO Unit :=
  echo s!"Debug mode is {if debug then "on" else "off"}"
end B1

-- block 3: the context object, a dict here; `cli(obj={})` is `run {}`
namespace B3
abbrev Obj := Std.HashMap String Bool

click_command sync (ctx : Context Obj) : IO Unit :=
  echo s!"Debug is {if ctx.obj.getD "DEBUG" false then "on" else "off"}"

click_group cli [commands := [sync]] (debug : Bool := false) (ctx : Context Obj) : IO Obj :=
  return ctx.obj.insert "DEBUG" debug

def main (args : List String) : IO UInt32 := cli.run {} args
end B3

-- block 4: `pass_obj`, written with `ctx.invoke`, is the binder type
-- `Pass τ`: the command receives the context's object (block 7 uses it)

-- block 5: a chained group
namespace B5
click_command validate [name := "validate"] : IO Unit := echo "validate"
click_command build [name := "build"] : IO Unit := echo "build"
click_group cli [chain := true, commands := [validate, build]] : IO Unit := pure ()
end B5

-- block 7: chained commands share the context object; each one's result
-- is the next one's object
namespace B7
abbrev NS := Std.HashMap String String

click_command lower (ns : Pass NS) : IO NS := return ns.insert "name" (ns.getD "name" "").toLower
click_command «show» [name := "show"] (ns : Pass NS) : IO Unit := echo (ns.getD "name" "")
click_group cli [chain := true, commands := [lower, «show»]] (name : String) (ns : Pass NS) : IO NS :=
  return ns.insert "name" name
end B7

-- block 9: a pipeline. Each chained command adds its processor to the
-- group's object; the result callback, which runs once the chain is done,
-- is the cleanup the group registers. The page's callback iterates
-- Python's builtin `input` (UP-053); it means `fin`, as here.
namespace B9
abbrev Processor := List String → List String
abbrev Processors := IO.Ref (List Processor)

click_command make_uppercase [name := "upper"] (processors : Pass Processors) : IO Unit :=
  processors.modify (· ++ [List.map String.toUpper])

click_command make_lowercase [name := "lower"] (processors : Pass Processors) : IO Unit :=
  processors.modify (· ++ [List.map String.toLower])

click_command make_strip [name := "strip"] (processors : Pass Processors) : IO Unit :=
  processors.modify (· ++ [List.map strip])

click_group cli [chain := true, invokeWithoutCommand := true,
    commands := [make_uppercase, make_lowercase, make_strip]] (fin : FileArg) [type := some (File "r")] :
    ClickM Processors := do
  let processors ← IO.mkRef []
  let text ← readFileArg fin
  callOnClose do  -- `process_pipeline(processors, fin)`
    let mut iterator := splitLines text
    for processor in ← processors.get do
      iterator := processor iterator
    for item in iterator do echo item
  return processors
end B9

-- block 10: a `default_map`, defaults by parameter name and nested maps by
-- subcommand name
namespace B10
def defaultMap : DefaultMap :=
  .node [("debug", ["True"])]  -- default for a top level option
    [("runserver", .node [("port", ["5000"])] [])]  -- default for a subcommand
end B10

-- block 11: `cli(default_map=...)` is `run`'s `defaultMap`, as in `programs`
namespace B11
click_command runserver (port : Int := 8000) : IO Unit := echo s!"Serving on http://127.0.0.1:{port}/"
click_group cli [commands := [runserver]] : IO Unit := pure ()

def defaultMap : DefaultMap := .node [] [("runserver", .node [("port", ["5000"])] [])]
end B11

-- blocks 13 and 14: defaults are the tokens of the value, so the pair is
-- `["3", "4"]` whether it is written `"3 4"` or `(3, 4)`
namespace B13
def defaultMap : DefaultMap := .node [] [("draw", .node [("point", ["3", "4"]), ("color", ["red"])] [])]
end B13

-- block 15: the `default_map` in the context settings
namespace B15
click_command runserver (port : Int := 8000) : IO Unit := echo s!"Serving on http://127.0.0.1:{port}/"
click_group cli [commands := [runserver],
    contextSettings := { defaultMap := some (.node [] [("runserver", .node [("port", ["5000"])] [])]) }] :
    IO Unit := pure ()
end B15

open Docs in
def programs : Docs.Programs :=
  [("cli@1", run B1.cli ()), ("cli@5", run B5.cli ()), ("cli@7", run B7.cli {}),
   ("cli@11+{\"default_map\": {\"runserver\": {\"port\": 5000}}}",
     fun prog args => B11.cli.run () args (prog := prog) (defaultMap := some B11.defaultMap)),
   ("cli@9", fun prog args => do B9.cli.run (← IO.mkRef []) args (prog := prog)),
   ("cli@15", run B15.cli ())]

end Docs.Commands

def main (args : List String) : IO UInt32 := Docs.main Docs.Commands.programs args
