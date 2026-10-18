import examples.docs.runner

/-!
Port of click's `docs/documentation.md`: its code blocks, in page order,
each under its block number `docs/documentation.md#block-<n>`
(`tests/golden/docblocks.py`). The `click:run` blocks are the golden runs
in `tests/golden/click.json`, replayed through `programs`.
-/

open LeanClick LeanClick.Terminal Text

namespace Docs.Documentation

-- block 1: help on an argument
namespace B1
/-- This script prints hello and a name one or more times. -/
click_command hello
    (name : String) "The name to print."
    (count : Int := 1) "number of greetings"
    : IO Unit := do
  for _ in [0:count.toNat] do
    if !name.isEmpty then echo s!"Hello {name}!" else echo "Hello!"
end B1

-- block 3: `short_help`
namespace B3
/-- Initializes the repository. -/
click_command init [name := "init", shortHelp := some "init the repo"] : IO Unit := pure ()

/-- A simple command line tool. -/
click_group cli [commands := [init]] : IO Unit := pure ()
end B3

-- block 5: `help=` and an epilog
namespace B5
/-- Greet NAME one or more times. -/
click_command greet
    (name : String)
    (count : Int := 1) "Number of greetings to print." [showDefault := true, type := some (IntRange (min := some 1))]
    (greeting : String := "Hello") "Greeting prefix to use." [showDefault := true]
    : IO Unit := do
  for _ in [0:count.toNat] do echo s!"{greeting} {name}!"

/-- Run the example CLI. -/
click_group cli [commands := [greet],
    help := "Example Click app with subcommands, options, and positional arguments.",
    epilog := some "Examples:\n\n\x08\n  python hello.py greet Alice --count 2\n"] : IO Unit := pure ()
end B5

-- block 7
namespace B7
/-- Initializes the repository. -/
click_command init [epilog := some "See https://example.com for more details"] : IO Unit := pure ()
end B7

-- block 9
namespace B9
/-- Print FILENAME. -/
click_command touch (filename : String) "The file to print." : IO Unit := echo filename
end B9

-- block 11
namespace B11
/-- Print FILENAME.

FILENAME is the name of the file to check. -/
click_command touch (filename : String) : IO Unit := echo filename
end B11

-- block 13
namespace B13
click_command dots (n : Int := 1) "number of dots" [showDefault := false] : IO Unit :=
  echo (String.ofList (List.replicate n.toNat '.'))
end B13

-- block 15
namespace B15
click_command dots
    (n : Int := 1) [showDefault := true]
    (gr : Bool := false) "Greet the world." [decls := ["--gr"], showDefault := true]
    (br : Bool := true) "Add a thematic break" [decls := ["--br"], showDefault := true]
    : IO Unit := do
  if gr then echo "Hello world!"
  echo (String.ofList (List.replicate n.toNat '.'))
  if br then echo (String.ofList (List.replicate n.toNat '-'))
end B15

-- block 17
namespace B17
click_command greet (username : Option String := none) [envvar := ["USERNAME"], showEnvvar := true] : IO Unit :=
  echo s!"Hello {username.getD "None"}!"
end B17

-- block 19: paragraphs are rewrapped
namespace B19
/--
This is a very long paragraph and as you
can see wrapped very early in the source text
but will be rewrapped to the terminal width in
the final output.

This is
a paragraph
that is compacted.
-/
click_command cli : IO Unit := pure ()
end B19

-- block 21: `\b` keeps a paragraph as written
namespace B21
/-- First paragraph.

\b
This is
a paragraph
without rewrapping.

And this is a paragraph
that will be rewrapped again.
-/
click_command cli : IO Unit := pure ()
end B21

-- block 23: `\f` ends the help text
namespace B23
/-- First paragraph.
\f

Words to not be included.
-/
click_command cli : IO Unit := pure ()
end B23

-- block 25: metavars
namespace B25
/-- This script prints 'hello <name>' a total of <count> times. -/
click_command hello [optionsMetavar := "[[options]]"]
    (count : Int := 1) "number of greetings" [metavar := some "<int>"]
    (name : String) [metavar := some "<name>"]
    : IO Unit := do
  for _ in [0:count.toNat] do echo s!"Hello {name}!"
end B25

-- block 27
namespace B27
click_command cli [contextSettings := { helpOptionNames := some ["-h", "--help"] }] : IO Unit := pure ()
end B27

open Docs in
def programs : Docs.Programs :=
  [("hello@1", run B1.hello ()), ("cli@3", run B3.cli ()), ("cli@5", run B5.cli ()), ("init@7", run B7.init ()),
   ("touch@9", run B9.touch ()), ("touch@11", run B11.touch ()), ("dots@13", run B13.dots ()),
   ("dots@15", run B15.dots ()), ("greet@17", run B17.greet ()), ("cli@19", run B19.cli ()),
   ("cli@21", run B21.cli ()), ("cli@23", run B23.cli ()), ("hello@25", run B25.hello ()), ("cli@27", run B27.cli ())]

end Docs.Documentation

def main (args : List String) : IO UInt32 := Docs.main Docs.Documentation.programs args
