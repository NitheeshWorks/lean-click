import examples.docs.runner

/-!
Port of click's `docs/options.md`: its code blocks, in page order, each
under its block number `docs/options.md#block-<n>`
(`tests/golden/docblocks.py`). The `click:run` blocks are the golden runs
in `tests/golden/click.json`, replayed through `programs`.
-/

open LeanClick LeanClick.Terminal Text

namespace Docs.Options

-- block 1: the name given explicitly
namespace B1
click_command echo (string_to_echo : Option String := none) [decls := ["--string-to-echo"]] : IO Unit :=
  Terminal.echo (string_to_echo.getD "")
end B1

-- block 3: the name from the spelling
namespace B3
click_command echo (string_to_echo : Option String := none) : IO Unit := Terminal.echo (string_to_echo.getD "")
end B3

-- block 5 (`click.echo(None)` prints an empty line)
namespace B5
click_command print_this (text : Option String := none) : IO Unit := echo (text.getD "")
end B5

-- block 8
namespace B8
click_command dots (n : Int := 1) : IO Unit := echo (String.ofList (List.replicate n.toNat '.'))
end B8

-- block 10: `nargs=2, type=float`
namespace B10
click_command findme (pos : Option (Dec × Dec) := none) : IO Unit := do
  if let some (a, b) := pos then echo s!"{a} / {b}"
end B10

-- block 12: a tuple type, `type=(str, int)`
namespace B12
click_command putitem (item : Option (String × Int) := none) : IO Unit := do
  if let some (name, id) := item then echo s!"name={name} id={id}"
end B12

-- block 14: the same with `nargs=2, type=click.Tuple([str, int])`
namespace B14
click_command putitem (item : Option (String × Int) := none) : IO Unit := do
  if let some (name, id) := item then echo s!"name={name} id={id}"
end B14

-- block 15
namespace B15
click_command commit (message : List String := []) [decls := ["--message", "-m"]] : IO Unit := do
  echo (pyTuple message)
  for m in message do echo m
end B15

-- block 17
namespace B17
click_command cli
    (a : Bool := false) [decls := ["-a"]]
    (b : Bool := false) [decls := ["-b"]]
    (c : Bool := false) [decls := ["-c"]]
    : IO Unit := echo s!"a={pyBool a} b={pyBool b} c={pyBool c}"
end B17

-- block 19
namespace B19
click_command cli
    (v : Bool := false) [decls := ["-v"]]
    (n : Option Int := none) [decls := ["-n"]]
    : IO Unit := echo s!"v={pyBool v} n={n.map toString |>.getD "None"}"
end B19

-- block 21
namespace B21
click_command log (verbose : Nat := 0) [decls := ["-v", "--verbose"], count := true] : IO Unit :=
  echo s!"Verbosity: {verbose}"
end B21

-- block 23
namespace B23
click_command info (shout : Bool := false) [decls := ["--shout"]] : IO Unit := do
  let rv := if shout then sysPlatform.toUpper ++ "!!!!111" else sysPlatform
  echo rv
end B23

-- block 25
namespace B25
click_command info (shout : Bool := false) [decls := ["--shout/--no-shout"]] : IO Unit := do
  let rv := if shout then sysPlatform.toUpper ++ "!!!!111" else sysPlatform
  echo rv
end B25

-- block 27
namespace B27
click_command log (debug : Bool := false) [decls := ["/debug;/no-debug"]] : IO Unit :=
  echo s!"debug={pyBool debug}"
end B27

-- block 28: a second secondary spelling
namespace B28
click_command info (shout : Bool := false) [decls := ["--shout/--no-shout", " /-N"]] : IO Unit := do
  let rv := if shout then sysPlatform.toUpper ++ "!!!!111" else sysPlatform
  echo rv
end B28

-- block 30: a feature switch
namespace B30
click_command info
    (transformation : String := "upper") [flagValues := [("--upper", "upper", ""), ("--lower", "lower", "")]]
    : IO Unit :=
  echo (if transformation == "upper" then sysPlatform.toUpper else sysPlatform.toLower)
end B30

-- blocks 32 and 33: the `--upper` option of block 30, with `default=True`
-- and with `default='upper'`; both are block 30's `transformation := "upper"`.

-- block 34
namespace B34
click_command info (enable_xyz : Bool := true) [decls := ["--with-xyz/--without-xyz"]] : IO Unit := pure ()
end B34

-- block 35: the same pair as two feature-switch options
namespace B35
click_command info
    (enable_xyz : String := "True") [flagValues := [("--without-xyz", "False", ""), ("--with-xyz", "True", "")]]
    : IO Unit := pure ()
end B35

-- block 36
namespace B36
click_command greet (username : Option String := none) [envvar := ["USERNAME"]] : IO Unit :=
  echo s!"Hello {username.getD "None"}!"
end B36

-- block 38
namespace B38
click_command greet (username : Option String := none) [envvar := ["ALT_USERNAME", "USERNAME"]] : IO Unit :=
  echo s!"Hello {username.getD "None"}!"
end B38

-- block 40
namespace B40
click_command perform
    (paths : List String := []) [decls := ["--path"], envvar := ["PATHS"], type := some (Path)]
    : IO Unit := do
  for path in paths do echo path
end B40

-- block 42
namespace B42
click_command chmod (w : Bool := false) [decls := ["+w/-w"]] : IO Unit := echo s!"writable={pyBool w}"
end B42

-- block 44: a value that may be left out
namespace B44
click_command hello (name : String := "Default") [flagValue := some "Flag"] : IO Unit :=
  echo s!"Hello, {name}!"
end B44

open Docs in
def programs : Docs.Programs :=
  [("echo@1", run B1.echo ()), ("echo@3", run B3.echo ()), ("print_this@5", run B5.print_this ()),
   ("dots@8", run B8.dots ()), ("findme@10", run B10.findme ()), ("putitem@12", run B12.putitem ()),
   ("commit@15", run B15.commit ()), ("cli@17", run B17.cli ()), ("cli@19", run B19.cli ()),
   ("log@21", run B21.log ()), ("info@23", run B23.info ()), ("info@25", run B25.info ()),
   ("info@28", run B28.info ()), ("info@30", run B30.info ()), ("greet@36", run B36.greet ()),
   ("greet@38", run B38.greet ()), ("perform@40", run B40.perform ()), ("chmod@42", run B42.chmod ()),
   ("hello@44", run B44.hello ())]

end Docs.Options

def main (args : List String) : IO UInt32 := Docs.main Docs.Options.programs args
