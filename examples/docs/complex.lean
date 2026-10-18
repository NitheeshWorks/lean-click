import examples.docs.runner

/-!
Port of click's `docs/complex.md`: its code blocks, in page order, each
under its block number `docs/complex.md#block-<n>`
(`tests/golden/docblocks.py`). The page has no `click:run` blocks;
`tests/golden/generate.py` runs its programs, and `tests/Conformance.lean`
replays the runs through `programs`.
-/

open LeanClick LeanClick.Terminal Text

namespace Docs.Complex

-- block 1: the group's result is the context object
namespace B1
structure Repo where
  home : String
  debug : Bool
  deriving Inhabited

def Repo.new (home : Option String := none) (debug : Bool := false) : IO Repo := do
  return { home := ← Docs.abspath (home.getD "."), debug }

click_group cli
    (repo_home : String := ".repo") [envvar := ["REPO_HOME"]]
    (debug : Bool := false) [envvar := ["REPO_DEBUG"]]
    : IO Repo := Repo.new repo_home debug
end B1

open B1 (Repo)

-- block 2
namespace B2
click_command clone (src : String) (dest : Option String) : IO Unit := pure ()
end B2

-- block 3: `pass_obj`
namespace B3
click_command clone (src : String) (dest : Option String) (repo : Pass Repo) : IO Unit := pure ()
end B3

-- block 4: `click.make_pass_decorator(Repo)` is the binder type `Pass Repo`,
-- which finds the innermost context object of that type

-- block 5
namespace B5
click_command clone (src : String) (dest : Option String) (repo : Pass Repo) : IO Unit := pure ()

click_group cli [commands := [clone]]
    (repo_home : String := ".repo") [envvar := ["REPO_HOME"]]
    (debug : Bool := false) [envvar := ["REPO_DEBUG"]]
    : IO Repo := Repo.new repo_home debug
end B5

-- block 6: `make_pass_decorator(Repo, ensure=True)`: the object always
-- exists in Lean; a command run on its own starts from `run`'s object

-- block 7
namespace B7
click_command cp (repo : Pass Repo) : IO Unit := do
  -- `isinstance(repo, Repo)` holds by the binder's type
  echo (pyBool true)
end B7

-- blocks 10 and 12 (foo.py, baz.py), defined before the groups that list them
namespace B10
click_group cli [name := "foo", help := "foo command for lazy example"] : IO Unit := pure ()
end B10

namespace B12
click_group cli [name := "baz", help := "baz command for lazy example"] : IO Unit := pure ()
end B12

-- block 11 (bar.py): `lazy_subcommands={"baz": "baz.cli"}`
namespace B11
click_group cli [name := "bar", help := "bar command for lazy example", commands := [B12.cli]] :
    IO Unit := pure ()
end B11

-- block 9 (main.py): `lazy_subcommands={"foo": "foo.cli", "bar": "bar.cli"}`. A
-- Lean program links every command, so block 8's `LazyGroup` is a group
-- that lists them.
namespace B9
click_group cli [help := "main CLI command for lazy example", commands := [B10.cli, B11.cli]] :
    IO Unit := pure ()
end B9

-- block 13: the library is imported when the command runs
namespace B13
/-- `mylibrary.foo_concrete`, which the page does not show. -/
opaque fooConcrete : Option Int → Option String → IO Unit

click_command foo (n : Option Int := none) [decls := ["-n"]] (w : Option String := none) [decls := ["-w"]] :
    IO Unit := fooConcrete n w
end B13

open Docs in
def programs : Docs.Programs :=
  [("cli@1", run B1.cli default), ("cli@5", run B5.cli default), ("cp@7", run B7.cp default),
   ("cli@9", run B9.cli ()), ("foo@13", run B13.foo ())]

end Docs.Complex

def main (args : List String) : IO UInt32 := Docs.main Docs.Complex.programs args
