import examples.docs.runner

/-!
Port of click's `docs/advanced.md`: its code blocks, in page order, each
under its block number `docs/advanced.md#block-<n>`
(`tests/golden/docblocks.py`). The `click:run` blocks are the golden runs
in `tests/golden/click.json`, replayed through `programs`. Blocks left out
are listed with their reasons in `examples/MANIFEST.toml`.
-/

open LeanClick LeanClick.Terminal Text

namespace Docs.Advanced

-- block 1: an eager flag that prints and exits is click's version option
-- machinery (`ctx.exit()` after the message), here without help text
namespace B1
def printVersion : Param := { versionOption "1.0" (message := "Version 1.0") with help := none }

click_command hello [params := [printVersion]] : IO Unit := echo "Hello World!"
end B1

-- block 3: a callback that parses the value into a pair, with a prompt
namespace B3
def validateRolls : Value → Except CallbackError Value
  | .str value =>
    -- `rolls, _, dice = value.partition("d")`
    let (rolls, dice) := match value.splitOn "d" with
      | r :: rest => (r, "d".intercalate rest)
      | [] => ("", "")
    match parseInt dice, parseInt rolls with
    | some d, some r => .ok (.list [.int d, .int r])
    | _, _ => .error (.bad "format must be 'NdM'")
  | v => .ok v

click_command roll [params := [option ["--rolls"] (type := some UNPROCESSED)
    (callback := some validateRolls) (default := "1d6") (prompt := true)]]
    (ctx : Context Unit)
    : ClickM Unit := do
  let (sides, times) ← ClickType.fetch (Int × Int) ctx.params "rolls"
  echo s!"Rolling a {sides}-sided dice {times} time(s)"
end B3

-- block 9: `ctx.forward(test)` calls `test` with this command's values,
-- `ctx.invoke(test, count=42)` with the given ones; a typed command is a
-- function (`test.main`), so both are calls
namespace B9
click_command test (count : Int := 1) : IO Unit := echo s!"Count: {count}"

click_command dist (count : Int := 1) : IO Unit := do
  test.main count
  test.main 42

click_group cli [name := "", commands := [test, dist]] : IO Unit := pure ()
end B9

-- block 11: the arguments are passed on as given (`ignore_unknown_options`)
namespace B11
/-- A fake wrapper around Python's timeit. -/
click_command cli [contextSettings := { ignoreUnknownOptions := true }]
    (verbose : Bool := false) "Enables verbose mode" [decls := ["-v", "--verbose"]]
    (timeit_args : List String) [type := some UNPROCESSED]
    : IO Unit := do
  let cmdline := ["echo", "python", "-mtimeit"] ++ timeit_args
  if verbose then echo s!"Invoking: {" ".intercalate cmdline}"
  -- `subprocess.call(cmdline)`
  let child ← IO.Process.spawn { cmd := cmdline.headD "", args := (cmdline.drop 1).toArray }
  let _ ← child.wait
end B11

-- block 13: a resource with a context manager's enter and exit
namespace B13
/-- A database handle; the page leaves `open_database` unwritten. -/
structure Database where
  query : String → IO (List String)
  close : IO Unit

opaque openDatabase : String → IO Database

structure Repo where
  home : String
  db : Database

/-- `Repo(home).__enter__()`. -/
def Repo.open (home : Option String := none) : IO Repo := do
  let home ← Docs.abspath (home.getD ".")
  return { home, db := ← openDatabase s!"{home}/repo.db" }

/-- `Repo.__exit__`. -/
def Repo.close (r : Repo) : IO Unit := r.db.close
end B13

open B13 (Repo)

-- block 14: `with Repo() as repo:`
def b14 (q : String) : IO Unit := do
  let repo ← Repo.open
  try discard <| repo.db.query q
  finally repo.close

-- block 15: the group opens the repository for its subcommands and closes
-- it when its context is torn down
namespace B15
/-- The page's `...`: the log query. -/
opaque logQuery : String

click_command log (obj : Pass Repo) : ClickM Unit := do
  -- obj is the repo opened in the cli group
  for entry in ← obj.db.query logQuery do echo entry

click_group cli [commands := [log]] (repo_home : String := ".repo") : ClickM Repo :=
  withResource (Repo.open repo_home) Repo.close
end B15

-- block 16: a cleanup registered by the callback
namespace B16
structure Db where
  recordUse : IO Unit
  save : IO Unit
  close : IO Unit

opaque openDb : String → IO Db

click_group cli (name : String := "repo.db") : ClickM Db := do
  let db ← openDb name
  callOnClose do
    db.recordUse
    db.save
    db.close
  return db
end B16

open Docs in
def programs : Docs.Programs :=
  [("hello@1", run B1.hello ()), ("roll@3", run B3.roll ()), ("cli@9", run B9.cli ()),
   ("cli@11", run B11.cli ())]

end Docs.Advanced

def main (args : List String) : IO UInt32 := Docs.main Docs.Advanced.programs args
