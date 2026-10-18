import LeanClick
import Lean

/-!
Port of click's `examples/aliases/aliases.py`: a group that resolves
command aliases from a config file (`aliases.ini`) and unambiguous
abbreviations (`aliases st` runs `status`).

click's `AliasedGroup` overrides `Group.get_command`; here the group's
`resolve` handler is that override. Parameter callbacks are pure in this
port, so the `--config` callback's work (loading the aliases into the
context object) happens in the resolver and in the group's body.

Upstream's `write_config` opens the file in binary mode and so fails with
`TypeError` (recorded as UP-042); this port writes the file as text.
-/

open LeanClick LeanClick.Terminal

open Lean Elab Term in
/-- The directory of this source file (Python's
`os.path.dirname(__file__)`). -/
elab "source_dir%" : term => do
  let file ← IO.FS.realPath (← getFileName)
  return mkStrLit ((file.parent.map (·.toString)).getD ".")

/-- The config in this example only holds aliases. -/
structure Config where
  path : String
  aliases : List (String × String) := []
  deriving Inhabited

def Config.addAlias (c : Config) (alias cmd : String) : Config :=
  { c with aliases := (c.aliases.filter (·.1 != alias)) ++ [(alias, cmd)] }

/-- `RawConfigParser.read` of the `[aliases]` section: `key = value` or
`key: value` lines, keys lowercased, comments (`#`, `;`) skipped. A
missing file is no config. -/
def Config.readConfig (c : Config) (filename : String) : IO Config := do
  let text ← try IO.FS.readFile filename catch _ => return c
  let strip := fun (s : String) => s.trimAscii.copy
  let mut sect := ""
  let mut cfg := c
  for line in text.splitOn "\n" do
    let l := strip line
    if l.isEmpty || l.startsWith "#" || l.startsWith ";" then continue
    if l.startsWith "[" && l.endsWith "]" then
      sect := strip ((l.drop 1).dropEnd 1).copy
      continue
    if sect != "aliases" then continue
    let cs := l.toList
    match cs.findIdx? (fun ch => ch == '=' || ch == ':') with
    | some i => cfg := cfg.addAlias (strip (String.ofList (cs.take i))).toLower (strip (String.ofList (cs.drop (i + 1))))
    | none => pure ()
  return cfg

/-- `RawConfigParser.write` of the aliases. -/
def Config.writeConfig (c : Config) (filename : String) : IO Unit :=
  IO.FS.writeFile filename ("[aliases]\n" ++ String.join (c.aliases.map fun (k, v) => s!"{k} = {v}\n") ++ "\n")

/-- What the `--config` callback does: load the given config file, or the
`aliases.ini` next to this program's source. -/
def readConfig (ctx : Context Config) : IO Config :=
  let value := (ctx.params.getString? "config").getD (source_dir% ++ "/aliases.ini")
  ctx.obj.readConfig value

/-- Pushes changes. -/
click_command push : IO Unit := echo "Push"

/-- Pulls changes. -/
click_command pull : IO Unit := echo "Pull"

/-- Clones a repository. -/
click_command clone : IO Unit := echo "Clone"

/-- Commits pending changes. -/
click_command commit : IO Unit := echo "Commit"

/-- Shows the status. -/
click_command status (config : Pass Config) : IO Unit := echo s!"Status for {config.path}"

/-- Adds an alias to the specified configuration file. -/
click_command alias
    (config : Pass Config)
    (alias_ : String) [metavar := some "ALIAS"]
    (cmd : String)
    (config_file : String := "aliases.ini")
      [decls := ["--config_file"], type := some (Path (exists_ := true) (dirOkay := false))]
    : IO Unit := do
  let config := config.addAlias alias_ cmd
  config.writeConfig config_file
  echo s!"Added '{alias_}' as alias for '{cmd}'"

def builtins : List (App Config) := [push, pull, clone, commit, status, alias]

/-- `AliasedGroup.get_command` for a name that is not a command: an alias
from the config, else the one command the name abbreviates. -/
def getCommand (ctx : Context Config) (cmdName : String) : ClickM (Option String) := do
  -- find the config object and ensure it's there
  let cfg ← readConfig ctx
  -- look up an explicit command alias in the config
  if let some actual := cfg.aliases.lookup cmdName then return some actual
  -- allow automatic abbreviation of the command, if only one found
  let found := (builtins.map (·.name)).filter fun x => x.toLower.startsWith cmdName.toLower
  match found with
  | [] => return none
  | [m] => return some m
  | _ => fail ctx s!"Too many matches: {", ".intercalate (Text.sortStrings found)}"

/-- An example application that supports aliases. -/
click_group cli [commands := builtins, resolve := getCommand, params := [
    option ["--config"] (type := some (Path (exists_ := true) (dirOkay := false))) (exposeValue := false)
      (help := "The config file to use instead of the default.")]]
    (ctx : Context Config)
    : IO Config := readConfig ctx

def main (args : List String) : IO UInt32 := do
  let cwd ← IO.currentDir
  cli.run { path := cwd.toString } args (prog := "aliases")
