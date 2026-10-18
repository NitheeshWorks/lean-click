import LeanClick

/-!
Port of click's `examples/repo/repo.py`: a command line tool that looks
like a distributed version control system (hg or git).
-/

open LeanClick LeanClick.Terminal

structure Repo where
  home : String
  config : List (String × String) := []
  verbose : Bool := false
  deriving Inhabited

def Repo.setConfig (repo : Repo) (key value : String) : IO Repo := do
  if repo.verbose then echo s!"  config[{key}] = {value}" (err := true)
  return { repo with config := repo.config ++ [(key, value)] }

/-- `os.path.abspath`. -/
def abspath (p : String) : IO String := do
  let cwd ← IO.currentDir
  return normalizePath (if p.startsWith "/" then p else cwd.toString ++ "/" ++ p)

/-- Clones a repository.

This will clone the repository at SRC into the folder DEST.  If DEST
is not provided this will automatically use the last path component
of SRC and create that folder. -/
click_command clone
    (src : String)
    (dest : Option String)
    (shallow : Bool := false) "Makes a checkout shallow or deep.  Deep by default."
      [decls := ["--shallow/--deep"]]
    (rev : String := "HEAD") "Clone a specific revision instead of HEAD." [decls := ["--rev", "-r"]]
    (repo : Pass Repo)
    : IO Unit := do
  let dest := dest.getD (match (src.splitOn "/").getLast? with
    | some "" | none => "."
    | some last => last)
  echo s!"Cloning repo {src} to {(dest.splitOn "/").getLast?.getD dest}"
  if shallow then echo "Making shallow checkout"
  echo s!"Checking out revision {rev}"

/-- Deletes a repository.

This will throw away the current repository. -/
click_command delete [params := [confirmationOption]]
    (repo : Pass Repo)
    : IO Unit := do
  echo s!"Destroying repo {repo.home}"
  echo "Deleted!"

/-- Sets the user credentials.

This will override the current user config. -/
click_command setuser
    (username : String) "The developer's shown username." [prompt := true]
    (email : String) "The developer's email address" [prompt := "E-Mail"]
    (password : String) "The login password." [password := true]
    (repo : Pass Repo)
    : IO Unit := do
  let repo ← repo.setConfig "username" username
  let repo ← repo.setConfig "email" email
  let _ ← repo.setConfig "password" (String.ofList (List.replicate password.length '*'))
  echo "Changed credentials."

/-- Commits outstanding changes.

Commit changes to the given files into the repository.  You will need to
"repo push" to push up your changes to other repositories.

If a list of files is omitted, all changes reported by "repo status"
will be committed. -/
click_command commit
    (message : List String := []) "The commit message.  If provided multiple times each argument gets converted into a new line."
      [decls := ["--message", "-m"]]
    (files : List String) [type := some (Path)]
    (repo : Pass Repo)
    : IO Unit := do
  let msg ← if message.isEmpty then do
      let marker := "# Files to be committed:"
      let hint := ["", "", marker, "#"] ++ files.map (s!"#   U {·}")
      match ← edit ("\n".intercalate hint) with
      | none => echo "Aborted!"; return
      | some message =>
        let msg := String.ofList (((message.splitOn marker).headD "").toList.reverse.dropWhile Char.isWhitespace).reverse
        if msg.isEmpty then echo "Aborted! Empty commit message"; return
        pure msg
    else pure ("\n".intercalate message)
  echo s!"Files to be committed: {Text.pyTuple files}"
  echo s!"Commit message:\n{msg}"

/-- Copies one or multiple files to a new location.  This copies all
files from SRC to DST. -/
click_command copy [shortHelp := some "Copies files."]
    (force : Bool := false) "forcibly copy over an existing managed file" [decls := ["--force"]]
    (src : List String) [type := some (Path)]
    (dst : String) [type := some (Path)]
    (repo : Pass Repo)
    : IO Unit := do
  for fn in src do
    echo s!"Copy from {fn} -> {dst}"

/-- Repo is a command line tool that showcases how to build complex
command line interfaces with Click.

This tool is supposed to look like a distributed version control
system to show how something like this can be structured. -/
click_group cli [commands := [clone, delete, setuser, commit, copy], params := [versionOption "1.0"]]
    (repo_home : String := ".repo") "Changes the repository folder location."
      [envvar := ["REPO_HOME"], metavar := some "PATH"]
    (config : List (String × String) := []) "Overrides a config key/value pair."
      [metavar := some "KEY VALUE"]
    (verbose : Bool := false) "Enables verbose mode." [decls := ["--verbose", "-v"]]
    (ctx : Context Repo)
    : IO Repo := do
  -- Create a repo object and remember it as the context object.  From
  -- this point onwards other commands can refer to it with `Pass Repo`.
  let mut repo : Repo := { home := ← abspath repo_home, verbose }
  for (key, value) in config do
    repo ← repo.setConfig key value
  return repo

def main (args : List String) : IO UInt32 := do
  cli.run { home := "" } args (prog := "repo")
