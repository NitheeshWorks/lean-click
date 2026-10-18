import LeanClick

/-!
Port of click's `examples/completion/completion.py`: shell completion,
with custom completions for an argument.
-/

open LeanClick LeanClick.Terminal

/-- Python's `sub in s`. -/
def isIn (sub s : String) : Bool := sub.isEmpty || (s.splitOn sub).length > 1

click_command ls
    (dir : Option String) [decls := ["--dir"], type := some (Path (fileOkay := false))]
    : IO Unit := do
  let entries ← System.FilePath.readDir (dir.getD ".")
  echo ("\n".intercalate (entries.toList.map (·.fileName)))

-- Returning a list of values is a shortcut to returning a list of
-- CompletionItem(value).
def getEnvVars (env : Env) (incomplete : String) : List CompletionItem :=
  env.vars.filterMap fun (k, _) => if isIn incomplete k then some k else none

click_command show_env [help := "A command to print environment variables"]
    (envvar : String) [shellComplete := some getEnvVars]
    : IO Unit := do
  echo s!"Environment variable: {envvar}"
  match ← IO.getEnv envvar with
  | some v => echo s!"Value: {v}"
  | none => throw (IO.userError s!"KeyError: {Text.pyRepr envvar}")

-- You can generate completions with help strings by returning a list
-- of CompletionItem. You can match on whatever you want, including
-- the help.
def listUsers (_env : Env) (incomplete : String) : List CompletionItem :=
  let items := [("bob", "butcher"), ("alice", "baker"), ("jerry", "candlestick maker")]
  items.filterMap fun (value, help) =>
    if isIn incomplete value || isIn incomplete help then some { value, help := some help } else none

click_command select_user [help := "Choose a user"]
    (user : String) [shellComplete := some listUsers]
    : IO Unit := echo s!"Chosen user is {user}"

click_group group [help := "A group that holds a subcommand", commands := [select_user]] : IO Unit := pure ()

click_group cli [commands := [ls, show_env, group]] : IO Unit := pure ()

def main (args : List String) : IO UInt32 := cli.run () args (prog := "completion")
