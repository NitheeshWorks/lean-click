import examples.docs.runner

/-!
Port of click's `docs/shell-completion.md`: its code blocks, in page order,
each under its block number `docs/shell-completion.md#block-<n>`
(`tests/golden/docblocks.py`). The page has no `click:run` blocks;
`tests/golden/generate.py` runs its programs with the completion
environment variables set, and `tests/Conformance.lean` replays the runs
through `programs`.
-/

open LeanClick LeanClick.Terminal Text

namespace Docs.ShellCompletion

-- block 1: a type that completes its values
namespace B1
/-- The name of an environment variable. -/
structure EnvVar where
  name : String

instance : ClickType EnvVar where
  type := .string
  ofValue | .str s => some { name := s } | _ => none
  toTokens v := [v.name]
  toValue v := .str v.name
  custom := some ("envvar", .ok)
  complete := some fun env incomplete =>
    env.vars.filterMap fun (name, _) => if name.startsWith incomplete then some name else none

click_command cli (ev : Option EnvVar := none) : IO Unit := do
  echo ((← IO.getEnv (ev.map (·.name) |>.getD "")).getD "")
end B1

-- block 2: a parameter's own completion
namespace B2
def completeEnvVars (env : Env) (incomplete : String) : List CompletionItem :=
  env.vars.filterMap fun (k, _) => if k.startsWith incomplete then some k else none

click_command cli (name : String) [shellComplete := some completeEnvVars] : IO Unit := do
  echo s!"Name: {name}"
  echo s!"Value: {(← IO.getEnv name).getD ""}"
end B2

-- blocks 3 to 5: support for another shell, "mysh". The page's template
-- names `%(foo_bar)s`, which click does not provide (UP-050); the program
-- name is `progName`.
namespace B3
def mysh : ShellComplete where
  name := "mysh"
  sourceTemplate v := s!"{v.completeFunc} \{
    response=$({v.completeVar}=mysh_complete {v.progName})
    # parse response and set completions somehow
}
call-on-complete {v.progName} {v.completeFunc}
"
end B3

-- block 4
namespace B4
def mysh : ShellComplete := { B3.mysh with
  completionArgs := do
    let args := splitArgString ((← IO.getEnv "COMP_WORDS").getD "")
    if (← IO.getEnv "COMP_PARTIAL") == some "1" then
      return (args.dropLast, args.getLastD "")
    return (args, "") }
end B4

-- block 5
namespace B5
def mysh : ShellComplete := { B4.mysh with formatCompletion := fun item => s!"{item.type}\t{item.value}" }
end B5

/-- `add_completion_class(MyshComplete)`: the shells a program supports
besides bash, zsh and fish. -/
def shells : List ShellComplete := [B5.mysh]

def programs : Docs.Programs :=
  [("cli@1", Docs.run B1.cli ()),
   ("cli@2", fun prog args => B2.cli.run () args (prog := prog) (shells := shells))]

end Docs.ShellCompletion

def main (args : List String) : IO UInt32 := Docs.main Docs.ShellCompletion.programs args
