import LeanClick

/-!
Prompts run as a real process (piped standard input), for the golden runs
in `tests/golden/click.json`. Each command mirrors an upstream test:

* `validate`: `tests/test_options.py::test_callback_validates_prompt`
* `number`: `tests/test_termui.py::test_hide_input_masks_value_in_error`'s
  `builtin-int` case, without `hide_input`, since a hidden prompt reads
  the terminal, not the pipe
* `password`: `tests/test_termui.py::test_confirmation_prompt` with the
  text `Confirm Password` (again without `hide_input`)
* `flag`: `tests/test_termui.py::test_flag_value_prompt`, a boolean flag
  asked with `[y/N]`
* `cleanup`: a group's `with_resource` and `call_on_close` cleanups run
  after its subcommand, newest first, also when the subcommand fails
  (`tests/test_context.py` covers them with `CliRunner`)
-/

open LeanClick LeanClick.Terminal

def checkPositive (value : Int) : Except CallbackError Int :=
  if value < 0 then throw (.bad "should be positive") else pure value

click_command «validate» [name := "validate"]
    (a : Int) [decls := ["-a"], prompt := true, callback := some checkPositive]
    : IO Unit := echo (toString a)

click_command number (n : Int) [decls := ["--n"], prompt := true] : IO Unit := echo (toString n)

click_command password
    (password : String) [prompt := "Confirm Password", confirmationPrompt := "Confirm Password"]
    : IO Unit := echo (Text.pyRepr password)

click_command flag
    (flag : Bool := false) [decls := ["--flag/--no-flag"], prompt := true]
    : IO Unit := echo (if flag then "True" else "False")

click_command ok : IO Unit := echo "sub"

click_command fail : ClickM Unit := throw (.failure "boom")

click_group cleanup [commands := [ok, fail]] : ClickM Unit := do
  discard <| withResource (do echo "enter") fun _ => echo "exit"
  callOnClose (echo "closed")

click_group prompts [commands := [«validate», number, password, flag, cleanup]] : IO Unit := pure ()

def main (args : List String) : IO UInt32 := prompts.run () args (prog := "prompts")
