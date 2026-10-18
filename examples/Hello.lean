import LeanClick

/-! click's README example, translated line for line. -/

open LeanClick LeanClick.Terminal

/-- Simple program that greets NAME for a total of COUNT times. -/
click_command hello
    (count : Int := 1) "Number of greetings."
    (name : String) "The person to greet." [prompt := "Your name"]
    : IO Unit := do
  for _ in [0:count.toNat] do
    IO.println s!"Hello, {name}!"

def main (args : List String) : IO UInt32 := hello.run () args
