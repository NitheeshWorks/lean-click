import LeanClick

/-!
Port of click's `examples/colors/colors.py`: prints some colors; the styles
are removed automatically when the output is not a terminal.
-/

open LeanClick LeanClick.Terminal

def allColors : List String :=
  ["black", "red", "green", "yellow", "blue", "magenta", "cyan", "white",
   "bright_black", "bright_red", "bright_green", "bright_yellow", "bright_blue",
   "bright_magenta", "bright_cyan", "bright_white"]

/-- This script prints some colors. It will also automatically remove
all ANSI styles if data is piped into a file.

Give it a try! -/
click_command cli : IO Unit := do
  for color in allColors do
    echo (style s!"I am colored {color}" (fg := color))
  for color in allColors do
    echo (style s!"I am colored {color} and bold" (fg := color) (bold := true))
  for color in allColors do
    echo (style s!"I am reverse colored {color}" (fg := color) (reverse := true))
  echo (style "I am blinking" (blink := true))
  echo (style "I am underlined" (underline := true))

def main (args : List String) : IO UInt32 := cli.run () args (prog := "colors")
