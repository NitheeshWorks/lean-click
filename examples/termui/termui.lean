import LeanClick

/-!
Port of click's `examples/termui/termui.py`: the terminal UI helpers
(styles, pager, progress bars, launching, editing, clearing, pausing and
reading single keys).
-/

open LeanClick LeanClick.Terminal

/-- Demonstrates ANSI color support. -/
click_command colordemo : IO Unit := do
  for color in ["red", "green", "blue"] do
    echo (style s!"I am colored {color}" (fg := color))
    echo (style s!"I am background colored {color}" (bg := color))

/-- Demonstrates using the pager. -/
click_command pager : IO Unit := do
  let lines := (List.range 200).map fun x => s!"{style (toString x) (fg := "green")}. Hello World!"
  echoViaPager ("\n".intercalate lines)

/-- `random.random()`. -/
def random : IO Float := do
  return (← IO.rand 0 1000000).toFloat / 1000000

/-- `time.sleep(seconds)`. -/
def sleep (seconds : Float) : IO Unit := IO.sleep (seconds * 1000).toUInt32

/-- Demonstrates the progress bar. -/
click_command progress
    (count : IntIn 1 100000 := 8000) "The number of items to process."
    : IO Unit := do
  let items := List.range count.toNat
  let processSlowly := fun (_ : Nat) => do sleep (0.002 * (← random))
  let filter := fun (items : List Nat) => items.filterM fun _ => do return (← random) > 0.3
  withProgressbar (progressbar items (label := "Processing accounts") (fillChar := style "#" (fg := "green")))
    fun bar => do
      for item in bar do processSlowly item
  let showItem := fun (item : Option Nat) => item.map (s!"Item #{·}")
  withProgressbar (progressbar (← filter items) (label := "Committing transaction")
      (fillChar := style "#" (fg := "yellow")) (itemShowFunc := some showItem))
    fun bar => do
      for item in bar do processSlowly item
  withProgressbar (progressbarLength count.toNat (label := "Counting")
      (barTemplate := "%(label)s  %(bar)s | %(info)s") (fillChar := style "█" (fg := "cyan"))
      (emptyChar := " "))
    fun bar => do
      for item in bar do processSlowly item
  withProgressbar (progressbarLength count.toNat (width := 0) (showPercent := some false)
      (showEta := false) (fillChar := style "#" (fg := "magenta")))
    fun bar => do
      for item in bar do processSlowly item
  -- 'Non-linear progress bar'
  let steps := (List.range 20).map fun x => Float.exp (x.toFloat * 1.0 / 20) - 1
  let count := (steps.foldl (· + ·) 0).toUInt64.toNat
  withProgressbar (progressbarLength count (showPercent := some false) (label := "Slowing progress bar")
      (fillChar := style "█" (fg := "green")))
    fun bar => do
      for item in steps do
        sleep item
        bar.update item

/-- Opens a file or URL In the default application. -/
click_command «open» [name := "open"] (url : String) : IO Unit := do
  let _ ← launch url

/-- Opens a file or URL In the default application. -/
click_command locate (url : String) : IO Unit := do
  let _ ← launch url (locate := true)

/-- Opens an editor with some text in it. -/
click_command edit [name := "edit"] : IO Unit := do
  let marker := "# Everything below is ignored\n"
  match ← Terminal.edit s!"\n\n{marker}" with
  | some message =>
    let msg := String.ofList (((message.splitOn marker).headD "").toList.reverse.dropWhile (· == '\n')).reverse
    if msg.isEmpty then echo "Empty message!" else echo s!"Message:\n{msg}"
  | none => echo "You did not enter anything!"

/-- Clears the entire screen. -/
click_command clear [name := "clear"] : IO Unit := Terminal.clear

/-- Waits for the user to press a button. -/
click_command pause [name := "pause"] : IO Unit := Terminal.pause

/-- Shows a simple menu. -/
click_command menu : IO Unit := do
  let mut menu := "main"
  -- `while True`, bounded by a generous number of key presses
  for _ in [0:1000000] do
    if menu == "main" then
      echo "Main menu:"
      echo "  d: debug menu"
      echo "  q: quit"
      let char ← getchar
      if char == "d" then menu := "debug"
      else if char == "q" then menu := "quit"
      else echo "Invalid input"
    else if menu == "debug" then
      echo "Debug menu"
      echo "  b: back"
      let char ← getchar
      if char == "b" then menu := "main"
      else echo "Invalid input"
    else if menu == "quit" then
      return

/-- This script showcases different terminal UI helpers in Click. -/
click_group cli [commands := [colordemo, pager, progress, «open», locate, edit, clear, pause, menu]] :
    IO Unit := pure ()

def main (args : List String) : IO UInt32 := cli.run () args (prog := "termui")
