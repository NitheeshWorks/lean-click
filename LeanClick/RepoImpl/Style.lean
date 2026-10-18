import LeanClick.RepoImpl.Text

/-!
# Styling

click's `style` and `unstyle`: ANSI escape sequences around a text, and
their removal. `Terminal.echo` strips them when the output is not a
terminal, as click does.
-/

namespace LeanClick

/-- An ANSI color by click's name (`red`, `bright_blue`, ...), a 256-color
index, or an RGB triple. -/
inductive Color where
  | named (name : String)
  | index (n : Nat)
  | rgb (r g b : Nat)
  deriving DecidableEq, Repr, Inhabited

instance : Coe String Color := ⟨.named⟩

def colorCodes : List (String × Nat) :=
  [("black", 30), ("red", 31), ("green", 32), ("yellow", 33), ("blue", 34), ("magenta", 35),
   ("cyan", 36), ("white", 37), ("reset", 39), ("bright_black", 90), ("bright_red", 91),
   ("bright_green", 92), ("bright_yellow", 93), ("bright_blue", 94), ("bright_magenta", 95),
   ("bright_cyan", 96), ("bright_white", 97)]

/-- The SGR parameters of a color, foreground (`offset = 0`) or background
(`offset = 10`). -/
def Color.sgr (c : Color) (offset : Nat) : String :=
  match c with
  | .named n => toString (((colorCodes.lookup n).getD 39) + offset)
  | .index i => s!"{38 + offset};5;{i}"
  | .rgb r g b => s!"{38 + offset};2;{r};{g};{b}"

/-- `click.style`: the text wrapped in the escape sequences for the given
attributes, reset at the end unless `reset := false`. -/
def style (text : String) (fg : Option Color := none) (bg : Option Color := none)
    (bold : Option Bool := none) (dim : Option Bool := none) (underline : Option Bool := none)
    (overline : Option Bool := none) (italic : Option Bool := none) (blink : Option Bool := none)
    (reverse : Option Bool := none) (strikethrough : Option Bool := none) (reset : Bool := true) :
    String :=
  let esc := fun (code : String) => "\x1b[" ++ code ++ "m"
  let flag := fun (b : Option Bool) (on off : Nat) => match b with
    | some true => esc (toString on)
    | some false => esc (toString off)
    | none => ""
  (match fg with | some c => esc (c.sgr 0) | none => "") ++
  (match bg with | some c => esc (c.sgr 10) | none => "") ++
  flag bold 1 22 ++ flag dim 2 22 ++ flag underline 4 24 ++ flag overline 53 55 ++
  flag italic 3 23 ++ flag blink 5 25 ++ flag reverse 7 27 ++ flag strikethrough 9 29 ++
  text ++ (if reset then esc "0" else "")

/-- `click.unstyle`: remove ANSI escape sequences (`ESC [ ... letter`). -/
def unstyle (s : String) : String :=
  let rec go : List Char → List Char → Nat → List Char
    | [], acc, _ => acc.reverse
    | _, acc, 0 => acc.reverse
    | '\x1b' :: '[' :: rest, acc, f + 1 =>
      let body := rest.dropWhile (fun c => c.isDigit || c == ';' || c == '?')
      match body with
      | c :: rest' => if c.isAlpha then go rest' acc f else go rest' (c :: acc) f
      | [] => acc.reverse
    | c :: rest, acc, f + 1 => go rest (c :: acc) f
  String.ofList (go s.toList [] (s.length + 1))

end LeanClick
