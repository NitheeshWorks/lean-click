import LeanClick.DataTypes

/-!
# Text utilities

The string-level pieces click borrows from Python: `repr` of a string,
option-prefix splitting, whitespace splitting, number parsing, the
`textwrap` line filler, `textwrap.shorten`, and `difflib`'s close-match
search for "Did you mean" hints. All total; loops recurse on an explicit fuel
that bounds the work.

These are tested, not proved (see `tests/Props.lean`); the proofs about the
parser only use `splitEq` and `startsDashDash`, which are simple enough to
reason about directly.
-/

namespace LeanClick.Text

/-- `n` spaces. -/
def spaces (n : Nat) : String := String.ofList (List.replicate n ' ')

/-- Python's `repr` of a `str`: single quotes unless the text contains a
single quote and no double quote. -/
def pyRepr (s : String) : String :=
  let cs := s.toList
  let q := if cs.contains '\'' && !cs.contains '"' then '"' else '\''
  let esc : Char → List Char := fun c =>
    if c == '\\' then ['\\', '\\']
    else if c == q then ['\\', q]
    else if c == '\n' then ['\\', 'n']
    else if c == '\r' then ['\\', 'r']
    else if c == '\t' then ['\\', 't']
    else [c]
  String.ofList (q :: cs.flatMap esc ++ [q])

/-- Python's `repr` of a tuple of strings (what `click.echo` prints for a
`multiple` or `nargs=-1` value). -/
def pyTuple : List String → String
  | [x] => "(" ++ pyRepr x ++ ",)"
  | xs => "(" ++ ", ".intercalate (xs.map pyRepr) ++ ")"

/-- Python's `str(b)` of a bool. -/
def pyBool (b : Bool) : String := if b then "True" else "False"

/-- Python's `sys.platform`. -/
def sysPlatform : String :=
  if System.Platform.isWindows then "win32" else if System.Platform.isOSX then "darwin" else "linux"

/-- click's `_split_opt`: the prefix (`-`, `--`, or another doubled
non-alphanumeric character) and the rest. -/
def splitOptList : List Char → List Char × List Char
  | [] => ([], [])
  | c :: rest =>
    if c.isAlphanum then ([], c :: rest)
    else match rest with
      | d :: rest' => if d == c then ([c, c], rest') else ([c], rest)
      | [] => ([c], [])

def splitOpt (opt : String) : String × String :=
  let (a, b) := splitOptList opt.toList
  (String.ofList a, String.ofList b)

/-- Does the token start with `--`? -/
def startsDashDash (tok : String) : Bool :=
  match tok.toList with
  | '-' :: '-' :: _ => true
  | _ => false

/-- Does the token look like an option to the scanner: starts with `-` and
is longer than one character. -/
def looksLikeOption (tok : String) : Bool :=
  match tok.toList with
  | '-' :: _ :: _ => true
  | _ => false

/-- Split at the first `=`: `--x=1` gives `("--x", some "1")`. -/
def splitEq (tok : String) : String × Option String :=
  let cs := tok.toList
  match cs.idxOf? '=' with
  | none => (tok, none)
  | some i => (String.ofList (cs.take i), some (String.ofList (cs.drop (i + 1))))

/-- Python's `str.split()` with no argument: split on runs of whitespace,
dropping empty pieces. -/
def splitWsList : List Char → List Char → List (List Char)
  | [], acc => if acc.isEmpty then [] else [acc.reverse]
  | c :: cs, acc =>
    if c.isWhitespace then
      (if acc.isEmpty then [] else [acc.reverse]) ++ splitWsList cs []
    else splitWsList cs (c :: acc)

def splitWs (s : String) : List String :=
  (splitWsList s.toList []).map String.ofList

/-- Python's `str.split(sep)` for a one-character separator: keeps empty
pieces. -/
def splitChar (s : String) (sep : Char) : List String :=
  s.splitOn (String.ofList [sep])

/-- Python's `str.strip()`. -/
def strip (s : String) : String :=
  String.ofList ((s.toList.dropWhile Char.isWhitespace).reverse.dropWhile Char.isWhitespace).reverse

/-- Python's `str.lstrip()`. -/
def lstrip (s : String) : String := String.ofList (s.toList.dropWhile Char.isWhitespace)

/-- `rstrip("\n")`. -/
def rstripNewlines (s : String) : String :=
  String.ofList (s.toList.reverse.dropWhile (· == '\n')).reverse

def lower (s : String) : String := s.toLower
def upper (s : String) : String := s.toUpper

/-- Replace every `a` by `b`. -/
def replaceChar (s : String) (a b : Char) : String :=
  String.ofList (s.toList.map fun c => if c == a then b else c)

/-- Python's `str.expandtabs()` (tab size 8). -/
def expandTabs (s : String) : String :=
  let rec go : List Char → Nat → List Char
    | [], _ => []
    | '\t' :: cs, col =>
      let n := 8 - col % 8
      List.replicate n ' ' ++ go cs (col + n)
    | '\n' :: cs, _ => '\n' :: go cs 0
    | c :: cs, col => c :: go cs (col + 1)
  String.ofList (go s.toList 0)

/-- Split into lines on `\n` (Python's `splitlines`, without the other line
boundaries). A trailing newline does not produce an empty last line. -/
def splitLines (s : String) : List String :=
  let parts := s.splitOn "\n"
  match parts.getLast? with
  | some "" => parts.dropLast
  | _ => parts

/-! ## Number parsing (Python's `int(str)` and `float(str)`) -/

/-- Digits with single underscores between them, as Python accepts. -/
def digitsWithUnderscores (cs : List Char) : Option (List Char) :=
  let rec go : List Char → Bool → Option (List Char)
    | [], _ => some []
    | '_' :: cs, prevDigit =>
      if prevDigit then
        match cs with
        | d :: _ => if d.isDigit then go cs false else none
        | [] => none
      else none
    | c :: cs, _ => if c.isDigit then (go cs true).map (c :: ·) else none
  match cs with
  | [] => none
  | c :: _ => if c.isDigit then go cs false else none

def digitsValue (cs : List Char) : Nat :=
  cs.foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0

/-- Python's `int(s)` for a decimal string. -/
def parseInt (s : String) : Option Int :=
  let cs := (strip s).toList
  let (neg, body) := match cs with
    | '-' :: r => (true, r)
    | '+' :: r => (false, r)
    | r => (false, r)
  match digitsWithUnderscores body with
  | none => none
  | some ds => let n : Int := digitsValue ds; some (if neg then -n else n)

/-- Python's `float(s)` for finite decimal strings; `inf`/`nan` are not
representable as `Dec` and are rejected, and a huge exponent stays exact
instead of overflowing to `inf` (deviation `dev.float-inf` in
`docs/traceability.toml`). -/
def parseDec (s : String) : Option Dec :=
  let cs := (strip s).toList
  let (neg, body) := match cs with
    | '-' :: r => (true, r)
    | '+' :: r => (false, r)
    | r => (false, r)
  let isE : Char → Bool := fun c => c == 'e' || c == 'E'
  let mantPart := body.takeWhile (fun c => !isE c)
  let expPart := body.dropWhile (fun c => !isE c)
  let intPart := mantPart.takeWhile (· != '.')
  let rest := mantPart.dropWhile (· != '.')
  let fracPart := rest.drop 1
  let hasDot := !rest.isEmpty
  let okInt : Option (List Char) := if intPart.isEmpty then some [] else digitsWithUnderscores intPart
  let okFrac : Option (List Char) := if fracPart.isEmpty then some [] else digitsWithUnderscores fracPart
  match okInt, okFrac with
  | some i, some f =>
    if i.isEmpty && f.isEmpty then none
    else if !hasDot && !fracPart.isEmpty then none
    else
      let e : Option Int := match expPart with
        | [] => some 0
        | _ :: ecs =>
          let (eneg, ebody) := match ecs with
            | '-' :: r => (true, r)
            | '+' :: r => (false, r)
            | r => (false, r)
          match digitsWithUnderscores ebody with
          | none => none
          | some ds => let n : Int := digitsValue ds; some (if eneg then -n else n)
      match e with
      | none => none
      | some ex =>
        if ex.natAbs > 4096 then none else
        let m : Int := digitsValue (i ++ f)
        let m := if neg then -m else m
        let fl : Int := f.length
        if ex ≥ fl then some ⟨m * (10 : Int) ^ (ex - fl).toNat, 0⟩
        else some ⟨m, (fl - ex).toNat⟩
  | _, _ => none

/-- Python's `str(float)` for a decimal: `4.0`, `0.5`, `1e-05`, `1e+16`. -/
def showDec (d : Dec) : String :=
  -- normalise trailing zeros away
  let rec norm (m : Int) (e : Nat) (fuel : Nat) : Int × Nat :=
    match fuel, e with
    | 0, _ => (m, e)
    | _, 0 => (m, 0)
    | f + 1, e' + 1 => if m % 10 == 0 then norm (m / 10) e' f else (m, e' + 1)
  let (m, e) := norm d.mant d.exp (d.exp + 1)
  let neg := m < 0
  let ds := (toString m.natAbs).toList
  let sign := if neg then "-" else ""
  if m == 0 then sign ++ "0.0" else
  -- scientific exponent of the leading digit
  let x : Int := (ds.length : Int) - 1 - e
  -- significant digits without trailing zeros
  let sig := (ds.reverse.dropWhile (· == '0')).reverse
  if x < -4 || x ≥ 16 then
    let mant := match sig with
      | [c] => String.ofList [c]
      | c :: r => String.ofList (c :: '.' :: r)
      | [] => "0"
    let ex := if x < 0 then "-" ++ (if x > -10 then "0" else "") ++ toString x.natAbs
      else "+" ++ (if x < 10 then "0" else "") ++ toString x.natAbs
    sign ++ mant ++ "e" ++ ex
  else if e == 0 then sign ++ String.ofList ds ++ ".0"
  else if ds.length > e then
    sign ++ String.ofList (ds.take (ds.length - e)) ++ "." ++ String.ofList (ds.drop (ds.length - e))
  else sign ++ "0." ++ String.ofList (List.replicate (e - ds.length) '0' ++ ds)

/-! ## `textwrap` -/

theorem length_dropWhile_le {α : Type} (p : α → Bool) : ∀ l : List α, (l.dropWhile p).length ≤ l.length
  | [] => by simp
  | x :: xs => by
    simp only [List.dropWhile_cons]
    split
    · have := length_dropWhile_le p xs; simp; omega
    · simp

/-- Split text into runs of whitespace and runs of non-whitespace. -/
def runs : List Char → List (List Char)
  | [] => []
  | c :: cs =>
    let w := c.isWhitespace
    let same := cs.takeWhile (fun d => d.isWhitespace == w)
    (c :: same) :: runs (cs.dropWhile (fun d => d.isWhitespace == w))
termination_by cs => cs.length
decreasing_by
  simp only [List.length_cons]
  have := length_dropWhile_le (fun d => d.isWhitespace == c.isWhitespace) cs
  omega

private def isLetter (c : Char) : Bool := c.isAlpha

/-- Split a word at hyphens the way `textwrap`'s `wordsep_re` does: after a
`-` preceded by two letters (or letter-hyphen-letter) and followed by a
letter, optional hyphen, letter. -/
def hyphenSplit (w : List Char) : List (List Char) :=
  let arr := w.toArray
  let n := arr.size
  let at_ (i : Nat) : Char := arr.getD i ' '
  let isL (i : Nat) : Bool := i < n && isLetter (at_ i)
  let cut (i : Nat) : Bool :=
    at_ i == '-' &&
    ((i ≥ 2 && isL (i - 1) && isL (i - 2)) ||
     (i ≥ 3 && isL (i - 1) && at_ (i - 2) == '-' && isL (i - 3))) &&
    isL (i + 1) && (isL (i + 2) || (at_ (i + 2) == '-' && isL (i + 3)))
  let rec go (i : Nat) (start : Nat) (fuel : Nat) : List (List Char) :=
    match fuel with
    | 0 => [(arr.extract start n).toList]
    | f + 1 =>
      if i ≥ n then [(arr.extract start n).toList]
      else if cut i then (arr.extract start (i + 1)).toList :: go (i + 1) (i + 1) f
      else go (i + 1) start f
  (go 0 0 (n + 1)).filter (!·.isEmpty)

/-- The chunks `textwrap` wraps: whitespace runs and (hyphen-split) words. -/
def chunks (s : String) : List (List Char) :=
  (runs s.toList).flatMap fun r =>
    match r with
    | c :: _ => if c.isWhitespace then [r] else hyphenSplit r
    | [] => []

private def isBlank (cs : List Char) : Bool := cs.all Char.isWhitespace

/-- Break a long word to fit `spaceLeft`, preferring a hyphen. -/
def longWordCut (chunk : List Char) (spaceLeft : Nat) : Nat :=
  if chunk.length > spaceLeft then
    let pre := chunk.take spaceLeft
    match (pre.reverse.idxOf? '-') with
    | some j =>
      let hyphen := pre.length - 1 - j
      if hyphen > 0 && (chunk.take hyphen).any (· != '-') then hyphen + 1 else spaceLeft
    | none => spaceLeft
  else spaceLeft

/-- `TextWrapper.wrap` with click's settings (`replace_whitespace=False`,
`drop_whitespace=True`, `break_long_words=True`, `break_on_hyphens=True`). -/
def wrapChunks (width : Nat) (initIndent subIndent : String) :
    List (List Char) → List String → Nat → List String
  | [], lines, _ => lines.reverse
  | _, lines, 0 => lines.reverse
  | cks, lines, fuel + 1 =>
    let indent := if lines.isEmpty then initIndent else subIndent
    let w : Int := (width : Int) - indent.length
    let cks := match cks with
      | c :: rest => if isBlank c && !lines.isEmpty then rest else c :: rest
      | [] => []
    -- take chunks while they fit
    let rec fill : List (List Char) → List (List Char) → Nat → List (List Char) × List (List Char)
      | [], acc, _ => (acc.reverse, [])
      | c :: rest, acc, len =>
        if (len + c.length : Int) ≤ w then fill rest (c :: acc) (len + c.length)
        else (acc.reverse, c :: rest)
    let (line, rest) := fill cks [] 0
    let curLen := (line.map List.length).sum
    let (line, rest) := match rest with
      | c :: rest' =>
        if (c.length : Int) > w then
          let spaceLeft := if w < 1 then 1 else (w - curLen).toNat
          let e := longWordCut c spaceLeft
          let piece := c.take e
          let remain := c.drop e
          (line ++ [piece], if remain.isEmpty then rest' else remain :: rest')
        else (line, c :: rest')
      | [] => (line, [])
    let line := match line.getLast? with
      | some l => if isBlank l then line.dropLast else line
      | none => line
    let lines := if line.isEmpty then lines
      else (indent ++ String.ofList line.flatten) :: lines
    wrapChunks width initIndent subIndent rest lines fuel

def wrap (width : Nat) (initIndent subIndent : String) (text : String) : List String :=
  let cks := chunks text
  wrapChunks width initIndent subIndent cks [] (2 * text.length + cks.length + 2)

/-- `TextWrapper.fill`. -/
def fill (width : Nat) (initIndent subIndent : String) (text : String) : String :=
  "\n".intercalate (wrap width initIndent subIndent text)

/-- click's `wrap_text` with `preserve_paragraphs=True`: paragraphs are
separated by blank lines; a paragraph whose first line is `\b` is not
rewrapped. -/
def wrapParagraphs (width : Nat) (initIndent subIndent : String) (text : String) : String :=
  let text := expandTabs text
  -- collect (indent, raw, text) paragraphs
  let step := fun (st : List (Nat × Bool × String) × List String × Option Nat) (line : String) =>
    let (ps, buf, ind) := st
    let flush := fun (ps : List (Nat × Bool × String)) (buf : List String) (ind : Option Nat) =>
      match buf.reverse with
      | [] => ps
      | b :: bs =>
        if strip b == "\x08" then (ind.getD 0, true, "\n".intercalate bs) :: ps
        else (ind.getD 0, false, " ".intercalate (b :: bs)) :: ps
    if line.isEmpty then (flush ps buf ind, [], none)
    else
      match ind with
      | none =>
        let l := lstrip line
        (ps, [l], some (line.length - l.length))
      | some i => (ps, line :: buf, some i)
  let (ps, buf, ind) := (splitLines text).foldl step ([], [], none)
  let ps := match buf.reverse with
    | [] => ps
    | b :: bs =>
      if strip b == "\x08" then (ind.getD 0, true, "\n".intercalate bs) :: ps
      else (ind.getD 0, false, " ".intercalate (b :: bs)) :: ps
  let render := fun (p : Nat × Bool × String) =>
    let (i, raw, t) := p
    let ex := spaces i
    if raw then "\n".intercalate ((splitLines t).map fun l => subIndent ++ ex ++ l)
    else fill width (initIndent ++ ex) (subIndent ++ ex) t
  "\n\n".intercalate (ps.reverse.map render)

/-- click's `_make_default_short_help`. -/
def shortHelp (help : String) (maxLen : Nat) : String :=
  let firstPara := (help.splitOn "\n\n").headD ""
  let words := splitWs firstPara
  let words := match words with
    | w :: ws => if w == "\x08" then ws else w :: ws
    | [] => []
  if words.isEmpty then "" else
  let arr := words.toArray
  let endsDot := fun (w : String) => w.toList.getLast? == some '.'
  let startsLower := fun (w : String) => match w.toList with
    | c :: _ => c.isLower
    | [] => false
  let rec cut (i : Nat) (fuel : Nat) : Nat :=
    match fuel with
    | 0 => arr.size
    | f + 1 =>
      if i ≥ arr.size then arr.size
      else if endsDot (arr.getD i "") && (i + 1 == arr.size || !startsLower (arr.getD (i + 1) "")) then i + 1
      else cut (i + 1) f
  let words := (arr.extract 0 (cut 0 (arr.size + 1))).toList
  let text := " ".intercalate words
  if text.length ≤ maxLen then text
  else if maxLen < 3 then "..."
  else
    -- textwrap.shorten: as many words as fit with the placeholder
    let rec take (ws : List String) (acc : List String) (len : Nat) : List String :=
      match ws with
      | [] => acc.reverse
      | w :: rest =>
        let newLen := if acc.isEmpty then w.length else len + 1 + w.length
        if newLen + 3 ≤ maxLen then take rest (w :: acc) newLen else acc.reverse
    let kept := take words [] 0
    if kept.isEmpty then "..." else " ".intercalate kept ++ "..."

/-! ## `difflib.get_close_matches` -/

/-- Longest common block of `a[alo:ahi]` and `b[blo:bhi]`, earliest in `a`
then earliest in `b`. -/
def longestMatch (a b : Array Char) (alo ahi blo bhi : Nat) : Nat × Nat × Nat :=
  let runLen := fun (i j : Nat) =>
    let rec go (k : Nat) (fuel : Nat) : Nat :=
      match fuel with
      | 0 => k
      | f + 1 =>
        if i + k < ahi && j + k < bhi && a.getD (i + k) ' ' == b.getD (j + k) ' ' then go (k + 1) f
        else k
    go 0 (ahi - i)
  (List.range (ahi - alo)).foldl (fun best di =>
    (List.range (bhi - blo)).foldl (fun best dj =>
      let i := alo + di
      let j := blo + dj
      let k := runLen i j
      if k > best.2.2 then (i, j, k) else best) best) (alo, blo, 0)

/-- Total size of `SequenceMatcher`'s matching blocks. -/
def matchCount (a b : Array Char) : Nat → Nat → Nat → Nat → Nat → Nat
  | 0, _, _, _, _ => 0
  | fuel + 1, alo, ahi, blo, bhi =>
    let (i, j, k) := longestMatch a b alo ahi blo bhi
    if k == 0 then 0
    else k + matchCount a b fuel alo i blo j + matchCount a b fuel (i + k) ahi (j + k) bhi

/-- `(2 * matches, total length)`: the ratio as a fraction. -/
def ratio (a b : String) : Nat × Nat :=
  let aa := a.toList.toArray
  let bb := b.toList.toArray
  let m := matchCount aa bb (aa.size + bb.size + 1) 0 aa.size 0 bb.size
  (2 * m, aa.size + bb.size)

/-- `difflib.get_close_matches(word, possibilities, n=3, cutoff=0.6)`. -/
def closeMatches (word : String) (possibilities : List String) : List String :=
  let scored := possibilities.filterMap fun p =>
    let (num, den) := ratio p word
    if den > 0 && 10 * num ≥ 6 * den then some (num, den, p) else none
  -- higher score first; ties broken by larger string, as `heapq.nlargest`
  let better := fun (x y : Nat × Nat × String) =>
    let (n1, d1, s1) := x
    let (n2, d2, s2) := y
    n1 * d2 > n2 * d1 || (n1 * d2 == n2 * d1 && decide (s2 < s1))
  let rec insert (x : Nat × Nat × String) : List (Nat × Nat × String) → List (Nat × Nat × String)
    | [] => [x]
    | y :: ys => if better x y then x :: y :: ys else y :: insert x ys
  let sorted := scored.foldl (fun acc x => insert x acc) []
  (sorted.take 3).map (·.2.2)

/-- Sort strings ascending (insertion sort; the lists are small). -/
def sortStrings (xs : List String) : List String :=
  let rec insert (x : String) : List String → List String
    | [] => [x]
    | y :: ys => if decide (x ≤ y) then x :: y :: ys else y :: insert x ys
  xs.foldl (fun acc x => insert x acc) []

/-- click's `_format_possibilities`. -/
def formatPossibilities (ps : List String) : String :=
  let s := ", ".intercalate ((sortStrings ps).map pyRepr)
  if ps.length == 1 then s!"Did you mean {s}?" else s!"(Did you mean one of: {s}?)"

end LeanClick.Text
