/-!
# Parameter types

The value types a parameter can be declared with (click's `ParamType`
hierarchy), and for each one the Lean type its parsed values inhabit
(`ParamType.Carrier`). The carrier is where refinements live: a `choice`
value is a member of its choice list, an `intRange` value carries the proof
that it is inside the bounds. A parsed value cannot be out of range, because
the type of out-of-range values is empty.
-/

namespace LeanClick

/-- An exact decimal `mant * 10 ^ (-exp)`. Floats are parsed into this, so
range checks are exact rational comparisons; `Dec.toFloat` converts at the
boundary. -/
structure Dec where
  mant : Int
  exp : Nat
  deriving DecidableEq, Repr, Inhabited

namespace Dec

/-- `a ≤ b`, by cross-multiplying to a common scale. -/
def le (a b : Dec) : Bool := a.mant * (10 : Int) ^ b.exp ≤ b.mant * (10 : Int) ^ a.exp

/-- `a < b`, by cross-multiplying to a common scale. -/
def lt (a b : Dec) : Bool := a.mant * (10 : Int) ^ b.exp < b.mant * (10 : Int) ^ a.exp

def ofInt (n : Int) : Dec := ⟨n, 0⟩

end Dec

/-- Bounds of click's `IntRange`. `none` means unbounded on that side. -/
structure IntBounds where
  min : Option Int := none
  max : Option Int := none
  minOpen : Bool := false
  maxOpen : Bool := false
  clamp : Bool := false
  deriving DecidableEq, Repr, Inhabited

/-- Bounds of click's `FloatRange`. -/
structure FloatBounds where
  min : Option Dec := none
  max : Option Dec := none
  minOpen : Bool := false
  maxOpen : Bool := false
  clamp : Bool := false
  deriving DecidableEq, Repr, Inhabited

/-- Is `n` inside the bounds? -/
def IntBounds.contains (r : IntBounds) (n : Int) : Bool :=
  (match r.min with
   | none => true
   | some m => if r.minOpen then decide (m < n) else decide (m ≤ n)) &&
  (match r.max with
   | none => true
   | some m => if r.maxOpen then decide (n < m) else decide (n ≤ m))

/-- Is `x` inside the bounds? -/
def FloatBounds.contains (r : FloatBounds) (x : Dec) : Bool :=
  (match r.min with
   | none => true
   | some m => if r.minOpen then Dec.lt m x else Dec.le m x) &&
  (match r.max with
   | none => true
   | some m => if r.maxOpen then Dec.lt x m else Dec.le x m)

/-- The range of a count option: `IntRange(min=0)`. -/
def countBounds : IntBounds := { min := some 0 }

/-- click's `Path` type: what the path must be. The checks run against the
filesystem view in `Env`, so the parser stays pure. -/
structure PathSpec where
  exists_ : Bool := false
  fileOkay : Bool := true
  dirOkay : Bool := true
  readable : Bool := true
  writable : Bool := false
  executable : Bool := false
  allowDash : Bool := false
  deriving DecidableEq, Repr, Inhabited

/-- click's `File` type: the open mode. The file is opened by
`LeanClick.Terminal`, never by the parser. -/
structure FileSpec where
  mode : String := "r"
  lazy : Option Bool := none
  deriving DecidableEq, Repr, Inhabited

/-- A parsed `File` value: the name (`-` is the standard stream) and the mode
it is to be opened with. -/
structure FileArg where
  name : String
  mode : String
  deriving DecidableEq, Repr, Inhabited

/-- A calendar date and time (click's `DateTime` values, naive). -/
structure Timestamp where
  year : Nat
  month : Nat
  day : Nat
  hour : Nat := 0
  minute : Nat := 0
  second : Nat := 0
  deriving DecidableEq, Repr, Inhabited

def Timestamp.isLeap (y : Nat) : Bool := (y % 4 == 0 && y % 100 != 0) || y % 400 == 0

def Timestamp.daysIn (y m : Nat) : Nat :=
  match m with
  | 2 => if Timestamp.isLeap y then 29 else 28
  | 4 | 6 | 9 | 11 => 30
  | _ => 31

/-- A real date: month 1-12, a day the month has, a time of day. -/
def Timestamp.valid (d : Timestamp) : Bool :=
  1 ≤ d.year && 1 ≤ d.month && d.month ≤ 12 && 1 ≤ d.day && d.day ≤ Timestamp.daysIn d.year d.month &&
  d.hour < 24 && d.minute < 60 && d.second < 60

/-- Canonical UUID text: 36 characters, hyphens at 8, 13, 18 and 23,
lower-case hex digits elsewhere (Python's `str(uuid.UUID(...))`). -/
def uuidCanonical (s : String) : Bool :=
  let cs := s.toList
  cs.length == 36 &&
  (List.range 36).all fun i =>
    let c := cs.getD i ' '
    if i == 8 || i == 13 || i == 18 || i == 23 then c == '-'
    else c.isDigit || ('a' ≤ c && c ≤ 'f')

/-- The parameter types. -/
inductive ParamType where
  | string
  | int
  | float
  | bool
  | choice (choices : List String) (caseSensitive : Bool := true)
  | path (spec : PathSpec := {})
  | file (spec : FileSpec := {})
  | intRange (bounds : IntBounds)
  | floatRange (bounds : FloatBounds)
  | uuid
  | dateTime (formats : List String := ["%Y-%m-%d", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%d %H:%M:%S"])
  /-- `click.Tuple`, as a cons list of per-position types (see `ParamType.tuple`). -/
  | tupleNil
  | tupleCons (head tail : ParamType)
  deriving DecidableEq, Repr, Inhabited

/-- `click.Tuple(types)`. -/
def ParamType.tuple : List ParamType → ParamType
  | [] => .tupleNil
  | t :: ts => .tupleCons t (ParamType.tuple ts)

/-- Is the type a `Tuple` (click's `is_composite`)? -/
def ParamType.isComposite : ParamType → Bool
  | .tupleNil | .tupleCons _ _ => true
  | _ => false

/-- The number of values a type takes (click's `arity`). -/
def ParamType.arity : ParamType → Nat
  | .tupleNil => 0
  | .tupleCons _ t => t.arity + 1
  | _ => 1

def ParamType.isIntRange : ParamType → Bool
  | .intRange _ => true
  | _ => false

/-- The Lean type a parsed value of each parameter type inhabits. The
refinements are the point: `choice` values are members of the list and
range values carry their bounds proof. -/
def ParamType.Carrier : ParamType → Type
  | .string => String
  | .int => Int
  | .float => Dec
  | .bool => Bool
  | .choice cs _ => {c : String // c ∈ cs}
  | .path _ => String
  | .file _ => FileArg
  | .intRange r => {n : Int // r.contains n = true}
  | .floatRange r => {x : Dec // r.contains x = true}
  | .uuid => {s : String // uuidCanonical s = true}
  | .dateTime _ => {d : Timestamp // d.valid = true}
  | .tupleNil => Unit
  | .tupleCons h t => h.Carrier × t.Carrier

/-- A parsed value with its refinement erased: what callbacks read. -/
inductive Value where
  | none
  | str (s : String)
  | int (n : Int)
  | dec (d : Dec)
  | bool (b : Bool)
  | file (f : FileArg)
  | dateTime (d : Timestamp)
  | list (vs : List Value)
  deriving Inhabited

/-- `Repr` by structural recursion (the derived printer of a nested
inductive is `partial`). -/
def Value.reprPrec : Value → Nat → Std.Format
  | .none, _ => "LeanClick.Value.none"
  | .str s, p => Repr.addAppParen ("LeanClick.Value.str " ++ repr s) p
  | .int n, p => Repr.addAppParen ("LeanClick.Value.int " ++ reprArg n) p
  | .dec d, p => Repr.addAppParen ("LeanClick.Value.dec " ++ reprArg d) p
  | .bool b, p => Repr.addAppParen ("LeanClick.Value.bool " ++ repr b) p
  | .file f, p => Repr.addAppParen ("LeanClick.Value.file " ++ reprArg f) p
  | .dateTime d, p => Repr.addAppParen ("LeanClick.Value.dateTime " ++ reprArg d) p
  | .list vs, p =>
    Repr.addAppParen ("LeanClick.Value.list " ++
      Std.Format.bracket "[" (Std.Format.joinSep (vs.attach.map fun ⟨v, _⟩ => Value.reprPrec v 0)
        ("," ++ Std.Format.line)) "]") p
termination_by v => sizeOf v
decreasing_by
  have := List.sizeOf_lt_of_mem ‹v ∈ vs›
  simp_wf; omega

instance : Repr Value := ⟨Value.reprPrec⟩

/-- Forget the refinement of a parsed value. -/
def ParamType.erase : (t : ParamType) → t.Carrier → Value
  | .string, s => .str s
  | .int, n => .int n
  | .float, d => .dec d
  | .bool, b => .bool b
  | .choice _ _, c => .str c.val
  | .path _, s => .str s
  | .file _, f => .file f
  | .intRange _, n => .int n.val
  | .floatRange _, x => .dec x.val
  | .uuid, u => .str u.val
  | .dateTime _, d => .dateTime d.val
  | .tupleNil, _ => .list []
  | .tupleCons h t, (x, y) => match t.erase y with
    | .list vs => .list (h.erase x :: vs)
    | v => .list [h.erase x, v]

end LeanClick
