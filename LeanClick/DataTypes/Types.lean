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
  deriving DecidableEq, Repr, Inhabited

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

/-- A parsed value with its refinement erased: what callbacks read. -/
inductive Value where
  | none
  | str (s : String)
  | int (n : Int)
  | dec (d : Dec)
  | bool (b : Bool)
  | file (f : FileArg)
  | list (vs : List Value)
  deriving Repr, Inhabited

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

end LeanClick
