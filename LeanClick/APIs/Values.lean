import LeanClick.APIs.Build

/-!
# Reading parsed values

What a callback reads from `ctx.params`: the value of a parameter by name,
with its refinement erased (`Value`) or through typed accessors.
-/

namespace LeanClick

namespace ParsedArgs

/-- The entry for a parameter name. -/
def entry? (a : ParsedArgs) (name : String) : Option Entry :=
  a.entries.find? (fun e => e.param.name == name)

/-- The value for a parameter name (`.none` when absent or unknown). -/
def get (a : ParsedArgs) (name : String) : Value :=
  match a.entry? name with
  | some e => e.override.getD e.toValue
  | none => .none

/-- Where the value for a parameter came from. -/
def source? (a : ParsedArgs) (name : String) : Option Source :=
  (a.entry? name).map (·.source)

def getString? (a : ParsedArgs) (name : String) : Option String :=
  match a.get name with | .str s => some s | _ => none

def getInt? (a : ParsedArgs) (name : String) : Option Int :=
  match a.get name with | .int n => some n | _ => none

def getBool? (a : ParsedArgs) (name : String) : Option Bool :=
  match a.get name with | .bool b => some b | _ => none

def getDec? (a : ParsedArgs) (name : String) : Option Dec :=
  match a.get name with | .dec d => some d | _ => none

def getFile? (a : ParsedArgs) (name : String) : Option FileArg :=
  match a.get name with | .file f => some f | _ => none

/-- A multi-value parameter's values as strings (shown through their types). -/
def getStrings (a : ParsedArgs) (name : String) : List String :=
  match a.get name with
  | .list vs => vs.filterMap fun v => match v with | .str s => some s | _ => none
  | _ => []

/-- The values exposed to the callback (click's `ctx.params`): every
parameter except the help and version options. -/
def params (a : ParsedArgs) : List (String × Value) :=
  (a.entries.filter (·.param.exposeValue)).map fun e => (e.param.name, a.get e.param.name)

end ParsedArgs

/-- Render a value for display (tests and examples). -/
def Value.render : Value → String
  | .none => "None"
  | .str s => s
  | .int n => toString n
  | .dec d => Text.showDec d
  | .bool b => if b then "True" else "False"
  | .file f => f.name
  | .dateTime d => showDateTime d
  | .list vs => "(" ++ ", ".intercalate (vs.attach.map fun ⟨v, _⟩ => Value.render v) ++ ")"
  | .record n fs => n ++ "(" ++ ", ".intercalate (fs.attach.map fun ⟨(k, v), _⟩ =>
      k ++ "=" ++ (match v with | .str s => Text.pyRepr s | w => Value.render w)) ++ ")"
termination_by v => sizeOf v
decreasing_by
  all_goals first
    | (have := List.sizeOf_lt_of_mem ‹v ∈ vs›; simp_wf; omega)
    | (rename_i h; have := List.sizeOf_lt_of_mem h; simp_wf; simp at this ⊢; omega)

end LeanClick
