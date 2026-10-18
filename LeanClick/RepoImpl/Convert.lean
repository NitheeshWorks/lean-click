import LeanClick.RepoImpl.Text

/-!
# Type conversion

`convert t s` turns one raw string into a value of `t.Carrier`, or fails
with the message click's type would raise (without the parameter hint,
which `Resolve` adds). Conversions that need the filesystem (`Path`, `File`)
read the `fs` view passed in, never the real filesystem.
-/

namespace LeanClick

open Text

/-- click's `ParamType.name`. -/
def ParamType.name : ParamType → String
  | .string => "text"
  | .int => "integer"
  | .float => "float"
  | .bool => "boolean"
  | .choice _ _ => "choice"
  | .path s =>
    if s.fileOkay && !s.dirOkay then "file"
    else if s.dirOkay && !s.fileOkay then "directory"
    else "path"
  | .file _ => "filename"
  | .intRange _ => "integer range"
  | .floatRange _ => "float range"

/-- `_describe_range` for integer bounds. -/
def IntBounds.describe (r : IntBounds) : String :=
  match r.min, r.max with
  | none, none => "x<=None"
  | none, some hi => s!"x{if r.maxOpen then "<" else "<="}{hi}"
  | some lo, none => s!"x{if r.minOpen then ">" else ">="}{lo}"
  | some lo, some hi =>
    s!"{lo}{if r.minOpen then "<" else "<="}x{if r.maxOpen then "<" else "<="}{hi}"

/-- `_describe_range` for float bounds. -/
def FloatBounds.describe (r : FloatBounds) : String :=
  match r.min, r.max with
  | none, none => "x<=None"
  | none, some hi => s!"x{if r.maxOpen then "<" else "<="}{showDec hi}"
  | some lo, none => s!"x{if r.minOpen then ">" else ">="}{showDec lo}"
  | some lo, some hi =>
    s!"{showDec lo}{if r.minOpen then "<" else "<="}x{if r.maxOpen then "<" else "<="}{showDec hi}"

/-- `envvar_list_splitter`: `Path` and `File` split on `:` (keeping empty
pieces), everything else on whitespace. -/
def ParamType.splitEnvvar (t : ParamType) (s : String) : List String :=
  match t with
  | .path _ | .file _ => splitChar s ':'
  | _ => splitWs s

/-- click's `BoolParamType.bool_states`. -/
def boolStates : List (String × Bool) :=
  [("1", true), ("0", false), ("yes", true), ("no", false), ("true", true),
   ("false", false), ("on", true), ("off", false), ("t", true), ("f", false),
   ("y", true), ("n", false), ("", false)]

/-- `str_to_bool`: strip, lower-case, look up. -/
def strToBool (s : String) : Option Bool :=
  let k := lower (strip s)
  (boolStates.find? (fun kv => kv.1 == k)).map (·.2)

/-- The recognised boolean spellings, sorted, as the error message lists
them. -/
def boolStatesText : String := ", ".intercalate (sortStrings (boolStates.map (·.1)))

/-- How a choice is matched: case-folded when not case sensitive. -/
def normChoice (caseSensitive : Bool) (s : String) : String :=
  if caseSensitive then s else lower s

/-- The "is not one of" message. -/
def choiceInvalid (cs : List String) (caseSensitive : Bool) (s : String) : String :=
  let shown := ", ".intercalate (cs.map (fun c => pyRepr (normChoice caseSensitive c)))
  if cs.length == 1 then s!"{pyRepr s} is not {shown}."
  else s!"{pyRepr s} is not one of {shown}."

/-- Find the declared choice the input matches, with its membership proof. -/
def matchChoice (cs : List String) (caseSensitive : Bool) (s : String) :
    Option {c : String // c ∈ cs} :=
  match h : cs.find? (fun c => normChoice caseSensitive c == normChoice caseSensitive s) with
  | some c => some ⟨c, List.mem_of_find?_eq_some h⟩
  | none => none

/-- `IntRange.convert`: check, clamping if asked. -/
def convertIntRange (r : IntBounds) (n : Int) : Except String {n : Int // r.contains n = true} :=
  let cand : Int :=
    if r.clamp then
      match r.min, r.max with
      | some lo, _ =>
        if (if r.minOpen then n ≤ lo else n < lo) then (if r.minOpen then lo + 1 else lo)
        else match r.max with
          | some hi => if (if r.maxOpen then n ≥ hi else n > hi) then (if r.maxOpen then hi - 1 else hi) else n
          | none => n
      | none, some hi => if (if r.maxOpen then n ≥ hi else n > hi) then (if r.maxOpen then hi - 1 else hi) else n
      | none, none => n
    else n
  if h : r.contains cand = true then .ok ⟨cand, h⟩
  else .error s!"{n} is not in the range {r.describe}."

/-- `FloatRange.convert`: check, clamping closed bounds if asked. -/
def convertFloatRange (r : FloatBounds) (x : Dec) : Except String {x : Dec // r.contains x = true} :=
  let cand : Dec :=
    if r.clamp then
      match r.min, r.max with
      | some lo, some hi => if Dec.lt x lo then lo else if Dec.lt hi x then hi else x
      | some lo, none => if Dec.lt x lo then lo else x
      | none, some hi => if Dec.lt hi x then hi else x
      | none, none => x
    else x
  if h : r.contains cand = true then .ok ⟨cand, h⟩
  else .error s!"{showDec x} is not in the range {r.describe}."

/-- `Path.convert` against the filesystem view. -/
def convertPath (fs : String → Option FileInfo) (spec : PathSpec) (s : String) :
    Except String String :=
  let isDash := spec.fileOkay && spec.allowDash && s == "-"
  if isDash then .ok s else
  let name := (ParamType.path spec).name
  let title := match name.toList with
    | c :: r => String.ofList (c.toUpper :: r)
    | [] => ""
  match fs s with
  | none => if spec.exists_ then .error s!"{title} {pyRepr s} does not exist." else .ok s
  | some info =>
    if !spec.fileOkay && info.isFile then .error s!"{title} {pyRepr s} is a file."
    else if !spec.dirOkay && info.isDir then .error s!"{title} {pyRepr s} is a directory."
    else if spec.readable && !info.readable then .error s!"{title} {pyRepr s} is not readable."
    else if spec.writable && !info.writable then .error s!"{title} {pyRepr s} is not writable."
    else if spec.executable && !info.executable then .error s!"{title} {pyRepr s} is not executable."
    else .ok s

/-- `File.convert`: a file opened for reading must exist and be readable;
writing is lazy (checked when opened). -/
def convertFile (fs : String → Option FileInfo) (spec : FileSpec) (s : String) :
    Except String FileArg :=
  let reading := !(spec.mode.toList.contains 'w' || spec.mode.toList.contains 'a' ||
    spec.mode.toList.contains 'x')
  if s == "-" || !reading then .ok ⟨s, spec.mode⟩
  else match fs s with
    | none => .error s!"{pyRepr s}: No such file or directory"
    | some info =>
      if info.isDir then .error s!"{pyRepr s}: Is a directory"
      else if !info.readable then .error s!"{pyRepr s}: Permission denied"
      else .ok ⟨s, spec.mode⟩

/-- Convert one raw string to a value of the parameter type. -/
def convert (fs : String → Option FileInfo) : (t : ParamType) → String → Except String t.Carrier
  | .string, s => .ok s
  | .int, s => match parseInt s with
    | some n => .ok n
    | none => .error s!"{pyRepr s} is not a valid integer."
  | .float, s => match parseDec s with
    | some d => .ok d
    | none => .error s!"{pyRepr s} is not a valid float."
  | .bool, s => match strToBool s with
    | some b => .ok b
    | none => .error s!"{pyRepr s} is not a valid boolean. Recognized values: {boolStatesText}"
  | .choice cs cs', s => match matchChoice cs cs' s with
    | some c => .ok c
    | none => .error (choiceInvalid cs cs' s)
  | .path spec, s => convertPath fs spec s
  | .file spec, s => convertFile fs spec s
  | .intRange r, s => match parseInt s with
    | some n => convertIntRange r n
    | none => .error s!"{pyRepr s} is not a valid integer range."
  | .floatRange r, s => match parseDec s with
    | some d => convertFloatRange r d
    | none => .error s!"{pyRepr s} is not a valid float range."

/-- Convert a list, keeping its length in the type. -/
def convertList (fs : String → Option FileInfo) (t : ParamType) :
    (xs : List String) → Except String {ys : List t.Carrier // ys.length = xs.length}
  | [] => .ok ⟨[], rfl⟩
  | x :: xs => do
    let y ← convert fs t x
    let ⟨ys, h⟩ ← convertList fs t xs
    return ⟨y :: ys, by simp [h]⟩

/-- Show a parsed value the way click's messages and defaults print it. -/
def ParamType.show : (t : ParamType) → t.Carrier → String
  | .string, s => s
  | .int, n => toString (show Int from n)
  | .float, d => showDec (show Dec from d)
  | .bool, b => if (show Bool from b) then "True" else "False"
  | .choice _ _, c => c.val
  | .path _, s => show String from s
  | .file _, f => (show FileArg from f).name
  | .intRange _, n => toString n.val
  | .floatRange _, x => showDec x.val

end LeanClick
