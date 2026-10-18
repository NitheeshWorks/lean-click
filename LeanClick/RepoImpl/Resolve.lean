import LeanClick.RepoImpl.Help

/-!
# Resolving one parameter

click's `consume_value` / `process_value` for one parameter:

1. the raw value, from the first source that has one, in the order
   command line, environment variable, default (`consume`); a prompt
   replaces a missing or default value when the option prompts;
2. conversion through the type, into the declared shape, with the shape
   proof (`convertShape`);
3. the required check (`Missing option` / `Missing argument`);
4. eager parameters (`--help`, `--version`) stop processing when set.
-/

namespace LeanClick

open Text

/-- A raw value before conversion. -/
inductive Raw where
  | tokens (xs : List String)
  | multi (xss : List (List String))
  | bool (b : Bool)
  | count (n : Nat)
  deriving DecidableEq, Repr, Inhabited

/-- Why processing stopped early. -/
inductive Stop where
  | error (e : UsageError)
  | eager (e : Eager)
  /-- An option needs an interactive answer that `Env.answers` lacks; `path`
  is the command path the answer is keyed by. -/
  | prompt (path : String) (p : Param) (defaultShown : Option String)
  /-- `--help` of a nested (chained) command, already rendered. -/
  | helpText (text : String)
  /-- A parameter callback aborted (`ctx.abort()`). -/
  | abort

/-- The command-line raw value of an option, from the scanner events. -/
def cmdRaw (events : List Event) (p : Param) : Option Raw :=
  let occs := (events.filter (fun e => e.dest == p.name)).map (·.occ)
  let vals := occs.filterMap fun o => match o with | .values xs => some xs | _ => none
  match p.decl with
  | .option _ (.value _ false) => vals.getLast?.map .tokens
  | .option _ (.value _ true) => if vals.isEmpty then none else some (.multi vals)
  | .option _ (.boolFlag _) =>
    (occs.filterMap fun o => match o with | .flag b => some b | _ => none).getLast?.map .bool
  | .option _ (.constFlag _ false) =>
    -- parameters sharing a name (feature switches) share their events: the
    -- last one given decides
    (occs.filterMap fun o => match o with | .const v => some v | _ => none).getLast?.map fun v => .tokens [v]
  | .option _ (.constFlag _ true) =>
    let vs := occs.filterMap fun o => match o with | .const v => some v | _ => none
    if vs.isEmpty then none else some (.multi (vs.map ([·])))
  | .option _ .count => if occs.isEmpty then none else some (.count occs.length)
  | .argument _ => none

/-- Split into chunks of `n`, dropping an incomplete tail (Python's
`batch`). -/
def batch (n : Nat) (xs : List String) : List (List String) :=
  let rec go : Nat → List String → List (List String)
    | 0, _ => []
    | f + 1, xs => if n == 0 || xs.length < n then [] else xs.take n :: go f (xs.drop n)
  go (xs.length + 1) xs

/-- The environment variable value of a parameter: the first non-empty of
its `envvar` names, then (options only) the automatic
`PREFIX_NAME` variable. -/
def nonEmpty : Option String → Option String
  | some x => if x.isEmpty then none else some x
  | none => none

def envString (get : String → Option String) (s : Settings) (p : Param) : Option String :=
  match p.envvar.findSome? (fun n => nonEmpty (get n)) with
  | some v => some v
  | none =>
    if p.isOption && p.allowFromAutoenv then
      match s.autoEnvvarPrefix with
      | some pre => nonEmpty (get (pre ++ "_" ++ upper p.name))
      | none => none
    else none

/-- The names a parameter's environment lookup reads. -/
def envNames (s : Settings) (p : Param) : List String :=
  p.envvar ++ (if p.isOption && p.allowFromAutoenv then
    match s.autoEnvvarPrefix with
    | some pre => [pre ++ "_" ++ upper p.name]
    | none => []
    else [])

/-- `value_from_envvar`: shape the environment string. -/
def envRaw (p : Param) (v : String) : Option Raw :=
  match p.decl with
  | .option _ (.constFlag c mult) =>
    let on := v == c || strToBool v == some true
    if !on then none else if mult then some (.multi [[c]]) else some (.tokens [c])
  | .option _ (.value n mult) =>
    if n == 1 && !mult then some (.tokens [v])
    else
      let parts := p.type.splitEnvvar v
      if mult then some (.multi (if n == 1 then parts.map ([·]) else batch n parts))
      else some (.tokens parts)
  | .option _ _ => some (.tokens [v])
  | .argument (.exactly 1) => some (.tokens [v])
  | .argument _ => some (.tokens (p.type.splitEnvvar v))

/-- The default as a raw value. -/
def defaultRaw (p : Param) : Option Raw :=
  match p.default with
  | none => none
  | some ds =>
    match p.shape with
    | .many n => some (.multi (if n == 1 then ds.map ([·]) else batch n ds))
    | _ => some (.tokens ds)

/-- Where an absent parameter's default comes from. -/
def Param.defaultSource (p : Param) : Source := if p.fromDefaultMap then .defaultMap else .default

/-- `consume_value` without the prompt: command line, then environment,
then the default (from `default_map` if it names the parameter). -/
def consume (cmd : Option Raw) (env : Option String) (p : Param) : Option Raw × Source :=
  match cmd with
  | some r => (some r, .commandline)
  | none =>
    match env.bind (envRaw p) with
    | some r => (some r, .environment)
    | none => (defaultRaw p, p.defaultSource)

/-- A boolean flag occurrence under the parameter type. -/
def boolValue (fs : String → Option FileInfo) : (t : ParamType) → Bool → Except String t.Carrier
  | .bool, b => .ok b
  | t, b => convert fs t (if b then "true" else "false")

/-- A count under the parameter type. -/
def countValue (fs : String → Option FileInfo) : (t : ParamType) → Nat → Except String t.Carrier
  | .intRange r, n => convertIntRange r n
  | .int, n => .ok (n : Int)
  | t, n => convert fs t (toString n)

def takesMsg (n k : Nat) : String :=
  if k == 1 then s!"Takes {n} values but 1 was given." else s!"Takes {n} values but {k} were given."

/-- Convert each occurrence of a multiple option, checking its arity. -/
def convertMany (fs : String → Option FileInfo) (t : ParamType) (n : Nat) :
    (xss : List (List String)) → Except String {yss : List (List t.Carrier) // ∀ ys ∈ yss, ys.length = n}
  | [] => .ok ⟨[], by simp⟩
  | xs :: xss =>
    if hc : t.isComposite && n == 1 then do
      let y ← convertComposite fs t xs
      let ⟨yss, hyss⟩ ← convertMany fs t n xss
      return ⟨[y] :: yss, by
        intro zs hz
        simp only [List.mem_cons] at hz
        rcases hz with rfl | hz
        · simp only [Bool.and_eq_true, beq_iff_eq] at hc; simp [hc.2]
        · exact hyss zs hz⟩
    else if h : xs.length = n then do
      let ⟨ys, hy⟩ ← convertList fs t xs
      let ⟨yss, hyss⟩ ← convertMany fs t n xss
      return ⟨ys :: yss, by
        intro zs hz
        simp only [List.mem_cons] at hz
        rcases hz with rfl | hz
        · omega
        · exact hyss zs hz⟩
    else .error (takesMsg n xs.length)

/-- `type_cast_value` into the declared shape, with the shape proof. -/
def convertShape (fs : String → Option FileInfo) (t : ParamType) :
    (s : Shape) → Raw → Except String {v : PValue t // v.Fits s}
  | .single, .tokens [x] => do let y ← convert fs t x; return ⟨.one y, trivial⟩
  | .single, .tokens xs =>
    if t.isComposite then do let y ← convertComposite fs t xs; return ⟨.one y, trivial⟩
    else .error (takesMsg 1 xs.length)
  | .single, .bool b => do let y ← boolValue fs t b; return ⟨.one y, trivial⟩
  | .single, .count n => do let y ← countValue fs t n; return ⟨.one y, trivial⟩
  | .tuple n, .tokens xs =>
    if h : xs.length = n then do
      let ⟨ys, hy⟩ ← convertList fs t xs
      return ⟨.tuple ys, by simp only [PValue.Fits]; omega⟩
    else .error (takesMsg n xs.length)
  | .star, .tokens xs => do
    let ⟨ys, _⟩ ← convertList fs t xs
    return ⟨.tuple ys, trivial⟩
  | .many n, .multi xss => do
    let ⟨yss, h⟩ ← convertMany fs t n xss
    return ⟨.many yss, h⟩
  | .many n, .tokens xs => do
    let ⟨yss, h⟩ ← convertMany fs t n (if n == 1 then xs.map ([·]) else batch n xs)
    return ⟨.many yss, h⟩
  | _, _ => .error "Value does not match the parameter's arity."

/-- The value an absent parameter takes: `False` for an optional boolean
flag, `0` for a count, an empty collection for a multi-value parameter,
otherwise nothing. -/
def absentFor (t : ParamType) : (s : Shape) → {v : PValue t // v.Fits s}
  | .many _ => ⟨.many [], by simp [PValue.Fits]⟩
  | .star => ⟨.tuple [], trivial⟩
  | .single => ⟨.none, trivial⟩
  | .tuple _ => ⟨.none, trivial⟩

def absentValue (p : Param) : {v : PValue p.type // v.Fits p.shape} := absentFor p.type p.shape

/-- Is the value missing (`value_is_missing`)? -/
def PValue.isMissing {t : ParamType} : PValue t → Bool
  | .none => true
  | .tuple [] => true
  | .many [] => true
  | _ => false

/-- The missing-value text of a type (`get_missing_message`). -/
def ParamType.missingMessage : ParamType → String
  | .choice cs cS => "Choose from:\n\t" ++ ",\n\t".intercalate (cs.map (normChoice cS))
  | _ => ""

/-- The `MissingParameter` error of a parameter. -/
def Param.missingError (p : Param) : UsageError :=
  ⟨.missingParameter (if p.isOption then "option" else "argument") p.errorHint p.type.missingMessage, none⟩

/-- The `BadParameter` error of a parameter. -/
def Param.badParameter (p : Param) (msg : String) : UsageError :=
  ⟨.badParameter (some p.errorHint) msg, none⟩

/-- Does an option prompt, given where its value came from? -/
def Param.wantsPrompt (p : Param) (raw : Option Raw) (src : Source) : Bool :=
  match p.prompt with
  | some ps => (raw.isNone || src == .default || src == .defaultMap) && (p.required || ps.required)
  | none => false

/-- The value of an absent boolean flag or count, before conversion. -/
def implicitRaw (p : Param) : Option Raw :=
  match p.decl with
  | .option _ (.boolFlag _) => if p.required then none else some (.bool false)
  | .option _ .count => some (.count 0)
  | _ => none

/-- The value of an entry with its refinements erased. A `multiple`
option with `nargs=1` gives a flat list, as click's tuple does. -/
def Entry.toValue (e : Entry) : Value :=
  let er := e.param.type.erase
  match e.value with
  | .none => .none
  | .one x => er x
  | .tuple xs => .list (xs.map er)
  | .many xss =>
    if e.param.shape == .many 1 then .list (xss.flatMap (fun xs => xs.map er))
    else .list (xss.map fun xs => .list (xs.map er))

/-- The raw value and its source, with a prompt answer replacing a missing
or default value when the option prompts. -/
def pickSource (p : Param) (answer : Option String) (cmd : Option Raw) (env : Option String) :
    Option Raw × Source :=
  let rs := consume cmd env p
  if p.wantsPrompt rs.1 rs.2 then
    match answer with
    | some a => (some (.tokens [a]), Source.prompt)
    | none => rs
  else rs

/-- Convert, check required, stop on an active eager parameter. -/
def finishParam (fs : String → Option FileInfo) (p : Param) (raw : Option Raw) (src : Source) :
    Except Stop Entry :=
  let conv : Except String {v : PValue p.type // v.Fits p.shape} :=
    match raw.or (implicitRaw p) with
    | none => .ok (absentValue p)
    | some r => convertShape fs p.type p.shape r
  match conv with
  | .error msg => .error (.error (p.badParameter msg))
  | .ok ⟨v, h⟩ =>
    if p.required && v.isMissing then .error (.error p.missingError)
    else
      match p.eager, p.type, v with
      | some e, .bool, .one true => .error (.eager e)
      | _, _, _ => .ok ⟨p, v, src, h, none⟩

/-- Pick the source, prompt if needed, convert, check required, stop on an
active eager parameter (everything but the callback). -/
def resolveCore (fs : String → Option FileInfo) (answer : Option String)
    (cmd : Option Raw) (env : Option String) (p : Param) : Except Stop Entry :=
  let rs := pickSource p answer cmd env
  if p.wantsPrompt rs.1 rs.2 && rs.2 != .prompt then
    .error (.prompt "" p (match rs.1.or (implicitRaw p) with
      | some (.tokens [d]) => some d
      | some (.bool b) => some (if b then "true" else "false")
      | _ => none))
  else finishParam fs p rs.1 rs.2

/-- click's parameter `callback`, run right after the conversion: it may
reject the value (`BadParameter`) or replace it. -/
def applyCallback (p : Param) (e : Entry) : Except Stop Entry :=
  match p.checks with
  | none => .ok e
  | some f =>
    match f e.toValue with
    | .error (.bad msg) => .error (.error (p.badParameter msg))
    | .error .abort => .error .abort
    | .ok v => .ok { e with override := some v }

/-- Process one parameter (`handle_parse_result`): resolve and convert it,
then run its callback. -/
def resolveParam (fs : String → Option FileInfo) (answer : Option String)
    (cmd : Option Raw) (env : Option String) (p : Param) : Except Stop Entry :=
  match resolveCore fs answer cmd env p with
  | .ok e => applyCallback p e
  | .error st => .error st

end LeanClick
