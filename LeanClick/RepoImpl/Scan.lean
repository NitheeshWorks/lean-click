import LeanClick.RepoImpl.Convert

/-!
# The scanner

A port of click's `parser.py` (itself derived from `optparse`): walks the
argument list once, matching option spellings against the table built from
the command's parameters, and collects

* the option *events* in invocation order (`--port 9000` is a `values`
  event for `port`, `--no-reload` a `flag false` event for `reload`),
* the positional tokens, and
* the unprocessed remainder (after `--`, or after the first positional when
  interspersed arguments are off, as in a group).

Then `unpack` distributes the positionals over the declared arguments by
their `nargs`, `nargs=-1` taking whatever is left in the middle.

An option spelling that is not in the table is an error (`No such option`),
never a token to skip; `undeclared_option_rejected` is proved against this
module.
-/

namespace LeanClick

open Text

/-- What a matched spelling does. -/
inductive Action where
  | takes (n : Nat)
  | flag (b : Bool)
  | const
  | count
  deriving DecidableEq, Repr, Inhabited

/-- One row of the parser table: a spelling, the parameter it belongs to,
and its action. -/
structure Spelling where
  opt : String
  dest : String
  action : Action
  deriving DecidableEq, Repr, Inhabited

/-- The rows a parameter contributes. Arguments contribute none. -/
def Param.spellings (p : Param) : List Spelling :=
  match p.decl with
  | .option opts (.value n _) => opts.map fun o => ⟨o, p.name, .takes n⟩
  | .option opts (.boolFlag sec) =>
    opts.map (fun o => ⟨o, p.name, .flag true⟩) ++ sec.map (fun o => ⟨o, p.name, .flag false⟩)
  | .option opts (.constFlag _ _) => opts.map fun o => ⟨o, p.name, .const⟩
  | .option opts .count => opts.map fun o => ⟨o, p.name, .count⟩
  | .argument _ => []

/-- Every spelling an option answers to (primary and secondary). -/
def Param.allOpts (p : Param) : List String :=
  match p.decl with
  | .option opts (.boolFlag sec) => opts ++ sec
  | .option opts _ => opts
  | .argument _ => []

def Param.isOption (p : Param) : Bool :=
  match p.decl with
  | .option _ _ => true
  | .argument _ => false

def Param.isArgument (p : Param) : Bool := !p.isOption

/-- The parser table of a parameter list. -/
def table (ps : List Param) : List Spelling := ps.flatMap Param.spellings

/-- click classifies `-x` as short and everything else (`--x`, `-xy`) as
long. -/
def isShortSpelling (o : String) : Bool :=
  let (pre, rest) := splitOpt o
  pre.length == 1 && rest.length == 1

/-- Look a spelling up; the last declaration wins, as with click's dict. -/
def lookup (tbl : List Spelling) (o : String) : Option Spelling :=
  tbl.reverse.find? (fun s => s.opt == o)

def lookupLong (tbl : List Spelling) (o : String) : Option Spelling :=
  lookup (tbl.filter (fun s => !isShortSpelling s.opt)) o

def lookupShort (tbl : List Spelling) (o : String) : Option Spelling :=
  lookup (tbl.filter (fun s => isShortSpelling s.opt)) o

/-- One parser event. -/
inductive Occ where
  | values (xs : List String)
  | flag (b : Bool)
  | const
  | count
  deriving DecidableEq, Repr, Inhabited

structure Event where
  dest : String
  occ : Occ
  deriving DecidableEq, Repr, Inhabited

/-- Scanner configuration, from the command and its context. -/
structure ScanCfg where
  tbl : List Spelling
  interspersed : Bool := true
  ignoreUnknown : Bool := false
  deriving Inhabited

/-- What the scanner produces. -/
structure Scanned where
  events : List Event
  largs : List String
  rargs : List String
  deriving DecidableEq, Repr, Inhabited

def requiresMsg (opt : String) (n : Nat) : String :=
  if n == 1 then s!"Option {pyRepr opt} requires an argument."
  else s!"Option {pyRepr opt} requires {n} arguments."

/-- `_get_value_from_state`: take `n` values, the attached one (`--x=1`,
`-x1`) first. Returns the values and how many tokens of `rest` were used. -/
def takeValues (opt : String) (n : Nat) (attached : Option String) (rest : List String) :
    Except UsageError (List String × Nat) :=
  match attached with
  | some v =>
    if n == 0 then .error ⟨.badOptionUsage opt s!"Option {pyRepr opt} does not take a value.", none⟩
    else if rest.length < n - 1 then .error ⟨.badOptionUsage opt (requiresMsg opt n), none⟩
    else .ok (v :: rest.take (n - 1), n - 1)
  | none =>
    if rest.length < n then .error ⟨.badOptionUsage opt (requiresMsg opt n), none⟩
    else .ok (rest.take n, n)

/-- The event a non-value action produces. -/
def Action.occ : Action → Occ
  | .takes _ => .values []
  | .flag b => .flag b
  | .const => .const
  | .count => .count

/-- `_match_short_opt` over the characters after the prefix. Returns the
events, the tokens of `rest` consumed, and the unknown characters (only with
`ignore_unknown_options`). -/
def matchShort (cfg : ScanCfg) (pfx : Char) :
    List Char → List String → List Event → List Char →
    Except UsageError (List Event × Nat × List Char)
  | [], _, evs, unk => .ok (evs, 0, unk)
  | ch :: cs, rest, evs, unk =>
    let opt := String.ofList [pfx, ch]
    match lookupShort cfg.tbl opt with
    | none =>
      if cfg.ignoreUnknown then matchShort cfg pfx cs rest evs (unk ++ [ch])
      else .error ⟨.noSuchOption opt [], none⟩
    | some sp =>
      match sp.action with
      | .takes n => do
        let attached := if cs.isEmpty then none else some (String.ofList cs)
        let (vals, k) ← takeValues opt n attached rest
        return (evs ++ [⟨sp.dest, .values vals⟩], k, unk)
      | a => matchShort cfg pfx cs rest (evs ++ [⟨sp.dest, a.occ⟩]) unk

/-- The long spellings, for "Did you mean" hints. -/
def longOpts (tbl : List Spelling) : List String :=
  (tbl.filter (fun s => !isShortSpelling s.opt)).map (·.opt)

/-- `_process_opts` for one option-looking token: the new events, how many
following tokens it consumed, and tokens to pass through as positionals. -/
def processOpt (cfg : ScanCfg) (tok : String) (rest : List String) :
    Except UsageError (List Event × Nat × List String) :=
  let (longOpt, explicit) := splitEq tok
  match lookupLong cfg.tbl longOpt with
  | some sp =>
    match sp.action with
    | .takes n => do
      let (vals, k) ← takeValues longOpt n explicit rest
      return ([⟨sp.dest, .values vals⟩], k, [])
    | a =>
      if explicit.isSome then
        .error ⟨.badOptionUsage longOpt s!"Option {pyRepr longOpt} does not take a value.", none⟩
      else .ok ([⟨sp.dest, a.occ⟩], 0, [])
  | none =>
    if !startsDashDash tok then
      match tok.toList with
      | p :: cs => do
        let (evs, k, unk) ← matchShort cfg p cs rest [] []
        return (evs, k, if unk.isEmpty then [] else [String.ofList (p :: unk)])
      | [] => .ok ([], 0, [])
    else if cfg.ignoreUnknown then .ok ([], 0, [tok])
    else .error ⟨.noSuchOption longOpt (closeMatches longOpt (longOpts cfg.tbl)), none⟩

/-- `_process_args_for_options`: the main scanning loop. -/
def scanLoop (cfg : ScanCfg) : List String → List Event → List String → Except UsageError Scanned
  | [], evs, largs => .ok ⟨evs, largs, []⟩
  | tok :: rest, evs, largs =>
    if tok == "--" then .ok ⟨evs, largs, rest⟩
    else if looksLikeOption tok then
      match processOpt cfg tok rest with
      | .error e => .error e
      | .ok (newEvs, k, pass) => scanLoop cfg (rest.drop k) (evs ++ newEvs) (largs ++ pass)
    else if cfg.interspersed then scanLoop cfg rest evs (largs ++ [tok])
    else .ok ⟨evs, largs, tok :: rest⟩
termination_by l => l.length
decreasing_by
  all_goals simp only [List.length_drop, List.length_cons]
  all_goals omega

def scan (cfg : ScanCfg) (argv : List String) : Except UsageError Scanned :=
  scanLoop cfg argv [] []

/-! ## Distributing positionals over arguments -/

/-- What the positional tokens give one argument: a value or a hole for
each slot, or the whole remainder for `nargs=-1`. -/
inductive ArgRaw where
  | single (v : Option String)
  | tuple (vs : List (Option String))
  | star (vs : List String)
  deriving DecidableEq, Repr, Inhabited

/-- Take `n` slots from the front, with holes when short. -/
def takeFront (n : Nat) (args : List String) : List (Option String) × List String :=
  ((List.range n).map (fun i => args[i]?), args.drop n)

/-- Take `n` slots from the back (holes at the front when short). -/
def takeBack (n : Nat) (args : List String) : List (Option String) × List String :=
  let (vs, rest) := takeFront n args.reverse
  (vs.reverse, rest.reverse)

def slotRaw : ArgNargs → List (Option String) → ArgRaw
  | .exactly 1, vs => .single (vs.headD none)
  | _, vs => .tuple vs

def unpackFront : List ArgNargs → List String → List ArgRaw × List String
  | [], args => ([], args)
  | n :: ns, args =>
    let k := match n with | .exactly k => k | .star => 0
    let (vs, rest) := takeFront k args
    let (rs, rest') := unpackFront ns rest
    (slotRaw n vs :: rs, rest')

/-- `post` processed from the right, each slot taking from the back. -/
def unpackBack : List ArgNargs → List String → List ArgRaw × List String
  | [], args => ([], args)
  | ns, args =>
    let rev := ns.reverse
    let rec go : List ArgNargs → List String → List ArgRaw × List String
      | [], args => ([], args)
      | n :: ns, args =>
        let k := match n with | .exactly k => k | .star => 0
        let (vs, rest) := takeBack k args
        let (rs, rest') := go ns rest
        (slotRaw n vs :: rs, rest')
    let (rs, rest) := go rev args
    (rs.reverse, rest)

/-- `_unpack_args`: front arguments from the left, the star argument (if
any) gets the middle, arguments after it from the right. -/
def unpack (specs : List ArgNargs) (args : List String) : List ArgRaw × List String :=
  match specs.idxOf? .star with
  | none => unpackFront specs args
  | some i =>
    let pre := specs.take i
    let post := specs.drop (i + 1)
    let (rsPre, rest) := unpackFront pre args
    let (rsPost, mid) := unpackBack post rest
    (rsPre ++ [.star mid] ++ rsPost, [])

end LeanClick
