import LeanClick.RepoImpl.Parse

/-!
# Specifications

The properties the parser, the help renderer and the exit-code mapping are
proved against. Each is a `Prop`-valued definition over the reference
implementation; `Proofs/Verified.lean` bundles a proof of each.
-/

namespace LeanClick

/-- `a` occurs in `b`. -/
def IsInfix (a b : String) : Prop := ∃ x y, b = x ++ a ++ y

namespace Spec

variable {σ : Type}

/-- The parser reads the environment only through the declared variable
names: two environments that agree on those names (and on the filesystem
view and prompt answers) give the same result. -/
def ParseDeterministic : Prop :=
  ∀ (cmd : Command σ) (s : Settings) (path : String) (argv : List String) (e₁ e₂ : Env),
    (∀ p ∈ cmd.allParams s, ∀ n ∈ envNames s p, e₁.get n = e₂.get n) →
    e₁.fs = e₂.fs → e₁.answers = e₂.answers →
    cmd.parseArgs s path argv e₁ = cmd.parseArgs s path argv e₂

/-- Every visible option of the table the parser uses has its help row in
the help page, and every spelling it accepts is in that row. -/
def HelpListsEveryParam : Prop :=
  ∀ (cmd : Command σ) (s : Settings) (path : String) (p : Param),
    p ∈ cmd.allParams s → p.isOption = true → p.hidden = false →
    IsInfix p.helpSpec (cmd.formatHelp s path) ∧ ∀ o ∈ p.allOpts, IsInfix o (cmd.formatHelp s path)

/-- A long option the table does not declare is rejected with
`No such option`, wherever the scanner reaches it. -/
def UndeclaredOptionRejected : Prop :=
  ∀ (cmd : Command σ) (s : Settings) (path : String) (env : Env) (pre post : List String) (tok : String),
    cmd.spec.ignoreUnknownOptions = false →
    Text.startsDashDash tok = true → tok ≠ "--" → '=' ∉ tok.toList →
    (∀ sp ∈ table (cmd.allParams s), sp.opt ≠ tok) →
    (∀ t ∈ pre, Text.looksLikeOption t = false ∧ t ≠ "--") →
    (pre = [] ∨ cmd.interspersed = true) →
    ∃ ps, cmd.parseArgs s path (pre ++ tok :: post) env =
      .error (.error ⟨.noSuchOption tok ps, some (cmd.ctxInfo s path)⟩)

/-- A required parameter with no value from any source is reported as
missing; and a successful parse never leaves a required parameter
missing. -/
def RequiredMissingError : Prop :=
  (∀ (fs : String → Option FileInfo) (ans : Option String) (env : Option String) (p : Param),
    p.required = true → p.prompt = none → p.default = none → p.decl.isCount = false →
    env.bind (envRaw p) = none →
    resolveParam fs ans none env p = .error (.error p.missingError)) ∧
  (∀ (cmd : Command σ) (s : Settings) (path : String) (argv : List String) (env : Env) (r : ParsedArgs),
    cmd.parseArgs s path argv env = .ok r →
    ∀ e ∈ r.entries, e.param.required = true → e.value.isMissing = false)

/-- An option taking `n` values yields exactly `n` per occurrence: the
scanner takes `n` tokens, and every parsed entry of shape `tuple n` holds
`n` values. -/
def NargsArity : Prop :=
  (∀ (opt : String) (n : Nat) (att : Option String) (rest vals : List String) (k : Nat),
    takeValues opt n att rest = .ok (vals, k) → vals.length = n) ∧
  (∀ (e : Entry) (n : Nat) (xs : List e.param.type.Carrier),
    e.param.shape = .tuple n → e.value = .tuple xs → xs.length = n) ∧
  (∀ (e : Entry) (n : Nat) (xss : List (List e.param.type.Carrier)),
    e.param.shape = .many n → e.value = .many xss → ∀ xs ∈ xss, xs.length = n)

/-- A choice value is a member of the choices, and is the first choice the
input matches. -/
def ChoiceInSet : Prop :=
  (∀ (cs : List String) (cS : Bool) (x : (ParamType.choice cs cS).Carrier), x.val ∈ cs) ∧
  (∀ (fs : String → Option FileInfo) (cs : List String) (cS : Bool) (s : String) c,
    convert fs (.choice cs cS) s = .ok c →
    c.val ∈ cs ∧ normChoice cS c.val = normChoice cS s ∧
    ∃ pre post, cs = pre ++ c.val :: post ∧ ∀ d ∈ pre, normChoice cS d ≠ normChoice cS s)

/-- `--x` stores `true`, `--no-x` stores `false`, and of several the last
one wins. -/
def BoolFlagPairs : Prop :=
  ∀ (cmd : Command σ) (s : Settings) (path : String) (env : Env) (p : Param)
    (opts sec : List String) (o : String) (b : Bool) (r : ParsedArgs),
    p ∈ cmd.allParams s → p.decl = .option opts (.boolFlag sec) →
    (if b then o ∈ opts else o ∈ sec) →
    (table (cmd.allParams s)).filter (fun sp => sp.opt == o) = [⟨o, p.name, .flag b⟩] →
    isShortSpelling o = false → Text.looksLikeOption o = true → o ≠ "--" → '=' ∉ o.toList →
    p.eager = none →
    cmd.parseArgs s path [o] env = .ok r →
    ∃ e ∈ r.entries, e.param = p ∧ e.source = .commandline ∧ e.toValue = .bool b

/-- Where a value comes from: the command line if present, else the
environment variable, else the default. -/
def EnvvarFallback : Prop :=
  (∀ (r : Raw) (env : Option String) (p : Param), consume (some r) env p = (some r, .commandline)) ∧
  (∀ (r : Raw) (env : Option String) (p : Param),
    env.bind (envRaw p) = some r → consume none env p = (some r, .environment)) ∧
  (∀ (env : Option String) (p : Param),
    env.bind (envRaw p) = none → consume none env p = (defaultRaw p, .default)) ∧
  (∀ (fs : String → Option FileInfo) (ans : Option String) (cmd : Option Raw) (env : Option String)
      (p : Param) (e : Entry),
    p.prompt = none → resolveParam fs ans cmd env p = .ok e →
    e.param = p ∧ e.source = (consume cmd env p).2)

/-- A group dispatches to the first subcommand with the given name. -/
def GroupDispatchFirstMatch : Prop :=
  ∀ (cmd : Command σ) (s : Settings) (path : String) (argv : List String) (env : Env)
    (lv : Level σ) (c : Command σ) (name : String) (rest : List String),
    cmd.parseLevel s path argv env = .ok lv → lv.sub = some (c, name, rest) →
    c.name = name ∧ ∃ pre post, cmd.subcommands = pre ++ c :: post ∧ ∀ d ∈ pre, d.name ≠ name

/-- Exit codes are bytes, and the framework's own are 0, 1 or 2. -/
def ExitCodeRange : Prop :=
  ∀ t : Termination, t.exitCode < 256 ∧ (t.isFramework = true → t.exitCode ≤ 2)

end Spec
end LeanClick
