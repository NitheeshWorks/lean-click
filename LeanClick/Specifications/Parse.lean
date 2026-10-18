import LeanClick.RepoImpl.Parse

/-!
# Specifications

The headline properties, each one idea, stated about click's public
operations: `Command.parse_args` (`Command.parseArgs`), `Group.get_command`
(`Command.findSub`), `Parameter.handle_parse_result` (`resolveParam`), the
help page (`Command.formatHelp`), type conversion (`convert`) and the exit
code. `Proofs/Verified.lean` bundles a proof of each.
-/

namespace LeanClick

/-- `a` occurs in `b`. -/
def IsInfix (a b : String) : Prop := ∃ x y, b = x ++ a ++ y

/-- A token the scanner treats as a long option with no attached value:
`--name`, not `--`, no `=`. -/
def IsBareLongOpt (tok : String) : Prop :=
  Text.startsDashDash tok = true ∧ tok ≠ "--" ∧ '=' ∉ tok.toList

/-- No two rows of the command's parser table share a spelling. Holds for
every well-formed click command (click warns on duplicates); decidable, so
`by decide` checks it for a concrete command. -/
def Command.Unambiguous {σ : Type} (cmd : Command σ) (s : Settings) : Prop :=
  ((table (cmd.allParams s)).map (·.opt)).Nodup

namespace Spec

variable {σ : Type}

/-- Parsing is a function of the arguments and of the environment variables
the command declares: environments that agree on those names (and on the
filesystem view and prompt answers) parse alike. -/
def ParseEnvLocal : Prop :=
  ∀ (cmd : Command σ) (s : Settings) (path : String) (argv : List String) (e₁ e₂ : Env),
    (∀ n ∈ cmd.envNames s, e₁.get n = e₂.get n) → e₁.fs = e₂.fs → e₁.answers = e₂.answers →
    cmd.parseArgs s path argv e₁ = cmd.parseArgs s path argv e₂

/-- Every spelling a visible option accepts appears in the help page. -/
def HelpListsEveryParam : Prop :=
  ∀ (cmd : Command σ) (s : Settings) (path : String) (p : Param),
    p ∈ cmd.allParams s → p.isOption = true → p.hidden = false →
    ∀ o ∈ p.allOpts, IsInfix o (cmd.formatHelp s path)

/-- A long option the table does not declare fails the parse with
`No such option`, wherever the scanner reaches it. -/
def UndeclaredOptionRejected : Prop :=
  ∀ (cmd : Command σ) (s : Settings) (path : String) (env : Env) (pre post : List String) (tok : String),
    cmd.spec.ignoreUnknownOptions = false → IsBareLongOpt tok →
    tok ∉ (table (cmd.allParams s)).map (·.opt) →
    (∀ t ∈ pre, (cmd.scanCfg s).isOptTok t = false ∧ t ≠ "--") →
    (pre = [] ∨ cmd.interspersed = true) →
    cmd.parseArgs s path (pre ++ tok :: post) env =
      .error (.error ⟨.noSuchOption tok (Text.closeMatches tok (longOpts (cmd.scanCfg s).tbl)),
        some (cmd.ctxInfo s path)⟩)

/-- A required parameter with no value from any source fails with click's
`Missing option` / `Missing argument` error. -/
def RequiredMissingError : Prop :=
  ∀ (fs : String → Option FileInfo) (ans : Option String) (env : Option String) (p : Param),
    p.required = true → p.prompt = none → p.default = none → p.decl.isCount = false →
    env.bind (envRaw p) = none →
    resolveParam fs ans none env p = .error (.error p.missingError)

/-- A successful parse leaves no required parameter missing. -/
def RequiredPresent : Prop :=
  ∀ (cmd : Command σ) (s : Settings) (path : String) (argv : List String) (env : Env) (r : ParsedArgs),
    cmd.parseArgs s path argv env = .ok r →
    ∀ e ∈ r.entries, e.param.required = true → e.value.isMissing = false

/-- A parsed `nargs = n` value holds exactly `n` values. -/
def NargsArity : Prop :=
  ∀ (e : Entry) (n : Nat) (xs : List e.param.type.Carrier),
    e.param.shape = .tuple n → e.value = .tuple xs → xs.length = n

/-- A choice converts exactly when some declared choice matches it, and the
result is the first such choice. -/
def ChoiceInSet : Prop :=
  ∀ (fs : String → Option FileInfo) (cs : List String) (cS : Bool) (s v : String),
    (∃ c, convert fs (.choice cs cS) s = .ok c ∧ c.val = v) ↔
    cs.find? (fun c => normChoice cS c == normChoice cS s) = some v

/-- For a boolean flag pair, a command line of its spellings stores whether
the last one is a primary (`--x`) spelling: `--x` gives true, `--no-x` gives
false, and the last occurrence wins. -/
def BoolFlagPairs : Prop :=
  ∀ (cmd : Command σ) (s : Settings) (path : String) (env : Env) (p : Param)
    (opts sec argv : List String) (r : ParsedArgs) (h : argv ≠ []),
    cmd.Unambiguous s → p ∈ cmd.allParams s → p.decl = .option opts (.boolFlag sec) → p.eager = none →
    (∀ t ∈ argv, t ∈ opts ++ sec ∧ IsBareLongOpt t) →
    cmd.parseArgs s path argv env = .ok r →
    ∃ e ∈ r.entries, e.param = p ∧ e.source = .commandline ∧
      e.toValue = .bool (decide (argv.getLast h ∈ opts))

/-- The command line beats the environment variable: `--port 9000` is
taken from the command line whatever `PORT` says. -/
def CommandlineBeatsEnvvar : Prop :=
  ∀ (cmd : Command σ) (s : Settings) (path : String) (env : Env) (p : Param)
    (opts : List String) (o v : String) (r : ParsedArgs),
    cmd.Unambiguous s → p ∈ cmd.allParams s → p.decl = .option opts (.value 1 false) →
    o ∈ opts → IsBareLongOpt o →
    cmd.parseArgs s path [o, v] env = .ok r →
    ∃ e ∈ r.entries, e.param = p ∧ e.source = .commandline

/-- The environment variable beats the default: with nothing on the command
line, a set variable supplies the option. -/
def EnvvarBeatsDefault : Prop :=
  ∀ (cmd : Command σ) (s : Settings) (path : String) (env : Env) (p : Param) (r : ParsedArgs),
    p ∈ cmd.allParams s → p.isOption = true → p.prompt = none →
    ((envString env.get s p).bind (envRaw p)).isSome = true →
    cmd.parseArgs s path [] env = .ok r →
    ∃ e ∈ r.entries, e.param = p ∧ e.source = .environment

/-- The default applies when neither the command line nor the environment
gives a value. -/
def DefaultWhenAbsent : Prop :=
  ∀ (cmd : Command σ) (s : Settings) (path : String) (env : Env) (p : Param) (r : ParsedArgs),
    p ∈ cmd.allParams s → p.isOption = true → p.prompt = none →
    (envString env.get s p).bind (envRaw p) = none →
    cmd.parseArgs s path [] env = .ok r →
    ∃ e ∈ r.entries, e.param = p ∧ e.source = .default

/-- A group runs the first subcommand registered under the name. -/
def GroupDispatchFirstMatch : Prop :=
  ∀ (cmd : Command σ) (s : Settings) (path : String) (argv : List String) (env : Env)
    (lv : Level σ) (c : Command σ) (name : String) (rest : List String),
    cmd.parseLevel s path argv env = .ok lv → lv.sub = some (c, name, rest) →
    cmd.subcommands.find? (fun d => d.name == name) = some c

/-- Exit codes are bytes. -/
def ExitCodeRange : Prop := ∀ t : Termination, t.exitCode < 256

/-- The framework's own exit codes are 0, 1 or 2 (anything else comes from
`ctx.exit(n)`). -/
def FrameworkExitCodes : Prop := ∀ t : Termination, t.isFramework = true → t.exitCode ≤ 2

end Spec
end LeanClick
