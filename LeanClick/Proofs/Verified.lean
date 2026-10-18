import LeanClick.Proofs.Help
import LeanClick.Proofs.Precedence
import LeanClick.Proofs.Unambiguous
import LeanClick.Proofs.Get

/-!
# The harness: one `Verified<Layer>` structure per layer

Each `Verified<Layer>` bundles a proof term for every
`Specifications/<Layer>.lean` property it claims. A specification with no
proof is a compile error the moment it needs a field here: you cannot
construct a `Verified<Layer>` value without every proof in hand. If a stated
specification turns out false, its slot's type flips to a refutation rather
than being dropped, so this file stays an honest map of what is verified.

What is tested rather than proved (text wrapping, `difflib` close matches,
number parsing, the `Terminal` effect module) has no slot here; see
`CONTRIBUTING.md`.
-/

namespace LeanClick

open Spec

/-- The parser layer: scanning, resolution, dispatch, exit codes. -/
structure VerifiedParser (σ : Type) where
  parseEnvLocal : ParseEnvLocal (σ := σ)
  undeclaredOptionRejected : UndeclaredOptionRejected (σ := σ)
  requiredMissingError : RequiredMissingError
  requiredPresent : RequiredPresent (σ := σ)
  nargsArity : NargsArity
  tupleArity : TupleArity
  choiceInSet : ChoiceInSet
  boolFlagPairs : BoolFlagPairs (σ := σ)
  commandlineBeatsEnvvar : CommandlineBeatsEnvvar (σ := σ)
  envvarBeatsDefault : EnvvarBeatsDefault (σ := σ)
  defaultWhenAbsent : DefaultWhenAbsent (σ := σ)
  groupDispatchFirstMatch : GroupDispatchFirstMatch (σ := σ)
  getIsConverted : GetIsConverted (σ := σ)
  exitCodeRange : ExitCodeRange
  frameworkExitCodes : FrameworkExitCodes

/-- The help layer: the page is rendered from the parser's own table. -/
structure VerifiedHelp (σ : Type) where
  helpListsEveryParam : HelpListsEveryParam (σ := σ)

theorem theVerifiedParser (σ : Type) : VerifiedParser σ where
  parseEnvLocal := parse_env_local
  undeclaredOptionRejected := undeclared_option_rejected
  requiredMissingError := required_missing_error
  requiredPresent := required_present
  nargsArity := nargs_arity
  tupleArity := tuple_arity
  choiceInSet := choice_in_set
  boolFlagPairs := bool_flag_pairs
  commandlineBeatsEnvvar := commandline_beats_envvar
  envvarBeatsDefault := envvar_beats_default
  defaultWhenAbsent := default_when_absent
  groupDispatchFirstMatch := group_dispatch_first_match
  getIsConverted := fun cmd s path argv env r name e h he hn => parse_get_eq_toValue cmd s path argv env r h name e he hn
  exitCodeRange := exit_code_range
  frameworkExitCodes := framework_exit_codes

theorem theVerifiedHelp (σ : Type) : VerifiedHelp σ where
  helpListsEveryParam := help_lists_every_param

end LeanClick
