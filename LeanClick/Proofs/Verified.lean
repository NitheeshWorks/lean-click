import LeanClick.Proofs.Help

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
  parseDeterministic : ParseDeterministic (σ := σ)
  undeclaredOptionRejected : UndeclaredOptionRejected (σ := σ)
  requiredMissingError : RequiredMissingError (σ := σ)
  nargsArity : NargsArity
  choiceInSet : ChoiceInSet
  boolFlagPairs : BoolFlagPairs (σ := σ)
  envvarFallback : EnvvarFallback
  groupDispatchFirstMatch : GroupDispatchFirstMatch (σ := σ)
  exitCodeRange : ExitCodeRange

/-- The help layer: the page is rendered from the parser's own table. -/
structure VerifiedHelp (σ : Type) where
  helpListsEveryParam : HelpListsEveryParam (σ := σ)

theorem theVerifiedParser (σ : Type) : VerifiedParser σ where
  parseDeterministic := parse_deterministic
  undeclaredOptionRejected := undeclared_option_rejected
  requiredMissingError := required_missing_error
  nargsArity := nargs_arity
  choiceInSet := choice_in_set
  boolFlagPairs := bool_flag_pairs
  envvarFallback := envvar_fallback
  groupDispatchFirstMatch := group_dispatch_first_match
  exitCodeRange := exit_code_range

theorem theVerifiedHelp (σ : Type) : VerifiedHelp σ where
  helpListsEveryParam := help_lists_every_param

end LeanClick
