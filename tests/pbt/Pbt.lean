import LeanClick
import Plausible

/-!
# Plausible properties

Each `example` here is a randomised check of a property that is tested
rather than proved, or a restatement of a theorem as a smoke test of the
definitions it is about.
-/

namespace Pbt

open LeanClick

/-- Python's `int(str(n)) == n`. -/
example : ∀ n : Int, Text.parseInt (toString n) = some n := by plausible

/-- `ctx.exit(n)` maps to a byte. -/
example : ∀ n : Nat, (Termination.raised (.exit n)).exitCode < 256 := by plausible

/-- `pyRepr` always quotes. -/
example : ∀ s : String, (Text.pyRepr s).length ≥ 2 := by plausible

/-- Splitting on whitespace never yields an empty piece. -/
example : ∀ s : String, (Text.splitWs s).all (fun w => !w.isEmpty) = true := by plausible

/-- The scanner never loses a positional token. -/
example : ∀ ws : List Nat,
    let toks := ws.map (fun n => "w" ++ toString n)
    (scan { tbl := [] } toks).toOption.map (·.largs) = some toks := by plausible

end Pbt
