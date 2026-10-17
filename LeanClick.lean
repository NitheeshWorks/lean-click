import LeanClick.APIs
import LeanClick.Proofs.Verified

/-!
# LeanClick

A clean-room port of click to Lean 4: a command-line parser whose help text is rendered from the same table the parser is proved against.

Layering, bottom to top (each layer imports only the ones below it):

* `DataTypes/`      structures and inductives; invariants are fields
* `RepoImpl/`       the executable definitions; total, structural recursion
* `Specifications/` the properties, stated in `Prop`
* `Proofs/`         the theorems; `Proofs/Verified.lean` bundles them
* `APIs/`           what users call; returns refinements `{r // Post r}`
-/
