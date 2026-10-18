import LeanClick.APIs
import LeanClick.Terminal

/-!
# LeanClick

A clean-room port of click to Lean 4: a command-line parser whose help text is rendered from the same table the parser is proved against.

Layering, bottom to top (each layer imports only the ones below it):

* `DataTypes/`      structures and inductives; invariants are fields
* `RepoImpl/`       the executable definitions; total, structural recursion
* `Specifications/` the properties, stated in `Prop`
* `Proofs/`         the theorems; `Proofs/Verified.lean` bundles them
* `APIs/`           what users call: builders (`Param.opt`, `Command.group`),
                    accessors, and `LeanClick.parse`, which returns
                    `{r : ParsedArgs // ParsedFor cmd s r}`
* `Terminal.lean`   the one effectful module: environment, filesystem view,
                    prompts, output, exit codes (`Terminal.main`)
-/
