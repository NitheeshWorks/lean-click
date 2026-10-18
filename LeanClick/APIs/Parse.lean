import LeanClick.APIs.Values
import LeanClick.Proofs.Verified

/-!
# The public parser

`LeanClick.parse` is the pure parser of one command:
`List String → Env → Except Halt ParsedArgs`, with its postcondition in the
type: one entry per parameter of the table, in declaration order, every
entry of the declared shape, and no required parameter missing.
-/

namespace LeanClick

variable {σ : Type}

/-- The postcondition of a successful parse. -/
def ParsedFor (cmd : Command σ) (s : Settings) (r : ParsedArgs) : Prop :=
  r.entries.map (·.param) = cmd.allParams s ∧
  ∀ e ∈ r.entries, e.param.required = true → e.value.isMissing = false

theorem mapM_except_map {α β ε : Type} (f : α → Except ε β) (g : β → α)
    (hg : ∀ x y, f x = .ok y → g y = x) :
    ∀ (l : List α) (r : List β), l.mapM f = .ok r → r.map g = l
  | [], r, h => by rw [List.mapM_nil] at h; cases h; rfl
  | a :: l, r, h => by
    rw [List.mapM_cons] at h
    cases hfa : f a with
    | error e => simp [hfa] at h; cases h
    | ok b =>
      cases hl : l.mapM f with
      | error e => simp [hfa, hl] at h; cases h
      | ok bs =>
        simp [hfa, hl] at h
        cases h
        simp [hg a b hfa, mapM_except_map f g hg l bs hl]

theorem parseArgs_parsedFor (cmd : Command σ) (s : Settings) (path : String) (argv : List String)
    (env : Env) (r : ParsedArgs) (h : cmd.parseArgs s path argv env = .ok r) : ParsedFor cmd s r := by
  refine ⟨?_, required_present cmd s path argv env r h⟩
  obtain ⟨sc, araws, _, hm⟩ := parseWith_ok_entries cmd s path argv _ _ _ r h
  exact mapM_except_map _ _ (fun p e he => (resolveWith_ok_entry _ _ _ _ _ _ _ p e he).1) _ _ hm

/-- Parse the arguments of one command in a context with settings `s`. -/
def Command.parseChecked (cmd : Command σ) (s : Settings) (path : String) (argv : List String)
    (env : Env) : Except Stop {r : ParsedArgs // ParsedFor cmd s r} :=
  match h : cmd.parseArgs s path argv env with
  | .ok r => .ok ⟨r, parseArgs_parsedFor cmd s path argv env r h⟩
  | .error st => .error st

/-- The pure parser of a top-level command invoked as `prog`:
`argv → Env → Except Halt ParsedArgs`, with the postcondition. -/
def parse (cmd : Command σ) (argv : List String) (env : Env := {}) (prog : String := cmd.name) :
    Except Halt {r : ParsedArgs // ParsedFor cmd (cmd.settings (some { width := env.width }) prog) r} :=
  let s := cmd.settings (some { width := env.width }) prog
  match cmd.parseChecked s prog argv env with
  | .ok r => .ok r
  | .error st => .error (st.toHalt cmd s prog prog)

/-- The help page of a top-level command, as `--help` prints it. -/
def helpPage (cmd : Command σ) (prog : String := cmd.name) (width : Nat := 80) : String :=
  cmd.getHelp (cmd.settings (some { width }) prog) prog

end LeanClick
