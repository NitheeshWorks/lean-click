import LeanClick.Proofs.Unambiguous
import LeanClick.APIs.Values

/-!
# Proofs: what the handler receives

`ParsedArgs.get` is the converted value (`Entry.toValue`, the value the
precedence and arity theorems talk about) when the parameter has no
callback, and the callback's result when it has one. So the theorems about
entries carry over to what a command's callback reads.
-/

namespace LeanClick

theorem finishParam_override (fs : String → Option FileInfo) (p : Param) (raw : Option Raw) (src : Source)
    (e : Entry) (h : finishParam fs p raw src = .ok e) : e.override = none := by
  unfold finishParam at h
  dsimp only at h
  split at h
  · cases h
  · split at h
    · cases h
    · split at h
      · cases h
      · cases h; rfl

theorem resolveCore_override (fs : String → Option FileInfo) (ans : Option String) (cmd : Option Raw)
    (env : Option String) (p : Param) (e : Entry) (h : resolveCore fs ans cmd env p = .ok e) :
    e.override = none := by
  unfold resolveCore at h
  dsimp only at h
  split at h
  · cases h
  · exact finishParam_override _ _ _ _ _ h

/-- The override of a resolved entry is its callback's result, or nothing
without a callback. -/
def Entry.CallbackResult (e : Entry) : Prop :=
  match e.param.callback with
  | none => e.override = none
  | some f => ∃ v, f e.toValue = .ok v ∧ e.override = some v

theorem resolveParam_callbackResult (fs : String → Option FileInfo) (ans : Option String)
    (cmd : Option Raw) (env : Option String) (p : Param) (e : Entry)
    (h : resolveParam fs ans cmd env p = .ok e) : e.CallbackResult := by
  unfold resolveParam at h
  cases hc : resolveCore fs ans cmd env p with
  | error st => rw [hc] at h; cases h
  | ok e0 =>
    rw [hc] at h
    dsimp only at h
    have hp := (resolveParam_source_core fs ans cmd env p e0 hc)
    have ho := resolveCore_override _ _ _ _ _ _ hc
    unfold applyCallback at h
    unfold Entry.CallbackResult
    split at h
    · rename_i hcb
      simp only [Except.ok.injEq] at h; subst h
      rw [hp, hcb]; exact ho
    · rename_i f hcb
      split at h
      · cases h
      · cases h
      · rename_i v hv
        simp only [Except.ok.injEq] at h; subst h
        simp only [hp, hcb]
        exact ⟨v, by simpa [Entry.toValue_with_override] using hv, rfl⟩

/-- Every entry of a successful parse holds its callback's result. -/
theorem parse_callbackResult {σ : Type} (cmd : Command σ) (s : Settings) (path : String)
    (argv : List String) (env : Env) (r : ParsedArgs) (h : cmd.parseArgs s path argv env = .ok r) :
    ∀ e ∈ r.entries, e.CallbackResult := by
  obtain ⟨sc, araws, _, hm⟩ := parseWith_ok_entries cmd s path argv _ _ _ r h
  exact mapM_except_ok_forall _ Entry.CallbackResult
    (fun p e he => resolveParam_callbackResult _ _ _ _ p e (resolveWith_ok_entry _ _ _ _ _ _ _ p e he).2.2) _ _ hm

/-- What a handler reads for a parameter: the converted value when it has
no callback, the callback's result when it has one. -/
theorem get_entry (a : ParsedArgs) (name : String) (e : Entry) (he : a.entry? name = some e)
    (hcb : e.CallbackResult) :
    (e.param.callback = none → a.get name = e.toValue) ∧
    (∀ f, e.param.callback = some f → ∃ v, f e.toValue = .ok v ∧ a.get name = v) := by
  unfold Entry.CallbackResult at hcb
  have hg : a.get name = e.override.getD e.toValue := by simp [ParsedArgs.get, he]
  constructor
  · intro hn
    rw [hn] at hcb
    rw [hg, hcb]; rfl
  · intro f hf
    rw [hf] at hcb
    obtain ⟨v, hv, ho⟩ := hcb
    exact ⟨v, hv, by rw [hg, ho]; rfl⟩

/-- For a successful parse, `get` is the converted value of a parameter
without a callback. -/
theorem parse_get_eq_toValue {σ : Type} (cmd : Command σ) (s : Settings) (path : String)
    (argv : List String) (env : Env) (r : ParsedArgs) (h : cmd.parseArgs s path argv env = .ok r)
    (name : String) (e : Entry) (he : r.entry? name = some e) (hn : e.param.callback = none) :
    r.get name = e.toValue :=
  (get_entry r name e he (parse_callbackResult cmd s path argv env r h e
    (List.mem_of_find?_eq_some he))).1 hn

end LeanClick
