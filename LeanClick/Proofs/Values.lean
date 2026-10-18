import LeanClick.Proofs.Lemmas

/-!
# Proofs: sources, shapes, choices, dispatch, exit codes
-/

namespace LeanClick

open Spec

/-! ## Exit codes -/

theorem exit_code_range : ExitCodeRange := by
  intro t
  rcases t with _ | h | e
  · simp [Termination.exitCode]
  · cases h <;> simp [Termination.exitCode]
  · cases e <;> simp [Termination.exitCode] <;> omega

theorem framework_exit_codes : FrameworkExitCodes := by
  intro t ht
  rcases t with _ | h | e
  · simp [Termination.exitCode]
  · cases h <;> simp [Termination.exitCode]
  · cases e <;> simp_all [Termination.exitCode, Termination.isFramework]

/-- A usage error, however it surfaces, exits with 2. -/
theorem usage_error_exit_2 (e : UsageError) :
    (Termination.halted (.error e)).exitCode = 2 ∧ (Termination.raised (.usage e)).exitCode = 2 :=
  ⟨rfl, rfl⟩

/-! ## Precedence: command line > environment > default -/

theorem consume_commandline (r : Raw) (env : Option String) (p : Param) :
    consume (some r) env p = (some r, .commandline) := rfl

theorem consume_environment (r : Raw) (env : Option String) (p : Param)
    (h : env.bind (envRaw p) = some r) : consume none env p = (some r, .environment) := by
  simp [consume, h]

theorem consume_default (env : Option String) (p : Param)
    (h : env.bind (envRaw p) = none) : consume none env p = (defaultRaw p, .default) := by
  simp [consume, h]

theorem wantsPrompt_none (p : Param) (raw : Option Raw) (src : Source) (h : p.prompt = none) :
    p.wantsPrompt raw src = false := by
  simp [Param.wantsPrompt, h]

theorem finishParam_ok (fs : String → Option FileInfo) (p : Param) (raw : Option Raw) (src : Source)
    (e : Entry) (h : finishParam fs p raw src = .ok e) :
    e.param = p ∧ e.source = src ∧ (e.param.required = true → e.value.isMissing = false) := by
  unfold finishParam at h
  dsimp only at h
  split at h
  · cases h
  · rename_i v hfit _
    split at h
    · cases h
    · rename_i hreq
      have hm : p.required = true → v.isMissing = false := by
        intro hr; simp [hr] at hreq; exact hreq
      split at h
      · cases h
      · cases h; exact ⟨rfl, rfl, hm⟩

theorem pickSource_noPrompt (p : Param) (ans : Option String) (cmd : Option Raw) (env : Option String)
    (hp : p.prompt = none) : pickSource p ans cmd env = consume cmd env p := by
  simp [pickSource, wantsPrompt_none p _ _ hp]

/-- The entry `resolveParam` builds answers `p`, with the source `consume`
chose (when the option does not prompt). -/
theorem resolveParam_source (fs : String → Option FileInfo) (ans : Option String) (cmd : Option Raw)
    (env : Option String) (p : Param) (e : Entry) (hp : p.prompt = none)
    (h : resolveParam fs ans cmd env p = .ok e) :
    e.param = p ∧ e.source = (consume cmd env p).2 := by
  unfold resolveParam at h
  simp only [pickSource_noPrompt p ans cmd env hp, wantsPrompt_none p _ _ hp, Bool.false_and,
    Bool.false_eq_true, ite_false] at h
  have := finishParam_ok fs p _ _ e h
  exact ⟨this.1, this.2.1⟩

/-! ## Shapes -/

theorem takeValues_length (opt : String) (n : Nat) (att : Option String) (rest vals : List String)
    (k : Nat) (h : takeValues opt n att rest = .ok (vals, k)) : vals.length = n := by
  unfold takeValues at h
  split at h
  · split at h
    · cases h
    · split at h
      · cases h
      · rename_i hn hlt
        cases h
        simp only [List.length_cons, List.length_take]
        simp at hn
        omega
  · split at h
    · cases h
    · rename_i hlt
      cases h
      simp only [List.length_take]
      omega

/-- A parsed `nargs = n` value holds exactly `n` values. -/
theorem nargs_arity (e : Entry) (n : Nat) (xs : List e.param.type.Carrier)
    (hs : e.param.shape = .tuple n) (hv : e.value = .tuple xs) : xs.length = n := by
  have h := e.h_fits
  rw [hv, hs] at h
  exact h

theorem entry_many_length (e : Entry) (n : Nat) (xss : List (List e.param.type.Carrier))
    (hs : e.param.shape = .many n) (hv : e.value = .many xss) : ∀ xs ∈ xss, xs.length = n := by
  have h := e.h_fits
  rw [hv, hs] at h
  exact h


/-! ## Choices -/

theorem choice_mem (cs : List String) (cS : Bool) (x : (ParamType.choice cs cS).Carrier) : x.val ∈ cs :=
  x.property

/-- A choice converts exactly when a declared choice matches, to the first match. -/
theorem choice_in_set (fs : String → Option FileInfo) (cs : List String) (cS : Bool) (s v : String) :
    (∃ c, convert fs (.choice cs cS) s = .ok c ∧ c.val = v) ↔
    cs.find? (fun c => normChoice cS c == normChoice cS s) = some v := by
  simp only [convert, matchChoice]
  constructor
  · rintro ⟨c, hc, rfl⟩
    split at hc
    · rename_i c' hm
      split at hm
      · rename_i c'' hf; cases hm; cases hc; exact hf
      · cases hm
    · cases hc
  · intro hf
    have hmem := List.mem_of_find?_eq_some hf
    refine ⟨⟨v, hmem⟩, ?_, rfl⟩
    split
    · rename_i c' hm
      split at hm
      · rename_i c'' hf'
        rw [hf] at hf'; cases hf'; cases hm; rfl
      · cases hm
    · rename_i hm
      split at hm
      · cases hm
      · rename_i hf'; rw [hf] at hf'; cases hf'


/-! ## Group dispatch -/

theorem parseLevel_sub (cmd : Command σ) (s : Settings) (path : String) (argv : List String) (env : Env)
    (lv : Level σ) (c : Command σ) (name : String) (rest : List String)
    (h : cmd.parseLevel s path argv env = .ok lv) (hs : lv.sub = some (c, name, rest)) :
    cmd.findSub name = some c := by
  unfold Command.parseLevel at h
  split at h
  · cases h
  · split at h
    · cases h; simp at hs
    · split at h
      · split at h
        · cases h; simp at hs
        · cases h
      · split at h
        · cases h; simp at hs; obtain ⟨rfl, rfl, rfl⟩ := hs; assumption
        · split at h
          · cases h
          · split at h <;> cases h

theorem group_dispatch_first_match : GroupDispatchFirstMatch (σ := σ) :=
  fun cmd s path argv env lv c name rest h hs => parseLevel_sub cmd s path argv env lv c name rest h hs

end LeanClick
