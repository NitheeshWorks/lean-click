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
    (h : env.bind (envRaw p) = none) : consume none env p = (defaultRaw p, p.defaultSource) := by
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

/-- A callback's replacement does not change the converted value. -/
@[simp] theorem Entry.toValue_with_override (e : Entry) (o : Option Value) :
    ({ e with override := o } : Entry).toValue = e.toValue := by
  obtain ⟨p, v, s, h, o'⟩ := e
  simp only [Entry.toValue]
  cases v <;> rfl

/-- A resolved parameter is what `resolveCore` made of it, with at most
its callback's replacement value added. -/
theorem resolveParam_ok (fs : String → Option FileInfo) (ans : Option String) (cmd : Option Raw)
    (env : Option String) (p : Param) (e : Entry) (h : resolveParam fs ans cmd env p = .ok e) :
    ∃ e0 o, resolveCore fs ans cmd env p = .ok e0 ∧ e = { e0 with override := o } := by
  unfold resolveParam at h
  cases hc : resolveCore fs ans cmd env p with
  | error st => rw [hc] at h; cases h
  | ok e0 =>
    rw [hc] at h
    dsimp only at h
    unfold applyCallback at h
    split at h
    · simp only [Except.ok.injEq] at h; subst h; exact ⟨e0, e0.override, rfl, rfl⟩
    · split at h
      · cases h
      · cases h
      · simp only [Except.ok.injEq] at h; subst h; exact ⟨e0, _, rfl, rfl⟩

theorem resolveParam_source_core (fs : String → Option FileInfo) (ans : Option String) (cmd : Option Raw)
    (env : Option String) (p : Param) (e : Entry) (h : resolveCore fs ans cmd env p = .ok e) :
    e.param = p := by
  unfold resolveCore at h
  dsimp only at h
  split at h
  · cases h
  · exact (finishParam_ok fs p _ _ e h).1

/-- The entry `resolveParam` builds answers `p`, with the source `consume`
chose (when the option does not prompt). -/
theorem resolveParam_source (fs : String → Option FileInfo) (ans : Option String) (cmd : Option Raw)
    (env : Option String) (p : Param) (e : Entry) (hp : p.prompt = none)
    (h : resolveParam fs ans cmd env p = .ok e) :
    e.param = p ∧ e.source = (consume cmd env p).2 := by
  obtain ⟨e0, o, hcore, rfl⟩ := resolveParam_ok _ _ _ _ _ _ h
  clear h
  have h := hcore
  clear hcore
  unfold resolveCore at h
  simp only [pickSource_noPrompt p ans cmd env hp, wantsPrompt_none p _ _ hp, Bool.false_and,
    Bool.false_eq_true, ite_false] at h
  have := finishParam_ok fs p _ _ e0 h
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


/-! ## Tuples -/

theorem components_length : ∀ (t : ParamType) (x : t.Carrier), (t.components x).length = t.arity
  | .tupleNil, _ => rfl
  | .tupleCons h t, (x, y) => by
    simp only [ParamType.components, List.length_cons, ParamType.arity, components_length t y]
  | .string, _ | .int, _ | .float, _ | .bool, _ | .choice _ _, _ | .path _, _ | .file _, _
  | .intRange _, _ | .floatRange _, _ | .uuid, _ | .dateTime _, _ => rfl

/-- A `Tuple` value converts from exactly `arity` tokens and holds one value
per position. -/
theorem tuple_arity : TupleArity := by
  intro fs t xs y h
  unfold convertComposite at h
  split at h
  · rename_i hl
    exact ⟨hl, components_length t y⟩
  · cases h

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

theorem resolveSub_ok (cmd : Command σ) (s : Settings) (path name : String) (rest : List String)
    (c : Command σ) (h : cmd.resolveSub s path name rest = .ok c) : cmd.findSub name = some c := by
  unfold Command.resolveSub at h
  dsimp only at h
  split at h
  · rename_i c' hc; cases h; exact hc
  · split at h
    · rename_i hb; simp at hb
    · split at h
      · cases h
      · split at h <;> cases h

theorem parseLevel_sub (cmd : Command σ) (s : Settings) (path : String) (argv : List String) (env : Env)
    (lv : Level σ) (c : Command σ) (name : String) (rest : List String)
    (h : cmd.parseLevel s path argv env = .ok lv) (hs : lv.sub = some (c, name, rest)) :
    cmd.findSub name = some c := by
  unfold Command.parseLevel Command.parseLevelWith at h
  split at h
  · cases h
  · dsimp only at h
    split at h
    · cases h; simp at hs
    · split at h
      · split at h
        · cases h; simp at hs
        · cases h
      · split at h
        · split at h
          · cases h
          · cases h; simp at hs
        · split at h
          · cases h
          · rename_i name0 rest0 _ c' hc
            have hf := resolveSub_ok cmd s path _ _ _ hc
            simp only [hf, Option.isSome_some, ite_true] at h
            cases h
            simp at hs
            obtain ⟨rfl, rfl, rfl⟩ := hs
            exact hf

theorem group_dispatch_first_match : GroupDispatchFirstMatch (σ := σ) :=
  fun cmd s path argv env lv c name rest h hs => parseLevel_sub cmd s path argv env lv c name rest h hs

end LeanClick
