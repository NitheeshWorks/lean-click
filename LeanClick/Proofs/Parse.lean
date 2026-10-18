import LeanClick.Proofs.Values

/-!
# Proofs: the command parser

`parse_deterministic`, `required_missing_error`,
`undeclared_option_rejected`, `bool_flag_pairs`.
-/

namespace LeanClick

open Spec Text

/-! ## Determinism -/

theorem findSome?_congr {α β : Type} (f g : α → Option β) :
    ∀ l : List α, (∀ x ∈ l, f x = g x) → l.findSome? f = l.findSome? g
  | [], _ => rfl
  | a :: l, h => by
    simp only [List.findSome?_cons]
    rw [h a (by simp), findSome?_congr f g l (fun x hx => h x (by simp [hx]))]

theorem envString_congr (g₁ g₂ : String → Option String) (s : Settings) (p : Param)
    (h : ∀ n ∈ envNames s p, g₁ n = g₂ n) : envString g₁ s p = envString g₂ s p := by
  unfold envString
  have hv : p.envvar.findSome? (fun n => nonEmpty (g₁ n)) =
      p.envvar.findSome? (fun n => nonEmpty (g₂ n)) := by
    apply findSome?_congr
    intro x hx
    rw [h x (by simp [envNames, hx])]
  rw [hv]
  split
  · rfl
  · split
    · split
      · rename_i pre hpre
        rw [h (pre ++ "_" ++ upper p.name) (by simp [envNames, *])]
      · rfl
    · rfl

theorem processAll_congr (order ps : List Param) (r₁ r₂ : Param → Except Stop Entry)
    (hmem : ∀ p ∈ order, p ∈ ps) (h : ∀ p ∈ ps, r₁ p = r₂ p) :
    processAll order ps r₁ = processAll order ps r₂ := by
  unfold processAll
  rw [mapM_congr_mem r₁ r₂ order (fun p hp => h p (hmem p hp)), mapM_congr_mem r₁ r₂ ps h]

theorem parseWith_congr (cmd : Command σ) (s : Settings) (path : String) (argv : List String)
    (fs : String → Option FileInfo) (answers : List ((String × String) × String))
    (envOf₁ envOf₂ : Param → Option String) (h : ∀ p ∈ cmd.allParams s, envOf₁ p = envOf₂ p) :
    cmd.parseWith s path argv fs answers envOf₁ = cmd.parseWith s path argv fs answers envOf₂ := by
  unfold Command.parseWith
  split
  · rfl
  · split
    · rfl
    · rename_i sc _
      dsimp only
      split
      · rfl
      · rename_i araws leftover _
        rw [processAll_congr _ _ (resolveWith fs answers path (cmd.ctxInfo s path) sc araws envOf₁)
          (resolveWith fs answers path (cmd.ctxInfo s path) sc araws envOf₂)
          (fun p hp => (mem_processingOrder _ _ p).1 hp)
          (fun p hp => by simp only [resolveWith, h p hp])]

theorem parse_deterministic : ParseDeterministic (σ := σ) := by
  intro cmd s path argv e₁ e₂ hvars hfs hans
  unfold Command.parseArgs
  rw [hfs, hans]
  apply parseWith_congr
  intro p hp
  exact envString_congr _ _ s p (hvars p hp)

/-! ## Required parameters -/

theorem parseWith_ok_entries (cmd : Command σ) (s : Settings) (path : String) (argv : List String)
    (fs : String → Option FileInfo) (answers : List ((String × String) × String))
    (envOf : Param → Option String) (r : ParsedArgs)
    (h : cmd.parseWith s path argv fs answers envOf = .ok r) :
    ∃ sc araws, scan (cmd.scanCfg s) argv = .ok sc ∧ (cmd.allParams s).mapM (resolveWith fs answers path (cmd.ctxInfo s path) sc araws envOf)
      = .ok r.entries := by
  unfold Command.parseWith at h
  split at h
  · cases h
  · split at h
    · cases h
    · rename_i sc hsc
      dsimp only at h
      split at h
      · cases h
      · rename_i araws leftover _
        split at h
        · cases h
        · rename_i entries hpa
          split at h
          · cases h
          · cases h
            refine ⟨sc, araws, hsc, ?_⟩
            unfold processAll at hpa
            split at hpa
            · cases hpa
            · exact hpa

theorem resolveParam_ok_entry (fs : String → Option FileInfo) (ans : Option String)
    (cmd : Option Raw) (env : Option String) (p : Param) (e : Entry)
    (h : resolveParam fs ans cmd env p = .ok e) :
    e.param = p ∧ (e.param.required = true → e.value.isMissing = false) := by
  unfold resolveParam at h
  dsimp only at h
  split at h
  · cases h
  · have := finishParam_ok fs p _ _ e h
    exact ⟨this.1, this.2.2⟩

theorem resolveWith_ok_entry (fs : String → Option FileInfo) (answers : List ((String × String) × String))
    (path : String) (info : CtxInfo) (sc : Scanned) (araws : List (String × Option Raw))
    (envOf : Param → Option String) (p : Param) (e : Entry)
    (h : resolveWith fs answers path info sc araws envOf p = .ok e) :
    e.param = p ∧ (e.param.required = true → e.value.isMissing = false) ∧
    resolveParam fs (answers.lookup (path, p.name)) (rawOf sc araws p) (envOf p) p = .ok e := by
  unfold resolveWith at h
  split at h
  · cases h
  · rename_i e' he
    cases h
    exact ⟨(resolveParam_ok_entry _ _ _ _ _ _ he).1, (resolveParam_ok_entry _ _ _ _ _ _ he).2, he⟩

theorem parse_ok_required_present (cmd : Command σ) (s : Settings) (path : String) (argv : List String)
    (env : Env) (r : ParsedArgs) (h : cmd.parseArgs s path argv env = .ok r) :
    ∀ e ∈ r.entries, e.param.required = true → e.value.isMissing = false := by
  obtain ⟨sc, araws, _, hm⟩ := parseWith_ok_entries cmd s path argv _ _ _ r h
  exact mapM_except_ok_forall _ (fun e : Entry => e.param.required = true → e.value.isMissing = false)
    (fun p e he => (resolveWith_ok_entry _ _ _ _ _ _ _ p e he).2.1) _ _ hm

theorem absentValue_missing (p : Param) : (absentValue p).val.isMissing = true := by
  unfold absentValue
  cases p.shape <;> rfl

theorem resolveParam_missing (fs : String → Option FileInfo) (ans : Option String) (env : Option String)
    (p : Param) (hreq : p.required = true) (hpr : p.prompt = none) (hd : p.default = none)
    (hc : p.decl.isCount = false) (he : env.bind (envRaw p) = none) :
    resolveParam fs ans none env p = .error (.error p.missingError) := by
  have himp : implicitRaw p = none := by
    unfold implicitRaw
    split
    · simp [hreq]
    · simp_all [Decl.isCount]
    · rfl
  have hdef : defaultRaw p = none := by simp [defaultRaw, hd]
  unfold resolveParam
  simp only [pickSource_noPrompt p ans none env hpr, consume_default env p he, hdef,
    wantsPrompt_none p _ _ hpr, Bool.false_and, ite_false, Bool.false_eq_true]
  unfold finishParam
  simp only [Option.or, himp]
  have hm := absentValue_missing p
  generalize absentValue p = av at hm
  obtain ⟨v, hv⟩ := av
  simp only at hm
  simp [hreq, hm]

theorem required_missing_error : RequiredMissingError (σ := σ) :=
  ⟨resolveParam_missing, parse_ok_required_present⟩

/-! ## Undeclared options -/

theorem splitEq_noEq (tok : String) (h : '=' ∉ tok.toList) : splitEq tok = (tok, none) := by
  unfold splitEq
  have : tok.toList.idxOf? '=' = none := List.idxOf?_eq_none_iff.2 h
  simp [this]

theorem lookup_none (tbl : List Spelling) (o : String) (h : ∀ sp ∈ tbl, sp.opt ≠ o) :
    lookup tbl o = none := by
  unfold lookup
  rw [List.find?_eq_none]
  intro x hx
  have := h x (List.mem_reverse.1 hx)
  simpa using this

theorem lookupLong_none (tbl : List Spelling) (o : String) (h : ∀ sp ∈ tbl, sp.opt ≠ o) :
    lookupLong tbl o = none :=
  lookup_none _ o (fun sp hsp => h sp (List.mem_filter.1 hsp).1)

theorem looksLikeOption_of_dashDash (tok : String) (h : startsDashDash tok = true) :
    looksLikeOption tok = true := by
  unfold startsDashDash at h
  unfold looksLikeOption
  split at h
  · rename_i rest heq; rw [heq]; rfl
  · cases h

theorem processOpt_undeclared (cfg : ScanCfg) (tok : String) (rest : List String)
    (hig : cfg.ignoreUnknown = false) (hdd : startsDashDash tok = true) (heq : '=' ∉ tok.toList)
    (hnot : ∀ sp ∈ cfg.tbl, sp.opt ≠ tok) :
    processOpt cfg tok rest = .error ⟨.noSuchOption tok (closeMatches tok (longOpts cfg.tbl)), none⟩ := by
  unfold processOpt
  rw [splitEq_noEq tok heq]
  dsimp only
  rw [lookupLong_none cfg.tbl tok hnot]
  simp [hdd, hig]

theorem scanLoop_positional (cfg : ScanCfg) (hi : cfg.interspersed = true) :
    ∀ (pre rest : List String) (evs : List Event) (largs : List String),
    (∀ t ∈ pre, looksLikeOption t = false ∧ t ≠ "--") →
    scanLoop cfg (pre ++ rest) evs largs = scanLoop cfg rest evs (largs ++ pre)
  | [], rest, evs, largs, _ => by simp
  | t :: pre, rest, evs, largs, h => by
    have ht := h t (by simp)
    rw [List.cons_append, scanLoop]
    have hne : (t == "--") = false := by simpa using ht.2
    simp only [hne, ht.1, hi, Bool.false_eq_true, ite_false, ite_true]
    rw [scanLoop_positional cfg hi pre rest evs (largs ++ [t]) (fun x hx => h x (by simp [hx]))]
    simp

theorem scanLoop_undeclared (cfg : ScanCfg) (tok : String) (post : List String) (evs : List Event)
    (largs : List String) (hig : cfg.ignoreUnknown = false) (hdd : startsDashDash tok = true)
    (hne : tok ≠ "--") (heq : '=' ∉ tok.toList) (hnot : ∀ sp ∈ cfg.tbl, sp.opt ≠ tok) :
    scanLoop cfg (tok :: post) evs largs =
      .error ⟨.noSuchOption tok (closeMatches tok (longOpts cfg.tbl)), none⟩ := by
  rw [scanLoop]
  have h1 : (tok == "--") = false := by simpa using hne
  simp only [h1, looksLikeOption_of_dashDash tok hdd, Bool.false_eq_true, ite_false, ite_true]
  rw [processOpt_undeclared cfg tok post hig hdd heq hnot]

theorem undeclared_option_rejected : UndeclaredOptionRejected (σ := σ) := by
  intro cmd s path env pre post tok hig hdd hne heq hnot hpre hint
  have hscan : scan (cmd.scanCfg s) (pre ++ tok :: post) =
      .error ⟨.noSuchOption tok (closeMatches tok (longOpts (cmd.scanCfg s).tbl)), none⟩ := by
    unfold scan
    rcases hint with rfl | hint
    · exact scanLoop_undeclared (cmd.scanCfg s) tok post [] [] hig hdd hne heq hnot
    · rw [scanLoop_positional (cmd.scanCfg s) hint pre (tok :: post) [] [] hpre]
      exact scanLoop_undeclared (cmd.scanCfg s) tok post [] _ hig hdd hne heq hnot
  refine ⟨closeMatches tok (longOpts (cmd.scanCfg s).tbl), ?_⟩
  unfold Command.parseArgs Command.parseWith
  have hnil : (pre ++ tok :: post).isEmpty = false := by simp
  simp only [hnil, Bool.false_and, Bool.false_eq_true, ite_false]
  rw [hscan]

/-! ## Boolean flag pairs -/

theorem lookupLong_unique (tbl : List Spelling) (o : String) (x : Spelling)
    (hf : tbl.filter (fun sp => sp.opt == o) = [x]) (hs : isShortSpelling o = false) :
    lookupLong tbl o = some x := by
  have hxo : x.opt = o := by
    have : x ∈ tbl.filter (fun sp => sp.opt == o) := by rw [hf]; simp
    simpa using (List.mem_filter.1 this).2
  unfold lookupLong lookup
  rw [← List.head?_filter, ← List.filter_reverse, List.filter_filter]
  have : tbl.filter (fun sp => (sp.opt == o) && !isShortSpelling sp.opt) = [x] := by
    rw [← hf]
    apply List.filter_congr
    intro sp _
    by_cases h : sp.opt = o
    · simp [h, hs]
    · simp [h]
  rw [List.filter_reverse, this]
  simp

theorem scan_single_flag (cfg : ScanCfg) (o d : String) (b : Bool)
    (hl : lookupLong cfg.tbl o = some ⟨o, d, .flag b⟩) (hlo : looksLikeOption o = true)
    (hne : o ≠ "--") (heq : '=' ∉ o.toList) :
    scan cfg [o] = .ok ⟨[⟨d, .flag b⟩], [], []⟩ := by
  unfold scan
  rw [scanLoop]
  have h1 : (o == "--") = false := by simpa using hne
  simp only [h1, hlo, Bool.false_eq_true, ite_false, ite_true]
  have hp : processOpt cfg o [] = .ok ([⟨d, .flag b⟩], 0, []) := by
    unfold processOpt
    rw [splitEq_noEq o heq]
    dsimp only
    rw [hl]
    simp [Action.occ]
  rw [hp]
  simp only [List.drop_zero, List.nil_append, List.append_nil]
  rw [scanLoop]

theorem wantsPrompt_cmdline (p : Param) (r : Raw) : p.wantsPrompt (some r) .commandline = false := by
  unfold Param.wantsPrompt
  split
  · simp
  · rfl

@[simp] theorem PValue.isMissing_one {t : ParamType} (x : t.Carrier) : (PValue.one x).isMissing = false := rfl

theorem pickSource_cmdline (p : Param) (ans : Option String) (r : Raw) (env : Option String) :
    pickSource p ans (some r) env = (some r, .commandline) := by
  simp [pickSource, consume, wantsPrompt_cmdline]

theorem bool_flag_pairs : BoolFlagPairs (σ := σ) := by
  intro cmd s path env p opts sec o b r hp hdecl hmem hf hs hlo hne heq hea hparse
  obtain ⟨sc, araws, hsc, hm⟩ := parseWith_ok_entries cmd s path [o] _ _ _ r hparse
  have hl := lookupLong_unique _ o _ hf hs
  have hsc' := scan_single_flag (cmd.scanCfg s) o p.name b hl hlo hne heq
  rw [hsc] at hsc'
  cases hsc'
  obtain ⟨e, he, hres⟩ := mapM_except_ok_mem _ _ _ hm p hp
  obtain ⟨hep, _, hrp⟩ := resolveWith_ok_entry _ _ _ _ _ _ _ p e hres
  refine ⟨e, he, hep, ?_⟩
  have hopt : p.isOption = true := by simp [Param.isOption, hdecl]
  have hraw : rawOf ⟨[⟨p.name, .flag b⟩], [], []⟩ araws p = some (.bool b) := by
    simp [rawOf, hopt, cmdRaw, hdecl]
  rw [hraw] at hrp
  obtain ⟨name, decl, type, required, default, envvar, help, metavar, hidden, showDefault,
    showEnvvar, showChoices, prompt, eager, exposeValue, allowFromAutoenv, h_bool, h_count⟩ := p
  simp only at hdecl hea
  subst hdecl hea
  have ht : type = .bool := h_bool rfl
  subst ht
  unfold resolveParam at hrp
  simp only [pickSource_cmdline, wantsPrompt_cmdline, Bool.false_and, Bool.false_eq_true,
    ite_false] at hrp
  unfold finishParam at hrp
  simp only [Param.shape, Option.or] at hrp
  rw [show convertShape env.fs ParamType.bool Shape.single (Raw.bool b) = .ok ⟨.one b, trivial⟩ from rfl] at hrp
  dsimp only at hrp
  split at hrp
  · cases hrp
  · cases hrp; exact ⟨rfl, rfl⟩

end LeanClick
