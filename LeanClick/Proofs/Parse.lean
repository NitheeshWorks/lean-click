import LeanClick.Proofs.Values

/-!
# Proofs: the command parser

`parse_env_local`, `required_missing_error`, `required_present`,
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

theorem parse_env_local : ParseEnvLocal (σ := σ) := by
  intro cmd s path argv e₁ e₂ hvars hfs hans
  unfold Command.parseArgs
  rw [hfs, hans]
  apply parseWith_congr
  intro p hp
  exact envString_congr _ _ s p fun n hn =>
    hvars n (List.mem_flatMap.2 ⟨p, hp, hn⟩)

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
  obtain ⟨e0, o, hcore, rfl⟩ := resolveParam_ok _ _ _ _ _ _ h
  clear h
  have h := hcore
  clear hcore
  unfold resolveCore at h
  dsimp only at h
  split at h
  · cases h
  · have := finishParam_ok fs p _ _ e0 h
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
  · cases h
  · rename_i e' he
    cases h
    exact ⟨(resolveParam_ok_entry _ _ _ _ _ _ he).1, (resolveParam_ok_entry _ _ _ _ _ _ he).2, he⟩

theorem required_present (cmd : Command σ) (s : Settings) (path : String) (argv : List String)
    (env : Env) (r : ParsedArgs) (h : cmd.parseArgs s path argv env = .ok r) :
    ∀ e ∈ r.entries, e.param.required = true → e.value.isMissing = false := by
  obtain ⟨sc, araws, _, hm⟩ := parseWith_ok_entries cmd s path argv _ _ _ r h
  exact mapM_except_ok_forall _ (fun e : Entry => e.param.required = true → e.value.isMissing = false)
    (fun p e he => (resolveWith_ok_entry _ _ _ _ _ _ _ p e he).2.1) _ _ hm

theorem absentValue_missing (p : Param) : (absentValue p).val.isMissing = true := by
  unfold absentValue
  cases p.shape <;> rfl

/-- A required parameter with no value from any source fails with click's
`Missing option` / `Missing argument` error. -/
theorem required_missing_error (fs : String → Option FileInfo) (ans : Option String) (env : Option String)
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
  suffices hcore : resolveCore fs ans none env p = .error (.error p.missingError) by
    unfold resolveParam; rw [hcore]
  unfold resolveCore
  simp only [pickSource_noPrompt p ans none env hpr, consume_default env p he, hdef,
    wantsPrompt_none p _ _ hpr, Bool.false_and, ite_false, Bool.false_eq_true]
  unfold finishParam
  simp only [Option.or, himp]
  have hm := absentValue_missing p
  generalize absentValue p = av at hm
  obtain ⟨v, hv⟩ := av
  simp only at hm
  simp [hreq, hm]



/-- The entry a successful parse holds for each parameter of the table is
what `resolveParam` makes of it from the scanned command line. The shared
step of every parse-level proof. -/
theorem parseArgs_entry (cmd : Command σ) (s : Settings) (path : String) (argv : List String)
    (env : Env) (r : ParsedArgs) (h : cmd.parseArgs s path argv env = .ok r) (p : Param)
    (hp : p ∈ cmd.allParams s) :
    ∃ sc araws, scan (cmd.scanCfg s) argv = .ok sc ∧ ∃ e ∈ r.entries, e.param = p ∧
      resolveParam env.fs (env.answers.lookup (path, p.name)) (rawOf sc araws p)
        (envString env.get s p) p = .ok e := by
  obtain ⟨sc, araws, hsc, hm⟩ := parseWith_ok_entries cmd s path argv _ _ _ r h
  obtain ⟨e, he, hres⟩ := mapM_except_ok_mem _ _ _ hm p hp
  obtain ⟨hep, _, hrp⟩ := resolveWith_ok_entry _ _ _ _ _ _ _ p e hres
  exact ⟨sc, araws, hsc, e, he, hep, hrp⟩

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

/-- The configuration of a command always knows `-` and `--`. -/
theorem scanCfg_prefixes (cmd : Command σ) (s : Settings) :
    ['-'] ∈ (cmd.scanCfg s).prefixes ∧ ['-', '-'] ∈ (cmd.scanCfg s).prefixes := by
  simp [Command.scanCfg, optPrefixes]

theorem isOptTok_of_dashDash (cfg : ScanCfg) (tok : String) (h : startsDashDash tok = true)
    (hp : ['-'] ∈ cfg.prefixes) : cfg.isOptTok tok = true := by
  unfold startsDashDash at h
  unfold ScanCfg.isOptTok
  split at h
  · rename_i rest heq
    have hl : tok.length = tok.toList.length := String.length_toList.symm
    rw [hl, heq]
    simp [hp]
  · cases h

theorem longPrefixed_of_dashDash (cfg : ScanCfg) (tok : String) (h : startsDashDash tok = true)
    (hp : ['-', '-'] ∈ cfg.prefixes) : cfg.longPrefixed tok = true := by
  unfold startsDashDash at h
  unfold ScanCfg.longPrefixed
  split at h
  · rename_i rest heq
    rw [heq]
    simp [hp]
  · cases h

theorem processOpt_undeclared (cfg : ScanCfg) (tok : String) (rest : List String)
    (hig : cfg.ignoreUnknown = false) (hdd : startsDashDash tok = true) (heq : '=' ∉ tok.toList)
    (hnot : ∀ sp ∈ cfg.tbl, sp.opt ≠ tok) (hp : ['-', '-'] ∈ cfg.prefixes) :
    processOpt cfg tok rest = .error ⟨.noSuchOption tok (closeMatches tok (longOpts cfg.tbl)), none⟩ := by
  unfold processOpt
  rw [splitEq_noEq tok heq]
  dsimp only
  rw [lookupLong_none cfg.tbl tok hnot]
  simp [longPrefixed_of_dashDash cfg tok hdd hp, hig]

theorem scanLoop_positional (cfg : ScanCfg) (hi : cfg.interspersed = true) :
    ∀ (pre rest : List String) (evs : List Event) (largs : List String),
    (∀ t ∈ pre, cfg.isOptTok t = false ∧ t ≠ "--") →
    scanLoop cfg (pre ++ rest) evs largs = scanLoop cfg rest evs (pre.reverse ++ largs)
  | [], rest, evs, largs, _ => by simp
  | t :: pre, rest, evs, largs, h => by
    have ht := h t (by simp)
    rw [List.cons_append, scanLoop]
    have hne : (t == "--") = false := by simpa using ht.2
    simp only [hne, ht.1, hi, Bool.false_eq_true, ite_false, ite_true]
    rw [scanLoop_positional cfg hi pre rest evs (t :: largs) (fun x hx => h x (by simp [hx]))]
    simp

theorem scanLoop_undeclared (cfg : ScanCfg) (tok : String) (post : List String) (evs : List Event)
    (largs : List String) (hig : cfg.ignoreUnknown = false) (hdd : startsDashDash tok = true)
    (hne : tok ≠ "--") (heq : '=' ∉ tok.toList) (hnot : ∀ sp ∈ cfg.tbl, sp.opt ≠ tok)
    (hp : ['-'] ∈ cfg.prefixes ∧ ['-', '-'] ∈ cfg.prefixes) :
    scanLoop cfg (tok :: post) evs largs =
      .error ⟨.noSuchOption tok (closeMatches tok (longOpts cfg.tbl)), none⟩ := by
  rw [scanLoop]
  have h1 : (tok == "--") = false := by simpa using hne
  simp only [h1, isOptTok_of_dashDash cfg tok hdd hp.1, Bool.false_eq_true, ite_false, ite_true]
  rw [processOpt_undeclared cfg tok post hig hdd heq hnot hp.2]

theorem undeclared_option_rejected : UndeclaredOptionRejected (σ := σ) := by
  intro cmd s path env pre post tok hig ⟨hdd, hne, heq⟩ hnot' hpre hint
  have hnot : ∀ sp ∈ table (cmd.allParams s), sp.opt ≠ tok := fun sp hsp h =>
    hnot' (List.mem_map.2 ⟨sp, hsp, h⟩)
  have hp := scanCfg_prefixes cmd s
  have hscan : scan (cmd.scanCfg s) (pre ++ tok :: post) =
      .error ⟨.noSuchOption tok (closeMatches tok (longOpts (cmd.scanCfg s).tbl)), none⟩ := by
    unfold scan
    rcases hint with rfl | hint
    · exact scanLoop_undeclared (cmd.scanCfg s) tok post [] [] hig hdd hne heq hnot hp
    · rw [scanLoop_positional (cmd.scanCfg s) hint pre (tok :: post) [] [] hpre]
      exact scanLoop_undeclared (cmd.scanCfg s) tok post [] _ hig hdd hne heq hnot hp
  unfold Command.parseArgs Command.parseWith
  have hnil : (pre ++ tok :: post).isEmpty = false := by simp
  simp only [hnil, Bool.false_and, Bool.false_eq_true, ite_false]
  rw [hscan]
  rfl

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

theorem wantsPrompt_cmdline (p : Param) (r : Raw) : p.wantsPrompt (some r) .commandline = false := by
  unfold Param.wantsPrompt
  split
  · simp
  · rfl

@[simp] theorem PValue.isMissing_one {t : ParamType} (x : t.Carrier) : (PValue.one x).isMissing = false := rfl

theorem pickSource_cmdline (p : Param) (ans : Option String) (r : Raw) (env : Option String) :
    pickSource p ans (some r) env = (some r, .commandline) := by
  simp [pickSource, consume, wantsPrompt_cmdline]

theorem filter_opt_singleton : ∀ (tbl : List Spelling), (tbl.map (·.opt)).Nodup →
    ∀ x ∈ tbl, tbl.filter (fun sp => sp.opt == x.opt) = [x]
  | [], _, x, hx => by simp at hx
  | a :: l, h, x, hx => by
    rw [List.map_cons, List.nodup_cons] at h
    obtain ⟨hna, hl⟩ := h
    simp only [List.mem_cons] at hx
    rcases hx with rfl | hx
    · rw [List.filter_cons_of_pos (by simp), List.filter_eq_nil_iff.2]
      intro y hy
      simp only [beq_iff_eq]
      intro heq
      exact hna (heq ▸ List.mem_map_of_mem hy)
    · have hne : a.opt ≠ x.opt := fun heq => hna (heq ▸ List.mem_map_of_mem hx)
      rw [List.filter_cons_of_neg (by simpa using hne)]
      exact filter_opt_singleton l hl x hx

theorem not_short_of_dashDash (t : String) (h : startsDashDash t = true) : isShortSpelling t = false := by
  unfold startsDashDash at h
  unfold isShortSpelling splitOpt
  split at h
  · rename_i rest heq
    rw [heq]
    simp only [splitOptList]
    have : (String.ofList ['-', '-']).length = 2 := by decide
    simp [this]
  · cases h

/-- In an unambiguous table, a long spelling looks up its own row. -/
theorem lookupLong_row (cmd : Command σ) (s : Settings) (hu : cmd.Unambiguous s) (sp : Spelling)
    (hsp : sp ∈ table (cmd.allParams s)) (hs : isShortSpelling sp.opt = false) :
    lookupLong (cmd.scanCfg s).tbl sp.opt = some sp :=
  lookupLong_unique _ _ _ (filter_opt_singleton _ hu sp hsp) hs

theorem boolFlag_row (ps : List Param) (p : Param) (hp : p ∈ ps) (opts sec : List String)
    (hd : p.decl = .option opts (.boolFlag sec)) (t : String) (ht : t ∈ opts ++ sec) :
    (⟨t, p.name, .flag (decide (t ∈ opts))⟩ : Spelling) ∈ table ps := by
  unfold table
  refine List.mem_flatMap.2 ⟨p, hp, ?_⟩
  unfold Param.spellings
  rw [hd]
  by_cases h : t ∈ opts
  · simp [h]
  · have : t ∈ sec := by simpa [h] using ht
    simp [h, this]

theorem scanLoop_flags (cfg : ScanCfg) (d : String) (b : String → Bool) :
    ∀ (toks : List String) (evs : List Event) (largs : List String),
    (∀ t ∈ toks, lookupLong cfg.tbl t = some ⟨t, d, .flag (b t)⟩ ∧ cfg.isOptTok t = true ∧
      t ≠ "--" ∧ '=' ∉ t.toList) →
    scanLoop cfg toks evs largs = .ok ⟨evs.reverse ++ toks.map (fun t => ⟨d, .flag (b t)⟩), largs.reverse, []⟩
  | [], evs, largs, _ => by rw [scanLoop]; simp
  | t :: ts, evs, largs, h => by
    obtain ⟨hl, hlo, hne, heq⟩ := h t (by simp)
    rw [scanLoop]
    have h1 : (t == "--") = false := by simpa using hne
    have hp : processOpt cfg t ts = .ok ([⟨d, .flag (b t)⟩], 0, []) := by
      unfold processOpt
      rw [splitEq_noEq t heq]
      dsimp only
      rw [hl]
      simp [Action.occ]
    simp only [h1, hlo, Bool.false_eq_true, ite_false, ite_true, hp, List.drop_zero]
    rw [scanLoop_flags cfg d b ts _ _ (fun x hx => h x (by simp [hx]))]
    simp

theorem cmdRaw_flags (p : Param) (opts sec : List String) (hd : p.decl = .option opts (.boolFlag sec))
    (b : String → Bool) (toks : List String) :
    cmdRaw (toks.map (fun t => ⟨p.name, .flag (b t)⟩)) p = (toks.map b).getLast?.map .bool := by
  simp only [cmdRaw, hd]
  congr 2
  induction toks with
  | nil => rfl
  | cons t ts ih => simp [ih]

/-- A boolean flag occurrence on the command line becomes that boolean,
sourced from the command line. -/
theorem resolveParam_boolFlag (fs : String → Option FileInfo) (ans : Option String) (env : Option String)
    (p : Param) (opts sec : List String) (hd : p.decl = .option opts (.boolFlag sec)) (hea : p.eager = none)
    (b : Bool) (e : Entry) (h : resolveParam fs ans (some (.bool b)) env p = .ok e) :
    e.param = p ∧ e.source = .commandline ∧ e.toValue = .bool b := by
  obtain ⟨e0, o, hcore, rfl⟩ := resolveParam_ok _ _ _ _ _ _ h
  clear h
  have h := hcore
  clear hcore
  obtain ⟨name, decl, type, required, default, envvar, help, metavar, hidden, showDefault,
    showEnvvar, showChoices, prompt, eager, exposeValue, allowFromAutoenv, callback, shellComplete, fromDefaultMap, h_bool, h_count⟩ := p
  simp only at hd hea
  subst hd hea
  have ht : type = .bool := h_bool rfl
  subst ht
  unfold resolveCore at h
  simp only [pickSource_cmdline, wantsPrompt_cmdline, Bool.false_and, Bool.false_eq_true,
    ite_false] at h
  unfold finishParam at h
  simp only [Param.shape, Option.or] at h
  rw [show convertShape fs ParamType.bool Shape.single (Raw.bool b) = .ok ⟨.one b, trivial⟩ from rfl] at h
  dsimp only at h
  split at h
  · cases h
  · cases h; exact ⟨rfl, rfl, rfl⟩

theorem bool_flag_pairs : BoolFlagPairs (σ := σ) := by
  intro cmd s path env p opts sec argv r hne hu hp hd hea hargv hparse
  obtain ⟨sc, araws, hsc, e, he, hep, hrp⟩ := parseArgs_entry cmd s path argv env r hparse p hp
  let b := fun t => decide (t ∈ opts)
  have hcfg := scanCfg_prefixes cmd s
  have hscan : scan (cmd.scanCfg s) argv = .ok ⟨argv.map (fun t => ⟨p.name, .flag (b t)⟩), [], []⟩ := by
    unfold scan
    rw [scanLoop_flags _ p.name b argv [] [] ?_]
    · simp
    · intro t ht
      obtain ⟨hmem, hdd, hne', heq⟩ := hargv t ht
      have hrow := boolFlag_row _ p hp opts sec hd t hmem
      exact ⟨lookupLong_row cmd s hu _ hrow (not_short_of_dashDash t hdd),
        isOptTok_of_dashDash _ t hdd hcfg.1, hne', heq⟩
  rw [hsc] at hscan
  cases hscan
  have hopt : p.isOption = true := by simp [Param.isOption, hd]
  have hraw : rawOf ⟨argv.map (fun t => ⟨p.name, .flag (b t)⟩), [], []⟩ araws p =
      some (.bool (b (argv.getLast hne))) := by
    simp only [rawOf, hopt, ite_true]
    rw [cmdRaw_flags p opts sec hd b argv, List.getLast?_map, List.getLast?_eq_some_getLast hne]
    rfl
  rw [hraw] at hrp
  obtain ⟨_, hsrc, hval⟩ := resolveParam_boolFlag _ _ _ p opts sec hd hea _ e hrp
  exact ⟨e, he, hep, hsrc, hval⟩

/-! ## Environment fallback -/

theorem cmdRaw_nil (p : Param) : cmdRaw [] p = none := by
  unfold cmdRaw
  split <;> simp

end LeanClick
