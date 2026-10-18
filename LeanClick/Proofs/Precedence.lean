import LeanClick.Proofs.Parse

/-!
# Proofs: command line > environment variable > default, for any argv

The scanner only produces an event for a parameter from a token that
`Mentions` one of its spellings (`scanLoop_events`); so a parameter absent
from `argv` has no command-line value, and its source is the environment or
the default. A parameter given as `--o v` / `--o=v` occurrences gets the
last occurrence's value (`scanLoop_values`), as `bool_flag_pairs` does for
flags.
-/

namespace LeanClick

open Spec Text

/-! ## Where events come from -/

theorem lookup_some (tbl : List Spelling) (o : String) (sp : Spelling) (h : lookup tbl o = some sp) :
    sp ∈ tbl ∧ sp.opt = o := by
  unfold lookup at h
  exact ⟨List.mem_reverse.1 (List.mem_of_find?_eq_some h), by simpa using List.find?_some h⟩

theorem lookupLong_some (tbl : List Spelling) (o : String) (sp : Spelling) (h : lookupLong tbl o = some sp) :
    sp ∈ tbl ∧ sp.opt = o := by
  obtain ⟨hm, ho⟩ := lookup_some _ _ _ h
  exact ⟨(List.mem_filter.1 hm).1, ho⟩

theorem lookupShort_some (tbl : List Spelling) (o : String) (sp : Spelling) (h : lookupShort tbl o = some sp) :
    sp ∈ tbl ∧ sp.opt = o := by
  obtain ⟨hm, ho⟩ := lookup_some _ _ _ h
  exact ⟨(List.mem_filter.1 hm).1, ho⟩

theorem matchShort_events (cfg : ScanCfg) (pfx : Char) :
    ∀ (cs : List Char) (rest : List String) (evs : List Event) (unk : List Char)
      (evs' : List Event) (k : Nat) (unk' : List Char),
    matchShort cfg pfx cs rest evs unk = .ok (evs', k, unk') →
    ∀ ev ∈ evs', ev ∈ evs ∨ ∃ sp ∈ cfg.tbl, sp.dest = ev.dest ∧ ∃ ch ∈ cs, sp.opt = String.ofList [pfx, ch]
  | [], _, evs, unk, evs', k, unk', h, ev, hev => by
    simp only [matchShort] at h
    cases h
    exact Or.inl hev
  | ch :: cs, rest, evs, unk, evs', k, unk', h, ev, hev => by
    unfold matchShort at h
    dsimp only at h
    cases hl : lookupShort cfg.tbl (String.ofList [pfx, ch]) with
    | none =>
      rw [hl] at h
      dsimp only at h
      cases hig : cfg.ignoreUnknown
      · simp [hig] at h
      · simp only [hig, ite_true] at h
        rcases matchShort_events cfg pfx cs rest evs _ evs' k unk' h ev hev with h1 | ⟨sp, hsp, hd, c, hc, ho⟩
        · exact Or.inl h1
        · exact Or.inr ⟨sp, hsp, hd, c, List.mem_cons_of_mem _ hc, ho⟩
    | some sp =>
      rw [hl] at h
      dsimp only at h
      obtain ⟨hsp, ho⟩ := lookupShort_some _ _ _ hl
      have here : ∃ sp' ∈ cfg.tbl, sp'.dest = sp.dest ∧ ∃ c ∈ ch :: cs, sp'.opt = String.ofList [pfx, c] :=
        ⟨sp, hsp, rfl, ch, by simp, ho⟩
      cases ha : sp.action with
      | takes n =>
        rw [ha] at h
        simp only [bind, Except.bind] at h
        split at h
        · cases h
        · cases h
          simp only [List.mem_append, List.mem_singleton] at hev
          rcases hev with hev | rfl
          · exact Or.inl hev
          · exact Or.inr here
      | takesOr c =>
        rw [ha] at h
        dsimp only at h
        cases h
        simp only [List.mem_append, List.mem_singleton] at hev
        rcases hev with hev | rfl
        · exact Or.inl hev
        · exact Or.inr here
      | flag b | const c | count =>
        rw [ha] at h
        dsimp only at h
        rcases matchShort_events cfg pfx cs rest _ unk evs' k unk' h ev hev with h1 | ⟨sp', hsp', hd, c, hc, ho'⟩
        · simp only [List.mem_append, List.mem_singleton] at h1
          rcases h1 with h1 | rfl
          · exact Or.inl h1
          · exact Or.inr here
        · exact Or.inr ⟨sp', hsp', hd, c, List.mem_cons_of_mem _ hc, ho'⟩

theorem processOpt_events (cfg : ScanCfg) (tok : String) (rest : List String)
    (newEvs : List Event) (k : Nat) (pass : List String)
    (h : processOpt cfg tok rest = .ok (newEvs, k, pass)) :
    ∀ ev ∈ newEvs, ∃ sp ∈ cfg.tbl, sp.dest = ev.dest ∧ Mentions tok sp.opt := by
  intro ev hev
  unfold processOpt at h
  rcases hsplit : splitEq tok with ⟨longOpt, explicit⟩
  rw [hsplit] at h
  dsimp only at h
  cases hl : lookupLong cfg.tbl longOpt with
  | some sp =>
    rw [hl] at h
    dsimp only at h
    obtain ⟨hsp, ho⟩ := lookupLong_some _ _ _ hl
    have hm : Mentions tok sp.opt := Or.inl (by rw [hsplit, ho])
    cases ha : sp.action with
    | takes n =>
      rw [ha] at h
      simp only [bind, Except.bind] at h
      split at h
      · cases h
      · cases h; simp at hev; subst hev; exact ⟨sp, hsp, rfl, hm⟩
    | takesOr c =>
      rw [ha] at h
      dsimp only at h
      cases h; simp at hev; subst hev; exact ⟨sp, hsp, rfl, hm⟩
    | flag b | const c | count =>
      rw [ha] at h
      dsimp only at h
      split at h
      · cases h
      · cases h; simp at hev; subst hev; exact ⟨sp, hsp, rfl, hm⟩
  | none =>
    rw [hl] at h
    dsimp only at h
    split at h
    · cases htok : tok.toList with
      | nil => rw [htok] at h; cases h; simp at hev
      | cons pf cs =>
        rw [htok] at h
        simp only [bind, Except.bind] at h
        split at h
        · cases h
        · rename_i res hres
          cases h
          obtain ⟨evs', k', unk'⟩ := res
          rcases matchShort_events cfg pf cs rest [] [] evs' k' unk' hres ev hev with h1 | ⟨sp, hsp, hd, ch, hch, ho⟩
          · simp at h1
          · exact ⟨sp, hsp, hd, Or.inr ⟨pf, ch, by rw [ho]; simp, by simp [htok], by simp [htok, hch]⟩⟩
    · split at h
      · cases h; simp at hev
      · cases h

theorem scanLoop_events (cfg : ScanCfg) :
    ∀ (n : Nat) (toks : List String), toks.length ≤ n →
    ∀ (evs : List Event) (largs : List String) (sc : Scanned),
    scanLoop cfg toks evs largs = .ok sc →
    ∀ ev ∈ sc.events, ev ∈ evs ∨ ∃ t ∈ toks, ∃ sp ∈ cfg.tbl, sp.dest = ev.dest ∧ Mentions t sp.opt
  | _, [], _, evs, largs, sc, h, ev, hev => by
    rw [scanLoop] at h
    cases h
    exact Or.inl (List.mem_reverse.1 hev)
  | 0, _ :: _, hl, _, _, _, _, _, _ => by simp at hl
  | n + 1, tok :: rest, hl, evs, largs, sc, h, ev, hev => by
    rw [scanLoop] at h
    split at h
    · cases h; exact Or.inl (List.mem_reverse.1 hev)
    · split at h
      · split at h
        · cases h
        · rename_i newEvs k pass hpo
          have hlen : (rest.drop k).length ≤ n := by simp at hl ⊢; omega
          rcases scanLoop_events cfg n _ hlen _ _ sc h ev hev with h1 | ⟨t, ht, sp, hsp, hd, hm⟩
          · simp only [List.mem_append, List.mem_reverse] at h1
            rcases h1 with h1 | h1
            · obtain ⟨sp, hsp, hd, hm⟩ := processOpt_events cfg tok rest newEvs k pass hpo ev h1
              exact Or.inr ⟨tok, by simp, sp, hsp, hd, hm⟩
            · exact Or.inl h1
          · exact Or.inr ⟨t, List.mem_cons_of_mem _ (List.mem_of_mem_drop ht), sp, hsp, hd, hm⟩
      · split at h
        · have hlen : rest.length ≤ n := by simp at hl; omega
          rcases scanLoop_events cfg n rest hlen evs _ sc h ev hev with h1 | ⟨t, ht, sp, hsp, hd, hm⟩
          · exact Or.inl h1
          · exact Or.inr ⟨t, List.mem_cons_of_mem _ ht, sp, hsp, hd, hm⟩
        · cases h; exact Or.inl (List.mem_reverse.1 hev)

theorem table_owner (ps : List Param) (sp : Spelling) (h : sp ∈ table ps) :
    ∃ q ∈ ps, sp.dest = q.name ∧ sp.opt ∈ q.allOpts := by
  unfold table at h
  obtain ⟨q, hq, hsp⟩ := List.mem_flatMap.1 h
  refine ⟨q, hq, ?_⟩
  unfold Param.spellings at hsp
  unfold Param.allOpts
  split at hsp <;> rename_i hd <;> simp only [hd, List.mem_map, List.mem_append, List.not_mem_nil] at hsp ⊢
  all_goals first
    | (rcases hsp with ⟨o, ho, rfl⟩ | ⟨o, ho, rfl⟩ <;> simp [ho]; done)
    | (obtain ⟨o, ho, rfl⟩ := hsp; simp [ho])
    | simp at hsp

theorem cmdRaw_quiet (evs : List Event) (p : Param) (h : ∀ ev ∈ evs, ev.dest ≠ p.name) :
    cmdRaw evs p = none := by
  have hnil : evs.filter (fun e => e.dest == p.name) = [] :=
    List.filter_eq_nil_iff.2 fun ev hev => by simpa using h ev hev
  unfold cmdRaw
  rw [hnil]
  split <;> simp

/-- A parameter absent from `argv` has no command-line value. -/
theorem rawOf_absent (cmd : Command σ) (s : Settings) (p : Param) (argv : List String) (sc : Scanned)
    (araws : List (String × Option Raw)) (hopt : p.isOption = true) (ha : Absent cmd s p argv)
    (hsc : scan (cmd.scanCfg s) argv = .ok sc) : rawOf sc araws p = none := by
  simp only [rawOf, hopt, ite_true]
  apply cmdRaw_quiet
  intro ev hev hname
  rcases scanLoop_events _ argv.length argv (Nat.le_refl _) [] [] sc hsc ev hev with h1 | ⟨t, ht, sp, hsp, hd, hm⟩
  · simp at h1
  · obtain ⟨q, hq, hqd, hqo⟩ := table_owner _ sp hsp
    exact ha t ht q hq (by rw [← hqd, hd, hname]) sp.opt hqo hm

/-! ## The value of a single-value option -/

theorem convertShape_single (fs : String → Option FileInfo) (t : ParamType) (v : String) :
    ∀ sh : Shape, sh = .single →
    (convertShape fs t sh (.tokens [v])).map (·.val) = (convert fs t v).map PValue.one := by
  intro sh hs
  subst hs
  simp only [convertShape]
  cases convert fs t v <;> rfl

/-- A single-value option resolved from one raw token holds that token
converted through its type. -/
theorem finishParam_value (fs : String → Option FileInfo) (p : Param) (opts : List String)
    (hd : p.decl = .option opts (.value 1 false)) (v : String) (src : Source) (e : Entry)
    (h : finishParam fs p (some (.tokens [v])) src = .ok e) :
    e.param = p ∧ e.source = src ∧ ∃ x, convert fs p.type v = .ok x ∧ e.toValue = p.type.erase x := by
  have hs : p.shape = .single := by simp [Param.shape, hd]
  unfold finishParam at h
  simp only [Option.or] at h
  cases hcv : convertShape fs p.type p.shape (.tokens [v]) with
  | error m => rw [hcv] at h; cases h
  | ok w =>
    rw [hcv] at h
    obtain ⟨w, hw⟩ := w
    have hval := convertShape_single fs p.type v p.shape hs
    rw [hcv] at hval
    cases hc : convert fs p.type v with
    | error m => rw [hc] at hval; cases hval
    | ok y =>
      rw [hc] at hval
      simp only [Except.map, Except.ok.injEq] at hval
      subst hval
      dsimp only at h
      split at h
      · cases h
      · split at h
        · cases h
        · cases h; exact ⟨rfl, rfl, y, rfl, rfl⟩

/-! ## Absent from the command line -/

theorem envvar_beats_default : EnvvarBeatsDefault (σ := σ) := by
  intro cmd s path env p opts argv w r hp hd hpr ha hw hparse
  obtain ⟨sc, araws, hsc, e, he, hep, hrp⟩ := parseArgs_entry cmd s path argv env r hparse p hp
  have hopt : p.isOption = true := by simp [Param.isOption, hd]
  rw [rawOf_absent cmd s p argv sc araws hopt ha hsc, hw] at hrp
  have henv : (some w).bind (envRaw p) = some (.tokens [w]) := by simp [envRaw, hd]
  obtain ⟨e0, o, hcore, rfl⟩ := resolveParam_ok _ _ _ _ _ _ hrp
  clear hrp
  have hrp := hcore
  clear hcore
  unfold resolveCore at hrp
  simp only [pickSource_noPrompt p _ _ _ hpr, consume_environment _ _ p henv,
    wantsPrompt_none p _ _ hpr, Bool.false_and, Bool.false_eq_true, ite_false] at hrp
  obtain ⟨_, hsrc, hval⟩ := finishParam_value _ p opts hd w _ e0 hrp
  exact ⟨_, he, hep, hsrc, by simpa only [Entry.toValue_with_override] using hval⟩

theorem default_when_absent : DefaultWhenAbsent (σ := σ) := by
  intro cmd s path env p opts argv d r hp hd hpr ha hw hdef hparse
  obtain ⟨sc, araws, hsc, e, he, hep, hrp⟩ := parseArgs_entry cmd s path argv env r hparse p hp
  have hopt : p.isOption = true := by simp [Param.isOption, hd]
  rw [rawOf_absent cmd s p argv sc araws hopt ha hsc, hw] at hrp
  have hdr : defaultRaw p = some (.tokens [d]) := by
    simp [defaultRaw, hdef, Param.shape, hd]
  obtain ⟨e0, o, hcore, rfl⟩ := resolveParam_ok _ _ _ _ _ _ hrp
  clear hrp
  have hrp := hcore
  clear hcore
  unfold resolveCore at hrp
  simp only [pickSource_noPrompt p _ _ _ hpr, consume_default (none : Option String) p rfl, hdr,
    wantsPrompt_none p _ _ hpr, Bool.false_and, Bool.false_eq_true, ite_false] at hrp
  obtain ⟨_, hsrc, hval⟩ := finishParam_value _ p opts hd d _ e0 hrp
  exact ⟨_, he, hep, hsrc, by simpa only [Entry.toValue_with_override] using hval⟩

/-! ## Given on the command line -/

theorem idxOf?_eq_append (o v : List Char) (h : '=' ∉ o) :
    (o ++ '=' :: v).idxOf? '=' = some o.length := by
  induction o with
  | nil => simp [List.idxOf?, List.findIdx?_cons]
  | cons c cs ih =>
    have hc : c ≠ '=' := fun h' => h (h' ▸ List.mem_cons_self)
    have hcs : '=' ∉ cs := fun h' => h (List.mem_cons_of_mem _ h')
    have := ih hcs
    simp only [List.idxOf?, List.cons_append] at this ⊢
    rw [List.findIdx?_cons]
    simp [hc, this]

theorem splitEq_attached (o v : String) (h : '=' ∉ o.toList) : splitEq (o ++ "=" ++ v) = (o, some v) := by
  unfold splitEq
  have ht : (o ++ "=" ++ v).toList = o.toList ++ '=' :: v.toList := by simp
  rw [ht]
  dsimp only
  rw [idxOf?_eq_append _ _ h]
  simp

theorem startsDashDash_append (o w : String) (h : startsDashDash o = true) : startsDashDash (o ++ w) = true := by
  unfold startsDashDash at h ⊢
  split at h
  · rename_i rest heq
    rw [String.toList_append, heq]
    rfl
  · cases h

theorem scanLoop_values (cfg : ScanCfg) (d : String)
    (hp : ['-'] ∈ cfg.prefixes ∧ ['-', '-'] ∈ cfg.prefixes) :
    ∀ (occs : List (String × String × Bool)) (evs : List Event) (largs : List String),
    (∀ x ∈ occs, lookupLong cfg.tbl x.1 = some ⟨x.1, d, .takes 1⟩ ∧ IsBareLongOpt x.1) →
    scanLoop cfg (occTokens occs) evs largs =
      .ok ⟨evs.reverse ++ occs.map (fun x => ⟨d, .values [x.2.1]⟩), largs.reverse, []⟩
  | [], evs, largs, _ => by simp only [occTokens]; rw [scanLoop]; simp
  | (o, v, att) :: rest, evs, largs, h => by
    obtain ⟨hl, hdd, hne, heq⟩ := h (o, v, att) (by simp)
    have ih := scanLoop_values cfg d hp rest (⟨d, .values [v]⟩ :: evs) largs (fun x hx => h x (by simp [hx]))
    cases att with
    | false =>
      simp only [occTokens]
      rw [scanLoop]
      have h1 : (o == "--") = false := by simpa using hne
      have hpo : processOpt cfg o (v :: occTokens rest) = .ok ([⟨d, .values [v]⟩], 1, []) := by
        unfold processOpt
        rw [splitEq_noEq o heq]
        dsimp only
        rw [hl]
        simp [takeValues]
        rfl
      simp only [h1, isOptTok_of_dashDash cfg o hdd hp.1, Bool.false_eq_true, ite_false, ite_true, hpo]
      rw [show List.drop 1 (v :: occTokens rest) = occTokens rest from rfl]
      simp only [List.reverse_cons, List.reverse_nil, List.nil_append, List.singleton_append]
      rw [ih]
      simp
    | true =>
      simp only [occTokens]
      rw [scanLoop]
      have hdd' := startsDashDash_append o ("=" ++ v) hdd
      rw [← String.append_assoc] at hdd'
      have h1 : (o ++ "=" ++ v == "--") = false := by
        have : (o ++ "=" ++ v).toList ≠ ['-', '-'] := by
          intro hc
          have : '=' ∈ (o ++ "=" ++ v).toList := by simp
          rw [hc] at this; simp at this
        simp only [beq_eq_false_iff_ne, ne_eq]
        intro hc; exact this (by rw [hc]; rfl)
      have hpo : processOpt cfg (o ++ "=" ++ v) (occTokens rest) = .ok ([⟨d, .values [v]⟩], 0, []) := by
        unfold processOpt
        rw [splitEq_attached o v heq]
        dsimp only
        rw [hl]
        simp [takeValues]
        rfl
      simp only [h1, isOptTok_of_dashDash cfg _ hdd' hp.1, Bool.false_eq_true, ite_false, ite_true, hpo,
        List.drop_zero]
      simp only [List.reverse_cons, List.reverse_nil, List.nil_append, List.singleton_append]
      rw [ih]
      simp

theorem cmdRaw_values (p : Param) (opts : List String) (hd : p.decl = .option opts (.value 1 false))
    (occs : List (String × String × Bool)) (h : occs ≠ []) :
    cmdRaw (occs.map (fun x => ⟨p.name, .values [x.2.1]⟩)) p = some (.tokens [(occs.getLast h).2.1]) := by
  simp only [cmdRaw, hd]
  have hf : (occs.map (fun x => (⟨p.name, .values [x.2.1]⟩ : Event))).filter (fun e => e.dest == p.name) =
      occs.map (fun x => ⟨p.name, .values [x.2.1]⟩) :=
    List.filter_eq_self.2 (fun a ha => by obtain ⟨x, _, rfl⟩ := List.mem_map.1 ha; simp)
  rw [hf]
  simp [List.filterMap_map, Function.comp_def, List.getLast?_map, List.getLast?_eq_some_getLast h]

theorem commandline_beats_envvar : CommandlineBeatsEnvvar (σ := σ) := by
  intro cmd s path env p opts occs r hne hu hp hd hea hocc hparse
  obtain ⟨sc, araws, hsc, e, he, hep, hrp⟩ := parseArgs_entry cmd s path _ env r hparse p hp
  have hscan := scanLoop_values (cmd.scanCfg s) p.name (scanCfg_prefixes cmd s) occs [] [] (by
    intro x hx
    obtain ⟨hmem, hbare⟩ := hocc x hx
    have hrow : (⟨x.1, p.name, .takes 1⟩ : Spelling) ∈ table (cmd.allParams s) := by
      unfold table
      refine List.mem_flatMap.2 ⟨p, hp, ?_⟩
      unfold Param.spellings
      rw [hd]
      simpa using hmem
    exact ⟨lookupLong_row cmd s hu _ hrow (not_short_of_dashDash _ hbare.1), hbare⟩)
  unfold scan at hsc
  rw [hsc] at hscan
  cases hscan
  have hopt : p.isOption = true := by simp [Param.isOption, hd]
  have hraw : rawOf ⟨[].reverse ++ occs.map (fun x => ⟨p.name, .values [x.2.1]⟩), [].reverse, []⟩ araws p =
      some (.tokens [(occs.getLast hne).2.1]) := by
    simp only [rawOf, hopt, ite_true, List.reverse_nil, List.nil_append]
    exact cmdRaw_values p opts hd occs hne
  rw [hraw] at hrp
  obtain ⟨e0, o, hcore, rfl⟩ := resolveParam_ok _ _ _ _ _ _ hrp
  clear hrp
  have hrp := hcore
  clear hcore
  unfold resolveCore at hrp
  simp only [pickSource_cmdline _ _ (.tokens [(occs.getLast hne).2.1]) _ (by simp),
    wantsPrompt_cmdline _ (.tokens [(occs.getLast hne).2.1]) (by simp), Bool.false_and, Bool.false_eq_true, ite_false] at hrp
  obtain ⟨_, hsrc, hval⟩ := finishParam_value _ p opts hd _ _ e0 hrp
  exact ⟨_, he, hep, hsrc, by simpa only [Entry.toValue_with_override] using hval⟩

end LeanClick
