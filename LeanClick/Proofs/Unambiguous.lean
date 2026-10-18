import LeanClick.Proofs.Precedence

/-!
# Proofs: when a command is unambiguous

A command's own spellings being distinct makes it unambiguous under *any*
context settings whose help names are distinct: the help option only takes
the names no parameter uses, and a `default_map` never changes a spelling.
This is what `click_command` instantiates, so its `<name>.unambiguous`
covers help names inherited from a parent group (fastapi-cli's `-h`).
-/

namespace LeanClick

theorem Param.spellings_withDefaultMap (dm : Option DefaultMap) (p : Param) :
    (p.withDefaultMap dm).spellings = p.spellings := by
  unfold Param.withDefaultMap
  split <;> rfl

theorem table_map_withDefaultMap (dm : Option DefaultMap) (ps : List Param) :
    table (ps.map (Param.withDefaultMap dm)) = table ps := by
  unfold table
  induction ps with
  | nil => rfl
  | cons p ps ih => simp [Param.spellings_withDefaultMap, ih]

theorem table_helpParam_opts (names : List String) :
    (table [helpParam names]).map (·.opt) = names := by
  simp [table, Param.spellings, helpParam, Function.comp_def]

/-- Distinct own spellings and distinct help names make a command
unambiguous in any context. -/
theorem Command.unambiguous_of {σ : Type} (cmd : Command σ) (s : Settings)
    (hp : ((table cmd.spec.params).map (·.opt)).Nodup) (hh : s.helpOptionNames.Nodup) :
    cmd.Unambiguous s := by
  unfold Command.Unambiguous Command.allParams
  dsimp only
  rw [table, List.flatMap_append, ← table, ← table, table_map_withDefaultMap, List.map_append,
    List.nodup_append]
  refine ⟨hp, ?_, ?_⟩
  · split
    · rw [table_helpParam_opts]
      exact List.Pairwise.sublist (List.filter_sublist) hh
    · simp [table]
  · intro a ha b hb hab
    split at hb
    · rw [table_helpParam_opts] at hb
      obtain ⟨sp, hsp, rfl⟩ := List.mem_map.1 ha
      obtain ⟨q, hq, _, hqo⟩ := table_owner _ sp hsp
      have := (List.mem_filter.1 hb).2
      simp only [Bool.not_eq_true', List.any_eq_false, Bool.not_eq_true] at this
      have hc := this q hq
      rw [← hab] at hc
      simp [hqo] at hc
    · simp [table] at hb

end LeanClick
