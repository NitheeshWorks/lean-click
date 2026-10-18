import LeanClick.Proofs.Parse

/-!
# Proofs: the help page lists the table the parser uses
-/

namespace LeanClick

open Spec Text

theorem renderRow_infix (w i fc : Nat) (row : String × String) :
    IsInfix row.1 (renderRow w i fc row) :=
  ⟨spaces i, rowTail w i fc row, rfl⟩

theorem renderDl_infix (w i : Nat) (rows : List (String × String)) (row : String × String)
    (h : row ∈ rows) : IsInfix row.1 (renderDl w i rows) := by
  unfold renderDl
  dsimp only
  exact (renderRow_infix _ _ _ row).trans (concatS_infix _ _ (List.mem_map_of_mem h))

theorem renderSection_infix (w : Nat) (name : String) (rows : List (String × String))
    (row : String × String) (h : row ∈ rows) : IsInfix row.1 (renderSection w name rows) := by
  unfold renderSection
  have hne : rows.isEmpty = false := by
    cases rows with
    | nil => simp at h
    | cons _ _ => rfl
  simp only [hne, Bool.false_eq_true, ite_false]
  exact (renderDl_infix w 2 rows row h).left _

theorem optionRows_mem (cmd : Command σ) (s : Settings) (p : Param) (hp : p ∈ cmd.allParams s)
    (ho : p.isOption = true) (hh : p.hidden = false) :
    ∃ help, (p.helpSpec, help) ∈ cmd.optionRows s := by
  have : ∃ h, p.optionRecord s = some (p.helpSpec, h) := by
    unfold Param.optionRecord; simp [hh]
  obtain ⟨h, hr⟩ := this
  exact ⟨h, List.mem_filterMap.2 ⟨p, hp, by simp [ho, hr]⟩⟩

theorem formatHelp_options_infix (cmd : Command σ) (s : Settings) (path : String) (t : String)
    (h : IsInfix t (renderSection s.width "Options" (cmd.optionRows s))) :
    IsInfix t (cmd.formatHelp s path) := by
  unfold Command.formatHelp
  exact ((h.left _).right _).right _

theorem joinOptions_infix (o : String) (l : List String) (h : o ∈ l) : IsInfix o (joinOptions l) := by
  unfold joinOptions
  apply intercalate_infix
  rw [mem_sortBy]
  exact h

theorem allOpts_in_helpSpec (p : Param) (ho : p.isOption = true) (o : String) (h : o ∈ p.allOpts) :
    IsInfix o p.helpSpec := by
  unfold Param.helpSpec
  simp only [ho, ite_true]
  have hprim : ∀ o ∈ p.primaryOpts, IsInfix o (joinOptions p.primaryOpts ++
      (if p.isFlagLike then "" else " " ++ p.makeMetavar)) :=
    fun o h => (joinOptions_infix o _ h).right _
  have hsec : ∀ o ∈ p.secondaryOpts, IsInfix o (joinOptions p.secondaryOpts ++
      (if p.isFlagLike then "" else " " ++ p.makeMetavar)) :=
    fun o h => (joinOptions_infix o _ h).right _
  have hcases : o ∈ p.primaryOpts ∨ o ∈ p.secondaryOpts := by
    unfold Param.allOpts at h
    unfold Param.primaryOpts Param.secondaryOpts
    cases hd : p.decl with
    | option opts kind =>
      rw [hd] at h
      cases kind <;> simp_all
    | argument n => rw [hd] at h; simp at h
  split
  · rename_i hse
    rcases hcases with hc | hc
    · exact hprim o hc
    · simp at hse; rw [hse] at hc; simp at hc
  · rcases hcases with hc | hc
    · exact ((hprim o hc).right _).right _
    · exact (hsec o hc).left _

/-- The help row of a visible option is on the page. -/
theorem helpSpec_in_help (cmd : Command σ) (s : Settings) (path : String) (p : Param)
    (hp : p ∈ cmd.allParams s) (ho : p.isOption = true) (hh : p.hidden = false) :
    IsInfix p.helpSpec (cmd.formatHelp s path) := by
  obtain ⟨help, hrow⟩ := optionRows_mem cmd s p hp ho hh
  exact formatHelp_options_infix cmd s path _ (renderSection_infix _ _ _ _ hrow)

theorem help_lists_every_param : HelpListsEveryParam (σ := σ) :=
  fun cmd s path p hp ho hh o h =>
    (allOpts_in_helpSpec p ho o h).trans (helpSpec_in_help cmd s path p hp ho hh)

/-- When the Positional arguments section is shown, it lists every
argument. -/
theorem help_lists_every_argument (cmd : Command σ) (s : Settings) (path : String) (p : Param)
    (hp : p ∈ cmd.allParams s) (ha : p.isArgument = true)
    (hdoc : ((cmd.allParams s).filter Param.isArgument).any (fun q => q.help.isSome) = true) :
    IsInfix p.helpSpec (cmd.formatHelp s path) := by
  have hrow : (p.helpSpec, p.help.getD "") ∈ cmd.argumentRows s := by
    unfold Command.argumentRows
    simp only [hdoc, ite_true]
    exact List.mem_map_of_mem (f := fun p => (p.helpSpec, p.help.getD "")) (List.mem_filter.2 ⟨hp, ha⟩)
  have h := renderSection_infix s.width "Positional arguments" _ _ hrow
  unfold Command.formatHelp
  exact (((h.left _).right _).right _).right _

/-- A group's help lists every visible subcommand. -/
theorem help_lists_every_subcommand (cmd : Command σ) (s : Settings) (path : String) (c : Command σ)
    (hg : cmd.isGroup = true) (hc : c ∈ cmd.subcommands) (hh : c.spec.hidden = false) :
    IsInfix c.name (cmd.formatHelp s path) := by
  have hrow : ∃ t, (c.name, t) ∈ cmd.commandRows s := by
    unfold Command.commandRows
    simp only [hg, Bool.not_true, Bool.false_eq_true, ite_false]
    exact ⟨_, List.mem_map_of_mem (List.mem_filter.2 ⟨hc, by simp [hh]⟩)⟩
  obtain ⟨t, hrow⟩ := hrow
  have h := renderSection_infix s.width "Commands" _ _ hrow
  unfold Command.formatHelp
  exact (h.left _).right _

end LeanClick
