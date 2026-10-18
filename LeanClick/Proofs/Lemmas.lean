import LeanClick.Specifications.Parse

/-!
# General lemmas

`mapM` in `Except`, stable sorting, and string containment, used by the
headline proofs.
-/

namespace LeanClick

theorem mapM_except_ok_mem {α β ε : Type} (f : α → Except ε β) :
    ∀ (l : List α) (r : List β), l.mapM f = .ok r → ∀ x ∈ l, ∃ y ∈ r, f x = .ok y
  | [], _, _, x, hx => by simp at hx
  | a :: l, r, h, x, hx => by
    rw [List.mapM_cons] at h
    cases hfa : f a with
    | error e => simp [hfa] at h; cases h
    | ok b =>
      cases hl : l.mapM f with
      | error e => simp [hfa, hl] at h; cases h
      | ok bs =>
        simp [hfa, hl] at h
        cases h
        simp only [List.mem_cons] at hx
        rcases hx with rfl | hx
        · exact ⟨b, by simp, hfa⟩
        · obtain ⟨y, hy, hfy⟩ := mapM_except_ok_mem f l bs hl x hx
          exact ⟨y, by simp [hy], hfy⟩

theorem mapM_except_ok_forall {α β ε : Type} (f : α → Except ε β) (P : β → Prop)
    (hP : ∀ x y, f x = .ok y → P y) :
    ∀ (l : List α) (r : List β), l.mapM f = .ok r → ∀ y ∈ r, P y
  | [], r, h, y, hy => by
    rw [List.mapM_nil] at h; cases h; simp at hy
  | a :: l, r, h, y, hy => by
    rw [List.mapM_cons] at h
    cases hfa : f a with
    | error e => simp [hfa] at h; cases h
    | ok b =>
      cases hl : l.mapM f with
      | error e => simp [hfa, hl] at h; cases h
      | ok bs =>
        simp [hfa, hl] at h
        cases h
        simp only [List.mem_cons] at hy
        rcases hy with rfl | hy
        · exact hP a _ hfa
        · exact mapM_except_ok_forall f P hP l bs hl y hy

theorem mapM_congr_mem {α β ε : Type} (f g : α → Except ε β) :
    ∀ (l : List α), (∀ x ∈ l, f x = g x) → l.mapM f = l.mapM g
  | [], _ => rfl
  | a :: l, h => by
    simp only [List.mapM_cons]
    rw [h a (by simp), mapM_congr_mem f g l (fun x hx => h x (by simp [hx]))]

theorem mem_insertBy {α : Type} (key : α → Nat) (x y : α) :
    ∀ l : List α, y ∈ insertBy key x l ↔ y = x ∨ y ∈ l
  | [] => by simp [insertBy]
  | z :: zs => by
    unfold insertBy
    split
    · simp
    · simp only [List.mem_cons, mem_insertBy key x y zs]
      constructor
      · rintro (h | h | h) <;> simp [h]
      · rintro (h | h | h) <;> simp [h]

theorem mem_sortBy {α : Type} (key : α → Nat) (y : α) :
    ∀ l : List α, y ∈ sortBy key l ↔ y ∈ l
  | [] => by simp [sortBy]
  | z :: zs => by
    have := mem_sortBy key y zs
    simp only [sortBy, List.foldr_cons] at this ⊢
    rw [mem_insertBy]
    simp [this]

theorem mem_processingOrder (ps : List Param) (inv : List String) (p : Param) :
    p ∈ processingOrder ps inv ↔ p ∈ ps := mem_sortBy _ _ _

/-! ## `IsInfix` -/

theorem IsInfix.refl (a : String) : IsInfix a a := ⟨"", "", by simp⟩

theorem IsInfix.trans {a b c : String} (h₁ : IsInfix a b) (h₂ : IsInfix b c) : IsInfix a c := by
  obtain ⟨x, y, rfl⟩ := h₁
  obtain ⟨u, v, rfl⟩ := h₂
  exact ⟨u ++ x, y ++ v, by simp [String.append_assoc]⟩

theorem IsInfix.left {a b : String} (c : String) (h : IsInfix a b) : IsInfix a (c ++ b) := by
  obtain ⟨x, y, rfl⟩ := h
  exact ⟨c ++ x, y, by simp [String.append_assoc]⟩

theorem IsInfix.right {a b : String} (c : String) (h : IsInfix a b) : IsInfix a (b ++ c) := by
  obtain ⟨x, y, rfl⟩ := h
  exact ⟨x, y ++ c, by simp [String.append_assoc]⟩

theorem IsInfix.prefix (a b : String) : IsInfix a (a ++ b) := ⟨"", b, by simp⟩

theorem IsInfix.suffix (a b : String) : IsInfix b (a ++ b) := ⟨a, "", by simp⟩

theorem concatS_infix (r : String) : ∀ l : List String, r ∈ l → IsInfix r (concatS l)
  | [], h => by simp at h
  | a :: l, h => by
    simp only [List.mem_cons] at h
    simp only [concatS]
    rcases h with rfl | h
    · exact IsInfix.prefix _ _
    · exact (concatS_infix r l h).left a

theorem intercalate_infix (sep o : String) : ∀ l : List String, o ∈ l → IsInfix o (sep.intercalate l)
  | [], h => by simp at h
  | [a], h => by
    simp at h; subst h; simp [String.intercalate]; exact IsInfix.refl _
  | a :: b :: l, h => by
    rw [String.intercalate_cons_cons]
    simp only [List.mem_cons] at h
    rcases h with rfl | h
    · exact (IsInfix.prefix _ _).right _ |>.trans (by rw [String.append_assoc]; exact IsInfix.refl _)
    · have := intercalate_infix sep o (b :: l) (by simpa using h)
      exact this.left _

end LeanClick
