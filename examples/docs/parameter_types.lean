import examples.docs.runner

/-!
Port of click's `docs/parameter-types.md`: its code blocks, in page order,
each under its block number `docs/parameter-types.md#block-<n>`
(`tests/golden/docblocks.py`). The `click:run` blocks are the golden runs
in `tests/golden/click.json`, replayed through `programs`.
-/

open LeanClick LeanClick.Terminal Text

namespace Docs.ParameterTypes

-- block 1: `click.Choice(HashType, case_sensitive=False)`
namespace B1
inductive HashType
  | MD5
  | SHA1

def HashType.name : HashType → String
  | .MD5 => "MD5"
  | .SHA1 => "SHA1"

/-- `str()` of an enum member. -/
instance : ToString HashType := ⟨fun h => s!"HashType.{h.name}"⟩

instance : ClickType HashType := .enum [.MD5, .SHA1] HashType.name (caseSensitive := false)

click_command digest (hash_type : Option HashType := none) : IO Unit :=
  echo (hash_type.map toString |>.getD "None")
end B1

-- block 3
namespace B3
click_command «repeat» [name := "repeat"]
    (count : Option Int := none) [type := some (IntRange (some 0) (some 20) (clamp := true))]
    (digit : Option Int := none) [type := some (IntRange (some 0) (some 9))]
    : IO Unit := do
  let (some count, some digit) := (count, digit) | throw (IO.userError "count and digit are required")
  echo (String.join (List.replicate count.toNat (toString digit)))
end B3

-- block 5: the type comes from the default. In Lean the binder states it,
-- so the body reports the static types.
namespace B5
click_command «show» [name := "show"] (count : Int := 5) (tag : String := "{'git'}") : IO Unit := do
  echo "count is int, tag is str"
end B5

-- block 7: a custom type, `click.ParamType` with `convert`
namespace B7
/-- An integer written in decimal, `0x` hex or leading-`0` octal. -/
structure BasedInt where
  val : Int

def digitsIn (base : Nat) (s : String) : Option Int :=
  if s.isEmpty then none else
  s.foldl (fun acc c => acc.bind fun n =>
    let d := if c.isDigit then c.toNat - '0'.toNat
      else if 'a' ≤ c.toLower && c.toLower ≤ 'f' then c.toLower.toNat - 'a'.toNat + 10 else base
    if d < base then some (n * base + d) else none) (some 0) |>.map Int.ofNat

def convert (value : String) : Option Int :=
  if (value.take 2).toString.toLower == "0x" then digitsIn 16 (value.drop 2).toString
  else if (value.take 1).toString == "0" then digitsIn 8 value
  else parseInt value

instance : ClickType BasedInt where
  type := .string
  ofValue | .int n => some { val := n } | _ => none
  toTokens n := [toString n.val]
  toValue n := .int n.val
  custom := some ("integer", fun
    | .int n => .ok (.int n)
    | .str s => match convert s with
      | some n => .ok (.int n)
      | none => .error s!"{pyRepr s} is not a valid integer"
    | v => .ok v)
end B7

open Docs in
def programs : Docs.Programs :=
  [("digest@1", run B1.digest ()), ("repeat@3", run B3.«repeat» ()), ("show@5", run B5.«show» ())]

end Docs.ParameterTypes

def main (args : List String) : IO UInt32 := Docs.main Docs.ParameterTypes.programs args
