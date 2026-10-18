import LeanClick.Terminal

/-!
# Typed commands

A command written as a Lean function, the way typer (and so fastapi-cli)
writes click commands: each parameter is a typed binder, a binder with a
default is an option (`--root-path` for `root_path`), a binder without one
is an argument, a `Bool` with a default is a flag pair (`--x` with `--no-x`), and
`Option α` / `List α` are optional and repeatable. The values reach the
body typed and refined; there is no lookup by name in user code.

```lean
/-- Run a FastAPI app in production mode. -/
click_command run
    (path : Argument (Option String)) "A path to a Python file."
    (host : String := "0.0.0.0") "The host to serve on."
    (port : Nat := 8000) "The port to serve on." [envvar := ["PORT"]]
    (reload : Bool := false) "Enable auto-reload."
    : IO Unit := do
  IO.println s!"serving on {host}:{port}"
```

`click_command` generates `run.main` (the typed function) and `run :
Terminal.App Unit` (the click command calling it). The doc comment is the
help text, as a docstring is in click. It also checks, when the file is
compiled, that no two parameters share a spelling (`Command.Unambiguous`,
the hypothesis of `bool_flag_pairs` and `commandline_beats_envvar`); a
clash is a compile error.
-/

namespace LeanClick

open Text Terminal

/-- A Lean type a parameter can have: its click type, how a parsed value
becomes an `α`, and how an `α` default is written for the parser. -/
class ClickType (α : Type) where
  type : ParamType
  ofValue : Value → Option α
  toTokens : α → List String
  /-- `Bool`: a flag. -/
  isFlag : Bool := false
  /-- `List α`: repeatable. -/
  multiple : Bool := false
  /-- `Option α`: may be absent. -/
  optional : Bool := false

/-- Mark a binder with a default as a positional argument. -/
abbrev Argument (α : Type) := α

/-- An integer in `[lo, hi]`, carrying its bounds proof: `IntRange(lo, hi)`. -/
abbrev IntIn (lo hi : Int) := {n : Int // lo ≤ n ∧ n ≤ hi}

def Dec.toFloat (d : Dec) : Float := Float.ofInt d.mant / (10 : Float) ^ d.exp.toFloat

instance : ClickType String where
  type := .string
  ofValue | .str s => some s | _ => none
  toTokens s := [s]

instance : ClickType Int where
  type := .int
  ofValue | .int n => some n | _ => none
  toTokens n := [toString n]

instance : ClickType Nat where
  type := .intRange { min := some 0 }
  ofValue | .int n => if 0 ≤ n then some n.toNat else none | _ => none
  toTokens n := [toString n]

instance (lo hi : Int) : ClickType (IntIn lo hi) where
  type := .intRange { min := some lo, max := some hi }
  ofValue | .int n => if h : lo ≤ n ∧ n ≤ hi then some ⟨n, h⟩ else none | _ => none
  toTokens n := [toString n.val]

instance : ClickType Dec where
  type := .float
  ofValue | .dec d => some d | _ => none
  toTokens d := [showDec d]

instance : ClickType Float where
  type := .float
  ofValue | .dec d => some d.toFloat | _ => none
  toTokens f := [toString f]

instance : ClickType Bool where
  type := .bool
  ofValue | .bool b => some b | _ => none
  toTokens b := [if b then "true" else "false"]
  isFlag := true

instance {α : Type} [ClickType α] : ClickType (Option α) where
  type := ClickType.type α
  ofValue | .none => some none | v => (ClickType.ofValue v).map some
  toTokens | some a => ClickType.toTokens a | none => []
  isFlag := ClickType.isFlag α
  optional := true

instance {α : Type} [ClickType α] : ClickType (List α) where
  type := ClickType.type α
  ofValue | .list vs => vs.mapM ClickType.ofValue | _ => none
  toTokens as := as.flatMap ClickType.toTokens
  multiple := true

namespace ClickType

/-- The option spelling of a binder name: `root_path` is `--root-path`. -/
def spelling (name : String) : String := "--" ++ replaceChar name '_' '-'

/-- The parameter a typed option binder declares. -/
def option (α : Type) [ClickType α] (name : String) (default : Option α := none) (help : String := "")
    (envvar : List String := []) (decls : List String := []) (hidden : Bool := false)
    (showDefault : Bool := false) (prompt : Option String := none) (metavar : Option String := none)
    (type : Option ParamType := none) : Param :=
  let decls := if decls.isEmpty then
      if ClickType.isFlag α then [spelling name ++ "/--no-" ++ (replaceChar name '_' '-')] else [spelling name]
    else decls
  let base : Param :=
    if ClickType.isFlag α then Param.flag decls
    else Param.opt decls (type.getD (ClickType.type α)) (multiple := ClickType.multiple α)
  { base with
    name, default := (default.map ClickType.toTokens).filter (!·.isEmpty),
    help := if help.isEmpty then none else some help,
    required := default.isNone && prompt.isNone && !ClickType.optional α && !ClickType.multiple α &&
      !ClickType.isFlag α,
    envvar, hidden, metavar, showDefault := if showDefault then .yes else .auto,
    prompt := prompt.map fun t => { text := t } }

/-- The parameter a typed argument binder declares. -/
def argument (α : Type) [ClickType α] (name : String) (default : Option α := none) (help : String := "")
    (envvar : List String := []) (metavar : Option String := none) (type : Option ParamType := none) :
    Param :=
  let base := Param.arg name (type.getD (ClickType.type α)) (if ClickType.multiple α then .star else .exactly 1)
  { base with
    default := (default.map ClickType.toTokens).filter (!·.isEmpty),
    help := if help.isEmpty then none else some help,
    required := default.isNone && !ClickType.optional α && !ClickType.multiple α,
    envvar, metavar }

/-- The typed value of a parameter. A value the parser produced always
converts (the declared type is `ClickType.type α`); the error is the
defensive case of a mismatched hand-built command. -/
def fetch (α : Type) [ClickType α] (a : ParsedArgs) (name : String) : ClickM α :=
  match ClickType.ofValue (a.get name) with
  | some x => pure x
  | none => throw (.failure s!"parameter {name} does not have the declared type")

end ClickType

/-! ## `click_command` -/

declare_syntax_cat clickBinder
declare_syntax_cat clickKw
syntax ident " := " term : clickKw
syntax "(" ident " : " term (" := " term)? ")" (ppSpace str)? (ppSpace "[" sepBy1(clickKw, ", ") "]")? : clickBinder

/-- Declare a click command as a typed Lean function (see the module doc). -/
syntax (docComment)? "click_command " ident (ppSpace clickBinder)* " : " term " := " term : command

open Lean in
macro_rules
  | `($[$doc?:docComment]? click_command $name:ident $bs:clickBinder* : $ret := $body) => do
    let help : String := match doc? with
      | some d => (d.getDocString).trimAscii.copy
      | none => ""
    let cliName := Syntax.mkStrLit (replaceChar name.getId.toString '_' '-')
    let mainName := mkIdentFrom name (name.getId ++ `main)
    let mut binders : Array (TSyntax `Lean.Parser.Term.bracketedBinder) := #[]
    let mut params : Array Term := #[]
    let mut fetches : Array (Ident × Term × StrLit) := #[]
    let mut args : Array Term := #[]
    for b in bs do
      match b with
      | `(clickBinder| ($x:ident : $t:term $[:= $d?]?) $[$h?:str]? $[[$kws:clickKw,*]]?) =>
        let kws : Array (TSyntax `clickKw) := match kws with | some k => k.getElems | none => #[]
        let mut ns : Array Ident := #[]
        let mut vs : Array Term := #[]
        for k in kws do
          match k with
          | `(clickKw| $n:ident := $v:term) => ns := ns.push n; vs := vs.push v
          | _ => Macro.throwUnsupported
        let pname := Syntax.mkStrLit x.getId.toString
        -- typer's rule: no default means an argument, unless the binder is
        -- given option spellings or a prompt
        let optionKw := ns.any fun n => n.getId == `decls || n.getId == `prompt
        let (isArg, τ) ← match t with
          | `(Argument $τ:term) => pure (true, τ)
          | _ => pure (d?.isNone && !optionKw, t)
        let helpLit : Term := match h? with | some h => h | none => Syntax.mkStrLit ""
        let dflt : Term ← match d? with
          | some d => `((some ($d : $τ)))
          | none => `(none)
        let builder : Ident := mkIdent (if isArg then ``LeanClick.ClickType.argument else ``LeanClick.ClickType.option)
        let extra : Array Syntax := (ns.zip vs).map fun (n, v) =>
          Syntax.node SourceInfo.none `Lean.Parser.Term.namedArgument
            #[Syntax.atom SourceInfo.none "(", n.raw, Syntax.atom SourceInfo.none ":=", v.raw,
              Syntax.atom SourceInfo.none ")"]
        let call ← `($builder $τ $pname (default := $dflt) (help := $helpLit))
        let call : Term := match call.raw with
          | Syntax.node i k #[f, Syntax.node i' k' as] => ⟨Syntax.node i k #[f, Syntax.node i' k' (as ++ extra)]⟩
          | c => ⟨c⟩
        params := params.push call
        binders := binders.push (← `(bracketedBinder| ($x : $τ)))
        fetches := fetches.push (x, τ, pname)
        args := args.push x
      | _ => Macro.throwUnsupported
    let helpLit := Syntax.mkStrLit help
    let c1 ← `(command| $[$doc?:docComment]? def $mainName $binders:bracketedBinder* : $ret := $body)
    let ctxId := mkIdent `ctx
    let mut cb ← `(do let _ ← ($mainName $args* : $ret); pure ())
    for (x, τ, pname) in fetches.reverse do
      cb ← `(LeanClick.ClickType.fetch $τ $(ctxId).params $pname >>= fun $x:ident => $cb)
    let c2 ← `(command| def $name : LeanClick.Terminal.App Unit :=
        LeanClick.command $cliName [$params,*] (help := $helpLit)
          (callback := fun $ctxId => $cb))
    -- no two parameters share a spelling (the `Unambiguous` hypothesis of
    -- the flag and precedence theorems), checked when the command is compiled
    let c3 ← `(command| #guard decide (LeanClick.Command.Unambiguous $name {}))
    return mkNullNode #[c1, c2, c3]

end LeanClick
