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
help text, as a docstring is in click. It also proves `run.unambiguous : ∀ s :
Settings, s.helpOptionNames.Nodup → Command.Unambiguous run s` (no two
parameters share a spelling, in any context, including help names inherited
from a parent group such as fastapi-cli's `-h`; the hypothesis of
`bool_flag_pairs` and `commandline_beats_envvar`), from
`Command.unambiguous_of` and a kernel `decide` on the command's own
spellings, so a clash is a compile error and users never discharge it.
-/

namespace LeanClick

open Text Terminal

/-- A Lean type a parameter can have: its click type, how a parsed value
becomes an `α`, and how an `α` default is written for the parser. -/
class ClickType (α : Type) where
  type : ParamType
  ofValue : Value → Option α
  toTokens : α → List String
  /-- The parsed value an `α` is (the inverse of `ofValue`), for callbacks
  that return a new value. -/
  toValue : α → Value
  /-- A custom type (a `click.ParamType` subclass): its name, which is the
  options' metavar, and its `convert` of the value `type` produced, failing
  with the type's message (`self.fail`). -/
  custom : Option (String × (Value → Except String Value)) := none
  /-- The type's `shell_complete`: completions for an incomplete value, used
  by parameters of this type that give no `shellComplete` of their own. -/
  complete : Option (Env → String → List CompletionItem) := none
  /-- `Bool`: a flag. -/
  isFlag : Bool := false
  /-- `List α`: repeatable. -/
  multiple : Bool := false
  /-- `Option α`: may be absent. -/
  optional : Bool := false

/-- `click.Choice(SomeEnum, case_sensitive=...)`: a type whose values are
named members. The choices are the names; a parsed name gives its member.
`instance : ClickType HashType := .enum [.MD5, .SHA1] HashType.name`. -/
@[instance_reducible] def ClickType.enum {α : Type} (members : List α) (name : α → String) (caseSensitive : Bool := true) :
    ClickType α where
  type := .choice (members.map name) caseSensitive
  ofValue | .str s => members.find? (name · == s) | _ => none
  toTokens a := [name a]
  toValue a := .str (name a)

/-- Mark a binder with a default as a positional argument. -/
abbrev Argument (α : Type) := α

/-- An integer in `[lo, hi]`, carrying its bounds proof: `IntRange(lo, hi)`. -/
abbrev IntIn (lo hi : Int) := {n : Int // lo ≤ n ∧ n ≤ hi}

/-- The value as a natural number (`range(count)`). -/
def IntIn.toNat {lo hi : Int} (n : IntIn lo hi) : Nat := n.val.toNat

instance : ToString Dec := ⟨showDec⟩

def Dec.toFloat (d : Dec) : Float := Float.ofInt d.mant / (10 : Float) ^ d.exp.toFloat

instance : ClickType String where
  type := .string
  ofValue | .str s => some s | _ => none
  toTokens s := [s]
  toValue := .str

instance : ClickType Int where
  type := .int
  ofValue | .int n => some n | _ => none
  toTokens n := [toString n]
  toValue := .int

instance : ClickType Nat where
  type := .intRange { min := some 0 }
  ofValue | .int n => if 0 ≤ n then some n.toNat else none | _ => none
  toTokens n := [toString n]
  toValue n := .int n

instance (lo hi : Int) : ClickType (IntIn lo hi) where
  type := .intRange { min := some lo, max := some hi }
  ofValue | .int n => if h : lo ≤ n ∧ n ≤ hi then some ⟨n, h⟩ else none | _ => none
  toTokens n := [toString n.val]
  toValue n := .int n.val

instance : ClickType Dec where
  type := .float
  ofValue | .dec d => some d | _ => none
  toTokens d := [showDec d]
  toValue := .dec

instance : ClickType Float where
  type := .float
  ofValue | .dec d => some d.toFloat | _ => none
  toTokens f := [toString f]
  toValue f := match parseDec (toString f) with | some d => .dec d | none => .none

/-- A UUID in canonical text form. -/
abbrev Uuid := {s : String // uuidCanonical s = true}

instance : ClickType Uuid where
  type := .uuid
  ofValue | .str s => if h : uuidCanonical s = true then some ⟨s, h⟩ else none | _ => none
  toTokens u := [u.val]
  toValue u := .str u.val

/-- A real calendar date and time. -/
abbrev ValidDateTime := {d : LeanClick.Timestamp // d.valid = true}

instance : ClickType ValidDateTime where
  type := .dateTime
  ofValue | .dateTime d => if h : d.valid = true then some ⟨d, h⟩ else none | _ => none
  toTokens d := [showDateTime d.val]
  toValue d := .dateTime d.val

instance : ClickType Bool where
  type := .bool
  ofValue | .bool b => some b | _ => none
  toTokens b := [if b then "true" else "false"]
  toValue := .bool
  isFlag := true

/-- A `click.File` value; give the mode with `type := some (File "rb")`. -/
instance : ClickType FileArg where
  type := .file {}
  ofValue | .file f => some f | _ => none
  toTokens f := [f.name]
  toValue := .file

/-- A pair is `Tuple [α, β]`: two values, each with its own type. -/
instance {α β : Type} [ClickType α] [ClickType β] : ClickType (α × β) where
  type := .tuple [ClickType.type α, ClickType.type β]
  ofValue | .list [a, b] => do return (← ClickType.ofValue a, ← ClickType.ofValue b) | _ => none
  toTokens p := ClickType.toTokens p.1 ++ ClickType.toTokens p.2
  toValue p := .list [ClickType.toValue p.1, ClickType.toValue p.2]

instance {α : Type} [ClickType α] : ClickType (Option α) where
  type := ClickType.type α
  ofValue | .none => some none | v => (ClickType.ofValue v).map some
  toTokens | some a => ClickType.toTokens a | none => []
  toValue | some a => ClickType.toValue a | none => .none
  custom := ClickType.custom α
  complete := ClickType.complete α
  isFlag := ClickType.isFlag α
  optional := true

instance {α : Type} [ClickType α] : ClickType (List α) where
  type := ClickType.type α
  ofValue | .list vs => vs.mapM ClickType.ofValue | _ => none
  toTokens as := as.flatMap ClickType.toTokens
  toValue as := .list (as.map ClickType.toValue)
  custom := ClickType.custom α
  complete := ClickType.complete α
  multiple := true

namespace ClickType

/-- A binder name with `_` replaced by `-` (structural, so the kernel can
evaluate it: `click_command` proves `Command.Unambiguous` by `decide`). -/
def dashed (name : String) : String :=
  String.ofList (name.toList.map fun c => if c == '_' then '-' else c)

/-- The option spelling of a binder name: `root_path` is `--root-path`. -/
def spelling (name : String) : String := "--" ++ dashed name

/-- Split a flag declaration into its primary and secondary spelling at `/`
(at `;` when the declaration itself starts with `/`, as click does). -/
def flagSplit (d : String) : String × String :=
  let sep := match d.toList with | '/' :: _ => ';' | _ => '/'
  let rec go : List Char → List Char × List Char
    | [] => ([], [])
    | c :: rest => if c == sep then ([], rest) else let (a, b) := go rest; (c :: a, b)
  let (a, b) := go d.toList
  (String.ofList a, String.ofList b)

/-- The primary and secondary spellings of a typed option. -/
def spellingsOf (isFlag : Bool) (name : String) (decls : List String) : List String × List String :=
  if decls.isEmpty then
    if isFlag then ([spelling name], ["--no-" ++ dashed name]) else ([spelling name], [])
  else if isFlag then
    -- each side trimmed, an empty side left out (`" /-N"` declares only a
    -- secondary spelling), as `Option._parse_decls` does
    let trim := fun (s : String) =>
      String.ofList ((s.toList.dropWhile Char.isWhitespace).reverse.dropWhile Char.isWhitespace).reverse
    decls.foldr (fun d acc =>
      let (a, b) := flagSplit d
      let (a, b) := (trim a, trim b)
      (if a.isEmpty then acc.1 else a :: acc.1, if b.isEmpty then acc.2 else b :: acc.2)) ([], [])
  else (decls, [])

/-- A custom type's conversion as a parameter's `typeCheck`: absent values
are left alone, and a repeated or multi-value parameter converts each item. -/
def customCheck (α : Type) [ClickType α] : Option (Value → Except CallbackError Value) :=
  (ClickType.custom α).map fun (_, conv) =>
    let one := fun (v : Value) => match conv v with
      | .ok w => .ok w
      | .error msg => .error (CallbackError.bad msg)
    fun v => match v with
      | .none => .ok .none
      | .list vs => (vs.mapM one).map .list
      | v => one v

/-- A typed callback (`callback=`) on the parsed value: it sees and returns
an `α`. -/
def typedCallback (α : Type) [ClickType α] (f : α → Except CallbackError α) :
    Value → Except CallbackError Value := fun v =>
  match ClickType.ofValue v with
  | some x => (f x).map ClickType.toValue
  | none => .ok v

/-- The parameter a typed option binder declares. -/
def option (α : Type) [ClickType α] (name : String) (default : Option α := none) (help : String := "")
    (envvar : List String := []) (decls : List String := []) (hidden : Bool := false)
    (showDefault : ShowDefault := .auto) (prompt : PromptText := .off) (confirmationPrompt : PromptText := .off)
    (hideInput : Bool := false) (metavar : Option String := none)
    (type : Option ParamType := none) (callback : Option (α → Except CallbackError α) := none)
    (exposeValue : Bool := true) (password : Bool := false)
    (shellComplete : Option (Env → String → List CompletionItem) := none)
    (count : Bool := false) (flagValue : Option α := none) (showEnvvar : Bool := false)
    (promptRequired : Bool := true) (dynamicDefault : Option (IO α) := none) :
    Param :=
  -- `click.password_option()`: prompted for, hidden, asked twice
  let (prompt, confirmationPrompt, hideInput) :=
    if password then ((if prompt == .off then .auto else prompt), (if confirmationPrompt == .off then .auto else confirmationPrompt), true)
    else (prompt, confirmationPrompt, hideInput)
  let (prim, sec) := spellingsOf (ClickType.isFlag α) name decls
  let base : Param :=
    if ClickType.isFlag α then { name, decl := .option prim (.boolFlag sec), type := .bool }
    else if count then { name, decl := .option prim .count, type := .intRange countBounds }
    else if !promptRequired && prompt != .off then
      -- `prompt_required=False`: prompts only when given without a value
      { name, decl := .option prim (.optValue none), type := type.getD (ClickType.type α) }
    else if let some fv := flagValue then
      -- `is_flag=False, flag_value=...`: the value may be left out
      { name, decl := .option prim (.optValue (ClickType.toTokens fv).head?),
        type := type.getD (ClickType.type α) }
    else
      let t := type.getD (ClickType.type α)
      { name, decl := .option prim (.value (if t.isComposite then t.arity else 1) (ClickType.multiple α)), type := t }
  { base with
    default := (default.map ClickType.toTokens).filter (!·.isEmpty),
    help := if help.isEmpty then none else some help,
    required := default.isNone && dynamicDefault.isNone && prompt == .off && !ClickType.optional α &&
      !ClickType.multiple α && !ClickType.isFlag α && !count,
    dynamicDefault := dynamicDefault.map fun act => do return ClickType.toTokens (← act),
    envvar, hidden, showDefault, showEnvvar,
    metavar := metavar.orElse fun _ => (ClickType.custom α).map fun (n, _) => upper n,
    prompt := (PromptText.spec name prompt confirmationPrompt hideInput).map ({ · with required := promptRequired }),
    exposeValue,
    typeCheck := customCheck α, callback := callback.map (typedCallback α),
    shellComplete := shellComplete.orElse fun _ => ClickType.complete α }

/-- The parameter a typed argument binder declares. -/
def argument (α : Type) [ClickType α] (name : String) (default : Option α := none) (help : String := "")
    (envvar : List String := []) (metavar : Option String := none) (type : Option ParamType := none)
    (callback : Option (α → Except CallbackError α) := none)
    (shellComplete : Option (Env → String → List CompletionItem) := none) :
    Param :=
  let t := type.getD (ClickType.type α)
  let nargs : ArgNargs := if ClickType.multiple α then .star else .exactly (if t.isComposite then t.arity else 1)
  let base : Param := { name, decl := .argument nargs, type := t }
  { base with
    default := (default.map ClickType.toTokens).filter (!·.isEmpty),
    help := if help.isEmpty then none else some help,
    required := default.isNone && !ClickType.optional α && !ClickType.multiple α,
    envvar, metavar, typeCheck := customCheck α, callback := callback.map (typedCallback α),
    shellComplete := shellComplete.orElse fun _ => ClickType.complete α }

/-- The typed value of a parameter. A value the parser produced always
converts (the declared type is `ClickType.type α`); the error is the
defensive case of a mismatched hand-built command. -/
def fetch (α : Type) [ClickType α] (a : ParsedArgs) (name : String) : ClickM α :=
  match ClickType.ofValue (a.get name) with
  | some x => pure x
  | none => throw (.failure s!"parameter {name} does not have the declared type")

end ClickType

/-- `click.prompt(text, default=..., type=...)`: ask until the answer
converts to the type (re-asking with `Error: ...`), and return it. A
missing answer (end of input) is an `Abort`. -/
def Terminal.prompt (α : Type) [ClickType α] (text : String) (default : Option α := none)
    (type : Option ParamType := none) (hideInput : Bool := false)
    (confirmationPrompt : PromptText := .off) (showDefault : Bool := true) : Terminal.ClickM α := do
  let t := type.getD (ClickType.type α)
  let p : Param := {
    name := "value", decl := .option [] (.value 1 false), type := t,
    prompt := PromptText.spec "value" (.text text) confirmationPrompt hideInput,
    showDefault := if showDefault then .auto else .no }
  let dflt := default.map fun d => " ".intercalate (ClickType.toTokens d)
  let some answer ← Terminal.promptParam (fun _ => none) p dflt | throw .abort
  let resolve := match t with | .path sp => sp.resolvePath | _ => false
  let info ← Terminal.statPath answer resolve
  match convert (fun q => if q == answer then info else none) t answer with
  | .error msg => throw (.failure msg)
  | .ok c => match ClickType.ofValue (ParamType.erase t c) with
    | some x => return x
    | none => throw (.failure "the answer does not have the declared type")

/-- Options that share a name and store constants (click's feature
switches, `flag_value`): one flag per `(spelling, value, help)`; the last
one given wins, and `default` names the value used when none is. -/
def ClickType.flagValueGroup (name : String) (values : List (String × String × String))
    (default : Option String := none) : List Param :=
  values.map fun (decl, v, help) =>
    let base : Param := { name, decl := .option [decl] (.constFlag v false) }
    { base with
      name, help := if help.isEmpty then none else some help,
      default := if default == some v then some [v] else none }

/-! ## `click_command` and `click_group` -/

/-- A binder of this type receives the context's object (click's
`make_pass_decorator`); it declares no parameter. -/
abbrev Pass (σ : Type) := σ

declare_syntax_cat clickBinder
declare_syntax_cat clickKw
syntax ident " := " term : clickKw
syntax "(" ident " : " term (" := " term)? ")" (ppSpace str)? (ppSpace "[" sepBy1(clickKw, ", ") "]")? : clickBinder

/-- Declare a click command as a typed Lean function (see the module doc). -/
syntax (docComment)? "click_command " ident (ppSpace "[" sepBy1(clickKw, ", ") "]")? (ppSpace clickBinder)*
  " : " term " := " term : command

/-- Declare a click group as a typed Lean function: the body runs as the
group's callback and returns the object its subcommands receive. -/
syntax (docComment)? "click_group " ident (ppSpace "[" sepBy1(clickKw, ", ") "]")? (ppSpace clickBinder)*
  " : " term " := " term : command

open Lean in
/-- Split a keyword list into names and values. -/
def clickKwPairs (kws : Array (TSyntax `clickKw)) : MacroM (Array (Ident × Term)) :=
  kws.mapM fun k => match k with
    | `(clickKw| $n:ident := $v:term) => pure (n, v)
    | _ => Macro.throwUnsupported

open Lean in
/-- `f (n₁ := v₁) ...` from a call and named arguments. -/
def appendNamedArgs (call : Term) (named : Array (Ident × Term)) : Term :=
  let extra : Array Syntax := named.map fun (n, v) =>
    Syntax.node SourceInfo.none `Lean.Parser.Term.namedArgument
      #[Syntax.atom SourceInfo.none "(", n.raw, Syntax.atom SourceInfo.none ":=", v.raw,
        Syntax.atom SourceInfo.none ")"]
  match call.raw with
  | Syntax.node i k #[f, Syntax.node i' k' as] => ⟨Syntax.node i k #[f, Syntax.node i' k' (as ++ extra)]⟩
  | c => ⟨c⟩

open Lean in
/-- The shared expansion of `click_command` and `click_group`. -/
def expandClick (isGroup : Bool) (doc? : Option (TSyntax `Lean.Parser.Command.docComment)) (name : Ident)
    (ckws : Array (TSyntax `clickKw)) (bs : Array (TSyntax `clickBinder)) (ret body : Term) :
    MacroM Syntax := do
  -- a line holding just `\b` is click's no-rewrap marker, the backspace a
  -- Python docstring's `\b` escape is
  let help : String := match doc? with
    | some d => "\n".intercalate (((d.getDocString).trimAscii.copy.splitOn "\n").map fun l =>
        if l.trimAscii.copy == "\\b" then "\x08"
        -- `\f` ends the help text, a Python docstring's form feed
        else if l.trimAscii.copy == "\\f" then "\x0c" else l)
    | none => ""
  let ckw ← clickKwPairs ckws
  let cliName : Term := match ckw.find? (·.1.getId == `name) with
    | some (_, v) => v
    | none => Syntax.mkStrLit (replaceChar name.getId.toString '_' '-')
  let resolve? := (ckw.find? (·.1.getId == `resolve)).map (·.2)
  let extraParams? := (ckw.find? (·.1.getId == `params)).map (·.2)
  -- `help := "..."` (click's `@command(help=...)`) stands for the docstring
  let helpKw? := (ckw.find? (·.1.getId == `help)).map (·.2)
  let cmdKws := ckw.filter fun (n, _) =>
    n.getId != `name && n.getId != `resolve && n.getId != `params && n.getId != `help
  let mainName := mkIdentFrom name (name.getId ++ `main)
  -- hygienic, so a binder may be called `ctx`
  let ctxId := mkIdent (← Macro.addMacroScope `clickCtx)
  let mut binders : Array (TSyntax `Lean.Parser.Term.bracketedBinder) := #[]
  let mut params : Array Term := #[]
  let mut fetches : Array (Ident × Term) := #[]
  let mut args : Array Term := #[]
  let mut objTy : Option Term := none
  for b in bs do
    match b with
    | `(clickBinder| ($x:ident : $t:term $[:= $d?]?) $[$h?:str]? $[[$kws:clickKw,*]]?) =>
      let kv ← clickKwPairs (match kws with | some k => k.getElems | none => #[])
      -- the callback's own name for the value, so a binder named like the
      -- command does not hide `<command>.main`
      let v := mkIdent (← Macro.addMacroScope x.getId)
      match t with
      | `(Pass $τ:term) =>
        objTy := some τ
        binders := binders.push (← `(bracketedBinder| ($x : $τ)))
        fetches := fetches.push (v, ← `(pure $(ctxId).obj))
        args := args.push v
      | `(Context $τ:term) =>
        objTy := some τ
        binders := binders.push (← `(bracketedBinder| ($x : LeanClick.Context $τ)))
        fetches := fetches.push (v, ← `(pure $ctxId))
        args := args.push v
      | _ =>
        let pname := Syntax.mkStrLit x.getId.toString
        let helpLit : Term := match h? with | some h => h | none => Syntax.mkStrLit ""
        match kv.find? (·.1.getId == `flagValues) with
        | some (_, vals) =>
          let dflt : Term ← match d? with
            | some d => `((some ($d : String)))
            | none => `(none)
          params := params.push (← `(LeanClick.ClickType.flagValueGroup $pname $vals $dflt))
          binders := binders.push (← `(bracketedBinder| ($x : $t)))
          fetches := fetches.push (v, ← `(LeanClick.ClickType.fetch $t $(ctxId).params $pname))
          args := args.push v
        | none =>
          -- typer's rule: no default means an argument, unless the binder is
          -- given option spellings or a prompt
          let optionKw := kv.any fun (n, _) => n.getId == `decls || n.getId == `prompt || n.getId == `password
          let (isArg, τ) ← match t with
            | `(Argument $τ:term) => pure (true, τ)
            | _ => pure (d?.isNone && !optionKw, t)
          -- a default of a refined type (`IntIn 1 100000 := 8000`) is its
          -- value; the refinement is checked when the command compiles
          let dflt : Term ← match d? with
            | some d => `((some (by first | exact ($d : $τ) | exact ⟨$d, by decide⟩)))
            | none => `(none)
          let builder : Ident := mkIdent (if isArg then ``LeanClick.ClickType.argument else ``LeanClick.ClickType.option)
          let call ← `($builder $τ $pname (default := $dflt) (help := $helpLit))
          let call := appendNamedArgs call kv
          params := params.push (← `([$call]))
          binders := binders.push (← `(bracketedBinder| ($x : $τ)))
          fetches := fetches.push (v, ← `(LeanClick.ClickType.fetch $τ $(ctxId).params $pname))
          args := args.push v
    | _ => Macro.throwUnsupported
  -- a command that neither receives the context nor its object works with
  -- any object type (it hands the object on), so it fits any group
  let returnsUnit' := match ret with
    | `(IO Unit) | `(Terminal.ClickM Unit) | `(LeanClick.Terminal.ClickM Unit) | `(ClickM Unit) => true
    | _ => false
  let generic := objTy.isNone && returnsUnit'
  -- not hygienic, so callers can write `cmd (σ := τ)`
  let σId := mkIdent `σ
  -- otherwise the object type is the one the body returns: a group whose
  -- body builds the object (`ctx.obj = Repo(...)`) is an `App Repo`
  let retObj : Term ← match ret with
    | `(IO $τ) | `(Terminal.ClickM $τ) | `(LeanClick.Terminal.ClickM $τ) | `(ClickM $τ) => pure τ
    | _ => `(Unit)
  let σ : Term ← match objTy with
    | some τ => pure τ
    | none => if generic then pure σId else pure retObj
  let helpLit : Term := helpKw?.getD (Syntax.mkStrLit help)
  -- a binder is a declared parameter even when the body ignores it (its
  -- name is the option's spelling, so it cannot be renamed to `_x`)
  let c1 ← `(command| set_option linter.unusedVariables false in
    $[$doc?:docComment]? def $mainName $binders:bracketedBinder* : $ret := $body)
  -- a body returning `Unit` hands the context's object on unchanged
  let returnsUnit := match ret with
    | `(IO Unit) | `(Terminal.ClickM Unit) | `(LeanClick.Terminal.ClickM Unit) | `(ClickM Unit) => true
    | _ => false
  -- a body returning the object type hands its result on (to the next
  -- command of a chain, or a group's subcommands)
  let mut cb ← if returnsUnit then `(do let _ ← ($mainName $args* : $ret); pure $(ctxId).obj)
    else `(do let r ← ($mainName $args* : $ret); pure r)
  for (x, fetch) in fetches.reverse do
    cb ← `($fetch >>= fun $x:ident => $cb)
  let paramList ← match extraParams? with
    | some extra => `(List.flatten [$params,*] ++ $extra)
    | none => `(List.flatten [$params,*])
  let handlers ← match resolve? with
    | some r => `(({ run := fun $ctxId => $cb, resolve := some $r } : LeanClick.Terminal.Handlers $σ))
    | none => `(({ run := fun $ctxId => $cb } : LeanClick.Terminal.Handlers $σ))
  let paramsId := mkIdentFrom name (name.getId ++ `params)
  let c0 ← `(command| /-- The command's parameters. -/ def $paramsId : List LeanClick.Param := $paramList)
  let builder : Ident := mkIdent (if isGroup then ``LeanClick.group else ``LeanClick.command)
  let call ← `($builder $cliName $paramsId (callback := $handlers) (help := $helpLit))
  let call := appendNamedArgs call cmdKws
  let c2 ← if generic then `(command| def $name {$σId : Type} : LeanClick.Terminal.App $σ := $call)
    else `(command| def $name : LeanClick.Terminal.App $σ := $call)
  -- no two parameters share a spelling (the `Unambiguous` hypothesis of
  -- the flag and precedence theorems), proved when the command is compiled
  let unambId := mkIdentFrom name (name.getId ++ `unambiguous)
  -- the kernel `decide` runs on the closed parameter list
  let hp ← `((by show ((LeanClick.table $paramsId).map (·.opt)).Nodup; decide))
  let c3 ← if generic then `(command|
    /-- No two parameters of the command share a spelling, in any context
    whose help option names are distinct (inherited ones included). -/
    theorem $unambId : ∀ {$σId : Type} (s : LeanClick.Settings), s.helpOptionNames.Nodup →
        LeanClick.Command.Unambiguous (@$name $σId) s :=
      fun s h => LeanClick.Command.unambiguous_of $name s $hp h)
    else `(command|
    /-- No two parameters of the command share a spelling, in any context
    whose help option names are distinct (inherited ones included). -/
    theorem $unambId : ∀ s : LeanClick.Settings, s.helpOptionNames.Nodup →
        LeanClick.Command.Unambiguous $name s :=
      fun s h => LeanClick.Command.unambiguous_of $name s $hp h)
  return mkNullNode #[c1, c0, c2, c3]

macro_rules
  | `($[$doc?:docComment]? click_command $name:ident $[[$ckws:clickKw,*]]? $bs:clickBinder* : $ret := $body) =>
    expandClick false doc? name (match ckws with | some k => k.getElems | none => #[]) bs ret body
  | `($[$doc?:docComment]? click_group $name:ident $[[$ckws:clickKw,*]]? $bs:clickBinder* : $ret := $body) =>
    expandClick true doc? name (match ckws with | some k => k.getElems | none => #[]) bs ret body

end LeanClick
