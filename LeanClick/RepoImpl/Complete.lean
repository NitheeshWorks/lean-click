import LeanClick.RepoImpl.Parse

/-!
# Shell completion

click's `shell_completion` protocol, pure part: given the complete words
before the cursor and the incomplete word, which command or parameter
completes it, and with what. `LeanClick.Terminal` reads the shell's
`COMP_WORDS`/`COMP_CWORD` and prints the items in each shell's format.

Resolution is resilient like click's (`resilient_parsing=True`): it follows
subcommands through the words, and a word the scanner rejects simply stops
the descent.
-/

namespace LeanClick

open Text

variable {σ : Type}

/-- click's `CompletionItem`: the value, the kind of completion the shell
should do (`plain`, `file`, `dir`), and help text. -/
structure CompletionItem where
  value : String
  type : String := "plain"
  help : Option String := none
  deriving DecidableEq, Repr, Inhabited

/-- Where completion resolved to: the innermost command, its settings and
path, the parameters given on the command line, the positionals it has,
and for a command inside a chained group, that group with the commands
already used. -/
structure CompletionCtx (σ : Type) where
  cmd : Command σ
  settings : Settings
  given : List String
  positionals : List String
  chainParent : Option (Command σ × List String) := none

/-- `_resolve_context`. -/
def resolveCompletion (prog : String) :
    Nat → Command σ → Settings → List String → Option (Command σ × List String) → CompletionCtx σ
  | 0, cmd, s, _, cp => ⟨cmd, s, [], [], cp⟩
  | fuel + 1, cmd, s, args, cp =>
    -- a chained subcommand parses without interspersed arguments
    let cmd' := if cp.isSome then cmd.asChained else cmd
    match scan (cmd'.scanCfg s) args with
    | .error _ => ⟨cmd, s, [], [], cp⟩
    | .ok sc =>
      let positional := sc.largs ++ sc.rargs
      let given := sc.events.map (·.dest)
      let here : CompletionCtx σ := ⟨cmd, s, given, positional, cp⟩
      let leftover := (unpack ((cmd.allParams s).filter Param.isArgument |>.map Param.argNargs) positional).2
      let leftover := if cmd.isGroup || cp.isSome then leftover else []
      match leftover with
      | [] => here
      | name :: rest =>
        -- inside a chain the next word may name a sibling
        let (owner, used) := match cp with
          | some (g, used) => (g, used)
          | none => (cmd, [])
        if !(cmd.isGroup || cp.isSome) then here else
        let target := if cmd.isGroup then cmd else owner
        match target.findSub name with
        | none => here
        | some c =>
          let cs := c.settings (some s) name
          let chainInfo := if target.spec.chain then some (target, used ++ [name])
            else cp.map fun (g, u) => (g, u ++ [name])
          resolveCompletion prog fuel c cs rest chainInfo

/-- Does the word start an option (`_start_of_option`)? -/
def startsOption (cfg : ScanCfg) (w : String) : Bool :=
  match w.toList with
  | c :: _ => cfg.prefixes.contains [c]
  | [] => false

/-- `_complete_visible_commands`. -/
def Command.completeCommands (cmd : Command σ) (s : Settings) (incomplete : String) : List CompletionItem :=
  (cmd.sortedSubcommands.filter fun c => !c.spec.hidden && c.name.startsWith incomplete).map fun c =>
    { value := c.name, help := let h := c.shortHelpStr (s.width - 6); if h.isEmpty then none else some h }

/-- `Command.shell_complete`: option names, when the word looks like one;
plus sibling commands inside a chain; plus subcommands for a group. -/
def CompletionCtx.completeCommand (c : CompletionCtx σ) (incomplete : String) : List CompletionItem :=
  let opts := match incomplete.toList with
    | ch :: _ =>
      if ch.isAlphanum then [] else
      (c.cmd.allParams c.settings).flatMap fun p =>
        let multi := match p.decl with
          | .option _ (.value _ true) | .option _ (.constFlag _ true) => true
          | _ => false
        if !p.isOption || p.hidden || (!multi && c.given.contains p.name) then []
        else (p.allOpts.filter (·.startsWith incomplete)).map fun o => { value := o, help := p.help }
    | [] => []
  let subs := if c.cmd.isGroup then c.cmd.completeCommands c.settings incomplete else []
  let siblings := match c.chainParent with
    | some (g, used) => (g.completeCommands c.settings incomplete).filter fun i => !used.contains i.value
    | none => []
  subs ++ opts ++ siblings

/-- A parameter's value completions: its `shell_complete` function, else
the type's (`ParamType.shell_complete`). -/
def Param.complete (p : Param) (incomplete : String) : List CompletionItem :=
  match p.shellComplete with
  | some f => (f incomplete).map fun v => { value := v }
  | none =>
  match p.type with
  | .choice cs cS =>
    let norm := fun (x : String) => if cS then x else lower x
    (cs.filter fun c => (normChoice cS c).startsWith (norm incomplete)).map fun c => { value := normChoice cS c }
  | .path spec => [{ value := incomplete, type := if spec.dirOkay && !spec.fileOkay then "dir" else "file" }]
  | .file _ => [{ value := incomplete, type := "file" }]
  | _ => []

/-- `_resolve_incomplete` and the completion it leads to. -/
def CompletionCtx.complete (c : CompletionCtx σ) (args : List String) (incomplete : String) :
    List CompletionItem :=
  let cfg := c.cmd.scanCfg c.settings
  let (args, incomplete) :=
    if incomplete == "=" then (args, "")
    else if incomplete.toList.contains '=' && startsOption cfg incomplete then
      let (n, v) := splitEq incomplete
      (args ++ [n], v.getD "")
    else (args, incomplete)
  if !args.contains "--" && startsOption cfg incomplete then c.completeCommand incomplete
  else
    let ps := c.cmd.allParams c.settings
    -- the last complete word is an option still waiting for its value
    let waiting := ps.find? fun p =>
      match p.decl with
      | .option opts (.value n _) =>
        let recent := (args.reverse.take n)
        match recent.find? (startsOption cfg) with
        | some o => opts.contains o && (recent.takeWhile (fun w => !startsOption cfg w)).length < n
        | none => false
      | _ => false
    match waiting with
    | some p => p.complete incomplete
    | none =>
      -- the first argument without its value
      let argPs := ps.filter Param.isArgument
      let (slots, _) := unpack (argPs.map Param.argNargs) c.positionals
      let open_ := (argPs.zip slots).find? fun (_, sl) => match sl with
        | .single v => v.isNone
        | .tuple vs => vs.any Option.isNone
        | .star _ => true
      match open_ with
      | some (p, _) => p.complete incomplete
      | none => c.completeCommand incomplete

/-- Complete for a top-level command invoked as `prog`: the words before
the cursor (without the program name) and the word being completed. -/
def Command.complete (cmd : Command σ) (prog : String) (args : List String) (incomplete : String) :
    List CompletionItem :=
  let s := cmd.settings (some {}) prog
  let c := resolveCompletion prog (args.length + 1) cmd s args none
  c.complete args incomplete

/-- Python's `shlex.split`, as click's `split_arg_string` uses it: words
separated by whitespace, single and double quotes, backslash escapes; an
unterminated quote ends the last word. -/
def splitArgString (s : String) : List String :=
  let rec go : List Char → List Char → Bool → Option Char → List String → Nat → List String
    | _, cur, started, _, acc, 0 => (if started then acc ++ [String.ofList cur.reverse] else acc)
    | [], cur, started, _, acc, _ => if started then acc ++ [String.ofList cur.reverse] else acc
    | c :: cs, cur, started, q, acc, f + 1 =>
      match q with
      | some qc =>
        if c == qc then go cs cur true none acc f
        else if c == '\\' && qc == '"' then
          match cs with
          | d :: cs' => go cs' (d :: cur) true q acc f
          | [] => go cs cur true q acc f
        else go cs (c :: cur) true q acc f
      | none =>
        if c.isWhitespace then
          go cs [] false none (if started then acc ++ [String.ofList cur.reverse] else acc) f
        else if c == '\'' || c == '"' then go cs cur true (some c) acc f
        else if c == '\\' then
          match cs with
          | d :: cs' => go cs' (d :: cur) true none acc f
          | [] => go cs cur true none acc f
        else go cs (c :: cur) true none acc f
  go s.toList [] false none [] (s.length + 1)

/-- The completion line for each shell (`format_completion`). -/
def CompletionItem.format (shell : String) (i : CompletionItem) : String :=
  match shell with
  | "zsh" =>
    let help := i.help.getD "_"
    let help := if help.isEmpty then "_" else help
    let value := if help != "_" then String.intercalate "\\:" (i.value.splitOn ":") else i.value
    s!"{i.type}\n{value}\n{help}"
  | "fish" =>
    match i.help with
    | some h =>
      if h.isEmpty then s!"{i.type},{i.value}"
      else
        let h := String.ofList (h.toList.flatMap fun c => if c == '\n' then ['\\', 'n'] else if c == '\t' then [' '] else [c])
        s!"{i.type},{i.value}\t{h}"
    | none => s!"{i.type},{i.value}"
  | _ => s!"{i.type},{i.value}"

/-- The environment variable that triggers completion: `_FOO_BAR_COMPLETE`
for a program `foo-bar`. -/
def completeVar (prog : String) : String :=
  "_" ++ String.ofList (prog.toList.map fun c => if c == '-' || c == '.' then '_' else c.toUpper) ++ "_COMPLETE"

/-- The shell function that asks the program for completions (each shell's
`source` script). Written for this port; it speaks click's protocol. -/
def completionScript (shell prog : String) : Option String :=
  let var := completeVar prog
  let fn := "_" ++ String.ofList (prog.toList.map fun c => if c.isAlphanum then c.toLower else '_') ++ "_completion"
  match shell with
  | "bash" => some s!"{fn}() \{
    local IFS=$'\\n'
    local response
    response=$(env COMP_WORDS=\"$\{COMP_WORDS[*]}\" COMP_CWORD=$COMP_CWORD {var}=bash_complete $1)
    COMPREPLY=()
    for line in $response; do
        local kind=$\{line%%,*}
        local value=$\{line#*,}
        if [[ $kind == dir ]]; then compopt -o dirnames; COMPREPLY=();
        elif [[ $kind == file ]]; then compopt -o default; COMPREPLY=();
        else COMPREPLY+=($value); fi
    done
    return 0
}
complete -o nosort -F {fn} {prog}
"
  | "zsh" => some s!"#compdef {prog}
{fn}() \{
    local -a described plain
    local kind value help
    local -a lines
    lines=(\"$\{(@f)$(env COMP_WORDS=\"$\{words[*]}\" COMP_CWORD=$((CURRENT-1)) {var}=zsh_complete {prog})}\")
    while (( $#lines >= 3 )); do
        kind=$lines[1]; value=$lines[2]; help=$lines[3]; lines=($\{lines[4,-1]})
        if [[ $kind == dir ]]; then _path_files -/
        elif [[ $kind == file ]]; then _path_files -f
        elif [[ $help == _ ]]; then plain+=($value)
        else described+=(\"$value:$help\"); fi
    done
    (( $#described )) && _describe 'values' described
    (( $#plain )) && compadd -U -V unsorted -a plain
}
compdef {fn} {prog}
"
  | "fish" => some s!"function {fn}
    set -l response (env {var}=fish_complete COMP_WORDS=(commandline -cp) COMP_CWORD=(commandline -t) {prog})
    for line in $response
        set -l kind (string split -m 1 , -- $line)[1]
        set -l value (string split -m 1 , -- $line)[2]
        switch $kind
            case dir
                __fish_complete_directories $value
            case file
                __fish_complete_path $value
            case '*'
                echo $value
        end
    end
end
complete --no-files --command {prog} --arguments \"({fn})\"
"
  | _ => none

end LeanClick
