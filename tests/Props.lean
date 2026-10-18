import LeanClick

/-!
# Property tests: fast feedback where there is no proof

Generates values with a seeded generator and checks the properties that are
tested rather than proved. Not a substitute for the theorems; the check that
runs in under a second so a mistake surfaces while the change is still in
your head.

Run with `lake exe props`, or a different seed with `lake exe props 12345`.
A failure prints the seed and the value, so it reproduces exactly.
-/

/-- xorshift64*: four lines, no dependency, deterministic from the seed. -/
structure Rng where
  s : UInt64
  deriving Inhabited

def Rng.next (r : Rng) : UInt64 × Rng :=
  let x := r.s
  let x := x ^^^ (x >>> 12)
  let x := x ^^^ (x <<< 25)
  let x := x ^^^ (x >>> 27)
  (x * 2685821657736338717, { s := x })

def Rng.nat (r : Rng) (bound : Nat) : Nat × Rng :=
  let (v, r) := r.next
  (if bound == 0 then 0 else v.toNat % bound, r)

/-- A property: a name and a check that, given a generator, either passes or
returns a description of the failing value. -/
structure Property where
  name : String
  check : Rng → Option String × Rng

open LeanClick

def Rng.pick {α : Type} [Inhabited α] (r : Rng) (xs : List α) : α × Rng :=
  let (i, r) := r.nat xs.length
  (xs.getD i default, r)

def Rng.list {α : Type} (r : Rng) (n : Nat) (g : Rng → α × Rng) : List α × Rng :=
  (List.range n).foldl (fun (acc, r) _ => let (x, r) := g r; (acc ++ [x], r)) ([], r)

def words : List String := ["alpha", "be", "c", "delta-epsilon", "x", "longerwordhere", "mid", "q-r-s", "tt"]

def genText (r : Rng) : String × Rng :=
  let (n, r) := r.nat 25
  let (ws, r) := r.list n (fun r => r.pick words)
  (" ".intercalate ws, r)

def nonWs (s : String) : List Char := s.toList.filter (!·.isWhitespace)

def wrapWidth : Property := ⟨"wrap: no line exceeds the width when words fit", fun r =>
  let (t, r) := genText r
  let (w, r) := r.nat 40
  let w := w + 16
  let ls := Text.wrap w "  " "  " t
  (if ls.all (·.length ≤ w) then none else some s!"width {w} text {t}", r)⟩

def wrapKeepsText : Property := ⟨"wrap: the words survive, in order", fun r =>
  let (t, r) := genText r
  let (w, r) := r.nat 30
  let out := Text.fill (w + 12) "" "" t
  (if nonWs out == nonWs t then none else some s!"text {t}", r)⟩

def intRoundtrip : Property := ⟨"parseInt inverts toString", fun r =>
  let (a, r) := r.nat 2000000
  let n : Int := (a : Int) - 1000000
  (if Text.parseInt (toString n) == some n then none else some s!"{n}", r)⟩

def decSame (a b : Dec) : Bool := Dec.le a b && Dec.le b a

def decRoundtrip : Property := ⟨"parseDec inverts showDec", fun r =>
  let (m, r) := r.nat 2000000
  let (e, r) := r.nat 12
  let d : Dec := ⟨(m : Int) - 1000000, e⟩
  (match Text.parseDec (Text.showDec d) with
   | some d' => if decSame d d' then none else some s!"{repr d} -> {Text.showDec d}"
   | none => some s!"{repr d} -> {Text.showDec d} unparsed", r)⟩

def opts : List String := ["--bound", "--count", "--host", "--port", "--reload", "--no-reload", "--verbose", "--app"]

def closeSubset : Property := ⟨"closeMatches: at most 3, all from the possibilities, exact match first", fun r =>
  let (w, r) := r.pick opts
  let (k, r) := r.nat 3
  let probe := String.ofList (w.toList.take (w.length - k))
  let ms := Text.closeMatches probe opts
  let exact := Text.closeMatches w opts
  (if ms.length ≤ 3 && ms.all (opts.contains ·) && exact.head? == some w then none
   else some s!"{probe}: {ms}", r)⟩

def cli : Command Unit := Command.command { name := "cli", params := [
  Param.opt ["--name", "-n"], (Param.opt ["--port"] .int).withDefault "8000",
  Param.flag ["--reload/--no-reload"], Param.count ["-v", "--verbose"],
  Param.opt ["--tag"] (multiple := true), Param.arg "files" (nargs := .star)] }

def tokens : List (List String) :=
  [["--name", "a"], ["-n", "b"], ["--name=c"], ["-nd"], ["--port", "1"], ["--port=2"], ["--reload"],
   ["--no-reload"], ["-v"], ["-vv"], ["--verbose"], ["--tag", "t1"], ["--tag=t2"], ["file.txt"], ["other"]]

def genArgv (r : Rng) : List String × Rng :=
  let (n, r) := r.nat 8
  let (chunks, r) := r.list n (fun r => r.pick tokens)
  (chunks.flatten, r)

def declaredAccepted : Property := ⟨"declared spellings never give 'No such option'", fun r =>
  let (argv, r) := genArgv r
  (match (cli.dispatch "cli" argv {}).halt with
   | some (.error e) => some s!"{argv}: {e.message}"
   | _ => none, r)⟩

def lastWins : Property := ⟨"value options and flag pairs: the last occurrence wins", fun r =>
  let (argv, r) := genArgv r
  let d := cli.dispatch "cli" argv {}
  let lastVal := fun (keys : List String) (eqPre : String) (attached : List (String × String)) (dflt : String) =>
    let rec go : List String → String → String
      | [], acc => acc
      | t :: rest, acc =>
        if keys.contains t then match rest with | v :: rest' => go rest' v | [] => acc
        else if t.startsWith eqPre then go rest (t.drop eqPre.length).copy
        else match attached.lookup t with
          | some v => go rest v
          | none => go rest acc
    go argv dflt
  let expectName := lastVal ["--name", "-n"] "--name=" [("-nd", "d")] "None"
  let expectPort := lastVal ["--port"] "--port=" [] "8000"
  let expectReload := (argv.filter (fun t => t == "--reload" || t == "--no-reload")).getLast?.map (· == "--reload")
  match d.frames with
  | [f] =>
    let got := ((f.args.get "name").render, (f.args.get "port").render, (f.args.get "reload").render)
    let want := (expectName, expectPort, match expectReload with | some true => "True" | _ => "False")
    (if got == want then none else some s!"{argv}: got {got} want {want}", r)
  | _ => (some s!"{argv}: no frame", r)⟩

def countIsOccurrences : Property := ⟨"count equals the number of v characters", fun r =>
  let (argv, r) := genArgv r
  let n := argv.foldl (fun acc t =>
    if t == "-v" || t == "--verbose" then acc + 1 else if t == "-vv" then acc + 2 else acc) 0
  match (cli.dispatch "cli" argv {}).frames with
  | [f] => (if (f.args.get "verbose").render == toString n then none else some s!"{argv}", r)
  | _ => (some s!"{argv}: no frame", r)⟩

def unpackConserves : Property := ⟨"unpack: arguments and leftover use every token once", fun r =>
  let (n, r) := r.nat 7
  let (toks, r) := r.list n (fun r => r.pick ["a", "b", "c", "d"])
  let (k, r) := r.nat 4
  let specs : List ArgNargs := (List.range k).map fun i => if i == 1 then .star else .exactly (i % 2 + 1)
  let (slots, rest) := unpack specs toks
  let used := slots.flatMap fun s => match s with
    | .single v => v.toList | .tuple vs => vs.filterMap id | .star vs => vs
  (if used.length + rest.length == toks.length then none else some s!"{specs.length} specs, {toks}", r)⟩

def shortHelpFits : Property := ⟨"short help fits its limit", fun r =>
  let (t, r) := genText r
  let (w, r) := r.nat 40
  let lim := w + 3
  let sh := Text.shortHelp t lim
  (if sh.length ≤ lim then none else some s!"{lim}: {sh}", r)⟩

def helpListsSpellings : Property := ⟨"help lists every spelling", fun r =>
  let page := cli.getHelp (cli.settings none "cli") "cli"
  let spell := (cli.allParams (cli.settings none "cli")).flatMap Param.allOpts
  (if spell.all (fun o => (page.splitOn o).length > 1) then none else some page, r)⟩

def properties : List Property :=
  [wrapWidth, wrapKeepsText, intRoundtrip, decRoundtrip, closeSubset, declaredAccepted, lastWins,
   countIsOccurrences, unpackConserves, shortHelpFits, helpListsSpellings]

def main (args : List String) : IO UInt32 := do
  let seed := (args.head? >>= String.toNat?).getD 88172645463325252
  let cases := 200
  let mut rng : Rng := { s := UInt64.ofNat seed }
  let mut failures := 0
  for p in properties do
    let mut failed : Option String := none
    for _ in [0:cases] do
      if failed.isSome then break
      let (res, rng') := p.check rng
      rng := rng'
      failed := res
    match failed with
    | none => IO.println s!"ok    {p.name} ({cases} cases)"
    | some why =>
      failures := failures + 1
      IO.println s!"FAIL  {p.name}: {why}  (seed {seed})"
  IO.println s!"{properties.length} properties, {failures} failing (seed {seed})"
  return if failures == 0 then 0 else 1
