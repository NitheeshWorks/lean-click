import LeanClick

/-!
Port of click's `examples/validation/validation.py`: parameters validated
by a callback, by a custom type, and by hand in the command.

Python's `urllib.parse.urlparse` and its `ParseResult` have no Lean
counterpart, so the part of them the example uses is ported here.
-/

open LeanClick LeanClick.Terminal

/-- `urllib.parse.ParseResult`. -/
structure ParseResult where
  scheme : String
  netloc : String
  path : String
  params : String
  query : String
  fragment : String
  deriving DecidableEq, Inhabited

/-- `repr(ParseResult(...))`. -/
def ParseResult.pyRepr (r : ParseResult) : String :=
  s!"ParseResult(scheme={Text.pyRepr r.scheme}, netloc={Text.pyRepr r.netloc}, path={Text.pyRepr r.path}, " ++
  s!"params={Text.pyRepr r.params}, query={Text.pyRepr r.query}, fragment={Text.pyRepr r.fragment})"

/-- `ParseResult.geturl()` (`urlunparse`). -/
def ParseResult.geturl (r : ParseResult) : String :=
  let url := if r.params.isEmpty then r.path else s!"{r.path};{r.params}"
  let url := if !r.netloc.isEmpty || r.scheme == "file" then
      "//" ++ r.netloc ++ (if !url.isEmpty && !url.startsWith "/" then "/" else "") ++ url
    else url
  let url := if r.scheme.isEmpty then url else r.scheme ++ ":" ++ url
  let url := if r.query.isEmpty then url else url ++ "?" ++ r.query
  if r.fragment.isEmpty then url else url ++ "#" ++ r.fragment

/-- The schemes `urlparse` splits `;params` for (`uses_params`). -/
def usesParams : List String :=
  ["", "ftp", "hdl", "prospero", "http", "imap", "https", "shttp", "rtsp", "rtsps", "rtspu",
   "sip", "sips", "mms", "sftp", "tel"]

/-- Split at the first occurrence of `c`: before, and after when found. -/
def splitAt (s : String) (c : Char) : String × Option String :=
  match s.splitOn (String.singleton c) with
  | a :: b :: more => (a, some (String.singleton c |>.intercalate (b :: more)))
  | _ => (s, none)

/-- `urllib.parse.urlparse(url)`. -/
def urlparse (url : String) : ParseResult :=
  -- leading and trailing C0 controls and spaces go, and so do tabs and newlines
  let url := String.ofList ((url.toList.dropWhile (·.toNat ≤ 32)).reverse.dropWhile (·.toNat ≤ 32)).reverse
  let url := String.ofList (url.toList.filter fun c => c != '\t' && c != '\r' && c != '\n')
  let schemeChars := fun (c : Char) => c.isAlphanum || c == '+' || c == '-' || c == '.'
  let (scheme, rest) := match url.toList.findIdx? (· == ':') with
    | some i =>
      let cs := url.toList.take i
      match cs with
      | c :: _ =>
        if c.toNat < 128 && c.isAlpha && cs.all schemeChars then
          ((String.ofList cs).toLower, String.ofList (url.toList.drop (i + 1)))
        else ("", url)
      | [] => ("", url)
    | none => ("", url)
  let (netloc, rest) := if rest.startsWith "//" then
      let body := rest.toList.drop 2
      let i := (body.findIdx? fun c => c == '/' || c == '?' || c == '#').getD body.length
      (String.ofList (body.take i), String.ofList (body.drop i))
    else ("", rest)
  let (rest, fragment) := match splitAt rest '#' with
    | (a, some f) => (a, f)
    | (a, none) => (a, "")
  let (rest, query) := match splitAt rest '?' with
    | (a, some q) => (a, q)
    | (a, none) => (a, "")
  let (path, params) :=
    if usesParams.contains scheme && rest.toList.contains ';' then
      let cs := rest.toList
      let start := match (cs.reverse.findIdx? (· == '/')) with
        | some j => cs.length - 1 - j
        | none => 0
      match ((cs.drop start).findIdx? (· == ';')) with
      | some k => (String.ofList (cs.take (start + k)), String.ofList (cs.drop (start + k + 1)))
      | none => (rest, "")
    else (rest, "")
  { scheme, netloc, path, params, query, fragment }

def validateCount (value : Int) : Except CallbackError Int :=
  if value < 0 || value % 2 != 0 then throw (.bad "Should be a positive, even integer.")
  else pure value

def ParseResult.toValue (r : ParseResult) : Value :=
  .record "ParseResult" [("scheme", .str r.scheme), ("netloc", .str r.netloc), ("path", .str r.path),
    ("params", .str r.params), ("query", .str r.query), ("fragment", .str r.fragment)]

/-- The `URL` type: an HTTP or HTTPS URL, parsed. -/
instance : ClickType ParseResult where
  type := .string
  ofValue
    | .record "ParseResult" [("scheme", .str a), ("netloc", .str b), ("path", .str c),
        ("params", .str d), ("query", .str e), ("fragment", .str f)] => some ⟨a, b, c, d, e, f⟩
    | _ => none
  toTokens r := [r.geturl]
  toValue := ParseResult.toValue
  custom := some ("url", fun v => match v with
    | .str s =>
      let r := urlparse s
      if r.scheme != "http" && r.scheme != "https" then
        .error s!"invalid URL scheme ({r.scheme}). Only HTTP URLs are allowed"
      else .ok r.toValue
    | v => .ok v)

/-- Validation.

This example validates parameters in different ways.  It does it
through callbacks, through a custom type as well as by validating
manually in the function. -/
click_command cli [params := [versionOption "1.0.0"]]
    (count : Int := 2) "A positive even number." [callback := some validateCount]
    (foo : Option String := none) "A mysterious parameter."
    (url : Option ParseResult := none) "A URL"
    : ClickM Unit := do
  if foo.isSome && foo != some "wat" then
    throw (badParameter "If a value is provided it needs to be the value \"wat\"." (paramHint := ["--foo"]))
  echo s!"count: {count}"
  echo s!"foo: {foo.getD "None"}"
  echo s!"url: {match url with | some u => u.pyRepr | none => "None"}"

def main (args : List String) : IO UInt32 := cli.run () args (prog := "validation")
