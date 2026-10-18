import examples.docs.runner

/-!
Port of click's `docs/utils.md`: its code blocks, in page order, each under
its block number `docs/utils.md#block-<n>` (`tests/golden/docblocks.py`).
The fragments are functions here, each named for its block; the `less`
commands are programs, and `tests/golden/generate.py` runs block 6's
(`tests/Conformance.lean` replays it through `programs`).
-/

open LeanClick LeanClick.Terminal Text

namespace Docs.Utils

-- block 1
def b1 : IO Unit := echo "Hello World!"

-- block 2: bytes are written as they are
def b2 : IO Unit := echoBytes "☃".toUTF8 (nl := false)

-- block 3
def b3 : IO Unit := echo "Hello World!" (err := true)

-- block 4
def b4 : IO Unit := do
  echo (style "Hello World!" (fg := "green"))
  echo (style "Some more text" (bg := "blue") (fg := "white"))
  echo (style "ATTENTION" (blink := true) (bold := true))

-- block 5
def b5 : IO Unit := do
  secho "Hello World!" (fg := "green")
  secho "Some more text" (bg := "blue") (fg := "white")
  secho "ATTENTION" (blink := true) (bold := true)

-- block 6
namespace B6
click_command less : IO Unit :=
  echoViaPager ("\n".intercalate ((List.range 200).map (s!"Line {·}")))
end B6

-- block 7: the generator's chunks
namespace B7
def generateOutput : List String := (List.range 50000).map (s!"Line {·}\n")

click_command less : IO Unit := echoViaPager generateOutput
end B7

-- block 8
namespace B8
click_command less : IO Unit :=
  withPagerFile fun pager => do
    for idx in [0:50000] do pager.putStrLn (toString idx)
end B8

-- block 9
def b9 : IO Unit := clear

-- block 10
def b10 : IO Unit := do
  echo "Continue? [yn] " (nl := false)
  let c ← getchar
  echo ""
  if c == "y" then echo "We will go on"
  else if c == "n" then echo "Abort!"
  else echo "Invalid input :("

-- block 11
def b11 : IO Unit := pause

-- block 12
def getCommitMessage : IO (Option String) := do
  let marker := "# Everything below is ignored\n"
  let message ← edit ("\n\n" ++ marker)
  return message.map fun m => rstripNewlines ((m.splitOn marker).headD "")

-- block 13
def b13 : IO Unit := discard <| edit (filename := some "/etc/passwd")

-- block 14
def b14 : IO Unit := do
  discard <| launch "https://click.palletsprojects.com/"
  discard <| launch "/my/downloaded/file.txt" (locate := true)

-- block 15
def b15 : IO Unit := echo s!"Path: {formatFilename "foo.txt"}"

-- block 16
def b16 : ClickM (IO.FS.Stream × IO.FS.Stream) := do
  let stdout ← openFile "-" "w"
  let testFile ← openFile "test.txt" "w"
  return (stdout, testFile)

-- block 17
def b17 (filename : String) : ClickM Unit := do
  let f ← openFile filename "w"
  f.putStr "Hello World!\n"

-- block 18: the page's fragment is not valid Python (its body is not
-- indented, and `ConfigParser` is Python 2's name); this is what it means:
-- every `section.key` of the application's `config.ini`
def appName := "My Application"

def readConfig : IO (List (String × String)) := do
  let cfg := s!"{← getAppDir appName}/config.ini"
  let text ← try IO.FS.readFile cfg catch _ => pure ""
  let mut sect := ""
  let mut rv := []
  for line in splitLines text do
    let line := strip line
    if line.startsWith "[" && line.endsWith "]" then
      sect := ((line.drop 1).dropEnd 1).toString
    else if let key :: value@(_ :: _) := line.splitOn "=" then
      if !line.startsWith "#" && !line.startsWith ";" then
        rv := rv ++ [(s!"{sect}.{(strip key).toLower}", strip ("=".intercalate value))]
  return rv

-- blocks 19 to 24: the page's names stand for the caller's
variable (allTheUsersToProcess : List String) (modifyTheUser : String → IO Unit)

-- block 19
def b19 : IO Unit := do
  for user in allTheUsersToProcess do modifyTheUser user

-- block 20
def b20 : IO Unit :=
  withProgressbar (progressbar allTheUsersToProcess) fun bar => do
    for user in bar do modifyTheUser user

-- block 21
def b21 (numberOfUsers : Nat) : IO Unit :=
  withProgressbar (progressbar allTheUsersToProcess (length := some numberOfUsers)) fun bar => do
    for user in bar do modifyTheUser user

-- block 22
def b22 : IO Unit :=
  withProgressbar (progressbar [1, 2, 3]) fun bar => do
    for x in bar do
      echo s!"sleep({x})..."
      IO.sleep (x * 1000).toUInt32

-- block 23
def b23 (numberOfUsers : Nat) : IO Unit :=
  withProgressbar (progressbar allTheUsersToProcess (label := "Modifying user accounts")
      (length := some numberOfUsers)) fun bar => do
    for user in bar do modifyTheUser user

-- block 24
structure Archive where
  size : Nat
  extract : IO Unit

def b24 (zipFile : List Archive) (totalSize : Nat) : IO Unit :=
  withProgressbar (progressbarLength totalSize (label := "Unzipping archive")) fun bar => do
    for archive in zipFile do
      archive.extract
      bar.update archive.size.toFloat

def programs : Docs.Programs :=
  [("less@6", Docs.run B6.less ()), ("less@7", Docs.run B7.less ()), ("less@8", Docs.run B8.less ())]

end Docs.Utils

def main (args : List String) : IO UInt32 := Docs.main Docs.Utils.programs args
