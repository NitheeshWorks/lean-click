import LeanClick

/-!
Port of click's `examples/inout/inout.py`: `cat` that reads files or
standard input (`-`) and writes to a file or standard output (`-`).
-/

open LeanClick LeanClick.Terminal

/-- This script works similar to the Unix `cat` command but it writes
into a specific file (which could be the standard output as denoted by
the ``-`` sign).

\b
Copy stdin to stdout:
    inout - -

\b
Copy foo.txt and bar.txt to stdout:
    inout foo.txt bar.txt -

\b
Write stdin into the file foo.txt
    inout - foo.txt -/
click_command cli
    (input : List FileArg) [type := some (File "rb")]
    (output : FileArg) [type := some (File "wb")]
    : ClickM Unit := do
  let output ← output.stream
  for f in input do
    let f ← f.stream
    repeat
      let chunk ← f.read 1024
      if chunk.isEmpty then break
      output.write chunk
      output.flush

def main (args : List String) : IO UInt32 := cli.run () args (prog := "inout")
