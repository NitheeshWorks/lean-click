import LeanClick

/-!
# Example: `fastapi dev` / `fastapi run`

fastapi-cli's typer app, as typed click commands. The bodies print what
they would hand to uvicorn, so this runs anywhere:

```
lake exe example dev main.py --port 9000 --no-reload
lake exe example --help
PORT=7000 lake exe example run
```
-/

open LeanClick LeanClick.Terminal

def serve (mode : String) (path : Option String) (host : String) (port : Nat) (reload : Bool) :
    IO Unit := do
  IO.println s!"Starting FastAPI in {mode} mode"
  IO.println s!"  path:   {path.getD "(auto-discovery)"}"
  IO.println s!"  server: http://{host}:{port}  reload={reload}"

/-- Run a FastAPI app in development mode. -/
click_command dev
    (path : Argument (Option String)) "A path to a Python file or package directory." [type := Path (exists_ := true)]
    (host : String := "127.0.0.1") "The host to serve on."
    (port : Nat := 8000) "The port to serve on." [envvar := ["PORT"]]
    (reload : Bool := true) "Enable auto-reload of the server when (code) files change."
    (root_path : String := "") "The root path."
    (app : Option String := none) "The name of the variable that contains the app."
    (entrypoint : Option String := none) "The app import string." [decls := ["--entrypoint", "-e"]]
    (proxy_headers : Bool := true) "Enable/Disable X-Forwarded-Proto, X-Forwarded-For, X-Forwarded-Port."
    (verbose : Bool := false) "Show detailed startup output." [decls := ["--verbose", "-v"]]
    : IO Unit :=
  serve "development" path host port reload

/-- Run a FastAPI app in production mode. -/
click_command run
    (path : Argument (Option String)) "A path to a Python file or package directory." [type := Path (exists_ := true)]
    (host : String := "0.0.0.0") "The host to serve on."
    (port : Nat := 8000) "The port to serve on." [envvar := ["PORT"]]
    (reload : Bool := false) "Enable auto-reload of the server when (code) files change."
    (workers : Option Nat := none) "Use multiple worker processes."
    (root_path : String := "") "The root path."
    (app : Option String := none) "The name of the variable that contains the app."
    (entrypoint : Option String := none) "The app import string." [decls := ["--entrypoint", "-e"]]
    (proxy_headers : Bool := true) "Enable/Disable X-Forwarded-Proto, X-Forwarded-For, X-Forwarded-Port."
    : IO Unit :=
  serve "production" path host port reload

def cli : App Unit :=
  group "fastapi" (help := "FastAPI CLI - The fastapi command line app.")
    (contextSettings := { helpOptionNames := some ["-h", "--help"] })
    [versionOption "0.1.0" (message := "FastAPI CLI version: %(version)s"),
     option ["--verbose/--no-verbose"] (help := "Enable verbose output")]
    [dev, run]

def main (args : List String) : IO UInt32 := cli.run () args
