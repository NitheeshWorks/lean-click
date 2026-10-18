import LeanClick

/-!
# Example: the shape of `fastapi dev` / `fastapi run`

A group with a `--version` option and two subcommands whose options mirror
fastapi-cli's typer declarations. The callbacks only print what they would
hand to the server, so this runs anywhere:

```
lake exe example dev main.py --port 9000 --no-reload
lake exe example --help
PORT=7000 lake exe example run
```
-/

open LeanClick LeanClick.Terminal

def serve (mode : String) : Callback Unit := fun ctx => do
  let a := ctx.params
  let host := (a.getString? "host").getD "?"
  let port := (a.getInt? "port").getD 0
  let reload := (a.getBool? "reload").getD false
  let path := (a.getString? "path").getD "(auto-discovery)"
  echo s!"Starting FastAPI in {mode} mode"
  echo s!"  path:   {path}"
  echo s!"  server: http://{host}:{port}  reload={reload}"
  echo s!"  source of --port: {repr (a.source? "port")}"

def runCmd (name mode host reload : String) : App Unit :=
  App.command {
    name, help := some s!"Run a FastAPI app in {mode} mode.",
    params := [
      (Param.arg "path" (.path { exists_ := true })).requiredParam false
        |>.withHelp "A path to a Python file or package directory.",
      (Param.opt ["--host"]).withDefault host |>.withHelp "The host to serve on.",
      (Param.opt ["--port"] .int).withDefault "8000" |>.withEnvvar ["PORT"]
        |>.withHelp "The port to serve on.",
      (Param.flag ["--reload/--no-reload"]).withDefault reload
        |>.withHelp "Enable auto-reload of the server when (code) files change.",
      (Param.opt ["--root-path"]).withDefault "" |>.withHelp "The root path.",
      (Param.opt ["--app"]).withHelp "The name of the variable that contains the app.",
      (Param.opt ["--entrypoint", "-e"]).withHelp "The app import string.",
      (Param.flag ["--proxy-headers/--no-proxy-headers"]).withDefault "true"
        |>.withHelp "Enable/Disable X-Forwarded-Proto, X-Forwarded-For, X-Forwarded-Port.",
      (Param.flag ["--verbose", "-v"]).withHelp "Show detailed startup output."] }
    (serve mode)

def cli : App Unit :=
  App.group {
    name := "fastapi", helpOptionNames := some ["-h", "--help"],
    help := some "FastAPI CLI - The fastapi command line app.",
    params := [Param.versionOption "0.1.0" (template := "FastAPI CLI version: %(version)s"),
      (Param.flag ["--verbose/--no-verbose"]).withHelp "Enable verbose output"] }
    [runCmd "dev" "development" "127.0.0.1" "true", runCmd "run" "production" "0.0.0.0" "false"]

def main (args : List String) : IO UInt32 := Terminal.main cli () args
