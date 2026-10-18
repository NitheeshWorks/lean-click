"""Record click 8.5.0's exact output for the golden conformance cases.

Run locally (never in CI) with the upstream-spec venv:

    upstream-spec/.venv/bin/python -I tests/golden/generate.py > tests/golden/click.json

Each fixture here has a twin of the same name in tests/Conformance.lean
(`goldenFixtures`); the Lean runner replays every scenario and compares the
exit code, stdout and stderr byte for byte.
"""
import json
import click
from click.testing import CliRunner


def nop(*a, **k):
    pass


F = {}


def fx(name):
    def deco(cmd):
        F[name] = cmd
        return cmd
    return deco


fx("sopt")(click.Command("cli", callback=nop, params=[click.Option(["--s"], default="no value")]))
fx("iopt")(click.Command("cli", callback=nop, params=[click.Option(["--i"], default=42)]))
fx("fopt")(click.Command("cli", callback=nop, params=[click.Option(["--f"], default=42.0)]))
fx("methodopt")(click.Command("cli", callback=nop, params=[click.Option(["--method"], type=click.Choice(["foo", "bar", "baz"]))]))
fx("methodarg")(click.Command("cli", callback=nop, params=[click.Argument(["method"], type=click.Choice(["foo", "bar", "baz"]))]))
fx("reqopt")(click.Command("cli", callback=nop, params=[click.Option(["--foo"], required=True)]))
fx("hiddenopt")(click.Command("cli", callback=nop, params=[click.Option(["--nope"], hidden=True)]))
fx("hiddencmd")(click.Group("cli", commands=[click.Command("nope", callback=nop, hidden=True)]))
fx("summary")(click.Group("cli", commands=[click.Command("cmd", callback=nop, help="\n        Summary line without period\n\n        Here is a sentence. And here too.\n        ")]))
fx("invaliddefault")(click.Command("cli", callback=nop, params=[click.Option(["-a"], type=click.Path(exists=True), default="not found", show_default=True)]))
fx("basicgroup")(click.Group("cli", help="This is the root.", commands=[click.Command("subcommand", callback=nop, help="This is a subcommand.")]))
fx("count")(click.Command("cli", callback=nop, params=[click.Option(["-v"], count=True, help="Verbosity", type=click.IntRange(0, 3))]))
fx("empty")(click.Command("cli", callback=nop))
fx("boundcount")(click.Command("cli", callback=nop, params=[click.Option(["--bound"]), click.Option(["--count"])]))
fx("msg")(click.Command("cli", callback=nop, params=[click.Option(["-m", "--message"], multiple=True, required=True)]))
fx("shout")(click.Command("cli", callback=nop, params=[click.Option(["--shout/--no-shout"], envvar="SHOUT")]))
fx("multidefault")(click.Command("cmd", callback=nop, params=[
    click.Option(["--arg1"], multiple=True, default=("foo", "bar"), show_default=True),
    click.Option(["--arg2"], multiple=True, default=(1, 2), type=int, show_default=True)]))
fx("showenvvar")(click.Command("cmd", callback=nop, params=[click.Option(["--arg1"], envvar="ARG1", show_envvar=True)]))
fx("showenvvarauto")(click.Command("cmd", callback=nop, context_settings={"auto_envvar_prefix": "TEST"}, params=[click.Option(["--arg1"], show_envvar=True)]))
fx("dashgroup")(click.Group("cli", context_settings={"auto_envvar_prefix": "TEST"}, commands=[click.Command("foo-bar", callback=nop, params=[click.Option(["--baz"], show_envvar=True)])]))
fx("reqflag")(click.Command("cli", callback=nop, params=[click.Option(["--on/--off"], required=True)]))
fx("misschoice")(click.Command("cmd", callback=nop, params=[click.Option(["--foo"], type=click.Choice(["foo", "bar"]), required=True)]))
fx("missenv1")(click.Command("cli", callback=nop, params=[click.Option(["--foo"], envvar="bar", required=True)]))
fx("missenv2")(click.Command("cli", callback=nop, params=[click.Option(["--foo"], envvar="bar", show_envvar=True, required=True)]))
fx("casechoice")(click.Command("cmd", callback=nop, params=[click.Option(["--foo"], type=click.Choice(["Orange", "Apple"], case_sensitive=False))]))
fx("casechoice2")(click.Command("cmd", callback=nop, params=[click.Option(["--foo"], type=click.Choice(["Orange", "Apple"]))]))
fx("flagdefault")(click.Command("cli", callback=nop, params=[click.Option(["--cache/--no-cache"], default=False, show_default=True, help="Enable the cache.")]))
fx("showdefaulttext")(click.Command("cli", callback=nop, params=[click.Option(["--limit"], show_default="unlimited")]))
fx("emptydefault")(click.Command("cli", callback=nop, params=[click.Option(["--limit"], default="", show_default=True)]))
fx("nodefault")(click.Command("cli", callback=nop, params=[click.Option(["--limit"], show_default=True)]))
fx("intrange")(click.Command("cli", callback=nop, params=[click.Option(["--count"], type=click.IntRange(1, 32))]))
fx("countdefault")(click.Command("cli", callback=nop, params=[click.Option(["--verbose"], count=True)]))
fx("copyx")(click.Command("copy", callback=nop, params=[click.Argument(["x"])]))
fx("missarg")(click.Command("cmd", callback=nop, params=[click.Argument(["arg"])]))
fx("tupitem")(click.Command("cmd", callback=nop, params=[click.Argument(["item"], nargs=2)]))
fx("star")(click.Command("cmd", callback=nop, params=[click.Argument(["arg"], nargs=-1, required=True)]))
fx("filearg")(click.Command("cmd", callback=nop, params=[click.Argument(["input"], type=click.File())]))
fx("noargshelp")(click.Command("cli", callback=nop, no_args_is_help=True))
fx("grpsub")(click.Group("cli", commands=[click.Command("sub", callback=nop)]))
fx("grpargs")(click.Group("cli", params=[click.Argument(["obj"])], commands=[click.Command("move", callback=nop)]))
fx("unknowncmd")(click.Group("cli"))
fx("suggest")(click.Group("cli", commands=[click.Command(n, callback=nop) for n in ["push", "declare", "refine"]]))
fx("fmtbasic")(click.Command("cli", callback=nop, help="First paragraph.\n\n        This is a very long second\n        paragraph and not correctly\n        wrapped but it will be rewrapped.\n\n        \b\n        This is\n        a paragraph\n        without rewrapping.\n\n        \b\n        1\n         2\n          3\n\n        And this is a paragraph\n        that will be rewrapped again.\n        "))
_args6 = [click.Argument([n]) for n in ["first", "second", "third", "fourth", "fifth", "sixth"]]
fx("longopts")(click.Group("cli", help="Top level command", commands=[click.Group("a-very-long", help="Second level", commands=[click.Command("command", callback=nop, help="A command.", params=_args6)])]))
fx("longname")(click.Group("cli", help="Top level command", commands=[click.Group("a-very-very-very-long", help="Second level", commands=[click.Command("command", callback=nop, help="A command.", params=[click.Argument([n]) for n in ["first", "second", "third", "fourth", "fifth", "sixth"]])])]))
fx("metavarmiss")(click.Command("cmd", callback=nop, params=[click.Argument(["arg"], metavar="metavar")]))
fx("metavarbad")(click.Command("cmd", callback=nop, params=[click.Argument(["arg"], type=click.INT, metavar="metavar")]))
fx("nested")(click.Group("cmd", commands=[click.Command("foo", callback=nop, params=[click.Argument(["bar"])])]))
fx("nohelp")(click.Command("cmd", callback=nop, add_help_option=False, params=[click.Argument(["arg"])]))
fx("customhelp")(click.Command("cmd", callback=nop, context_settings={"help_option_names": ["--man"]}, params=[click.Argument(["arg"])]))
fx("version")(click.Command("cli", callback=nop, params=[]))
click.version_option("1.0")(F["version"])
fx("prefixes")(click.Command("cli", callback=nop, params=[click.Option(["++foo"], is_flag=True, help="das foo"), click.Option(["--bar"], is_flag=True, help="das bar")]))
fx("winstyle")(click.Command("cmd", callback=nop, context_settings={"help_option_names": ["/?"]}, params=[click.Option(["/debug;/no-debug"], help="Enables or disables debug mode.")]))
fx("float1e20")(click.Command("cli", callback=nop, params=[click.Option(["--f"], default=1e20, show_default=True)]))


def rej(ctx, p, v):
    raise click.BadParameter("no")


fx("callbacks")(click.Command("c", callback=nop, params=[click.Option(["--a"], callback=rej), click.Option(["--b"], type=int)]))


def pos(ctx, p, v):
    if v < 0:
        raise click.BadParameter("Value needs to be positive")
    return v


fx("validation")(click.Command("cmd", callback=nop, params=[click.Option(["--foo"], callback=pos, default=1)]))
fx("chainhelp")(click.Group("cli", chain=True, help="ROOT HELP", commands=[click.Command("sdist", callback=nop, help="SDIST HELP"), click.Command("bdist", callback=nop, help="BDIST HELP")]))
fx("chainunknown")(click.Group("cli", chain=True, commands=[click.Command("a", callback=nop)]))
fx("uuid")(click.Command("cli", callback=nop, params=[click.Option(["--u"], type=click.UUID, default="ba122011-349f-423b-873b-9d6a79c688ab")]))
fx("datetime")(click.Command("cli", callback=nop, params=[click.Option(["--start_date"], type=click.DateTime())]))
fx("datetimearg")(click.Command("cli", callback=nop, params=[click.Argument(["when"], type=click.DateTime(formats=["%Y-%m-%d"]), required=False)]))
fx("tuple")(click.Command("c", callback=nop, params=[
    click.Option(["--item"], type=(str, int), multiple=True),
    click.Option(["--pt"], type=click.Tuple([int, float]), default=(1, 2.5), show_default=True),
    click.Argument(["pair"], type=(str, int))]))


def dev_like(name, host, reload):
    return click.Command(name, callback=nop, help="Run a FastAPI app in development mode.", params=[
        click.Argument(["path"], type=click.Path(), required=False),
        click.Option(["--host"], default=host, help="The host to serve on."),
        click.Option(["--port"], default=8000, envvar="PORT", help="The port to serve on."),
        click.Option(["--reload/--no-reload"], default=reload, help="Enable auto-reload."),
        click.Option(["--root-path"], default=""),
        click.Option(["--app"]),
        click.Option(["--entrypoint", "-e"]),
        click.Option(["--proxy-headers/--no-proxy-headers"], default=True),
        click.Option(["--verbose", "-v"], is_flag=True)])


fx("fastapi")(click.Group("fastapi", context_settings={"help_option_names": ["-h", "--help"]}, params=[
    click.Option(["--verbose/--no-verbose"])], commands=[dev_like("dev", "127.0.0.1", True), dev_like("run", "0.0.0.0", False)]))

def hint_group(extra):
    foo = click.Command("foo", callback=nop, params=[click.Argument(["required_arg"])] + extra)
    return click.Group("cli", context_settings={"help_option_names": ["-h", "--help"]}, commands=[foo])


fx("hint1")(hint_group([]))
fx("hint2")(hint_group([click.Option(["--host", "-h"])]))
fx("hint3")(hint_group([click.Option(["--help-file", "--help"])]))
fx("hint4")(hint_group([click.Option(["--host", "-h"]), click.Option(["--help-file", "--help"])]))
fx("flagmap")(click.Group("cli", commands=[click.Command("foo", callback=nop, params=[click.Option(["--name/--no-name"], is_flag=True, show_default=True, help="name flag")])]))
fx("typedserve")(click.Command("serve", callback=nop, help="Serve an app.", params=[
    click.Argument(["path"], required=False, help="A path."),
    click.Option(["--host"], default="127.0.0.1", help="The host."),
    click.Option(["--port"], type=click.IntRange(min=0), default=8000, envvar="PORT", help="The port."),
    click.Option(["--reload/--no-reload"], default=True, help="Reload."),
    click.Option(["--workers"], type=click.IntRange(min=0)),
    click.Option(["--tag"], multiple=True),
    click.Option(["--level"], type=click.IntRange(0, 3), default=1)]))

SCENARIOS = [
    ("sopt", ["--s"]), ("sopt", []), ("sopt", ["--s="]),
    ("iopt", ["--i=x"]), ("fopt", ["--f=x"]),
    ("methodopt", ["--method=meh"]), ("methodopt", ["--help"]),
    ("methodarg", ["meh"]), ("methodarg", ["--help"]), ("methodarg", []),
    ("reqopt", []), ("hiddenopt", ["--help"]), ("hiddencmd", ["--help"]), ("summary", ["--help"]),
    ("invaliddefault", ["--help"]), ("basicgroup", ["--help"]), ("basicgroup", []), ("basicgroup", ["subcommand"]),
    ("count", ["-vvvv"]), ("count", ["--help"]), ("count", ["-vvv"]),
    ("empty", ["--foo"]), ("empty", ["-f"]),
    ("boundcount", ["--cat"]), ("boundcount", ["--bounds"]), ("boundcount", ["--bount"]),
    ("msg", []), ("msg", ["-m", "foo", "-mbar"]),
    ("shout", [], {"SHOUT": "tr ue"}), ("shout", [], {"SHOUT": "10"}), ("shout", [], {"SHOUT": "x:y"}),
    ("multidefault", ["--help"]), ("showenvvar", ["--help"]), ("showenvvarauto", ["--help"]),
    ("dashgroup", ["foo-bar", "--help"]), ("reqflag", []), ("misschoice", []), ("missenv1", []), ("missenv2", []),
    ("casechoice", ["--foo", "x"]), ("casechoice2", ["--foo", "apple"]),
    ("flagdefault", ["--help"]), ("showdefaulttext", ["--help"]), ("emptydefault", ["--help"]), ("nodefault", ["--help"]),
    ("intrange", ["--help"]), ("intrange", ["--count", "40"]), ("countdefault", ["--help"]),
    ("copyx", ["foo", "bar"]), ("copyx", ["foo", "bar", "baz"]), ("missarg", []), ("tupitem", ["a"]), ("star", []),
    ("filearg", ["missing.txt"]), ("noargshelp", []), ("grpsub", []), ("grpargs", ["obj1"]), ("grpargs", ["obj1", "move"]),
    ("unknowncmd", ["unknown"]), ("suggest", ["pause"]), ("suggest", ["decline"]), ("suggest", ["--help"]),
    ("fmtbasic", ["--help"], {}, 60), ("longopts", ["a-very-long", "command", "--help"], {}, 54),
    ("longname", ["a-very-very-very-long", "command", "--help"], {}, 54),
    ("metavarmiss", []), ("metavarbad", ["3.14"]), ("nested", ["foo"]), ("nohelp", []), ("customhelp", []),
    ("version", ["--version"]), ("version", ["--help"]),
    ("prefixes", ["--help"]), ("prefixes", ["++foo", "--bar"]), ("winstyle", ["/?"]), ("winstyle", ["/debug"]),
    ("float1e20", ["--help"]),
    ("callbacks", ["--a", "1", "--b", "x"]), ("callbacks", ["--b", "x", "--a", "1"]), ("callbacks", []),
    ("validation", ["--foo", "-1"]), ("validation", ["--foo", "42"]),
    ("chainhelp", ["--help"]), ("chainhelp", ["sdist", "--help"]), ("chainhelp", ["bdist", "sdist", "--help"]),
    ("chainhelp", ["bdist", "sdist", "bdist"]), ("chainunknown", ["a", "b"]),
    ("uuid", ["--u=x"]), ("uuid", ["--help"]),
    ("datetime", ["--start_date=2015-09"]), ("datetime", ["--help"]), ("datetimearg", ["--help"]),
    ("tuple", ["a", "x"]), ("tuple", ["a"]), ("tuple", ["--pt", "1"]), ("tuple", ["--help"]), ("tuple", ["a", "1"]),
    ("fastapi", ["--help"]), ("fastapi", ["dev", "-h"]), ("fastapi", ["run", "-h"]), ("fastapi", ["dev", "--port", "eighty"]),
    ("fastapi", ["dev", "--prot", "1"]), ("fastapi", ["dev", "--port"]), ("fastapi", ["dev", "a", "b"]), ("fastapi", []),
    ("fastapi", ["deve"]),
    ("tuple", ["--item", "peter", "x", "a", "1"]),
    ("hint1", ["foo"]), ("hint2", ["foo"]), ("hint3", ["foo"]), ("hint4", ["foo"]),
    ("flagmap", ["foo", "--help"]), ("flagmap", ["foo", "--help"], {}, None, {"foo": {"name": True}}),
    ("typedserve", ["--help"]), ("typedserve", ["--level", "5"]), ("typedserve", ["--port", "x"]),
]

out = []
runner = CliRunner()
for sc in SCENARIOS:
    name, argv = sc[0], sc[1]
    env = sc[2] if len(sc) > 2 else {}
    width = sc[3] if len(sc) > 3 else None
    dmap = sc[4] if len(sc) > 4 else None
    kw = {"env": env}
    if dmap is not None:
        kw["default_map"] = dmap
    if width is not None:
        kw["terminal_width"] = width
    res = runner.invoke(F[name], argv, prog_name=F[name].name, **kw)
    out.append({"fixture": name, "argv": argv, "env": env, "width": width or 80, "default_map": dmap,
                "code": res.exit_code, "stdout": res.stdout, "stderr": res.stderr})
# Whole-process scenarios: click's README quickstart run as a real program
# with piped stdin, against `lake exe hello` (examples/Hello.lean).
import subprocess
import sys

HELLO = """
import click

@click.command()
@click.option("--count", default=1, help="Number of greetings.")
@click.option("--name", prompt="Your name", help="The person to greet.")
def hello(count, name):
    \"\"\"Simple program that greets NAME for a total of COUNT times.\"\"\"
    for _ in range(count):
        click.echo(f"Hello, {name}!")

hello(prog_name="hello")
"""
PROCESS = [(["--count=3"], "Click\n"), ([], ""), (["--count=x"], ""), (["--help"], ""), (["--count", "2"], "Ann\n"),
           (["--bogus"], "")]
for argv, inp in PROCESS:
    res = subprocess.run([sys.executable, "-I", "-c", HELLO, *argv], input=inp, capture_output=True, text=True,
                         env={"COLUMNS": "80"})
    out.append({"process": "hello", "argv": argv, "input": inp, "code": res.returncode,
                "stdout": res.stdout, "stderr": res.stderr})
print(json.dumps(out, indent=1, ensure_ascii=False))
