#!/usr/bin/env python3
"""Validate docs/traceability.toml against the code.

spec -> code : every `realises` and `theorem` symbol resolves in Lean
spec -> test : every `test` path exists
status       : one of implemented | partial | tested | excluded | deviation

  scripts/traceability-check.py [--no-lean]
"""
import re, subprocess, sys, tomllib
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
LIB = "LeanClick"
STATUSES = {"implemented", "partial", "tested", "excluded", "deviation"}

def main():
    doc = tomllib.loads((REPO / "docs" / "traceability.toml").read_text())
    rows = doc.get("requirement", [])
    bad = []
    symbols = []
    for row in rows:
        for k in ("id", "upstream", "title", "status"):
            if k not in row: bad.append(f"{row.get('id','?')}: missing {k}")
        if row.get("status") not in STATUSES: bad.append(f"{row.get('id')}: bad status {row.get('status')}")
        symbols += row.get("realises", []) + row.get("theorem", [])
        for t in row.get("test", []):
            p = t.split("::")[0]
            if not (REPO / p).exists(): bad.append(f"{row['id']}: test path {p} missing")
    if symbols and "--no-lean" not in sys.argv:
        body = f"import {LIB}\n" + "\n".join(f"#check @{s}" for s in sorted(set(symbols)))
        tmp = REPO / ".lake" / "trace-check.lean"; tmp.parent.mkdir(exist_ok=True)
        tmp.write_text(body)
        proc = subprocess.run(["lake", "env", "lean", str(tmp)], cwd=REPO, capture_output=True, text=True)
        for line in (proc.stdout + proc.stderr).splitlines():
            if re.search(r":\d+:\d+: error", line): bad.append(line.strip())
    # spec -> test: the upstream tests the conformance suite re-expresses, from
    # the case citations in tests/Conformance.lean and the golden scenarios'
    # `upstream` fields; every implemented/tested row citing an upstream test
    # must be among them
    cited = set(re.findall(r'"((?:fastapi-cli )?tests/[\w/]+\.py::\w+)"', (REPO / "tests" / "Conformance.lean").read_text()))
    golden = REPO / "tests" / "golden" / "click.json"
    if golden.exists():
        import json
        for sc in json.loads(golden.read_text()):
            if sc.get("upstream"): cited.add(sc["upstream"])
            else: bad.append(f"golden scenario {sc.get('fixture') or sc.get('process')} {sc.get('argv')}: no upstream test")
    for row in rows:
        m = re.match(r"((?:fastapi-cli )?tests/[\w/]+\.py::\w+)", row.get("upstream", ""))
        if m and row.get("status") in ("implemented", "tested") and m.group(1) not in cited:
            bad.append(f"{row['id']}: upstream test {m.group(1)} is not re-expressed in tests/")
    print(f"{len(cited)} upstream tests cited by the conformance suite")
    print(f"{len(rows)} requirements, {len(set(symbols))} symbols, {len(bad)} problems")
    for b in bad: print("  " + b)
    sys.exit(1 if bad else 0)

if __name__ == "__main__":
    main()
