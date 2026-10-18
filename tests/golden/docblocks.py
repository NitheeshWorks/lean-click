"""The code blocks of a click docs page, in page order.

A block is a `click:example` or `click:run` directive inside an
`{eval-rst}` fence, a `.. code-block:: python` directive, or a `python`,
`pycon` or `{code-block} python` fence. Blocks are numbered from 1 in page
order; `docs/<page>.md#block-<n>` names one (examples/MANIFEST.toml, the
golden scenarios, and the first comment of each section of
examples/docs/<page>.lean).
"""
import re
import textwrap

PY = {"python", "py", "pycon", "python3"}
FENCE = re.compile(r"^\s*```\s*(\{code-block\}\s*)?(\{eval-rst\}|[\w-]+)?\s*$")
DIRECTIVE = re.compile(r"^\.\. (click:example|click:run|code-block|sourcecode)::\s*(\S*)")


def blocks(text):
    """[(kind, code, line)] with kind click:example, click:run or python."""
    lines = text.split("\n")
    out = []
    i = 0
    while i < len(lines):
        m = FENCE.match(lines[i])
        if not m or lines[i].strip() == "```":
            i += 1
            continue
        lang = (m.group(2) or "").strip()
        j = i + 1
        while j < len(lines) and lines[j].strip() != "```":
            j += 1
        body = lines[i + 1:j]
        if lang == "{eval-rst}":
            k = 0
            while k < len(body):
                d = DIRECTIVE.match(body[k])
                if not d:
                    k += 1
                    continue
                kind, arg = d.group(1), d.group(2)
                k += 1
                while k < len(body) and re.match(r"^\s+:\w+:", body[k]):
                    k += 1
                seg = []
                while k < len(body) and (body[k].strip() == "" or body[k].startswith((" ", "\t"))):
                    seg.append(body[k])
                    k += 1
                code = textwrap.dedent("\n".join(seg)).strip("\n")
                if kind in ("code-block", "sourcecode"):
                    if arg in PY:
                        out.append(("python", code, i + 1))
                else:
                    out.append((kind, code, i + 1))
        elif lang in PY:
            code = "\n".join(l for l in body if not re.match(r"^:\w+:", l))
            out.append(("python", textwrap.dedent(code).strip("\n"), i + 1))
        i = j + 1
    return out
