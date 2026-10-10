"""Editorial Ink (scheme D) palette for PBMC analyses."""
from __future__ import annotations

import json
from pathlib import Path

_JSON_CANDIDATES = [
    Path(__file__).resolve().parent / "editorial_ink.json",
]

def _load() -> dict:
    for p in _JSON_CANDIDATES:
        if p.exists():
            return json.loads(p.read_text())
    raise FileNotFoundError("editorial_ink.json not found")

PAL = _load()
INK = PAL.get("ink", "#2B2B2B")
PAPER = PAL.get("paper", "#FFFFFF")
CLINICAL = PAL["clinical"]
LINEAGE = PAL["lineage"]
CLUSTER26 = PAL["cluster26"]
SEQUENTIAL = PAL["sequential"]
DIVERGING = PAL["diverging"]

_RULES = [
    ("Plasma", r"plasma|plasmablast"),
    ("B", r"\bb cell|\bb-|exhausted b|^b$|b naive|b memory|b intermediate"),
    ("CD4_T", r"cd4|t helper|treg"),
    ("CD8_T", r"cd8|cytotoxic"),
    ("NK", r"\bnk\b|natural killer"),
    ("CD16_Mono", r"cd16|non-classical|nonclassical"),
    ("CD14_Mono", r"cd14|classical mono|monocyte"),
    ("pDC", r"pdc|plasmacytoid"),
    ("cDC", r"cdc|myeloid dendritic|\bdc\b|dendritic"),
    ("MAIT", r"mait"),
    ("gdT", r"gd t|gdt|gamma.?delta|vd2"),
    ("Platelet", r"platelet|megakary"),
    ("RBC", r"eryth|\brbc\b"),
    ("Prolif", r"prolif|cycling|g2m|mki67"),
]


def _is_cluster26_label(x: str) -> bool:
    import re

    if re.fullmatch(r"[0-9]+", x):
        return True
    if re.search(r"_c[0-9]+", x):
        return True
    if re.search(r"(^|[ ·])c[0-9]+(\b| ·|$)", x):
        return True
    if " · " in x:
        return True
    return False


def _match_one(label: str) -> str | None:
    import re

    x = str(label)
    if x in CLINICAL:
        return CLINICAL[x]
    if x in LINEAGE:
        return LINEAGE[x]
    # Subtype / numeric cluster IDs recycle cluster26 — do not collapse
    # "c0 · CD4_TCM" or "NK_CD16dim" to one lineage hue.
    if _is_cluster26_label(x):
        return None
    xl = x.lower().replace("_lowconf", "")
    for key, pat in _RULES:
        if re.search(pat, xl):
            return LINEAGE[key]
    return None


def colors_for(labels) -> dict[str, str]:
    levs = []
    seen = set()
    for v in labels:
        s = str(v)
        if s not in seen and s not in ("nan", "None"):
            seen.add(s)
            levs.append(s)
    out: dict[str, str] = {}
    leftover = []
    for lv in levs:
        hit = _match_one(lv)
        if hit:
            out[lv] = hit
        else:
            leftover.append(lv)

    def _key(v: str):
        try:
            return (0, int(v), v)
        except ValueError:
            return (1, 0, v)

    leftover.sort(key=_key)
    for i, lv in enumerate(leftover):
        out[lv] = CLUSTER26[i % len(CLUSTER26)]
    return out
