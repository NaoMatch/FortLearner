#!/usr/bin/env python3
"""
Generate a minimal machine-readable JSON of Fortran C bindings used by Python.

This parser is intentionally lightweight and targets common patterns in:
  - fortlearner/kmeans/bindings/mod_kmeans_binding.f90
  - fortlearner/utils/check/bindings/mod_error_bindings.f90

Usage:
  python tools/gen_fortran_signatures.py > docs/fortran/signatures.json
"""
from __future__ import annotations
import json
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]

BIND_FILES = [
    ROOT / "fortlearner/kmeans/bindings/mod_kmeans_binding.f90",
    ROOT / "fortlearner/utils/check/bindings/mod_error_bindings.f90",
]

re_head = re.compile(r"\b(subroutine|function)\s+(\w+)\s*\(([^)]*)\).*bind\s*\(\s*C\s*,\s*name\s*=\s*\"([^\"]+)\"\s*\)", re.IGNORECASE)
re_decl = re.compile(r"\b(integer|real|logical|character)\s*\(([^)]*)\)\s*,?\s*(intent\s*\(\s*(in|out|inout)\s*\))?\s*::\s*([^!]+)", re.IGNORECASE)
re_value = re.compile(r"\bvalue\b", re.IGNORECASE)

ctype_map = {
    "c_int64_t": "int64",
    "c_long": "long",
    "c_double": "double",
    "c_bool": "bool",
    "c_size_t": "size_t",
    # character arrays are handled as char[]
}

def parse_file(path: Path) -> list[dict]:
    text = path.read_text(encoding="utf-8", errors="ignore")
    lines = text.splitlines()
    out = []
    i = 0
    while i < len(lines):
        line = lines[i]
        m = re_head.search(line)
        if not m:
            i += 1
            continue
        kind, name, arglist, cname = m.group(1).lower(), m.group(2), m.group(3), m.group(4)
        entry = {
            "module": path.stem,
            "symbol": name,
            "kind": kind,
            "bind": "C",
            "c_name": cname,
            "params": [],
        }
        # scan forward for declarations until end of routine
        j = i + 1
        # rough block end: next contains 'end subroutine name' or 'end function name'
        while j < len(lines):
            l = lines[j].strip()
            if re.match(rf"end\s+{kind}\b", l, re.IGNORECASE):
                break
            dm = re_decl.search(l)
            if dm:
                basety, kindspec, intent_full, intent, names = dm.groups()
                # detect VALUE in kindspec or trailing clauses
                value = bool(re_value.search(l))
                # split multiple names
                for var in [n.strip() for n in names.split(',') if n.strip()]:
                    # arrays
                    if '(' in var and ')' in var:
                        varname = var.split('(')[0].strip()
                        cty = "char[]" if basety.lower() == "character" else None
                        entry["params"].append({
                            "name": varname,
                            "ctype": cty or "double*",
                            "intent": (intent or "in").lower(),
                        })
                    else:
                        # map kindspec to ctype when possible
                        cty = None
                        for ksym, cval in ctype_map.items():
                            if ksym in kindspec:
                                cty = cval
                                break
                        if basety.lower() == "character":
                            cty = "char[]"
                        entry["params"].append({
                            "name": var.strip(),
                            "ctype": cty or basety.lower(),
                            "intent": ("value" if value else (intent or "in").lower()),
                        })
            j += 1
        # crude return type detection for functions
        if kind == "function":
            # search returns in head or later declarations matching the function result symbol
            entry["returns"] = {"ctype": "double" if "score" in name else "int64"}
        out.append(entry)
        i = j + 1
    return out

def main() -> None:
    bindings = []
    for f in BIND_FILES:
        if f.exists():
            bindings.extend(parse_file(f))
    print(json.dumps({"bindings": bindings}, ensure_ascii=False, indent=2))

if __name__ == "__main__":
    main()

