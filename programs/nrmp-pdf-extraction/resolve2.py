#!/usr/bin/env python3
"""Second-stage resolver for rows tsv_fix could not settle.

Reads the parsed year CSV (with badnum '??' rows and conflict crops),
the review index (which holds tess_raw and vis partial reads), and the
RA-collected data as an independent witness.

Ladder for unresolved rows:
  1. tess raw = two clean digit tokens & Vision partial matches one -> accept
  2. tess raw = two clean digit tokens & RA (year,code) equals them  -> accept
  3. else -> keep ?? ; goes to the montage list
Conflicts (tess vs vision both confident):
  RA agrees with one side -> that side wins; else montage.

Additionally, provenance tags from tsv_fix (flags.csv next to the review dir)
route weakly-supported rows through the RA witness:
  vision_fix rows (Vision-only numbers): RA disagrees -> montage
  clean rows (tesseract-only, no Vision sighting): RA disagrees -> montage

Usage: resolve2.py YEAR parsed.csv review_index.csv ra.csv out.csv montage_list.csv
"""
import sys
import csv
import re
import collections

GLYPH = str.maketrans({
    "I": "1", "l": "1", "|": "1", "]": "1", "[": "1", "}": "1", "{": "1",
    "(": "1", ")": "1", "!": "1", "i": "1",
    "O": "0", "o": "0", "Q": "0",
    "S": "5", "s": "5", "Z": "2", "z": "2", "B": "8", "G": "6",
})


def numy(t):
    t2 = t.strip(".,;:'\"").translate(GLYPH)
    return int(t2) if t2.isdigit() else None


year, parsed_csv, idx_csv, ra_csv, out_csv, mont_csv = sys.argv[1:7]

ra = {}
with open(ra_csv) as f:
    for r in csv.DictReader(f):
        if r["year"] == year:
            try:
                ra[r["code"]] = (int(float(r["quota"])), int(float(r["matched"])))
            except ValueError:
                pass

# provenance tags from tsv_fix
import os
tags = {}
flags_path = os.path.join(os.path.dirname(idx_csv), "..", "flags.csv")
if os.path.exists(flags_path):
    with open(flags_path) as f:
        for r in csv.DictReader(f):
            tags[(r["column"], r["code"])] = (r["tag"], r["quota"], r["matched"])

# review index: keyed by (column, code)
idx = {}
with open(idx_csv) as f:
    for r in csv.DictReader(f):
        m = re.match(r"tess_raw=(.*?) vis=(.*)$", r["detail"])
        tess_raw, vis_raw = (m.group(1), m.group(2)) if m else ("", "")
        mc = re.match(r"tess=(\d+)/(\d+) vis=(\d+)/(\d+)", r["detail"])
        idx[(r["column"], r["code"])] = {
            "crop": r["crop"], "tess_raw": tess_raw, "vis_raw": vis_raw,
            "conflict": mc.groups() if mc else None}

rows = list(csv.DictReader(open(parsed_csv)))
mont = []
stats = collections.Counter()
for r in rows:
    col = r["src"].split(".fixed.txt")[0]
    key = (col, r["code"])
    info = idx.get(key)
    if r["flag"] == "badnum" and r["quota"] == "" or r["quota"] == "??":
        stats["unresolved_in"] += 1
        tess_toks = (info["tess_raw"].split() if info else [])
        nums = [numy(t) for t in tess_toks]
        nums = [n for n in nums if n is not None and n <= 999]
        vis_nums = [numy(t) for t in re.split(r"[/ ]", info["vis_raw"])] if info else []
        vis_nums = [n for n in vis_nums if n is not None]
        raqm = ra.get(r["code"])
        if len(nums) == 2 and vis_nums and (nums[0] in vis_nums or nums[1] in vis_nums):
            r["quota"], r["matched"] = nums
            r["flag"] = "lowconf_vision_partial"
        elif len(nums) == 2 and raqm and tuple(nums) == raqm:
            r["quota"], r["matched"] = nums
            r["flag"] = "lowconf_ra_confirmed"
        else:
            if info:
                mont.append({**info, "code": r["code"], "column": col})
            r["flag"] = "montage"
        stats[r["flag"]] += 1
    elif tags.get(key) and not (info and info["conflict"]):
        # carry provenance tag through to the final flag
        if r["flag"] == "ok":
            r["flag"] = tags[key][0]
    elif info and info["conflict"]:
        tq, tm, vq, vm = info["conflict"]
        raqm = ra.get(r["code"])
        if raqm == (int(tq), int(tm)):
            r["quota"], r["matched"] = tq, tm
            r["flag"] = "conflict_ra_tess"
        elif raqm == (int(vq), int(vm)):
            r["quota"], r["matched"] = vq, vm
            r["flag"] = "conflict_ra_vision"
        else:
            mont.append({**info, "code": r["code"], "column": col})
            r["flag"] = "montage"
        stats[r["flag"]] += 1

with open(out_csv, "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=rows[0].keys())
    w.writeheader()
    w.writerows(rows)
with open(mont_csv, "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=["crop", "column", "code", "tess_raw",
                                      "vis_raw", "conflict"])
    w.writeheader()
    for m in mont:
        w.writerow({k: m.get(k, "") for k in w.fieldnames})
print(f"{year}: " + " ".join(f"{k}={v}" for k, v in sorted(stats.items()))
      + f" | montage rows: {len(mont)}")
