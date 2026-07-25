#!/usr/bin/env python3
"""Merge tesseract-parsed rows (structure) with Vision-parsed rows (digit
authority) for one year, keyed on program code.

Flags:
  agree        both engines parsed identical numbers
  vision_fix   tesseract number was repaired/garbled; Vision supplied it
  tess_only    Vision missed the row; tesseract numbers used
  vision_only  tesseract missed the row; context inferred from institution
  conflict     both engines confident but disagree -> needs manual check

Usage: merge_year.py tess.csv vision.csv out.csv audit.csv
"""
import sys
import csv
import collections

tess_path, vis_path, out_path, audit_path = sys.argv[1:5]

tess = list(csv.DictReader(open(tess_path)))
vis = list(csv.DictReader(open(vis_path)))

vis_by_code = collections.defaultdict(list)
for v in vis:
    vis_by_code[v["code"]].append(v)

out, audit = [], []
used_vis = set()

for t in tess:
    code = t["code"]
    cands = vis_by_code.get(code, [])
    v = cands[0] if len(cands) == 1 else None
    if v is not None:
        used_vis.add(code)
    vq = v["quota"] if v else ""
    vm = v["matched"] if v else ""
    v_full = v is not None and vq != "" and vm != ""
    tq, tm = t["quota"], t["matched"]
    row = dict(t)
    if t["flag"] in ("ok",):
        if v_full and (vq != tq or vm != tm):
            row["flag"] = "conflict"
            audit.append({**row, "tess": f"{tq}/{tm}", "vision": f"{vq}/{vm}"})
        else:
            row["flag"] = "agree" if v_full else "tess_only"
    elif t["flag"] in ("repaired", "badnum"):
        if v_full:
            row["quota"], row["matched"] = vq, vm
            row["flag"] = "agree" if (v_full and tq == vq and tm == vm) else "vision_fix"
        elif t["flag"] == "repaired":
            row["flag"] = "tess_repaired_unverified"
            audit.append({**row, "tess": f"{tq}/{tm}", "vision": ""})
        else:
            row["flag"] = "unresolved"
            audit.append({**row, "tess": f"{tq}/{tm}", "vision": ""})
    out.append(row)

# vision rows absent from tesseract: infer context from institution prefix
ctx_by_inst = {}
for t in tess:
    ctx_by_inst.setdefault(t["code"][:4], (t["state"], t["hospital"], t["city"]))
tess_codes = {t["code"] for t in tess}
year = tess[0]["year"] if tess else ""
for code, cands in vis_by_code.items():
    if code in tess_codes or len(cands) != 1:
        continue
    v = cands[0]
    if v["quota"] == "" or v["matched"] == "":
        continue
    ctx = ctx_by_inst.get(code[:4], ("", "", ""))
    row = {"year": year, "state": ctx[0], "hospital": ctx[1], "city": ctx[2],
           "program": "", "code": code, "quota": v["quota"],
           "matched": v["matched"], "flag": "vision_only", "src": v["src"]}
    out.append(row)
    audit.append({**row, "tess": "", "vision": f'{v["quota"]}/{v["matched"]}'})

with open(out_path, "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=["year", "state", "hospital", "city",
                                      "program", "code", "quota", "matched",
                                      "flag", "src"])
    w.writeheader()
    w.writerows({k: r.get(k, "") for k in w.fieldnames} for r in out)

with open(audit_path, "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=["year", "state", "hospital", "city",
                                      "program", "code", "quota", "matched",
                                      "flag", "src", "tess", "vision"])
    w.writeheader()
    w.writerows({k: r.get(k, "") for k in w.fieldnames} for r in audit)

cnt = collections.Counter(r["flag"] for r in out)
tq = sum(int(r["quota"]) for r in out if str(r["quota"]).isdigit())
tm = sum(int(r["matched"]) for r in out if str(r["matched"]).isdigit())
print(f"{out_path.split('/')[-1]}: rows={len(out)} quota={tq} matched={tm} | "
      + " ".join(f"{k}={v}" for k, v in sorted(cnt.items())))
