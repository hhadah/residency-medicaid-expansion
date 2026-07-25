#!/usr/bin/env python3
"""Apply human-read montage transcriptions to a resolved year CSV.

Usage: apply_montage.py YEAR res.csv manifest.csv out.csv read1.csv [read2.csv ...]
Rows with flag=montage matching (column, code) get quota/matched from the
readings and flag=manual. '?' readings stay flagged montage_unreadable.
"""
import sys
import csv

year, res_csv, manifest_csv, out_csv = sys.argv[1:5]
reads = {}
plusses = {}
for p in sys.argv[5:]:
    with open(p) as f:
        for ln in f:
            parts = [x.strip() for x in ln.strip().split(",")]
            if len(parts) >= 3 and parts[0].endswith(".png") \
                    and not parts[0].startswith("rej_"):
                reads[parts[0]] = (parts[1], parts[2])
                plusses[parts[0]] = "+" in parts[3:]

# manifest: crop -> (year, code, column)
man = {}
for r in csv.DictReader(open(manifest_csv)):
    man[r["crop"]] = r

bykey = {}
for crop, qm in reads.items():
    m = man.get(crop)
    if m and m["year"] == year:
        bykey[(m["column"], m["code"])] = (qm, plusses.get(crop, False))

rows = list(csv.DictReader(open(res_csv)))
n_ap = n_un = 0
for r in rows:
    if r["flag"] != "montage":
        continue
    col = r["src"].split(".fixed.txt")[0]
    hit = bykey.get((col, r["code"]))
    if hit is None:
        continue
    qm, plus = hit
    if qm[0].isdigit() and qm[1].isdigit():
        r["quota"], r["matched"] = qm
        if plus and "plus_flag" in r:
            r["plus_flag"] = "1"
        r["flag"] = "manual"
        n_ap += 1
    else:
        r["flag"] = "montage_unreadable"
        n_un += 1
left = sum(1 for r in rows if r["flag"] == "montage")
with open(out_csv, "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=rows[0].keys())
    w.writeheader()
    w.writerows(rows)
print(f"{year}: applied={n_ap} unreadable={n_un} still_montage={left}")
