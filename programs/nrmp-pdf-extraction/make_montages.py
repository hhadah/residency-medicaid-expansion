#!/usr/bin/env python3
"""Build labeled montage sheets from a montage list csv.

Usage: make_montages.py YEAR mont.csv review_dir out_dir manifest.csv
Each sheet stacks up to 14 crops vertically with the crop filename printed
above each tile. Manifest maps sheet -> ordered crop names.
"""
import sys
import os
import csv
import subprocess

year, mont_csv, review_dir, out_dir, manifest_csv = sys.argv[1:6]
os.makedirs(out_dir, exist_ok=True)
rows = [r for r in csv.DictReader(open(mont_csv)) if r["crop"]]
PER = 14
manifest = []
for i in range(0, len(rows), PER):
    batch = rows[i:i + PER]
    sheet = f"sheet_{year}_{i//PER:02d}.png"
    args = ["magick", "montage", "-label", "%f"]
    for r in batch:
        args.append(os.path.join(review_dir, r["crop"]))
    args += ["-tile", "1x", "-geometry", "+6+6", "-background", "#888",
             "-pointsize", "28", os.path.join(out_dir, sheet)]
    subprocess.run(args, check=True)
    for r in batch:
        manifest.append({"sheet": sheet, "crop": r["crop"], "year": year,
                         "code": r["code"], "column": r["column"]})
exists = os.path.exists(manifest_csv)
with open(manifest_csv, "a", newline="") as f:
    w = csv.DictWriter(f, fieldnames=["sheet", "crop", "year", "code", "column"])
    if not exists:
        w.writeheader()
    w.writerows(manifest)
print(f"{year}: {len(rows)} crops -> {(len(rows)+PER-1)//PER} sheets in {out_dir}")
