#!/usr/bin/env python3
"""Crop full-width row images for a list of (year, src, code) taken from the
combined CSV (src like 'pg-05_L.fixed.txt:41'). Emits crops + manifest rows.

Usage: crop_by_src.py list.csv ocr_root out_dir manifest_out
list.csv columns: year,src,code
"""
import sys
import os
import re
import csv
from PIL import Image

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from tsv_fix import read_tess_tsv  # noqa: E402

list_csv, ocr_root, out_dir, man_out = sys.argv[1:5]
os.makedirs(out_dir, exist_ok=True)
rows = list(csv.DictReader(open(list_csv)))
cache = {}
out = []
for r in rows:
    m = re.match(r"(pg-\d+_[LR])\.fixed\.txt:(\d+)", r["src"])
    if not m:
        continue
    col, line_no = m.group(1), int(m.group(2))
    key = (r["year"], col)
    if key not in cache:
        cache[key] = (read_tess_tsv(os.path.join(ocr_root, r["year"], col + ".tsv")),
                      Image.open(os.path.join(ocr_root, r["year"], col + ".png")))
    groups, img = cache[key]
    if line_no - 1 >= len(groups):
        continue
    ws = groups[line_no - 1]
    y0 = max(0, min(w["y"] for w in ws) - 8)
    y1 = min(img.size[1], max(w["y"] + w["h"] for w in ws) + 8)
    c = img.crop((0, y0, img.size[0], y1))
    c = c.resize((c.size[0] * 2, c.size[1] * 2), Image.LANCZOS)
    name = f"mq_{r['year']}_{col}_{line_no:03d}.png"
    c.save(os.path.join(out_dir, name))
    out.append({"crop": name, "year": r["year"], "column": col,
                "code": r["code"], "line": line_no})
with open(man_out, "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=["crop", "year", "column", "code", "line"])
    w.writeheader()
    w.writerows(out)
print(f"crops: {len(out)}")
