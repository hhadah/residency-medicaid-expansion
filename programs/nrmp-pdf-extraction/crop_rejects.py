#!/usr/bin/env python3
"""Crop full-line images for parser-rejected data lines so they can be
transcribed from sheets.

fixed.txt line N corresponds 1:1 to the Nth line group in the column TSV.
Usage: crop_rejects.py YEAR ocr_dir rejects.txt out_review_dir mont_append.csv
Only rejects containing a 5+ digit token (data-like) are cropped.
"""
import sys
import os
import re
import csv
from PIL import Image

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from tsv_fix import read_tess_tsv  # noqa: E402

year, ocr_dir, rej_path, review_dir, mont_csv = sys.argv[1:6]
os.makedirs(review_dir, exist_ok=True)
rows = []
with open(rej_path) as f:
    for ln in f:
        m = re.match(r"(pg-\d+_[LR])\.fixed\.txt:(\d+): (NOPARSE|BADNUM): (.*)$", ln.strip())
        if m and re.search(r"\d{5,}", m.group(4)):
            rows.append((m.group(1), int(m.group(2)), m.group(4)))

out = []
cache = {}
for col, line_no, text in rows:
    if col not in cache:
        cache[col] = (read_tess_tsv(os.path.join(ocr_dir, col + ".tsv")),
                      Image.open(os.path.join(ocr_dir, col + ".png")))
    groups, img = cache[col]
    if line_no - 1 >= len(groups):
        continue
    ws = groups[line_no - 1]
    x0 = max(0, min(w["x"] for w in ws) - 8)
    y0 = max(0, min(w["y"] for w in ws) - 8)
    x1 = img.size[0]
    y1 = min(img.size[1], max(w["y"] + w["h"] for w in ws) + 8)
    c = img.crop((x0, y0, x1, y1))
    c = c.resize((c.size[0] * 2, c.size[1] * 2), Image.LANCZOS)
    name = f"rej_{col}_{line_no:03d}.png"
    c.save(os.path.join(review_dir, name))
    out.append({"crop": name, "column": col, "code": f"LINE{line_no}",
                "tess_raw": text, "vis_raw": "", "conflict": ""})

exists = os.path.exists(mont_csv)
with open(mont_csv, "a", newline="") as f:
    w = csv.DictWriter(f, fieldnames=["crop", "column", "code", "tess_raw",
                                      "vis_raw", "conflict"])
    if not exists:
        w.writeheader()
    w.writerows(out)
print(f"{year}: {len(out)} reject-line crops")
