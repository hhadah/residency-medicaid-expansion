#!/usr/bin/env python3
"""Column split v4: derive the true gutter from OCR word boxes.

Uses the existing L/R tesseract TSVs (R boxes offset by the current split) to
reconstruct full-page word intervals; the gutter is the widest x-gap free of
word boxes in the central band. Ink rules and specks are ignored because only
real words bound the gap.

Usage: split_cols4.py page.png cur_L.tsv cur_R.tsv [--apply out_L out_R]
Prints: old_split new_split
"""
import sys
import csv
from PIL import Image

page_png, l_tsv, r_tsv = sys.argv[1:4]
apply_i = "--apply" in sys.argv

img = Image.open(page_png)
W, H = img.size


def words(path, offset):
    out = []
    with open(path) as f:
        rd = csv.reader(f, delimiter="\t", quoting=csv.QUOTE_NONE)
        next(rd, None)
        for row in rd:
            if len(row) < 12 or row[0] != "5":
                continue
            t = row[11].strip()
            if not any(c.isalnum() for c in t):
                continue
            x, w, h = int(row[6]), int(row[8]), int(row[9])
            conf = float(row[10])
            if conf < 40 or w < 8 or not (12 <= h <= 80):
                continue
            out.append((x + offset, x + offset + w))
    return out


# current split = width of the L image
lw = Image.open(l_tsv[:-4] + ".png").size[0]
iv = words(l_tsv, 0) + words(r_tsv, lw + 8)
iv.sort()
lo, hi = int(W * 0.35), int(W * 0.65)
# merge intervals, find widest gap within band
merged = []
for a, b in iv:
    if merged and a <= merged[-1][1] + 2:
        merged[-1][1] = max(merged[-1][1], b)
    else:
        merged.append([a, b])
best_gap, best_mid = 0, None
for i in range(len(merged) - 1):
    a, b = merged[i][1], merged[i + 1][0]
    g0, g1 = max(a, lo), min(b, hi)
    if g1 - g0 > best_gap:
        best_gap, best_mid = g1 - g0, (g0 + g1) // 2
new_split = best_mid if best_mid else lw
print(f"{page_png.split('/')[-1]} old={lw} new={new_split} gap={best_gap}")
if apply_i and abs(new_split - lw) > 40:
    i = sys.argv.index("--apply")
    out_l, out_r = sys.argv[i + 1], sys.argv[i + 2]
    g = Image.open(page_png).convert("L")
    g.crop((0, 0, new_split - 4, H)).save(out_l)
    g.crop((new_split + 4, 0, W, H)).save(out_r)
    print("RESPLIT")
