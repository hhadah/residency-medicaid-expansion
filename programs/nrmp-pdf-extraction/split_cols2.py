#!/usr/bin/env python3
"""Column split v2: choose the RIGHTMOST whitespace valley (run of near-empty
pixel columns >= min_run) in the central band — the true inter-column gutter —
rather than the widest, which can be the quota-matched gap inside the left
column.

Usage: split_cols2.py in.png out_L.png out_R.png   (prints chosen split)
       split_cols2.py --check in.png               (prints split only)
"""
import sys
from PIL import Image

check = sys.argv[1] == "--check"
inp = sys.argv[2] if check else sys.argv[1]
img = Image.open(inp).convert("L")
w, h = img.size
binimg = img.point(lambda v: 255 if v < 128 else 0)
prof = list(binimg.resize((w, 1), Image.BOX).getdata())
lo, hi = int(w * 0.35), int(w * 0.65)
MIN_RUN = 40
valleys = []
run, start = 0, None
for i in range(lo, hi + 1):
    if i < hi and prof[i] <= 1:
        if start is None:
            start = i
        run += 1
    else:
        if start is not None and run >= MIN_RUN:
            valleys.append((start, start + run))
        run, start = 0, None
if valleys:
    s, e = valleys[-1]
    split = (s + e) // 2
else:
    split = w // 2
print(f"{inp.split('/')[-1]}: split={split} ({100*split/w:.1f}%) "
      f"valleys={[(a, b - a) for a, b in valleys]}")
if not check:
    img.crop((0, 0, split, h)).save(sys.argv[2])
    img.crop((split, 0, w, h)).save(sys.argv[3])
