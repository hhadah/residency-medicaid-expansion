#!/usr/bin/env python3
"""Repair tesseract OCR of NRMP tables using a spatial join against Apple
Vision full-column OCR, emitting per-column .fixed.txt files.

For each tesseract TSV line containing a program code:
  - numeric tail tokens (quota, matched) that are clean and confident are kept
  - otherwise Vision observations lying inside the row's y-band and right of
    the code supply the numbers
  - clean-vs-Vision disagreements are flagged (conflict) and cropped
  - rows neither engine can resolve are cropped for manual review
Crops go to <ocr_dir>/review/ as individual PNGs plus an index csv.

If an RA-witness csv is supplied (year,code,quota,matched), resolved rows whose
numbers disagree with the witness are demoted to '?? ??' + crop (ra_mismatch),
so every weakly-supported disagreement gets a human look.

Usage: tsv_fix.py YEAR ocr_dir vision_tsv [ra.csv] [--old]
"""
import sys
import os
import re
import csv
import glob
from PIL import Image

CONF_T = 80.0
MODERN_CODE_OK = re.compile(r"^\d{7}[A-Z]\d$")
OLD_CODE_OK = re.compile(r"^\d{6}$")
GLYPH = str.maketrans({
    "I": "1", "l": "1", "|": "1", "]": "1", "[": "1", "}": "1", "{": "1",
    "(": "1", ")": "1", "!": "1", "i": "1",
    "O": "0", "o": "0", "Q": "0",
    "S": "5", "s": "5", "Z": "2", "z": "2", "B": "8", "G": "6",
})


def is_codeish(t, old):
    t = t.strip(".,;:'\"-")
    if old:
        return bool(re.match(r"^[\dOoIl|!\]\[}{()iQSZB]{6}\+?$", t)) and \
            sum(c.isdigit() for c in t) >= 4
    if len(t) < 8 or len(t) > 11:
        return False
    return sum(c.isdigit() for c in t) >= 6


def clean_code(t, old):
    t = t.strip(".,;:'\"-").replace(" ", "")
    if old:
        t2 = t.rstrip("+").translate(GLYPH)
        return t2 if OLD_CODE_OK.match(t2) else None
    if MODERN_CODE_OK.match(t):
        return t
    if len(t) >= 9:
        for i in range(len(t) - 8):
            sub = t[i:i + 9]
            head, letter, last = sub[:7], sub[7], sub[8]
            head2, last2 = head.translate(GLYPH), last.translate(GLYPH)
            if head2.isdigit() and letter.isalpha() and last2.isdigit():
                return head2 + letter.upper() + last2
    if len(t) == 10:
        head, mid = t[:7], t[7:]
        head2 = head.translate(GLYPH)
        m = re.match(r"^([A-Za-z])[Oo]?([0-9OoQIl|!\]\[}{()i])$", mid)
        if head2.isdigit() and m:
            return head2 + m.group(1).upper() + m.group(2).translate(GLYPH)
    return None


def numy(t):
    t2 = t.strip(".,;:'\"").translate(GLYPH)
    return t2 if t2.isdigit() else None


def read_tess_tsv(path):
    lines = {}
    with open(path) as f:
        rd = csv.reader(f, delimiter="\t", quoting=csv.QUOTE_NONE)
        next(rd, None)
        for row in rd:
            if len(row) < 12 or row[0] != "5" or not row[11].strip():
                continue
            key = (int(row[2]), int(row[3]), int(row[4]))
            lines.setdefault(key, []).append(
                {"x": int(row[6]), "y": int(row[7]), "w": int(row[8]),
                 "h": int(row[9]), "conf": float(row[10]), "text": row[11]})
    return [sorted(v, key=lambda w: w["x"]) for k, v in sorted(lines.items())]


def read_vision(path):
    """vision tsv -> {png_basename: [(x, ymid, text), ...]} normalized coords."""
    out, cur = {}, None
    with open(path) as f:
        for ln in f:
            ln = ln.rstrip("\n")
            if ln.startswith("===FILE:"):
                cur = os.path.basename(ln[8:-3])
                out[cur] = []
            elif cur and ln.count("\t") == 4:
                x, y, w, h, t = ln.split("\t")
                x, y, w, h = float(x), float(y), float(w), float(h)
                out[cur].append((x, y + h / 2, w, t))
    return out


def vision_tail_numbers(vobs, y0, y1, xmin):
    """Numeric vision tokens whose y-center is inside [y0,y1] and x>xmin."""
    toks = []
    for x, ym, w, t in vobs:
        if not (y0 <= ym <= y1):
            continue
        parts = t.split()
        step = w / max(len(t), 1)
        pos = 0
        for p in parts:
            idx = t.find(p, pos)
            pos = idx + len(p)
            px = x + idx * step
            if px < xmin:
                continue
            toks.append((px, p))
    toks.sort()
    plus = any(p == "+" for _, p in toks)
    nums = [numy(p) for _, p in toks if p != "+"]
    nums = [n for n in nums if n is not None and len(n) <= 3]
    return nums, plus


def main():
    year = sys.argv[1]
    ocr_dir = sys.argv[2].rstrip("/")
    vision_tsv = sys.argv[3]
    old = "--old" in sys.argv
    ra = {}
    if len(sys.argv) > 4 and sys.argv[4].endswith(".csv"):
        with open(sys.argv[4]) as f:
            for r in csv.DictReader(f):
                if r["year"] == year:
                    try:
                        ra[r["code"]] = (int(float(r["quota"])),
                                         int(float(r["matched"])))
                    except ValueError:
                        pass

    vis = read_vision(vision_tsv)
    review_dir = os.path.join(ocr_dir, "review")
    os.makedirs(review_dir, exist_ok=True)
    import collections
    stats = collections.Counter()
    review_idx = []
    flag_rows = []

    for tsv in sorted(glob.glob(os.path.join(ocr_dir, "pg-*_[LR].tsv"))):
        base = tsv[:-4]
        png = base + ".png"
        img = Image.open(png)
        W, H = img.size
        vobs = vis.get(os.path.basename(png), [])
        out_lines = []
        for ws in read_tess_tsv(tsv):
            toks = [w["text"] for w in ws]
            ci = next((i for i, t in enumerate(toks) if is_codeish(t, old)), None)
            joined = None
            if ci is None:
                for i in range(len(toks) - 1):
                    j = clean_code(toks[i] + toks[i + 1], old)
                    if j:
                        ci, joined = i + 1, j
                        break
            if ci is None or ci < 1:
                out_lines.append(" ".join(toks))
                continue
            code = joined or clean_code(toks[ci], old)
            ci_name = ci - 1 if joined else ci
            name = " ".join(w["text"] for w in ws[:ci_name])
            code_w = ws[ci]
            plus_t = any(t.strip() == "+" for t in toks[ci + 1:])
            tail = []
            for w in ws[ci + 1:]:
                if w["text"].strip() == "+":
                    continue
                n = numy(w["text"])
                good = (n is not None and w["conf"] >= CONF_T
                        and w["text"].strip(".,;:'\"") == n and len(n) <= 3)
                tail.append((n, good, w))
            t_ok = len(tail) == 2 and all(g for _, g, _ in tail)
            # vision lookup in the row band
            y0 = (min(w["y"] for w in ws) - 4) / H
            y1 = (max(w["y"] + w["h"] for w in ws) + 4) / H
            xmin = (code_w["x"] + code_w["w"] - 2) / W
            vnums, vplus = vision_tail_numbers(vobs, y0, y1, xmin)
            v_ok = len(vnums) == 2
            plus = plus_t or vplus

            def emit(q, m, tag):
                out_lines.append(f"{name} {code or toks[ci]} {q} {m}"
                                 + (" +" if plus and old else ""))
                stats[tag] += 1
                flag_rows.append([os.path.basename(base), code or toks[ci],
                                  tag, q, m])

            def emit_checked(q, m, tag):
                """Emit with plausibility + witness checks.

                matched>quota is impossible -> always a review crop.
                RA disagreement demotes only Vision-only numbers (the RA file
                is itself ~18% wrong, and two-engine agreement beats it)."""
                raqm = ra.get(code) if code else None
                if int(m) > int(q):
                    emit("??", "??", "unresolved")
                    review_idx.append(make_crop(
                        img, ws, base, len(review_idx), review_dir, code,
                        f"m_gt_q {tag}={q}/{m}"))
                elif tag == "vision_fix" and raqm and raqm != (int(q), int(m)):
                    emit("??", "??", "unresolved")
                    review_idx.append(make_crop(
                        img, ws, base, len(review_idx), review_dir, code,
                        f"ra_mismatch {tag}={q}/{m} ra={raqm[0]}/{raqm[1]}"))
                elif raqm and raqm != (int(q), int(m)):
                    emit(q, m, tag + "_ra_diff")
                else:
                    emit(q, m, tag)

            if t_ok and v_ok:
                if [t[0] for t in tail] == vnums:
                    emit_checked(tail[0][0], tail[1][0], "vision_confirmed")
                else:
                    # both confident but disagree -> crop for manual check
                    emit(tail[0][0], tail[1][0], "conflict")
                    review_idx.append(make_crop(
                        img, ws, base, len(review_idx), review_dir, code,
                        f"tess={tail[0][0]}/{tail[1][0]} vis={vnums[0]}/{vnums[1]}"))
            elif t_ok:
                emit_checked(tail[0][0], tail[1][0], "clean")
            elif v_ok:
                emit_checked(vnums[0], vnums[1], "vision_fix")
            else:
                emit("??", "??", "unresolved")
                review_idx.append(make_crop(
                    img, ws, base, len(review_idx), review_dir, code,
                    f"tess_raw={' '.join(w['text'] for w in ws[ci+1:])} "
                    f"vis={'/'.join(vnums) if vnums else '-'}"))
        with open(base + ".fixed.txt", "w") as f:
            f.write("\n".join(out_lines) + "\n")

    with open(os.path.join(ocr_dir, "review", "index.csv"), "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["crop", "column", "code", "detail"])
        w.writerows(review_idx)
    with open(os.path.join(ocr_dir, "flags.csv"), "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["column", "code", "tag", "quota", "matched"])
        w.writerows(flag_rows)
    print(f"{year}: " + " ".join(f"{k}={v}" for k, v in stats.items())
          + f" | review crops: {len(review_idx)}")


def make_crop(img, ws, base, idx, review_dir, code, detail):
    # full column width: tesseract may have missed the rightmost (matched)
    # digits entirely, so never bound the crop by detected words
    x0 = max(0, min(w["x"] for w in ws) - 8)
    y0 = max(0, min(w["y"] for w in ws) - 8)
    x1 = img.size[0]
    y1 = min(img.size[1], max(w["y"] + w["h"] for w in ws) + 8)
    c = img.crop((x0, y0, x1, y1))
    c = c.resize((c.size[0] * 2, c.size[1] * 2), Image.LANCZOS)
    name = f"{os.path.basename(base)}_{idx:03d}.png"
    c.save(os.path.join(review_dir, name))
    return [name, os.path.basename(base), code or "", detail]


if __name__ == "__main__":
    main()
