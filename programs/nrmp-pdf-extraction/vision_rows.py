#!/usr/bin/env python3
"""Reconstruct program rows from visionocr TSV observations.

Clusters observations into visual rows by y, sorts tokens by x, and extracts
(code, quota, matched) for rows anchored on a program code.

Usage: vision_rows.py vision_YEAR.tsv out.csv [--old]
  --old : 2000-2001 format, 6-digit codes possibly followed by + flag
"""
import sys
import csv
import re

MODERN_CODE = re.compile(r"^(\d{7}[A-Z]\d)$")
OLD_CODE = re.compile(r"^(\d{6})$")

GLYPH = str.maketrans({
    "I": "1", "l": "1", "|": "1", "]": "1", "[": "1", "}": "1", "{": "1",
    "(": "1", ")": "1", "!": "1", "i": "1",
    "O": "0", "o": "0", "Q": "0",
    "S": "5", "s": "5", "Z": "2", "z": "2", "B": "8", "G": "6",
})


def parse_num(tok):
    t = tok.translate(GLYPH).replace(",", "").replace(".", "")
    return int(t) if t.isdigit() else None


def norm_code_token(tok, old=False):
    """Clean a candidate code token; return code or None."""
    t = tok.strip().replace(" ", "")
    if old:
        t2 = t.translate(GLYPH)
        return t2 if re.match(r"^\d{6}$", t2) else None
    if re.match(r"^\d{7}[A-Z]\d$", t):
        return t
    # last char O->0 etc; letter position 8
    if len(t) == 9:
        head, letter, last = t[:7], t[7], t[8]
        head = head.translate(GLYPH)
        last = last.translate(GLYPH)
        if head.isdigit() and letter.isalpha() and last.isdigit():
            return head + letter.upper() + last
    return None


def rows_from_obs(obs, old=False):
    """obs: list of (x, y, w, h, text). Returns row dicts."""
    # cluster into lines by y using median obs height
    obs = sorted(obs, key=lambda o: o[1])
    if not obs:
        return []
    heights = sorted(o[3] for o in obs)
    tol = max(0.004, heights[len(heights) // 2] * 0.6)
    lines, cur, cur_y = [], [], None
    for o in obs:
        if cur_y is None or o[1] - cur_y <= tol:
            cur.append(o)
            cur_y = o[1] if cur_y is None else (cur_y + o[1]) / 2
        else:
            lines.append(cur)
            cur, cur_y = [o], o[1]
    if cur:
        lines.append(cur)

    out = []
    for line in lines:
        line.sort(key=lambda o: o[0])
        # split every observation text into tokens with approximate x
        toks = []
        for x, y, w, h, text in line:
            parts = text.split()
            if not parts:
                continue
            step = w / max(len(text), 1)
            pos = 0
            for p in parts:
                idx = text.find(p, pos)
                pos = idx + len(p)
                toks.append((x + idx * step, p))
        toks.sort(key=lambda t: t[0])
        code, code_i = None, None
        for i, (x, p) in enumerate(toks):
            c = norm_code_token(p, old)
            if c:
                code, code_i = c, i
                break
        if code is None:
            # try joining adjacent tokens (code split by space)
            for i in range(len(toks) - 1):
                c = norm_code_token(toks[i][1] + toks[i + 1][1], old)
                if c:
                    code, code_i = c, i + 1
                    break
        if code is None:
            continue
        tail = [p for _, p in toks[code_i + 1:]]
        plus = 1 if any(p == "+" for p in tail) else 0
        nums = [parse_num(p) for p in tail if p != "+"]
        nums = [n for n in nums if n is not None]
        q, m = (nums[0], nums[1]) if len(nums) >= 2 else (
            (nums[0], None) if nums else (None, None))
        out.append({"code": code, "quota": q, "matched": m,
                    "n_tail": len(nums), "plus_flag": plus})
    return out


def main():
    tsv_path, out_path = sys.argv[1], sys.argv[2]
    old = "--old" in sys.argv
    rows, obs, fname = [], [], None
    def flush():
        for r in rows_from_obs(obs, old):
            r["src"] = fname
            rows.append(r)
    with open(tsv_path) as f:
        for ln in f:
            ln = ln.rstrip("\n")
            if ln.startswith("===FILE:"):
                flush()
                obs, fname = [], ln[8:-3].split("/")[-1]
                continue
            parts = ln.split("\t")
            if len(parts) != 5:
                continue
            x, y, w, h = map(float, parts[:4])
            obs.append((x, y, w, h, parts[4]))
    flush()
    with open(out_path, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=["code", "quota", "matched",
                                          "n_tail", "plus_flag", "src"])
        w.writeheader()
        w.writerows(rows)
    full = sum(1 for r in rows if r["n_tail"] >= 2)
    print(f"{tsv_path.split('/')[-1]}: rows={len(rows)} with-2-nums={full}")


if __name__ == "__main__":
    main()
