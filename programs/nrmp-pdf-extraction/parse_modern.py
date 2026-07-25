#!/usr/bin/env python3
"""Parse NRMP 'Match Outcome for All Institutions by State' tables, modern format
(9-char program codes), from column-split text (pdftotext -layout or tesseract OCR).

Usage: parse_modern.py YEAR out.csv rejects.txt glob1 [glob2 ...]
"""
import re
import sys
import csv

STATES = [
    "ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA", "COLORADO",
    "CONNECTICUT", "DELAWARE", "DISTRICT OF COLUMBIA", "FLORIDA", "GEORGIA",
    "HAWAII", "IDAHO", "ILLINOIS", "INDIANA", "IOWA", "KANSAS", "KENTUCKY",
    "LOUISIANA", "MAINE", "MARYLAND", "MASSACHUSETTS", "MICHIGAN", "MINNESOTA",
    "MISSISSIPPI", "MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE",
    "NEW JERSEY", "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", "NORTH DAKOTA",
    "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA", "PUERTO RICO", "RHODE ISLAND",
    "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE", "TEXAS", "UTAH", "VERMONT",
    "VIRGINIA", "WASHINGTON", "WEST VIRGINIA", "WISCONSIN", "WYOMING",
]
STATE_IDX = {s: i for i, s in enumerate(STATES)}
MAX_STATE_GAP = 3  # a legit new section is at most a few states ahead

ROW_RE = re.compile(r"^\s*(.+?)\s+(\d{7}[A-Z]\d)\s+(\S+)\s+(\S+)\s*$")
GLUED_RE = re.compile(r"^\s*(.{3,}?)(\d{7}[A-Z]\d)\s+(\S+)\s+(\S+)\s*$")
CITY_HDR_RE = re.compile(r"^\s*([A-Z][A-Z &/\.'-]+?)\s+Code\s+Quota\s+Matched\s*$")
SKIP_RE = re.compile(
    r"(NRMP Program Results|Main Residency Match|Match Outcome for All"
    r"|RESULTS AND D|^\s*C[O0]DE\s+QU[O0]TA|C[O0]DE\s+QU[O0]TA\s+MATCHED\s*$"
    r"|^\s*Page\s+\d+\s+of\s+\d+|^\s*\d+\s*[/of]+\s*\d+\s*$|^\s*-?\s*\d+\s*-?\s*$"
    r"|idency Match|Main Resi|^\s*ain Residency)",
    re.IGNORECASE,
)
CONT_RE = re.compile(r"[\s(]*CONTINUED\)?\s*$", re.IGNORECASE)

# OCR glyph -> digit map, applied only to the quota/matched fields
GLYPH = str.maketrans({
    "I": "1", "l": "1", "|": "1", "]": "1", "[": "1", "}": "1", "{": "1",
    "(": "1", ")": "1", "!": "1", "i": "1",
    "O": "0", "o": "0", "Q": "0",
    "S": "5", "s": "5", "Z": "2", "z": "2", "B": "8", "G": "6",
})

# a 9-char code with OCR damage: spaces inside ("1903 140C0"), stray O after the
# letter ("1044140CO0", "1041140MO0"), trailing O ("1024160C0O"), glyphed last
# digit ("3081120C!", "1039140P I")
CODE_GAP_RE = re.compile(
    r"(?<![\dA-Za-z])((?:\d ?){7})([A-Za-z])[Oo]? ?([0-9OoQIl|\!\]\[}{()i])[Oo]?(?![\dA-Za-z])")


def ocr_normalize(line):
    s = line.replace("’", "'").replace("‘", "'").replace("—", " ").replace("–", " ")
    def _fix(m):
        digits = m.group(1).replace(" ", "")
        letter = m.group(2).upper()
        last = m.group(3).translate(GLYPH)
        return digits + letter + last
    return CODE_GAP_RE.sub(_fix, s)


def parse_num(tok):
    t = tok.translate(GLYPH).replace(",", "").replace(".", "")
    return int(t) if t.isdigit() else None


def norm_caps(line):
    return re.sub(r"\s+", " ", re.sub(r"[^A-Z ]", "", line.upper())).strip()


def parse_lines(lines, year):
    out, rejects = [], []
    state = hospital = city = None
    for src_label, ln_no, raw in lines:
        line = raw.rstrip("\n")
        if not line.strip():
            continue
        if SKIP_RE.search(line):
            continue
        line = ocr_normalize(line)
        m = ROW_RE.match(line) or GLUED_RE.match(line)
        if m:
            q, mt = parse_num(m.group(3)), parse_num(m.group(4))
            raw_tail = f"{m.group(3)} {m.group(4)}"
            if q is None or mt is None:
                flag = "badnum"  # keep row with context; numbers filled from Vision
                q = q if q is not None else ""
                mt = mt if mt is not None else ""
            else:
                flag = "ok" if raw_tail == f"{q} {mt}" else "repaired"
            out.append({
                "year": year, "state": state, "hospital": hospital, "city": city,
                "program": m.group(1).strip(), "code": m.group(2),
                "quota": q, "matched": mt, "flag": flag,
                "src": f"{src_label}:{ln_no}",
            })
            continue
        txt = line.strip()
        # a data-like line that failed to parse (has code-ish token) -> reject list
        if re.search(r"\d{6,}", txt):
            rejects.append(f"{src_label}:{ln_no}: NOPARSE: {line}")
            continue
        caps = norm_caps(txt)
        if caps in STATE_IDX and not any(c.islower() for c in txt):
            gap = STATE_IDX[caps] - STATE_IDX[state] if state else 1
            if state is None or 0 < gap <= MAX_STATE_GAP:
                state = caps
                continue
            # same state again or far jump: it's a city (e.g. NEW YORK, WASHINGTON)
            city = caps
            continue
        mc = CITY_HDR_RE.match(line)
        if mc:
            city = mc.group(1).strip()
            continue
        if CONT_RE.search(txt) and len(CONT_RE.sub("", txt).strip()) >= 3:
            hospital = CONT_RE.sub("", txt).strip()
            continue
        if not any(c.islower() for c in txt) and re.match(r"^[A-Z][A-Z &/\.'’-]+$", txt) and len(txt) >= 3:
            city = txt
            continue
        if re.search(r"[A-Za-z]{3}", txt) and re.search(r"[aeiouAEIOU]", txt) and len(txt) >= 5:
            # keep city: a new block re-states its city, a page-break continuation doesn't
            hospital = txt
            continue
        rejects.append(f"{src_label}:{ln_no}: MISC: {line}")
    return out, rejects


def main():
    import glob as _glob
    year = int(sys.argv[1])
    out_csv, rej_path = sys.argv[2], sys.argv[3]
    paths = []
    for pat in sys.argv[4:]:
        paths.extend(sorted(_glob.glob(pat)))
    all_lines = []
    for path in paths:
        with open(path, encoding="utf-8", errors="replace") as f:
            label = path.split("/")[-1]
            all_lines.extend((label, i, ln) for i, ln in enumerate(f, 1))
    rows, rejects = parse_lines(all_lines, year)
    with open(out_csv, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=[
            "year", "state", "hospital", "city", "program", "code",
            "quota", "matched", "flag", "src"])
        w.writeheader()
        w.writerows(rows)
    with open(rej_path, "w") as f:
        f.write("\n".join(rejects))
    tq = sum(r["quota"] for r in rows if isinstance(r["quota"], int))
    tm = sum(r["matched"] for r in rows if isinstance(r["matched"], int))
    nrep = sum(1 for r in rows if r["flag"] != "ok")
    print(f"{year}: rows={len(rows)} quota={tq} matched={tm} repaired={nrep} rejects={len(rejects)}")


if __name__ == "__main__":
    main()
