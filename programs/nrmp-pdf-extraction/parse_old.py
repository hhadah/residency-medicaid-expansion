#!/usr/bin/env python3
"""Parse NRMP 'Hospital Results by Location' tables (2000-2001 typewriter format)
from column-split tesseract text.

Format: STATE (postal-code order, repeated at column tops) -> HOSPITAL (caps)
-> CITY (caps) -> rows: 'SPECIALTY NAME <type letter> <6-digit code> <quota> <matched> [+]'
Code = 4-digit institution + 2-digit sequential program number.
Ends with 'TOTALS <quota> <matched>'.

Usage: parse_old.py YEAR out.csv rejects.txt glob1 [glob2 ...]
"""
import re
import sys
import csv

# postal-code order (AK, AL, AR, AZ, CA, ...) as used by these volumes
POSTAL_ORDER = [
    ("AK", "ALASKA"), ("AL", "ALABAMA"), ("AR", "ARKANSAS"), ("AZ", "ARIZONA"),
    ("CA", "CALIFORNIA"), ("CO", "COLORADO"), ("CT", "CONNECTICUT"),
    ("DC", "DISTRICT OF COLUMBIA"), ("DE", "DELAWARE"), ("FL", "FLORIDA"),
    ("GA", "GEORGIA"), ("HI", "HAWAII"), ("IA", "IOWA"), ("ID", "IDAHO"),
    ("IL", "ILLINOIS"), ("IN", "INDIANA"), ("KS", "KANSAS"), ("KY", "KENTUCKY"),
    ("LA", "LOUISIANA"), ("MA", "MASSACHUSETTS"), ("MD", "MARYLAND"),
    ("ME", "MAINE"), ("MI", "MICHIGAN"), ("MN", "MINNESOTA"), ("MO", "MISSOURI"),
    ("MS", "MISSISSIPPI"), ("MT", "MONTANA"), ("NC", "NORTH CAROLINA"),
    ("ND", "NORTH DAKOTA"), ("NE", "NEBRASKA"), ("NH", "NEW HAMPSHIRE"),
    ("NJ", "NEW JERSEY"), ("NM", "NEW MEXICO"), ("NV", "NEVADA"),
    ("NY", "NEW YORK"), ("OH", "OHIO"), ("OK", "OKLAHOMA"), ("OR", "OREGON"),
    ("PA", "PENNSYLVANIA"), ("PR", "PUERTO RICO"), ("RI", "RHODE ISLAND"),
    ("SC", "SOUTH CAROLINA"), ("SD", "SOUTH DAKOTA"), ("TN", "TENNESSEE"),
    ("TX", "TEXAS"), ("UT", "UTAH"), ("VA", "VIRGINIA"), ("VT", "VERMONT"),
    ("WA", "WASHINGTON"), ("WI", "WISCONSIN"), ("WV", "WEST VIRGINIA"),
    ("WY", "WYOMING"),
]
STATE_IDX = {name: i for i, (ab, name) in enumerate(POSTAL_ORDER)}
MAX_STATE_GAP = 4

ROW_RE = re.compile(
    r"^\s*(.+?)\s+(\d{4}\s?\d{2})\s+(\S+)\s+(\S+?)(\s*\+)?\s*$")
TOTALS_RE = re.compile(r"^\s*TOTALS?\s+([\d,]+)\s+([\d,]+)\s*$", re.IGNORECASE)
SKIP_RE = re.compile(
    r"(HOSPITAL RESULTS BY LOCATION|^\s*C[O0]DE\s+QU[O0]TA"
    r"|C[O0]DE\s+QU[O0]TA\s+MATCHED|^\s*PAGE\s*[-~=]*\s*\d+|^\s*CONTINUED\s*$)",
    re.IGNORECASE,
)
CONT_RE = re.compile(r"[\s(]*CONTINUED\)?\s*$", re.IGNORECASE)

GLYPH = str.maketrans({
    "I": "1", "l": "1", "|": "1", "]": "1", "[": "1", "}": "1", "{": "1",
    "(": "1", ")": "1", "!": "1", "i": "1",
    "O": "0", "o": "0", "Q": "0",
    "S": "5", "s": "5", "Z": "2", "z": "2", "B": "8", "G": "6",
})


def parse_num(tok):
    t = tok.translate(GLYPH).replace(",", "").replace(".", "")
    return int(t) if t.isdigit() else None


def norm_caps(line):
    return re.sub(r"\s+", " ", re.sub(r"[^A-Z ]", "", line.upper())).strip()


def parse_lines(lines, year):
    out, rejects, totals = [], [], None
    state = hospital = city = None
    buffer = []  # pending caps lines: [... hospital, city]
    for src_label, ln_no, raw in lines:
        line = raw.rstrip("\n")
        if not line.strip():
            continue
        if SKIP_RE.search(line):
            continue
        mt_ = TOTALS_RE.match(line)
        if mt_:
            totals = (parse_num(mt_.group(1)), parse_num(mt_.group(2)))
            continue
        m = ROW_RE.match(line)
        if m and re.search(r"[A-Za-z]{3}", m.group(1)):
            q, mt = parse_num(m.group(3)), parse_num(m.group(4))
            badnum = q is None or mt is None
            if badnum:
                q = q if q is not None else ""
                mt = mt if mt is not None else ""
            if buffer:
                if len(buffer) >= 2:
                    hospital, city = buffer[-2], buffer[-1]
                else:
                    city = buffer[-1]
                buffer = []
            name = m.group(1).strip()
            ptype = ""
            mtype = re.match(r"^(.*?)\s+([A-Z])$", name)
            if mtype:
                name, ptype = mtype.group(1), mtype.group(2)
            code = m.group(2).replace(" ", "")
            raw_tail = f"{m.group(3)} {m.group(4)}"
            flag = ("badnum" if badnum
                    else "ok" if raw_tail == f"{q} {mt}" else "repaired")
            out.append({
                "year": year, "state": state, "hospital": hospital, "city": city,
                "program": name, "program_type": ptype, "code": code,
                "quota": q, "matched": mt,
                "plus_flag": 1 if m.group(5) else 0, "flag": flag,
                "src": f"{src_label}:{ln_no}",
            })
            continue
        txt = line.strip()
        if re.search(r"\d{5,}", txt):
            rejects.append(f"{src_label}:{ln_no}: NOPARSE: {line}")
            continue
        caps = norm_caps(txt)
        if caps in STATE_IDX:
            if state == caps:
                continue  # column-top repeat of current state
            gap = STATE_IDX[caps] - STATE_IDX[state] if state else 1
            if state is None or 0 < gap <= MAX_STATE_GAP:
                state = caps
                buffer = []
                continue
            buffer.append(caps)  # far jump: treat as city/hospital text
            continue
        if CONT_RE.search(txt) and len(CONT_RE.sub("", txt).strip()) >= 3:
            hospital = CONT_RE.sub("", txt).strip()
            buffer = []
            continue
        if re.search(r"[A-Za-z]{3}", txt) and len(txt) >= 4:
            buffer.append(txt)
            if len(buffer) > 2:
                buffer = buffer[-2:]
            continue
        rejects.append(f"{src_label}:{ln_no}: MISC: {line}")
    return out, rejects, totals


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
    rows, rejects, totals = parse_lines(all_lines, year)
    with open(out_csv, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=[
            "year", "state", "hospital", "city", "program", "program_type",
            "code", "quota", "matched", "plus_flag", "flag", "src"])
        w.writeheader()
        w.writerows(rows)
    with open(rej_path, "w") as f:
        f.write("\n".join(rejects))
    tq = sum(r["quota"] for r in rows if isinstance(r["quota"], int))
    tm = sum(r["matched"] for r in rows if isinstance(r["matched"], int))
    nrep = sum(1 for r in rows if r["flag"] != "ok")
    print(f"{year}: rows={len(rows)} quota={tq} matched={tm} repaired={nrep} "
          f"rejects={len(rejects)} printed_totals={totals}")


if __name__ == "__main__":
    main()
