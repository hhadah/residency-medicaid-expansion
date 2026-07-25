#!/usr/bin/env python3
"""Assemble the 2000-2009 NRMP program-level dataset and run validation.

Inputs: per-year CSVs in scratch csv/ (final_YYYY.csv preferred, else
res_YYYY.csv, else nrmp_YYYY.csv), overrides.csv (code corrections),
ra_data.csv (RA witness), and the 2010-2019 dta exported baseline.

Output: combined CSV + validation report to stdout.
Usage: assemble_validate.py csv_dir out_csv
"""
import sys
import os
import re
import csv
import collections

csv_dir, out_path = sys.argv[1], sys.argv[2]

STATE_AB = {
    "ALABAMA": "AL", "ALASKA": "AK", "ARIZONA": "AZ", "ARKANSAS": "AR",
    "CALIFORNIA": "CA", "COLORADO": "CO", "CONNECTICUT": "CT", "DELAWARE": "DE",
    "DISTRICT OF COLUMBIA": "DC", "FLORIDA": "FL", "GEORGIA": "GA",
    "HAWAII": "HI", "IDAHO": "ID", "ILLINOIS": "IL", "INDIANA": "IN",
    "IOWA": "IA", "KANSAS": "KS", "KENTUCKY": "KY", "LOUISIANA": "LA",
    "MAINE": "ME", "MARYLAND": "MD", "MASSACHUSETTS": "MA", "MICHIGAN": "MI",
    "MINNESOTA": "MN", "MISSISSIPPI": "MS", "MISSOURI": "MO", "MONTANA": "MT",
    "NEBRASKA": "NE", "NEVADA": "NV", "NEW HAMPSHIRE": "NH", "NEW JERSEY": "NJ",
    "NEW MEXICO": "NM", "NEW YORK": "NY", "NORTH CAROLINA": "NC",
    "NORTH DAKOTA": "ND", "OHIO": "OH", "OKLAHOMA": "OK", "OREGON": "OR",
    "PENNSYLVANIA": "PA", "PUERTO RICO": "PR", "RHODE ISLAND": "RI",
    "SOUTH CAROLINA": "SC", "SOUTH DAKOTA": "SD", "TENNESSEE": "TN",
    "TEXAS": "TX", "UTAH": "UT", "VERMONT": "VT", "VIRGINIA": "VA",
    "WASHINGTON": "WA", "WEST VIRGINIA": "WV", "WISCONSIN": "WI",
    "WYOMING": "WY",
}

overrides = {}
opath = os.path.join(csv_dir, "overrides.csv")
if os.path.exists(opath):
    for r in csv.DictReader(open(opath)):
        overrides[(r["year"], r["old_code"])] = r

# row-level verified corrections from the final verification pass
# keyed by (year, column, line); values (code, quota, matched, plus)
verified = {}
mdir = os.path.join(os.path.dirname(csv_dir.rstrip("/")), "montage")
crop_info = {}
for mf in ("manifest5.csv", "manifest6.csv"):
    p = os.path.join(mdir, mf)
    if os.path.exists(p):
        crop_info.update({r["crop"]: r for r in csv.DictReader(open(p))})
if crop_info:
    import glob as _g
    for rp in sorted(_g.glob(os.path.join(mdir, "read_V*.csv"))):
        for ln in open(rp):
            parts = [x.strip() for x in ln.strip().split(",")]
            if len(parts) < 4 or parts[0] not in crop_info:
                continue
            info = crop_info[parts[0]]
            code, q, m = parts[1].replace(" ", ""), parts[2], parts[3]
            if not (q.isdigit() and m.isdigit()):
                continue
            verified[(info["year"], info["column"], int(info["line"]))] = (
                code, int(q), int(m), "+" in parts[4:])

rows_out = []
report = []
for year in range(2000, 2010):
    y = str(year)
    src = None
    for cand in (f"final_{y}.csv", f"res_{y}.csv", f"nrmp_{y}.csv"):
        p = os.path.join(csv_dir, cand)
        if os.path.exists(p):
            src = p
            break
    rows = list(csv.DictReader(open(src)))
    stats = collections.Counter()
    for r in rows:
        m = re.match(r"(pg-\d+_[LR])\.fixed\.txt:(\d+)", r.get("src", ""))
        if m and (y, m.group(1), int(m.group(2))) in verified:
            code, q, mt, plus = verified[(y, m.group(1), int(m.group(2)))]
            if (len(code) == len(r["code"])) or not r["code"]:
                r["code"], r["quota"], r["matched"] = code, q, mt
                r["flag"] = "verified"
        ov = overrides.get((y, r["code"]))
        if ov:
            r["code"] = ov["new_code"]
            r["quota"], r["matched"] = ov["quota"], ov["matched"]
            r["flag"] = "manual_code_fix"
        q, m = str(r["quota"]), str(r["matched"])
        if not (q.isdigit() and m.isdigit()):
            stats["dropped_unresolved"] += 1
            continue
        old = year <= 2001
        code = r["code"]
        rows_out.append({
            "year": year,
            "state": STATE_AB.get(r["state"], r["state"]),
            "hospital": r["hospital"],
            "city": r["city"],
            "program": r["program"],
            "program_type": r.get("program_type", ""),
            "code": code,
            "inst_code": code[:4],
            "quota": int(q),
            "matched": int(m),
            "plus_flag": r.get("plus_flag", ""),
            "flag": r["flag"],
            "src": r["src"],
        })
        stats[r["flag"]] += 1
    report.append((year, dict(stats)))

# dedup exact (year, code) — keep the best-evidenced row
PRIO = {"verified": 0, "manual": 1, "manual_code_fix": 1, "agree": 2,
        "vision_confirmed": 2, "ok": 3, "clean": 4, "clean_ra_diff": 5,
        "vision_confirmed_ra_diff": 5}
seen = {}
dups = []
for r in rows_out:
    key = (r["year"], r["code"])
    if key in seen:
        prev = seen[key]
        if (prev["quota"], prev["matched"]) == (r["quota"], r["matched"]):
            dups.append((key, "identical"))
            continue
        keep, drop = sorted([prev, r], key=lambda x: PRIO.get(x["flag"], 6))
        seen[key] = keep
        dups.append((key, f'kept {keep["quota"]}/{keep["matched"]} ({keep["flag"]}) '
                          f'dropped {drop["quota"]}/{drop["matched"]} ({drop["flag"]})'))
        continue
    seen[key] = r
deduped = list(seen.values())

with open(out_path, "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=list(deduped[0].keys()))
    w.writeheader()
    w.writerows(deduped)

# RA agreement (2000-01 via constructed pseudo-code: inst4+spec2+'0'+type+'0')
ra = collections.defaultdict(dict)
rp = os.path.join(csv_dir, "ra_data.csv")
if os.path.exists(rp):
    for r in csv.DictReader(open(rp)):
        try:
            ra[int(r["year"])][r["code"]] = (int(float(r["quota"])),
                                             int(float(r["matched"])))
        except ValueError:
            pass

print("== RA-data agreement (info only; RA file has known errors) ==")
for year in range(2000, 2010):
    sub = [r for r in deduped if r["year"] == year]
    hit = agree = 0
    for r in sub:
        key = r["code"]
        if year <= 2001 and len(r["code"]) == 6 and r.get("program_type"):
            key = r["code"] + "0" + r["program_type"] + "0"
            key = r["code"][:4] + r["code"][4:6] + "0" + r["program_type"] + "0"
        raqm = ra.get(year, {}).get(key)
        if raqm:
            hit += 1
            if raqm == (r["quota"], r["matched"]):
                agree += 1
    pct = 100 * agree / hit if hit else 0
    print(f"{year}: RA overlap {hit}/{len(sub)} rows, agreement {pct:.1f}%")

print("\n== per-year summary (post-dedup) ==")
for year, st in report:
    sub = [r for r in deduped if r["year"] == year]
    tq = sum(r["quota"] for r in sub)
    tm = sum(r["matched"] for r in sub)
    mr = tm / tq if tq else 0
    print(f"{year}: rows={len(sub)} quota={tq} matched={tm} rate={mr:.3f}")
    weak = {k: v for k, v in st.items() if k not in
            ("ok", "clean", "vision_confirmed", "agree")}
    print(f"    flags: {weak}")
print(f"\nduplicate (year,code) resolved: {len(dups)}")
for d in dups[:15]:
    print("   ", d)
mgq = [r for r in deduped if r["matched"] > r["quota"]]
print(f"matched>quota rows: {len(mgq)}")
for r in mgq[:10]:
    print("   ", r["year"], r["code"], r["quota"], r["matched"], r["flag"])

# seam check vs 2010 baseline (dta exported to csv: inst, quota10)
bp = os.path.join(csv_dir, "dta2010.csv")
if os.path.exists(bp):
    q09 = collections.Counter()
    for r in deduped:
        if r["year"] == 2009:
            q09[r["inst_code"]] += r["quota"]
    q10 = collections.Counter()
    for r in csv.DictReader(open(bp)):
        q10[r["inst"]] += int(float(r["quota10"]))
    both = set(q09) & set(q10)
    import math
    n = len(both)
    if n > 2:
        xs = [q09[k] for k in both]
        ys = [q10[k] for k in both]
        mx, my = sum(xs) / n, sum(ys) / n
        cov = sum((x - mx) * (y - my) for x, y in zip(xs, ys))
        sd = math.sqrt(sum((x - mx) ** 2 for x in xs) *
                       sum((y - my) ** 2 for y in ys))
        print(f"\n== 2009-2010 seam == inst in both: {n}, "
              f"only-2009: {len(set(q09)-set(q10))}, "
              f"only-2010: {len(set(q10)-set(q09))}, "
              f"quota corr: {cov/sd:.4f}")
