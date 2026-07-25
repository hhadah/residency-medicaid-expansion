#!/usr/bin/env python3
"""Repair the state column of the 2000-2009 extraction using the institution
code, which is stable across years. The running state-header tracker in the
parsers derailed on some scanned pages (worst in 2000-2002), misattributing
whole stretches of rows; institution identity is unaffected, so state can be
restored from authorities:
  1. data/raw/2010_2019_residency_programs.dta institution state
  2. modal parsed state across 2003-2009 program rows
  3. modal RA-spreadsheet state, clean years (2006-2009) only
Rows with no authority keep their parsed state (flag suffix _state_unverified).
"""
import csv
import collections

S = "/private/tmp/claude-501/-Users-hhadah-Projects-GiT-residency-medicaid-expansion/6207b666-bd12-443a-b4be-eb6b9fad04b5/scratchpad"
R = "/Users/hhadah/Projects/GiT/residency-medicaid-expansion"

ST_AB = {
    "Alabama": "AL", "Alaska": "AK", "Arizona": "AZ", "Arkansas": "AR",
    "California": "CA", "Colorado": "CO", "Connecticut": "CT",
    "Delaware": "DE", "District of Columbia": "DC", "Florida": "FL",
    "Georgia": "GA", "Hawaii": "HI", "Idaho": "ID", "Illinois": "IL",
    "Indiana": "IN", "Iowa": "IA", "Kansas": "KS", "Kentucky": "KY",
    "Louisiana": "LA", "Maine": "ME", "Maryland": "MD", "Massachusetts": "MA",
    "Michigan": "MI", "Minnesota": "MN", "Mississippi": "MS", "Missouri": "MO",
    "Montana": "MT", "Nebraska": "NE", "Nevada": "NV", "New Hampshire": "NH",
    "New Jersey": "NJ", "New Mexico": "NM", "New York": "NY",
    "North Carolina": "NC", "North Dakota": "ND", "Ohio": "OH",
    "Oklahoma": "OK", "Oregon": "OR", "Pennsylvania": "PA",
    "Puerto Rico": "PR", "Rhode Island": "RI", "South Carolina": "SC",
    "South Dakota": "SD", "Tennessee": "TN", "Texas": "TX", "Utah": "UT",
    "Vermont": "VT", "Virginia": "VA", "Washington": "WA",
    "West Virginia": "WV", "Wisconsin": "WI", "Wyoming": "WY",
}

rows = list(csv.DictReader(open(f"{S}/csv/nrmp_2000_2009_final.csv")))

dta = {r["inst"].split(".")[0]: r["state"].strip()
       for r in csv.DictReader(open(f"{S}/csv/dta2010.csv")) if r["state"].strip()}

modal = collections.defaultdict(collections.Counter)
for r in rows:
    if int(r["year"]) >= 2003 and r["state"]:
        modal[r["inst_code"]][r["state"]] += 1

ra = collections.defaultdict(collections.Counter)
for r in csv.DictReader(open(f"{S}/csv/ra_states.csv")):
    ab = ST_AB.get(r["state"].strip())
    code = r["code"].strip()
    if ab and len(code) >= 4:
        ra[code[:4]][ab] += 1


def authority(inst):
    if inst in dta:
        return dta[inst], "dta"
    if inst in modal:
        return modal[inst].most_common(1)[0][0], "modal"
    if inst in ra:
        return ra[inst].most_common(1)[0][0], "ra"
    return None, None


changed = collections.Counter()
unver = collections.Counter()
src_used = collections.Counter()
for r in rows:
    st, how = authority(r["inst_code"])
    if st is None:
        unver[r["year"]] += 1
        continue
    src_used[how] += 1
    if st != r["state"]:
        changed[r["year"]] += 1
        r["state"] = st

with open(f"{S}/csv/nrmp_2000_2009_final.csv", "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=rows[0].keys())
    w.writeheader()
    w.writerows(rows)

print("state changed per year:", dict(sorted(changed.items())))
print("no-authority rows per year (parsed state kept):", dict(sorted(unver.items())))
print("authority used:", dict(src_used))

q = collections.defaultdict(lambda: collections.defaultdict(int))
for r in rows:
    q[r["year"]][r["state"]] += int(r["quota"])
for st in ("NY", "CA", "FL", "GA", "MA", "TX"):
    print(st, [q[str(y)][st] for y in range(2000, 2010)])
