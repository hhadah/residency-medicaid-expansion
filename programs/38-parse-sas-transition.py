#!/usr/bin/env python3
# =============================================================================
# 38-parse-sas-transition.py
# Parse the ACGME ADS public reports on the AOA->ACGME Single Accreditation
# System (SAS) transition into flat CSVs for the entrant-classification
# exercise (desk review 2026-07-26, return condition):
#
#   input : data/raw/sas-transition/sas_programs_report18.pdf
#           ("All Programs that Applied for Accreditation Under the Single
#            Accreditation System", ADS public Report 18 — full historical
#            list incl. withdrawals; downloaded 2026-07-26)
#           data/raw/sas-transition/sas_sponsors_report14.pdf
#           (ADS public Report 14 — sponsor-level list, current AY snapshot)
#   output: data/raw/sas_transition_programs.csv
#             program_number, program_name, city, state, specialty,
#             accreditation_status, effective_date
#           data/raw/sas_transition_institutions.csv
#             institution_name, state, n_programs  (deduplicated name x state)
#
# Requires poppler's pdftotext on PATH. Run from the repo root.
# =============================================================================
import csv
import re
import subprocess
import sys
from collections import Counter
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
RAW = ROOT / "data" / "raw"
PDF_PROGRAMS = RAW / "sas-transition" / "sas_programs_report18.pdf"
PDF_SPONSORS = RAW / "sas-transition" / "sas_sponsors_report14.pdf"

CITY_ST = re.compile(r"([A-Za-z .'\-]+),?\s+([A-Z]{2})[,.]?\s+(\d{5})")
DATE = re.compile(r"\d{2}/\d{2}/\d{4}")
REC_START = re.compile(r"^\[(\d{10})\]\s+(.*)$")
SPONSOR_START = re.compile(r"^\[(\d{6})\]\s+(.*)$")

STATUSES = [
    "Continued Accreditation (Continued Pre-Accreditation)",
    "Voluntary Withdrawal (Continued Pre-Accreditation)",
    "Continued Accreditation",
    "Initial Accreditation",
    "Voluntary Withdrawal",
    "Withdrawal of Accreditation",
    "Accreditation Withheld",
    "Continued Pre-Accreditation",
    "Pre-Accreditation Withdrawn",
    "Probationary Accreditation",
]


def pdf_text(pdf: Path) -> str:
    return subprocess.run(
        ["pdftotext", "-layout", str(pdf), "-"],
        check=True, capture_output=True, text=True,
    ).stdout


def split_records(text: str, start_re: re.Pattern) -> list[list[str]]:
    """Group layout lines into records beginning at [number] lines."""
    records, current = [], None
    for line in text.splitlines():
        if start_re.match(line):
            if current:
                records.append(current)
            current = [line]
        elif current is not None and line.strip():
            # page headers/footers restart between pages; drop them
            if line.startswith(("Program Number", "Sponsor Number",
                                "Academic Year", "United States",
                                "List of ", "All Programs")):
                continue
            if "Accreditation Council for Graduate Medical Education" in line:
                continue
            current.append(line)
    if current:
        records.append(current)
    return records


def parse_programs() -> list[dict]:
    rows = []
    for rec in split_records(pdf_text(PDF_PROGRAMS), REC_START):
        first = rec[0]
        m = REC_START.match(first)
        number, rest = m.group(1), m.group(2)
        # first line layout: name | address col | director | status | date | specialty
        # the specialty is the right-most field after the effective date
        specialty = ""
        date_m = DATE.search(first)
        if date_m:
            specialty = first[date_m.end():].strip()
        # name continuation: subsequent lines whose column-1 text starts at
        # the left margin (no leading spaces) extend the program name
        name_parts = [rest.split("   ")[0].strip()]
        for line in rec[1:]:
            if not line.startswith(" "):
                name_parts.append(line.split("   ")[0].strip())
        name = re.sub(r"\s+", " ", " ".join(p for p in name_parts if p))
        blob = " ".join(rec)
        # city/state: search line by line; within a matching line, the city is
        # the text after the last multi-space column gap
        city, state = "", ""
        for line in rec:
            cm = CITY_ST.search(line)
            if cm:
                city = re.split(r"\s{2,}", cm.group(1).strip())[-1]
                state = cm.group(2)
                break
        status = ""
        for s in STATUSES:
            if s.split(" (")[0] in blob:
                status = s.split(" (")[0]
                break
        eff = DATE.search(first)
        rows.append({
            "program_number": number,
            "program_name": name,
            "city": city,
            "state": state,
            "specialty": specialty,
            "accreditation_status": status,
            "effective_date": eff.group(0) if eff else "",
        })
    return rows


def main() -> None:
    programs = parse_programs()
    out_programs = RAW / "sas_transition_programs.csv"
    with out_programs.open("w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=list(programs[0].keys()))
        w.writeheader()
        w.writerows(programs)

    # institution-level rollup: unique program-name x state
    counts = Counter((p["program_name"], p["state"]) for p in programs)
    out_inst = RAW / "sas_transition_institutions.csv"
    with out_inst.open("w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["institution_name", "state", "n_programs"])
        for (name, state), n in sorted(counts.items()):
            w.writerow([name, state, n])

    n_state_missing = sum(1 for p in programs if not p["state"])
    print(f"programs parsed: {len(programs)}  (missing state: {n_state_missing})")
    print(f"institutions   : {len(counts)}")
    print(f"wrote {out_programs}")
    print(f"wrote {out_inst}")


if __name__ == "__main__":
    sys.exit(main())
