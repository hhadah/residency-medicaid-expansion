#!/usr/bin/env python3
"""
Multiple-testing correction for the primary family of hypotheses, on the
YEAR-VARYING per-capita outcome (the paper's headline specification).

The family is: full sample (headline), urban hospitals, rural hospitals,
primary care, non-primary care, offered positions (quota), and -- added in the
referee response (the referees noted the family excluded the estimates the
paper leans on most) -- the two mechanism arms and the cross-formula
difference. For each member
we read the average post-expansion effect, its SE, and TWO p-values:
  (i)  the clustered joint post-treatment p-value from the Stata summary
       tables (scripts 24/25), and
  (ii) the randomization-inference p-value from the permutation scripts
       (script 32 for the headline; script 33 for the other five members),
then compute Benjamini-Hochberg (1995) FDR q-values under EACH standard.
RI is the conservative standard the text leads with (uniform inference across
the family); the clustered q-values remain reported for comparison.

Outputs:
  - output/tables/multiple-testing-qvalues.csv
  - my_paper/tables/multiple-testing-qvalues.tex   (bare tabular fragment)
  - my_paper/figures/appx-fdr-forest.png  + output/figures/  (forest plot)
"""
import os
import csv
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

TOPDIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
TABDIR = os.path.join(TOPDIR, "output", "tables")
FIGDIRS = [os.path.join(TOPDIR, "my_paper", "figures"),
           os.path.join(TOPDIR, "output", "figures")]
SUITE = os.path.join(TABDIR, "yearvarying-suite-summary.csv")
SPEC  = os.path.join(TABDIR, "yearvarying-specialty-summary.csv")
RI_HEAD = os.path.join(TABDIR, "ri-yearvarying-summary.csv")
RI_EXT  = os.path.join(TABDIR, "ri-extended-summary.csv")


def read_rows(path, key_col):
    rows = {}
    with open(path) as f:
        for r in csv.DictReader(f):
            rows[r[key_col].strip()] = r
    return rows


def g(row, *names):
    """Fetch first present numeric field from a row (columns vary by table)."""
    for n in names:
        if n in row and row[n] not in (None, "", "."):
            return float(row[n])
    return float("nan")


suite = read_rows(SUITE, "spec")
spec  = read_rows(SPEC, "specialty")
ri_h  = read_rows(RI_HEAD, "spec")
ri_e  = read_rows(RI_EXT, "spec")


def require(rows, key, path):
    """Fail hard if an expected spec row is missing (prevents stale families)."""
    if key not in rows:
        raise SystemExit(f"ERROR: spec row '{key}' missing from {path}; "
                         "re-run the upstream Stata script before building q-values.")
    return rows[key]


for lab in ("headline", "urban", "rural", "quota", "mech_volume", "mech_notvolume", "mech_diff"):
    require(suite, lab, SUITE)
for lab in ("Primary Care", "Non-Primary Care"):
    require(spec, lab, SPEC)

# (label, avg, se, p_clustered, p_ri) assembled from the reproduced summaries.
family = [
    ("Full sample (headline)", suite["headline"], require(ri_h, "headline_matched100k", RI_HEAD)),
    ("Urban hospitals",        suite["urban"],    require(ri_e, "urban", RI_EXT)),
    ("Rural hospitals",        suite["rural"],    require(ri_e, "rural", RI_EXT)),
    ("Primary care",           spec["Primary Care"],     require(ri_e, "primary", RI_EXT)),
    ("Non-primary care",       spec["Non-Primary Care"], require(ri_e, "nonprimary", RI_EXT)),
    ("Offered positions (quota)", suite["quota"], require(ri_e, "quota", RI_EXT)),
    ("Volume-responsive arm",   suite["mech_volume"],    require(ri_h, "mech_volume", RI_HEAD)),
    ("Non-responsive arm",      suite["mech_notvolume"], require(ri_h, "mech_nonresponsive", RI_HEAD)),
    ("Cross-formula difference", suite["mech_diff"],     require(ri_e, "mechdiff", RI_EXT)),
]
labels = [x[0] for x in family]
avg  = np.array([g(x[1], "avg_treat") for x in family], float)
se   = np.array([g(x[1], "avg_se") for x in family], float)
p    = np.array([g(x[1], "treat_p") for x in family], float)
p_ri = np.array([g(x[2], "ri_p") for x in family], float)
M = len(p)

# RI vintage check: if the RI observed ATT does not match the reported
# estimate, the RI scripts have not been rerun on the current panel. The
# affected rows are FLAGGED as stale rather than fatal (2026-07-25: RI rerun
# on the full 2000-2019 panel deferred; see 99-run-all-analysis.do).
ri_stale = np.zeros(len(family), bool)
for i, (lab, row, rirow) in enumerate(family):
    a, o = g(row, "avg_treat"), g(rirow, "obs_att")
    # The cross-formula difference is constructed differently in the RI script
    # (split-sample arm difference) than in the suite (pooled hetby + nlcom),
    # so a small numerical gap is expected there, not staleness.
    tol = 3e-3 if lab == "Cross-formula difference" else 5e-4
    if np.isfinite(a) and np.isfinite(o) and abs(a - o) > tol:
        ri_stale[i] = True
        print(f"WARNING: '{lab}' RI p is STALE (RI obs {o:.4f} vs current {a:.4f}); "
              "rerun scripts 30-34 on the current panel.")
if np.isnan(p).any():
    raise SystemExit("ERROR: missing clustered p-values in the family; check upstream CSVs.")


def bh_qvalues(pvals):
    """Benjamini-Hochberg (1995) FDR q-values, enforced monotone in p."""
    pv = np.asarray(pvals, float)
    m = len(pv)
    order = np.argsort(pv)
    ps = pv[order]
    raw = ps * m / np.arange(1, m + 1)
    qs = np.minimum.accumulate(raw[::-1])[::-1]
    qs = np.clip(qs, 0, 1)
    q = np.empty(m)
    q[order] = qs
    return q


q_cl = bh_qvalues(p)
q_ri = bh_qvalues(p_ri)

# ---- write CSV ----
os.makedirs(TABDIR, exist_ok=True)
csv_path = os.path.join(TABDIR, "multiple-testing-qvalues.csv")
with open(csv_path, "w") as f:
    f.write("hypothesis,avg_effect,se,p_clustered,q_clustered,p_ri,q_ri,ri_stale\n")
    for lab, a, s, pv, qv, pr, qr, st in zip(labels, avg, se, p, q_cl, p_ri, q_ri, ri_stale):
        f.write(f"{lab},{a:.4f},{s:.4f},{pv:.4f},{qv:.4f},{pr:.4f},{qr:.4f},{int(st)}\n")

# ---- write LaTeX tabular fragment (bare tabular, per project standard) ----
tex_path = os.path.join(TOPDIR, "my_paper", "tables", "multiple-testing-qvalues.tex")
os.makedirs(os.path.dirname(tex_path), exist_ok=True)


def fmt_p(v):
    return "$<$0.001" if v < 0.001 else f"{v:.3f}"


with open(tex_path, "w") as f:
    f.write("\\begin{tabular}{lcccc}\n\\toprule\n")
    f.write(" & \\multicolumn{2}{c}{Clustered} & \\multicolumn{2}{c}{Randomization inference} \\\\\n")
    f.write("\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}\n")
    f.write("Hypothesis (avg.\\ post effect, matched per 100{,}000) & $p$-value & $q$-value & $p$-value & $q$-value \\\\\n")
    f.write("\\midrule\n")
    for lab, pv, qv, pr, qr, st in zip(labels, p, q_cl, p_ri, q_ri, ri_stale):
        star = "$^{s}$" if st else ""
        f.write(f"{lab} & {fmt_p(pv)} & {fmt_p(qv)} & {fmt_p(pr)}{star} & {fmt_p(qr)}{star} \\\\\n")
    f.write("\\bottomrule\n\\end{tabular}\n")

# ---- forest plot ----
# Sort most-negative effect at top. Filled marker = survives FDR (q<0.05)
# under the conservative RI standard; label reports both q-values.
idx = np.argsort(avg)          # ascending: most negative first
ypos = np.arange(M)[::-1]      # top row for the first (most negative)
lo = avg - 1.96 * se
hi = avg + 1.96 * se

fig, ax = plt.subplots(figsize=(8.2, 4.6))
for rank, i in enumerate(idx):
    y = ypos[rank]
    survives = q_ri[i] < 0.05
    color = "#7a1f1f" if survives else "#888888"
    ax.plot([lo[i], hi[i]], [y, y], color=color, lw=1.8, zorder=2)
    ax.plot(avg[i], y,
            marker="o", ms=8, zorder=3,
            mfc=(color if survives else "white"), mec=color, mew=1.8)
    ax.text(hi.max() + 0.006, y,
            f"$q_{{cl}}$={q_cl[i]:.3f} / $q_{{RI}}$={q_ri[i]:.3f}"
            + ("$^{\\ast}$" if survives else ""),
            va="center", ha="left", fontsize=8.5, color="#333333")

ax.axvline(0, color="black", lw=1, ls="--", zorder=1)
ax.set_yticks(ypos)
ax.set_yticklabels([labels[i] for i in idx], fontsize=10)
ax.set_xlabel("Average post-expansion effect (matched positions per 100,000, year-varying)", fontsize=10)
ax.set_ylim(-0.7, M - 0.3)
ax.margins(x=0.02)
xr = hi.max() - lo.min()
if not np.isfinite(xr) or xr <= 0:  # degenerate range guard
    xr = max(abs(hi.max()), 1e-6)
ax.set_xlim(lo.min() - 0.05 * xr, hi.max() + 0.42 * xr)
for s in ("top", "right"):
    ax.spines[s].set_visible(False)
fig.tight_layout()
for d in FIGDIRS:
    os.makedirs(d, exist_ok=True)
    fig.savefig(os.path.join(d, "appx-fdr-forest.png"), dpi=200, bbox_inches="tight")
    fig.savefig(os.path.join(d, "appx-fdr-forest.pdf"), bbox_inches="tight")
plt.close(fig)

# ---- console summary ----
print("Primary family: Benjamini-Hochberg FDR q-values (year-varying per-capita)")
print(f"{'Hypothesis':30s} {'avg':>8s} {'se':>7s} {'p_cl':>8s} {'q_cl':>8s} {'p_ri':>8s} {'q_ri':>8s}  survives(RI)")
for lab, a, s, pv, qv, pr, qr in zip(labels, avg, se, p, q_cl, p_ri, q_ri):
    print(f"{lab:30s} {a:8.4f} {s:7.4f} {pv:8.4f} {qv:8.4f} {pr:8.4f} {qr:8.4f}  {'YES' if qr < 0.05 else 'no'}")
print(f"\nWrote {csv_path}")
print(f"Wrote {tex_path}")
print("Wrote appx-fdr-forest.png to my_paper/figures/ and output/figures/")
