#!/bin/zsh
# Complete rendering/splitting where missing, then tesseract with tsv+txt output
set -u
SCRATCH="/private/tmp/claude-501/-Users-hhadah-Projects-GiT-residency-medicaid-expansion/6207b666-bd12-443a-b4be-eb6b9fad04b5/scratchpad"
BOX="/Users/hhadah/Library/CloudStorage/Box-Box/Murphy Institute - Encoding Physician Data/Hospital-level, 2000 to 2014"
for y in "$@"; do
  mkdir -p "$SCRATCH/ocr/$y"
  n=$(pdfinfo "$BOX/resultsanddata$y.pdf" | awk '/^Pages/{print $2}')
  npng=$(ls "$SCRATCH/ocr/$y"/pg-*.png 2>/dev/null | grep -cv '_[LR].png' || true)
  echo "=== $y ($n pages, $npng rendered) ==="
  if (( npng < n )); then
    pdftoppm -gray -png -r 300 "$BOX/resultsanddata$y.pdf" "$SCRATCH/ocr/$y/pg"
  fi
  for img in "$SCRATCH/ocr/$y"/pg-*.png; do
    base="${img%.png}"
    [[ "$base" == *_L || "$base" == *_R ]] && continue
    if [[ ! -f "${base}_L.png" ]]; then
      python3 "$SCRATCH/split_cols.py" "$img" "${base}_L.png" "${base}_R.png" >/dev/null 2>&1
    fi
    for side in L R; do
      if [[ ! -f "${base}_${side}.tsv" ]]; then
        tesseract "${base}_${side}.png" "${base}_${side}" --psm 6 tsv txt 2>/dev/null
      fi
    done
  done
  echo "$y done: $(ls "$SCRATCH/ocr/$y"/*_L.tsv 2>/dev/null | wc -l | tr -d ' ') L-tsv files"
done
echo ALL_DONE
