#!/usr/bin/env bash
set -euo pipefail

OUTDIR=out/
TEMPLATES=("templates/template1.tex" "templates/template2.tex")

mkdir -p "$OUTDIR"
echo "Output directory: $OUTDIR"
# Generate mock data 5 times, outputting JSON files
for i in $(seq 1 5); do
  DATA="${OUTDIR}mock_data_${i}.json"
  echo "Generating mock data #$i: $DATA"
  Rscript generate_mock_data.R --amount 10 --outfile "$DATA"
done

# For each JSON file, run interpolate on each template
echo "Interpolating JSON files with templates..."
for i in $(seq 1 5); do
  DATA="${OUTDIR}mock_data_${i}.json"
  for TEMPLATE in "${TEMPLATES[@]}"; do
    OUTPREFIX="${OUTDIR}report_${i}_$(basename "${TEMPLATE%.tex}")"
    echo "Interpolating $TEMPLATE with $DATA → $OUTPREFIX"
    Rscript interpolate.R "$TEMPLATE" "$DATA" --outprefix "$OUTPREFIX"
  done
done

# Compile all generated .tex files into PDFs and clean up
echo "Compiling LaTeX files into PDFs..."
cd "$OUTDIR"
for TEXFILE in report_*.tex; do
  echo "Compiling $TEXFILE"
  pdflatex "$TEXFILE" && \
  rm "${TEXFILE%.tex}.aux" "${TEXFILE%.tex}.log" "${TEXFILE%.tex}.tex"
done
cd -

echo "All done."
