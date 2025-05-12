#!/usr/bin/env bash
TEMPLATE=templates/CHEO_template.tex
OUTDIR=out/

mkdir -p "$OUTDIR"

DATA="${OUTDIR}mock_data.json"

Rscript generate_mock_data.R --amount 10 --outfile "$DATA"
Rscript interpolate.R "$TEMPLATE" "$DATA" --outprefix "${OUTDIR}/report_"
cd "$OUTDIR"
for TEXFILE in *.tex; do
  pdflatex "$TEXFILE" && 
    rm "${TEXFILE%.tex}.aux" "${TEXFILE%.tex}.log" "${TEXFILE%.tex}.tex"
done
cd -
