#!/usr/bin/env bash
set -euo pipefail

OUTDIR=out/
TEMPLATES=("templates/fakeHospital1.tex" "templates/fakeHospital2.tex")

#helper function to print usage information
usage () {
  cat << EOF

run.sh v0.0.1 

by Jochen Weile <jweile@oicr.on.ca> 2025

This script generates a mock report using R scripts and LaTeX.

Usage: run.sh [-a|--amount <INTEGER>] [-o|--outdir <DIR>]

-a|--amount : The number of mock data entries to generate (default: 3)
-o|--outdir : The output directory where the reports will be saved (default: out/)

EOF
 exit $1
}

#Parse Arguments
PARAMS=""
OUTDIR="out/"
AMOUNT=3  # Changed default from 10 to 3
while (( "$#" )); do
  case "$1" in
    -h|--help)
      usage 0
      shift
      ;;
    -a|--amount)
      if [ -n "$2" ] && [ ${2:0:1} != "-" ]; then
        AMOUNT="$2"
        shift 2
      else
        echo "ERROR: Argument for $1 is missing" >&2
        usage 1
      fi
      ;;
    -o|--outdir)
      if [ -n "$2" ] && [ ${2:0:1} != "-" ]; then
        OUTDIR="$2"
        shift 2
      else
        echo "ERROR: Argument for $1 is missing" >&2
        usage 1
      fi
      ;;
    --) # end of options indicates that the main command follows
      shift
      PARAMS="$PARAMS $@"
      eval set -- ""
      ;;
    -*|--*=) # unsupported flags
      echo "ERROR: Unsupported flag $1" >&2
      usage 1
      ;;
    *) # positional parameter
      PARAMS="$PARAMS $1"
      shift
      ;;
  esac
done
#reset command arguments as only positional parameters
eval set -- "$PARAMS"

# Ensure we're in the correct directory (project root)
cd "$(dirname "$0")/.."

mkdir -p "$OUTDIR"
echo "Generating $AMOUNT mock datasets with both hospital templates..."
echo "Output directory: $OUTDIR"

# Generate mock data using the AMOUNT variable
for i in $(seq 1 $AMOUNT); do
  DATA="${OUTDIR}mock_data_${i}.json"
  echo "Generating mock data #$i: $DATA"
  Rscript scripts/generate_mock_data.R --amount 1 --outfile "$DATA"
done

# For each JSON file, run interpolate on each template
echo "Interpolating JSON files with templates..."
for i in $(seq 1 $AMOUNT); do
  DATA="${OUTDIR}mock_data_${i}.json"
  for TEMPLATE in "${TEMPLATES[@]}"; do
    TEMPLATE_NAME=$(basename "${TEMPLATE%.tex}")
    OUTPREFIX="${OUTDIR}report_${i}_${TEMPLATE_NAME}"
    echo "Interpolating $TEMPLATE with $DATA → $OUTPREFIX"
    Rscript scripts/interpolate.R "$TEMPLATE" "$DATA" --outprefix "$OUTPREFIX"
  done
done

# Compile all generated .tex files into PDFs and clean up
echo "Compiling LaTeX files into PDFs..."
cd "$OUTDIR"
for TEXFILE in report_*.tex; do
  if [ -f "$TEXFILE" ]; then
    echo "Compiling $TEXFILE"
    pdflatex "$TEXFILE" && \
    rm -f "${TEXFILE%.tex}.aux" "${TEXFILE%.tex}.log" "${TEXFILE%.tex}.tex"
  fi
done
cd -

echo "All done! Generated $AMOUNT datasets with both hospital templates."
echo "PDFs are available in: $OUTDIR"