#!/usr/bin/env bash
set -euo pipefail

#helper function to print usage information
usage () {
  cat << EOF

run.sh v0.0.1 

by Jochen Weile <jweile@oicr.on.ca> 2025

This script generates a mock report using R scripts and LaTeX.

Usage: run.sh [-a|--amount <INTEGER>] [-o|--outdir <DIR>] <TEMPLATE> 

<TEMPLATE>  : The input directory containing the fastq.gz files
-a|--amount : The number of mock data entries to generate (default: 10)
-o|--outdir : The output directory where the reports will be saved (default: out/)

EOF
 exit $1
}

#Parse Arguments
PARAMS=""
OUTDIR="out/"
AMOUNT=10
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

TEMPLATE="${1:-templates/CHEO_template.tex}"
# Check if the template file exists
if [[ ! -f "$TEMPLATE" ]]; then
  echo "Template file not found: $TEMPLATE"
  exit 1
fi
# Check if the output directory exists, if not create it
mkdir -p "$OUTDIR"

# Define location for mock data
DATA="${OUTDIR}mock_data.json"

# Generate the mock data
Rscript scripts/generate_mock_data.R --amount 10 --outfile "$DATA"
# Interpolate the template with the mock data
Rscript scripts/interpolate.R "$TEMPLATE" "$DATA" --outprefix "${OUTDIR}/report_"
# Compile the interpolated LaTeX files to PDF
cd "$OUTDIR"
for TEXFILE in *.tex; do
  pdflatex -halt-on-error -interaction batchmode "$TEXFILE" && 
    rm "${TEXFILE%.tex}.aux" "${TEXFILE%.tex}.log" "${TEXFILE%.tex}.tex"
done
cd -
