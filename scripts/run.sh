#!/usr/bin/env bash
set -euo pipefail

#helper function to print usage information
usage () {
  cat << EOF

run.sh v0.0.1 

by Jochen Weile <jweile@oicr.on.ca> 2025

This script generates a mock report using R scripts and LaTeX.

Usage: run.sh [-a|--amount <INTEGER>] [-o|--outdir <DIR>] [-s|--seed <INTEGER>]

-a|--amount : The number of mock data entries to generate (default: 10)
-o|--outdir : The output directory where the reports will be saved (default: out/)
-s|--seed   : An RNG seed to use (default: None)

EOF
 exit $1
}

#helper function to print error messages and exit
die () {
  echo "ERROR: $1">&2
  exit 1
}

#infer the directory of where this script (and thus hopefully 
#the others we will be calling below) are located.
SCRIPTDIR="$(dirname "$0")"

#Parse Arguments
PARAMS=""
OUTDIR="out/"
AMOUNT=1
SEED=""
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
        die "Argument for $1 is missing"
      fi
      ;;
    -o|--outdir)
      if [ -n "$2" ] && [ ${2:0:1} != "-" ]; then
        OUTDIR="$2"
        shift 2
      else
        die "ERROR: Argument for $1 is missing" 
      fi
      ;;
    -s|--seed)
      if [ -n "$2" ] && [ ${2:0:1} != "-" ]; then
        SEED="$2"
        shift 2
      else
        die "ERROR: Argument for $1 is missing" 
      fi
      ;;
    --) # end of options indicates that the main command follows
      shift
      PARAMS="$PARAMS $@"
      eval set -- ""
      ;;
    -*|--*=) # unsupported flags
      die "ERROR: Unsupported flag $1" 
      ;;
    *) # positional parameter
      PARAMS="$PARAMS $1"
      shift
      ;;
  esac
done
#reset command arguments as only positional parameters
eval set -- "$PARAMS"
# #Template is the first positional parameter. If not provided, use a default template
# TEMPLATE="${1:-templates/fakeHospital2.tex}"
# # Check if the template file exists
# if [[ ! -f "$TEMPLATE" ]]; then
#   echo "Template file not found: $TEMPLATE"
#   exit 1
# fi

# Check if TeX-live or tinytex is installed
if [[ -z $(which pdflatex) ]]; then
  die "LaTeX or TeXlive is not installed!"
fi

if [[ -n "$SEED" ]]; then
  SEEDPARAM="--seed $SEED"
else
  SEEDPARAM=""
fi

echo "Output directory: $OUTDIR"
mkdir -p "$OUTDIR"
DATA="$OUTDIR/mock_data.json"

# Generate mock data using the AMOUNT variable
echo "Generating mock data files..."
Rscript "${SCRIPTDIR}/generate_mock_data.R" --amount "$AMOUNT" --outfile "$DATA" $SEEDPARAM

# Run interpolate with the given template and generated data
echo "Interpolating JSON files with template..."
Rscript "${SCRIPTDIR}/interpolate.R" "$DATA" --outprefix "$OUTDIR/report"

# Compile all generated .tex files into PDFs and clean up
echo "Compiling LaTeX files into PDFs..."
cd "$OUTDIR"
for TEXFILE in report_*.tex; do
  echo "Converting $TEXFILE to markdown"
  pandoc -o "${TEXFILE%.tex}.md" "$TEXFILE"
  echo "Compiling $TEXFILE to PDF"
  pdflatex -halt-on-error -interaction batchmode "$TEXFILE" && \
    rm -f "${TEXFILE%.tex}.aux" "${TEXFILE%.tex}.log" "${TEXFILE%.tex}.tex" || \
      die "Compilation failed. Check ${TEXFILE%.tex}.log for error message."
done
cd -

# Distress the generated PDFs
echo "Distressing PDFs..."
for PDF in "$OUTDIR"/report_*.pdf; do
  echo "Distressing $PDF"
  "${SCRIPTDIR}/simulate_copier.sh" "$PDF"
done

echo "All done."
