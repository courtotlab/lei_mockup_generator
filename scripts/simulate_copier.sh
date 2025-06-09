#!/usr/bin/env bash

INPDF="$1"
OUTPDF="${INPDF%.pdf}_distressed.pdf"

if [[ -z $(which magick) ]]; then
  echo "Error: ImageMagick is not installed or not found in PATH."
  exit 1
fi
if [[ ! -f "${INPDF}" ]]; then
  echo "Error: Input PDF file '${INPDF}' does not exist."
  exit 1
fi

rnorm() {
  SD=$1
  Rscript -e "rnorm(1,0,${SD})|>format(digits=2)|>cat('\n')"
}
runif() {
  MIN=$1
  MAX=$2
  Rscript -e "runif(1,${MIN},${MAX})|>format(digits=2)|>cat('\n')"
}
rint() {
  MIN=$1
  MAX=$2
  echo $RANDOM % $((MAX - MIN + 1)) + $MIN | bc
}

BLUR_RADIUS=$(rint 0 2)
BLUR_SIGMA=$(runif 0.5 1.5)
STRETCH=$(rint 1 7)
ROTATE=$(runif "-1" "1")
# ROTATE=$(rnorm 0.2)
NOISE_INTENSITY=$(runif 0.1 2)
# NOISE_INTENSITY=$(runif 1 10)

magick -density 200 "${INPDF}" -colorspace gray -linear-stretch "${STRETCH}%x10%" -rotate "${ROTATE}" -repage +0 -blur ${BLUR_RADIUS}x${BLUR_SIGMA} -attenuate ${NOISE_INTENSITY} +noise poisson "${OUTPDF}"


