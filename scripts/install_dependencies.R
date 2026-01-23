#!/usr/bin/env Rscript


## Assuming this is run in a fresh pixi environment, nothing should
## Be installed yet, so we don't check for existing packages

repo <- "https://cloud.r-project.org/"

#"normal" packages
install.packages(
  c("httr", "argparser", "RJSONIO", "yaml", "hash", "stringr", "data.table", "readr"),
  repos = repo
)

#biomart via bioconductor
install.packages("BiocManager", repos = repo)
BiocManager::install("biomaRt", ask = FALSE)

#github packages via "remotes"
install.packages("remotes", repos = repo)
remotes::install_github("VariantEffect/hgvsParseR")

#tinytex installer
install.packages("tinytex", repos = repo)
tinytex::install_tinytex()
