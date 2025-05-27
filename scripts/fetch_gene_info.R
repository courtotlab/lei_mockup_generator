#!/usr/bin/Rscript

library(yaml)
library(biomaRt)
library(httr)
library(RJSONIO)

#get gene list from field values
gene_table <- yaml.load_file("data/field_values.yml")

#query biomart for genes
ensembl <- useMart("ensembl")
human_ds <- useDataset("hsapiens_gene_ensembl", mart = ensembl)
results <- getBM(
  attributes = c(
    "external_gene_name",
    "refseq_mrna",
    "transcript_is_canonical",
    "coding",
    "chromosome_name",
    "start_position"
  ),
  filters = "external_gene_name",
  values = gene_table$genes,
  mart = human_ds
)
#filter out non-canonical transcripts and empty values
results_filtered <- results[which(
  results$transcript_is_canonical == 1 &
    results$coding != "Sequence unavailable" &
    !is.na(results$chromosome_name)
), ]
#filter out duplicates
results_filtered <- results_filtered[
  !duplicated(results_filtered$external_gene_name),
]
#re-order table columns
results_filtered <- results_filtered[, c(
  "external_gene_name", "refseq_mrna",
  "chromosome_name", "start_position",
  "coding"
)]

# Check transcript IDs against entrez e-utils to get version code 
# and resolve cases with multiple IDs
counter <- 1
base_url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi"
strsplit(results_filtered$refseq_mrna, ";") |> lapply(\(nms) {
  nm_str <- paste0(nms, collapse = ",")
  url <- paste0(base_url, "?db=nuccore&id=", nm_str, "&retmode=json")
  cat("Processing ", results_filtered[counter, "external_gene_name"], "\n")
  #sleep 300ms per iteration, to not stress out the eutils API
  Sys.sleep(0.3)
  response <- GET(url)
  counter <<- counter + 1
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, "text"))
    uids <- data$result$uids
    if (length(uids) > 0) {
      accessions <- sapply(data$result[uids], \(e) e$accessionversion)
      #pick the one with the most revisions
      versions <- as.integer(sub("^.+\\.", "", accessions))
      #if there's a tie, pick the last one.
      winner_idx <- max(which(versions == max(versions)))
      return(accessions[winner_idx])
    }
  }
  #fallback: make something up; pick the last one and use version 1
  return(paste0(nms[length(nms)], ".1"))
}) -> refseq_accessions

#fix entries with contig names as chromosomes and update transcripts
results_final <- results_filtered
results_final$chromosome_name <- 
  gsub("^HSCHR|_.+$", "", results_filtered$chromosome_name)
results_final$refseq_mrna <- do.call(c, refseq_accessions)

# write result to file
write.csv(results_final, "data/gene_info.csv", row.names = FALSE)
