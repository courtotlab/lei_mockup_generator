#!/usr/bin/env Rscript

library(yaml)
library(biomaRt)
library(httr)
library(RJSONIO)
library(readr)
options(timeout=1500)
#get gene list from field values
gene_table <- yaml.load_file("data/field_values.yml")

#query biomart for genes
get_connections <- function(biomart, dataset) {
  mirrors <- c(
    "https://useast.ensembl.org",
    "https://www.ensembl.org",
    "https://uswest.ensembl.org",
    "https://asia.ensembl.org"
  )
  connections <- lapply(mirrors, function(mirror) {
    tryCatch({
      useEnsembl(biomart = biomart, dataset = dataset, host = mirror)
    }, error = function(e) {
      message(paste("Failed to connect to", mirror, ":", e$message))
      NULL
    })
  })
  connections <- Filter(Negate(is.null), connections)
  if (length(connections) == 0) {
    stop("No working connections to Ensembl found.")
  }
  return(connections)
}

# Tries remaining valid mirrors and skips if any other error occurs
fetch_gene_results <- function(biomart, dataset, attributes, filters, values) {
  connections <- get_connections(biomart, dataset)

  for (conn in connections) {
    message(paste("Trying query on", conn@host))

    result <- tryCatch({
      getBM(
        attributes = attributes,
        filters = filters,
        values = values,
        mart = conn
      )
    }, error = function(e) {
      message(paste("Query failed on", conn@host, ":", e$message))
      NULL
    })

    if (!is.null(result)) {
      return(result)  # stop at first successful mirror
    }
  }

  stop("All Ensembl mirrors failed during query execution.")
}


gene_results <- fetch_gene_results(
  biomart = "genes",
  dataset = "hsapiens_gene_ensembl",
  attributes = c(
    "ensembl_gene_id",
    "external_gene_name",
    "refseq_mrna",
    "transcript_is_canonical",
    "coding",
    "chromosome_name",
    "start_position",
    "rank"
  ),
  filters = "external_gene_name",
  values = gene_table$genes
)


#filter out non-canonical transcripts and empty values
filtered_genes <- gene_results[which(
  gene_results$transcript_is_canonical == 1 &
    gene_results$coding != "Sequence unavailable" &
    !is.na(gene_results$chromosome_name)
), ]
#filter out duplicates
filtered_genes <- filtered_genes[
  !duplicated(filtered_genes$external_gene_name),
]

#re-order table columns
filtered_genes <- filtered_genes[, c(
  "ensembl_gene_id",
  "external_gene_name", "refseq_mrna",
  "chromosome_name", "start_position",
  "coding", "rank" #rank = exon numbers
)]

# Check transcript IDs against entrez e-utils to get version code
# and resolve cases with multiple IDs
counter <- 1
base_url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi"
strsplit(filtered_genes$refseq_mrna, ";") |> lapply(\(nms) {
  nm_str <- paste0(nms, collapse = ",")
  url <- paste0(base_url, "?db=nuccore&id=", nm_str, "&retmode=json")
  cat("Processing ", filtered_genes[counter, "external_gene_name"], "\n")
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
results_final <- filtered_genes
results_final$chromosome_name <-
  gsub("^HSCHR|_.+$", "", filtered_genes$chromosome_name)
results_final$refseq_mrna <- do.call(c, refseq_accessions)
# write result to file
write.csv(results_final, "data/gene_info.csv", row.names = FALSE)
#write.csv(exons_filtered, "data/exon_info.csv", row.names = FALSE)
paste("Gene info, exon info, saved to
data/gene_info.csv",
      sep = "\n") |> cat()


# Fetch variant information from ClinVar FTP
clinvar <- read_tsv(
  "https://ftp.ncbi.nlm.nih.gov/pub/clinvar/tab_delimited/variant_summary.txt.gz",
  show_col_types = FALSE
)

# Filter by matching gene name
clinvar <- clinvar[clinvar$GeneSymbol %in% results_final$external_gene_name,]
# Derive variation ID from integer to string
clinvar$VariationID <- sprintf("VCV%09d", as.integer(clinvar$VariationID))


# Use fetch_gene_results instead
snp_results <- fetch_gene_results(
  biomart = "snp",
  dataset = "hsapiens_snp",
  attributes = c(
    "synonym_name",
    "minor_allele_freq",
    "minor_allele_count"
  ),
  filters = c("variation_synonym_source", "snp_synonym_filter"),
  values = list("ClinVar", clinvar$VariationID)
)

variants_merged <- merge(
  clinvar,
  snp_results,
  by.x = "VariationID",
  by.y = "synonym_name"
)

# Fill unknown minor allele frequencies with default 1 in 100 million
na_idx <- is.na(variants_merged$minor_allele_freq)
variants_merged$minor_allele_freq[na_idx] <- 1e-8
variants_merged$minor_allele_count[na_idx] <- 1

# Clinical significance likelihoods
# Takes worst case of multiple reports
# > If marked as "pathogenic/likely pathogenic", assume pathogenic
determine_clinsig <- function(clinsig) {

  clinsig_prob <- function(report) {
    if (grepl("^pathogenic", report, ignore.case = TRUE)) {
      return(0.99)
    } else if (grepl("^likely pathogenic", report, ignore.case = TRUE)) {
      return(0.9)
    } else if (grepl("^likely benign", report, ignore.case = TRUE)) {
      return(0.1)
    } else if (grepl("^benign", report, ignore.case = TRUE)) {
      return(0.01)
    } else {
      # Catch-all
      return(0.5)
    }
  }

  # Multiple reports are separated by a slash
  # Compute the max probability
  reports <- strsplit(clinsig, "/")[[1]]
  max(sapply(reports, clinsig_prob))
}

variants_merged$ClinicalProbability <- sapply(
  variants_merged$ClinicalSignificance,
  determine_clinsig
)

write.csv(variants_merged, "data/var_info.csv", row.names = FALSE)
paste("ClinVar variant info saved to data/var_info.csv", sep = "\n") |> cat()

#The generated csv file is short 43 genes, which were added in a new commit
