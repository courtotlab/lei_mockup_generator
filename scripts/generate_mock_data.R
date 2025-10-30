#!/usr/bin/env Rscript
# mockups.R generates a mock dataset in JSON format

library(yaml)
library(hgvsParseR)
library(RJSONIO)
library(argparser)
library(stringr)
library(data.table)

#parse command line arguments
ap <- arg_parser("generate a mock dataset in JSON format", name = "mockups.R")
ap <- add_argument(ap,
  "--amount",
  help = "number of reports to generate",
  default = 1L
)
ap <- add_argument(ap,
  "--outfile",
  help = "the output file",
  default = "mock_data.json"
)
args <- parse_args(ap)
num_reports <- args$amount
out_file <- args$outfile

#Load source data
field_values <- yaml.load_file("data/field_values.yml")
gene_info <- read.csv("data/gene_info.csv", row.names = 1)
var_info <- read.csv("data/var_info.csv")
exons_df <- read.csv("data/exon_info.csv", stringsAsFactors = FALSE)

# Helper function to sample from fields
sample_field <- function(name, num = 1) sample(field_values[[name]], num)

#generate UUIDv4
gen_uuid <- function() {
  alphanum <- function(n) {
    paste(sample(c(letters[1:6], 0:9), n, replace = TRUE), collapse = "")
  }
  paste(
    alphanum(8), "-", alphanum(4), "-4", alphanum(3), "-",
    sample(c("8", "9", "a", "b"), 1), alphanum(3), "-", alphanum(12),
    collapse = "", sep = ""
  )
}

# sample from a power-law distribution using rejection sampling
rpow <- function(n, alpha = 1000, range = 5e5) {
  # n: Number of samples to generate
  # alpha: Power-law exponent (must be > 1)
  # range: Range / tail-length of the distribution (default is 5e5)
  if (alpha <= 1) {
    stop("Alpha must be greater than 1 for a valid power-law distribution.")
  }
  # Define the target power-law probability density function
  power_law_pdf <- function(x) {
    ifelse(x >= 1, (alpha - 1) * x^(-alpha), 0)
  }
  # Define the proposal distribution (uniform in this case)
  proposal_pdf <- function(x) {
    ifelse(x >= 1, 1, 0)
  }
  # Find the maximum ratio of target PDF to proposal PDF
  M <- (alpha - 1) * 1^(-alpha)
  # Rejection sampling
  samples <- numeric(0)
  while (length(samples) < n) {
    # Sample from the proposal distribution
    x_proposal <- runif(1, min = 1, max = 1 * 10) # Adjust range as needed
    # Compute acceptance probability
    accprob <- power_law_pdf(x_proposal) / (M * proposal_pdf(x_proposal))
    # Accept or reject the sample
    if (runif(1) < accprob) {
      samples <- c(samples, x_proposal)
    }
  }
  # Scale the samples to the desired range
  samples_out <- samples * range - range
  return(samples_out)
}

# Generate a set of threee dates (collected, received, verified)
gen_dates <- function() {
  #generate a random date in the range of 2020-2025
  jan2020 <- 1577854800L
  dec2025 <- 1767243540L
  d1 <- runif(1, jan2020, dec2025) |> as.integer() |> as.POSIXct()
  # add a random amount of time in the range of a few days to the date
  # to get the received and verified dates
  d2 <- d1 + round(rnorm(1, mean = 60 * 60 * 24 * 1.5, sd = 60 * 60 * 12))
  d3 <- d2 + round(rnorm(1, mean = 60 * 60 * 24 * 1.5, sd = 60 * 60 * 12))
  list(
    date_collected = as.character(d1),
    date_received = as.character(d2),
    date_verified = as.character(d3)
  )
}

# Declare external_gene_name as a global variable to avoid binding warnings
globalVariables(c("external_gene_name"))

find_exon_number <- function(chromosome, hgvsg, gene_symbol, exons_df) {
  # Normalize chromosome name
  variant_chr <- gsub("^chr", "", chromosome)

  # Extract numeric position from HGVSg (e.g., "g.77510022T>C" → 77510022)
  variant_pos <- as.numeric(sub("^g\\.(\\d+).*", "\\1", hgvsg))

  # Filter exons for matching gene and chromosome
  exon_match1 <- exons_df[
    exons_df$external_gene_name == gene_symbol &
      exons_df$chromosome_name == variant_chr
  ]
  exon_match <- exon_match1[
    exons_df$exon_chrom_start <= variant_pos &
      exons_df$exon_chrom_end >= variant_pos, drop = TRUE
  ]

  # Return the exon number (rank), or a random number if not found
  if (nrow(exon_match) > 0) {
    return(exon_match$rank[1])  # return first match
  } else {
    return(sample(1:20, 1))  # return a random exon number if not found
  }
}

get_clinvar <- function(df) {

  data <- list()
  data$variant_id <- df$VariationID
  data$chromosome <- paste0("chr", df$Chromosome)
  data$type <- df$Type
  data$interpretation <- df$ClinicalSignificance

  res <- str_match(
      df$Name,
      # TranscriptID(GeneSymbol):VariantString (Protein)
      regex("([^(^)]*).*:([^(^)^ ]*)\\s*(?:\\((.*)\\))?")
  )

  data$mega_hgvs <- df$Name
  data$transcript_id <- res[2]
  data$hgvsc <- res[3]

  if (!is.na(res[4])) {
    data$hgvsp <- res[4]
  }
  else {
    data$hgvsp <- "No protein change"
  }

  data$zygosity <- sample(
    field_values$zygosity, 1,
    prob = c(.8, .2)
  )

  return(data)
}

#Sample random variants
sample_variants <- function(genes, patient_var) {

  selected <- sample(genes, 1L)
  subset <- patient_var[patient_var$GeneSymbol == selected,]

  # Improved call
  df <- sapply(
    rownames(subset),
    \(row) {
      df <- subset[row,]

      data <- list()
      data$gene_symbol <- selected

      # Handle exon assignment with safe checking
      if (is.null(data$exon) || length(data$exon) == 0 || is.na(data$exon)) {
        # Try to get exon from gene_info, with fallback to random number
        rank_data <- gene_info[
          gene_info$external_gene_name == data$gene_symbol, "rank"]
        if (!is.na(rank_data) && rank_data != "") {
          data$exon <- sample(strsplit(rank_data, ";")[[1]], 1)
        } else {
          data$exon <- sample(1:20, 1)
        }
      }

      data$mafac <- df$minor_allele_count
      data$mafaf <- df$minor_allele_freq
      data$mafan <- data$mafac / data$mafaf

      data <- merge(data, get_clinvar(df))

      data
    }
  )

  # Must be transposed to fit into JSON format
  t(df)
}

gen_patient_variants <- function() {

  # Lambda for Poisson -> sum of all MAF
  num_variants <- rpois(1, sum(var_info$minor_allele_freq))

  # This index contains all variants that are present in patient
  # Sample variants based on biological probability
  var_idx <- sample(
    rownames(var_info),
    size = num_variants,
    prob = var_info$minor_allele_freq
  )
  var_info[var_idx,]
}

gen_genes <- function(variants) {

  # Select uniformly random genes
  gene_symbols <- sample(
    unique(variants$GeneSymbol),
    round(runif(1, 5, 20))
  )

  refseq_accs <- gene_info[
    gene_info$external_gene_name %in% gene_symbols, "refseq_mrna"]
  mapply(
    \(s, a) list(gene_symbol = s, refseq_mrna = a),
    gene_symbols,
    refseq_accs,
    SIMPLIFY = FALSE
  )
}

# report_date"
# report_type": Pathology, Molecular Genetics
# testing_context": "Clinical",
# ordering_clinic"
# testing_laboratory"
# sequencing scope: "Gene panel","Targeted variant testing"
# tested_genes
# sample_type: "Amplified DNA"
# "analysis_type":"Variant analysis", "Fusion analysis"
# Per variant:
#   "variant_ids": "VCVO00483847"
#   "chromosome": chrX
#   hgvsc
#   hgvsg
#   hgvsp
#   gene_symbol
#   transcript_id":  "NM_000179.2"
#   "exon"
#   "reference_genome"

# Generate mock-up data using sampling functions
generate_mockup <- function() {
  data <- list()
  data <- c(data, gen_dates())
  data$report_type <- sample_field("report_types")
  data$testing_context <- sample_field("testing_contexts")
  data$ordering_clinic <- sample_field("clinics")
  data$testing_laboratory <- sample_field("labs")
  data$sequencing_scope <- sample_field("scopes")

  # Generate random patient variants
  patient_var <- gen_patient_variants()

  data$tested_genes <- gen_genes(patient_var)
  data$num_tested_genes <- length(data$tested_genes)
  data$sample_type <- sample_field("sample_types")
  data$analysis_type <- sample_field("analysis_types")
  data$variants <- sample_variants(
    names(data$tested_genes),
    patient_var
  )
  data$num_variants <- nrow(data$variants)

  rgs <- field_values$reference_genomes
  # Assign multinomial probabilites based on number of entries
  rg_probs <- rep(0.01, length(rgs))
  rg_probs[which(rgs == "GRCh38")] <- 1 - 0.01 * (length(rgs) - 1)
  data$reference_genome <- sample(
    field_values$reference_genomes, 1, prob = rg_probs
  )
  data
}

uuids <- replicate(num_reports, {
  gen_uuid()
})

out <- replicate(num_reports, {
  generate_mockup()
}, simplify = FALSE)
names(out) <- uuids

#write JSON output to file
json_out <- toJSON(out)
cat(json_out, file = out_file)

cat("Done!")
