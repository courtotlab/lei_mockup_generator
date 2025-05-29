#!/usr/bin/Rscript
# mockups.R generates a mock dataset in JSON format

library(yaml)
library(hgvsParseR)
library(RJSONIO)
library(argparser)
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

# Generate a set of threee dates (collected, received, verified)
gen_dates <- function() {
  d1 <- paste0(
    "20", sample(20:25, 1), "-", sample(12, 1), "-",
    sample(29, 1), " ", sample(23, 1), ":", sample(59, 1)
  ) |> as.POSIXct()
  d2 <- d1 + round(rnorm(1, mean = 60 * 60 * 24, sd = 60 * 60 * 12))
  d3 <- d2 + round(rnorm(1, mean = 60 * 60 * 24, sd = 60 * 60 * 12))
  list(
    date_collected = as.character(d1),
    date_received = as.character(d2),
    date_verified = as.character(d3)
  )
}

# Generate mock variants
# TODO: Future iterations could sample from Clinvar instead
gen_var <- function(gene, amount = 1) {
  cds <- gene_info[gene, "coding"]
  pos <- sample(nchar(cds), amount)
  from <- sapply(pos, \(p) substr(cds, p, p))
  to <- sapply(from, \(fr) sample(setdiff(c("A", "C", "G", "T"), fr), 1))
  data.frame(pos = pos, from = from, to = as.vector(to))
}

# Generate HGVS identifiers for variants
gen_hgvs <- function(var_data, gene) {
  b <- new.hgvs.builder.c()
  hgvsc <- sapply(seq_len(nrow(var_data)), \(i) {
    with(var_data[i, ], {
      b$substitution(pos, from, to)
    })
  })
  b <- new.hgvs.builder.g()
  cds_start <- gene_info[gene,"start_position"]
  hgvsg <- sapply(seq_len(nrow(var_data)), \(i) {
    with(var_data[i, ], {
      b$substitution(cds_start + pos - 1, from, to)
    })
  })
  cds_seq <- gene_info[gene, "coding"]
  hgvs <- do.call(rbind,lapply(hgvsc, \(.hgvsc) translateHGVS(.hgvsc,cds_seq)))
  hgvs <- cbind(hgvsg=hgvsg,hgvs)
  hgvs
}

# Generate VCV accessions
gen_vcv <- function(amount=1) {
  #[SRV]CV[A-Z0-9]{9}
  replicate(amount, {
    paste0("VCV00", paste0(sample(10, 7, replace = TRUE) - 1, collapse = ""))
  })
}

find_variant_exon <- function(variant_chr, variant_pos, gene_symbol) {
  matches <- exons_df[
    exons_df$external_gene_name == gene_symbol &
      exons_df$chromosome_name == variant_chr &
      exons_df$exon_chrom_start <= variant_pos &
      exons_df$exon_chrom_end >= variant_pos,
  ]
  if (nrow(matches) > 0) {
    return(matches$rank[1])  # rank = exon number
  } else {
    return(NA)
  }
}



#Sample random variants
sample_variants <- function(genes) {
  num_variants <- max(1, rpois(1, 1))
  replicate(num_variants, {
    data <- list()
    data$gene_symbol <- sample_field("genes", 1L)
    data$variant_id <- gen_vcv()
    data$chromosome <- paste0("chr", 
      gene_info[data$gene_symbol, "chromosome_name"]
    )
    var_data <- gen_var(data$gene_symbol)
    hgvs <- gen_hgvs(var_data, data$gene_symbol)
    data <- c(data, hgvs[, 1:3])
    # TODO: Add aapos, fromAA, toAA
    data$transcript_id <- gene_info[data$gene_symbol, "refseq_mrna"]
    # TODO: data$exon
    data$exon <- find_variant_exon(
      data$chromosome,
      as.numeric(data$hgvsg),
      data$gene_symbol
    )
    #make function to query biomart for exon derived from variant position
    data$reference_genome <- "GRCh38"
    data$zygosity <- sample(
      field_values$zygosity, 1,
      prob = c(.8, .2)
    )
    data$interpretation <- sample(
      field_values$interpretation, 1,
      prob = c(.6, .3, .1)
    )
    data
  }, simplify = FALSE)
}

gen_genes <- function() {
  gene_symbols <- sample_field("genes", round(runif(1, 5, 20)))
  refseq_accs <- gene_info[gene_symbols, "refseq_mrna"]
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
  data$tested_genes <- gen_genes()
  data$num_tested_genes <- length(data$tested_genes)
  data$sample_type <- sample_field("sample_types")
  data$analysis_type <- sample_field("analysis_types")
  data$variants <- sample_variants(names(data$tested_genes))
  data$num_variants <- length(data$variants)
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