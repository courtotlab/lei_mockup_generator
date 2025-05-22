#!/usr/bin/Rscript

library(argparser)
library(RJSONIO)
library(yaml)
library(hgvsParseR)

#parse command line arguments
ap <- arg_parser(
  "interpolate template with mockup values",
  name = "interpolate.R"
)
ap <- add_argument(ap,
  "template_file",
  help = "template file to be interpolated (.tex)"
)
ap <- add_argument(ap,
  "json_file",
  help = "json data file with the values to interpolate into the template (.json)"
)
ap <- add_argument(ap,
  "--outprefix",
  help = "the output file"
)
args <- parse_args(ap)
# args <- list(
#   template_file = "templates/CHEO_template.tex",
#   json_file = "mock_data.json",
#   outprefix = "test/CHEO"
# )
if (is.na(args$outprefix)) {
  args$outprefix = sub("\\.tex$", "", basename(args$template_file))
}

# Read the template
lines <- readLines(args$template_file)
text <- paste(lines, collapse = "\n")
blurb_data <- read_yaml("data/text_pieces.yml")

# Read the json data
mock_data <- fromJSON(args$json_file)

#helper function to extract data labels from template
extract <- function(text, rx, capture = TRUE) {
  matches <- gregexpr(rx, text, perl = TRUE)
  pos <- matches[[1]]
  if (length(pos) == 1 && pos[[1]] == -1) {
    #no matches, return empty data frame
    return(data.frame(label = character(), start = integer(), end = integer()))
  }
  len <- attr(matches[[1]], "match.length")
  if (capture) {
    labels <- cbind(
      attr(matches[[1]], "capture.start"),
      attr(matches[[1]], "capture.length")
    ) |> apply(1, \(x) substr(text, x[[1]], x[[1]] + x[[2]] - 1))
  } else {
    labels <- NA
  }
  data.frame(label = labels, start = pos, end = pos + len - 1)
}

# helper function to escape special characters for latex
tex_escape <- function(str) {
  #fixed substitution with pipe support
  g <- function(x, s, r) gsub(s, r, x, fixed = TRUE)
  #auto-prepend backslash
  e <- function(x, s) g(x, s, paste0("\\", s))
  #sequentially apply all rules
  str |>
    g("\\", "\\textasciibackslash") |>
    g("~", "\\textasciitilde") |>
    g("^", "\\textasciicircum") |>
    e("%") |> e("&") |> e("$") |> e("#") |> e("_") |> e("{") |> e("}")
}

#capitalize a word ("hello" -> "Hello")
cap <- function(txt) {
  substr(txt, 1, 1) <- toupper(substr(txt, 1, 1))
  txt
}
#convert a number to text (2 -> "two")
num2text <- function(num, one = FALSE) {
  if (num == 1 && !one) {
    "a" # says "a" instead of "one"
  }
  blurb_data$numbers[[num]]
}
#convert an amino acid's code to its full name
aaname <- function(aa) {
  blurb_data$residues[[tolower(aa)]]
}

# generates a summary text blurb for a set of variants
summary_blurb <- function(variants, suffix = "detected.") {
  #if there are no variants, we're done
  if (length(variants) == 0) {
    return(paste("No variants", suffix))
  }
  interpretations <- sapply(variants, `[[`, "interpretation")
  #rename "pathogenic" to "pathogenic variant", etc
  interpretations <- sapply(interpretations, \(iname) {
    #also, convert to lower case
    iname <- tolower(iname)
    if (!grepl("variant", iname)) {
      paste(iname, "variant")
    } else {
      iname
    }
  })
  #count how many there are of each type
  inter_table <- table(interpretations)
  #generate strings for each type/number (e.g "two pathogenic variants")
  vstrings <- sapply(names(inter_table), \(iname) {
    #translate the number to a text string (2 -> "two")
    numstr <- inter_table[[iname]] |> num2text()
    #add plural when number is greater 1
    if (inter_table[[iname]] > 1) {
      iname <- sub("variant", "variants", iname)
    }
    paste(numstr, iname)
  })
  #if there's only one type, we're done
  if (length(vstrings) == 1) {
    return(cap(paste(vstrings[[1]], suffix)))
  }
  #concatenate with commas and "and"
  paste(
    paste(vstrings[-length(vstrings)], collapse = ", "),
    "and", vstrings[[length(vstrings)]], suffix
  )
}


long_blurb <- function(variants) {
  #if there are no variants, we're done
  if (length(variants) == 0) {
    return("No variants were detected.")
  }
  hgvsps <- sapply(variants, `[[`, "hgvsp")
  var_data <- hgvsParseR::parseHGVS(hgvsps)

  var_blurbs <- lapply(seq_along(variants), \(i) {
    variant <- c(variants[[i]], var_data[i, ])
    if (!is.na(variant$variant) && variant$variant == "Ter") {
      variant$type <- "stop"
    }

    intro <- substitute(paste(
      bold(variants$gene_symbol, variants$hgvsc, variants$hgvsp, 
           variants$zygosity, variants$zygosity)
    ))
  
    location <- paste(
      "The", variant$hgvsc, "occurs at position", variant$start,
      "in exon ", variant$exon, "of the", variant$gene_symbol,
      "within chromosome ", variant$chromosome, ". It causes "
    )
    effect <- switch(variant$type,
      synonymous = "causes no amino acid change.",
      stop = paste(
        "causes an early translation termination at position",
        variant$start, "."
      ),
    substitution = paste(
      "causes an amino acid substitution, which replaces",
      aaname(variant$ancestral), "with", aaname(variant$variant), "."
    )
  )
  interpretation_text <- if (grepl("uncertain", variant$interpretation)) {
      paste(
        "According to ClinVar, the evidence collected to date is",
        "insufficient to firmly establish the clinical significance of this",
        "variant, therefore it is classified as a",
        tolower(variant$interpretation), "."
      )
    } else {
      paste(
        "In accordance with existing evidence, this variant is therefore",
        "classified as a", tolower(variant$interpretation), "variant."
      )
    }
    Conservation_text <- paste(
      "Functional studies have demonstrated that the",
      variant$gene_symbol, variant$hgvsc,
      "variant leads to abnormal behaviour of the", 
      variant$gene_symbol, "gene.",
      "This variant is located in a highly conserved region of the protein",
      "and it is predicted to be damaging to the protein function,",
      "contributing to oncogenesis.",
      paste0(variant$ancestral, variant$start),
      "residue is weakly conserved in evolution.",
      "In silico analysis programs (SIFT, PolyPhen-2, Mutation Taster) predict",
      "this variant",
      if (grepl("uncertain", variant$interpretation)) {
        "to be tolerated"
      } else {
        "not to be tolerated"
      },
      ". This variant is listed in ClinVar",
      paste0("(", variant$variant_id, ")"),
      "and it has been implicated in lung and blood cancers.",
      "Pubmed references:", 
      paste(sample(1e8:1e9, sample(3:8, 1)), collapse = ", ")
    )
    paste(location, effect, interpretation_text,
          Conservation_text, sep = "\n")
  })
  paste(var_blurbs, collapse = "\n\n")
}

#extract iterator sections

rx_begin_iter <- "\\\\begin\\{dataiter\\}\\{([^}]+)\\}"
rx_end_iter <- "\\\\end\\{dataiter\\}"
iter_starts <- extract(text, rx_begin_iter)
iter_ends <- extract(text, rx_end_iter, capture = FALSE)
#assert that each dataiter begin also has an end
stopifnot(nrow(iter_starts) == nrow(iter_ends))

#split text into sections and iterators
text_sections <- list()
last_end <- 0
for (i in seq_len(nrow(iter_starts))) {
  text_sections[[paste0("text_", i)]] <-
    substr(text, last_end + 1, iter_starts[i, "start"] - 1)
  text_sections[[paste0("iter_", i, ":", iter_starts[i, "label"])]] <-
    substr(text, iter_starts[i, "end"] + 1, iter_ends[i, "start"] - 1)
  last_end <- iter_ends[i, "end"]
}
text_sections[[paste0("text_", i + 1)]] <-
  substr(text, last_end + 1, nchar(text))

#extract field positions in each section
rx_field <- "\\\\data\\{([^}]+)\\}"
section_fields <- lapply(text_sections, \(txt) extract(txt, rx_field))

#iterate over datasets
# for (uuid in names(mock_data)) {
outputs <- lapply(names(mock_data), \(uuid) {
  dataset <- mock_data[[uuid]]
  #perform interpolations
  inter_sections <- lapply(seq_along(text_sections), \(i) {
    section_name <- names(text_sections)[[i]]
    txt <- text_sections[[i]]
    fields <- section_fields[[i]]
    #if this is a regular text section:
    if (startsWith(section_name, "text_")) {
      for (j in seq_len(nrow(fields))) {
        label <- fields[j, "label"]
        marker <- paste0("\\data{", label, "}")
        if (label == "blurb") {
          blurb <- long_blurb(dataset$variants)
          txt <- sub(marker, blurb, txt, fixed = "TRUE")
        } else if (label == "summary_blurb") {
          blurb <- summary_blurb(dataset$variants)
          txt <- sub(marker, blurb, txt, fixed = "TRUE")
        } else if (!(label %in% names(dataset))) {
          cat("Skipping unsupported label: ", label, "\n")
          txt <- sub(marker, paste0("\\textit{Missing ", label, "}"), txt, fixed = TRUE)
        } else {
          value <- tex_escape(dataset[[label]])
          # cat(label, " -> ", value, "\n")
          txt <- sub(marker, value, txt, fixed = "TRUE")
        }
      }
      txt
    } else {
      #otherwise, if this is an iterator section:
      iter_type <- sub("^[^:]+:", "", section_name)
      subdatasets <- dataset[[iter_type]]
      rows <- list()
      for (k in seq_along(subdatasets)) {
        sds <- subdatasets[[k]]
        row <- txt
        for (j in seq_len(nrow(fields))) {
          label <- fields[j, "label"]
          marker <- paste0("\\data{", label, "}")
          if (!(label %in% names(sds))) {
            cat("Skipping unsupported label: ", label, "\n")
            row <- sub(marker, "MISSING DATA!", row, fixed = TRUE)
          } else {
            value <- tex_escape(sds[[label]])
            # cat(label, " -> ", value, "\n")
            row <- sub(marker, value, row, fixed = TRUE)
          }
        }
        rows <- c(rows, row)
      }
      paste(rows, collapse = "")
    }
  })
  paste0(inter_sections, collapse = "")
}) |> setNames(names(mock_data))

#write outputs to file
for (uuid in names(outputs)) {
  outfile <- paste0(args$outprefix, "_", uuid, ".tex")
  cat(outputs[[uuid]], file = outfile)
}

cat("Done!")
