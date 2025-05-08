#!/usr/bin/Rscript

library(argparser)
library(RJSONIO)

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
#   out_prefix = "CHEO"
# )

# Read the template
lines <- readLines(args$template_file)
text <- paste(lines, collapse = "\n")

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
for (uuid in names(mock_data)) {
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
        marker <- paste0("\\\\data\\{", label, "\\}")
        if (label == "blurb") {
          # TODO: implement blurb generator
          txt <- sub(marker, "BLURB GOES HERE.", txt)
        } else if (!(label %in% names(dataset))) {
          cat("Skipping unsupported label: ", label, "\n")
          txt <- sub(marker, "MISSING DATA!", txt)
        } else {
          value <- dataset[[fields[j, "label"]]]
          txt <- sub(marker, value, txt)
        }
      }
    } else {
      #otherwise, if this is an iterator section:
      iter_type <- sub("^[^:]+:", "", section_name)
      subdataset <- dataset[[iter_type]]
      #TODO: More here
    }
  })
}