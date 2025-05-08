# LEI Report Mockup Generator

Here we generate mockup clinical report PDFs to be used for evaluating clinical data extraction tools.

## Structure: 
  1. `scripts/` contains the following R scripts:
    * `fetch_gene_info.R` : Generates `data/gene_info.csv`. Uses web-services to pull location data, transcripts and sequence data for a list of genes. 
    * `generate_mock_data.R` : Generates mockup data in `json` format. 
    * `interpolate.R` : Interpolates the mockup data into LaTeX templates.
  2. `data/` contains source data for mockup generation:
    * `fieldValues.yml` 