# Site adjustment factors

Return site adjustment factors of selected phenotypic descriptors.

## Usage

``` r
siteAdjustmentFactors(
  phenoData,
  descriptors = c("Diameter at breast height (m)", "Lower crown height (m)",
    "Timber height (m)", "Total height (m)", "Crown radius (m)")
)
```

## Arguments

- phenoData:

  phenoData tibble containing phenotype data

- descriptors:

  columns of phenoData on which calculate site correction factors

## Examples

``` r
library(dplyr)

## Retrieve file paths for example data
files <- list.files(system.file("phenotypeDataCollectionSheets",
  package = "pdi"
), full.names = TRUE)

## Prepare data
d <- map(files, readPhenotypeSheet) %>%
  map(preparePhenotypeData) %>%
  bind_rows() %>%
  siteAdjustment()

sa_factors <- siteAdjustmentFactors(d)
```
