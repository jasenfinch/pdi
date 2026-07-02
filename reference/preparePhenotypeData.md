# Prepare phenotype data

Process parsed phenotype data sheets into a tibble suitable for random
forest analysis.

## Usage

``` r
preparePhenotypeData(phenotypeData)
```

## Arguments

- phenotypeData:

  parsed phenotype data collection sheet returned from
  `readPhenotypeSheet`

## Examples

``` r
library(dplyr)

## Retrieve file paths for example data
files <- list.files(system.file('phenotypeDataCollectionSheets',
  package = 'pdi'),full.names = TRUE)

## Prepare data
d <- map(files,readPhenotypeSheet) %>%
  map(preparePhenotypeData)
```
