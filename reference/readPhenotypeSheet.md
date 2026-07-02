# Read phenptyping sheet

Parse .xlsx phenotype data collection sheets.

## Usage

``` r
readPhenotypeSheet(file)
```

## Arguments

- file:

  file path to excel file to parse

## Examples

``` r
library(dplyr)

## Retrieve file paths for example data
files <- list.files(system.file('phenotypeDataCollectionSheets',
  package = 'pdi'),full.names = TRUE)

## Prepare data
d <- readPhenotypeSheet(files[1])
```
