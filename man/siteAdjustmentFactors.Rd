% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/siteAdjustmentFactors.R
\name{siteAdjustmentFactors}
\alias{siteAdjustmentFactors}
\title{Site adjustment factors}
\usage{
siteAdjustmentFactors(
  phenoData,
  descriptors = c("Diameter at breast height (m)", "Lower crown height (m)",
    "Timber height (m)", "Total height (m)", "Crown radius (m)")
)
}
\arguments{
\item{phenoData}{phenoData tibble containing phenotype data}

\item{descriptors}{columns of phenoData on which calculate site correction factors}
}
\description{
Return site adjustment factors of selected phenotypic descriptors.
}
\examples{
library(dplyr)

## Retrieve file paths for example data
files <- list.files(system.file('phenotypeDataCollectionSheets',
  package = 'pdi'),full.names = TRUE)

## Prepare data
d <- map(files,readPhenotypeSheet) \%>\%
  map(preparePhenotypeData) \%>\%
  bind_rows() \%>\%
  siteAdjustment()

sa_factors <- siteAdjustmentFactors(d)
}
