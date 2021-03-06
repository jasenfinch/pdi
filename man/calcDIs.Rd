% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcDIs.R
\name{calcDIs}
\alias{calcDIs}
\title{Calculate Decline Indexes}
\usage{
calcDIs(rfModels, PDI = TRUE, DAI = TRUE, invertPDI = TRUE, invertDAI = TRUE)
}
\arguments{
\item{rfModels}{list containing random forest models as returned by \code{rf()}}

\item{PDI}{TRUE/FALSE, calculate PDI?}

\item{DAI}{TRUE/FALSE, calculate DAI?}

\item{invertPDI}{invert the PDI scale? TRUE/FALSE. Ignored if argument PDI is FALSE}

\item{invertDAI}{invert the DAI scale? TRUE/FALSE. Ignored if argument DAI is FALSE}
}
\description{
Calculate Phenotypic Decline Index (PDI) and Decline Acuteness Index (DAI).
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
  siteAdjustment() \%>\%
   mutate(`Live crown ratio (\%)` = liveCrownRatio(`Total height (m)`,
     `Lower crown height (m)`),
     `Crown condition (\%)` = crownCondition(`Missing crown (\%)`,
                               `Crown transparency (\%)`),
     `Crown volume (m^3)` = crownVolume(`Crown radius (m)`,
                               `Total height (m)`,
                               `Lower crown height (m)`,
                               `Crown condition (\%)`),
     `Bleed prevalence (\%)` = bleedPrevalence(`Active bleed length (mm)`,
                               `Active bleeds`,
                               `Black staining length (mm)`,
                               `Black staining`,
                               `Diameter at breast height (m)`),
     `Agrilus exit hole density (m^-2)` = agrilusExitHoleDensity(`Agrilus exit holes`,
                               `Diameter at breast height (m)`)
)

t <- makeAnalysisTable(d)

## Generate random forest models
m <- rf(t,cls = NULL,nreps = 10)

## Calculate decline indexese
DIs <- calcDIs(m,DAI = FALSE,invertPDI = FALSE) \%>\%
  bind_cols(d \%>\%
    select(Location,ID,Status))
}
