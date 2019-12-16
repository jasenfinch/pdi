#' Calculate Decline Indexes
#' @description Calculate Phenotypic Decline Index (PDI) and Decline Acuteness Index from phenotypic descriptors.
#' @param files vector of file paths to phenotype data collection sheets (.xlsx) 
#' @importFrom purrr map_chr
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom dplyr bind_cols
#' @examples 
#' files <- list.files(system.file('phenotypeDataCollectionSheets',package = 'pdi'),full.names = TRUE)
#' DIs <- calcDIs(files)
#' @export

calcDIs <- function(files){
  
  phenoData <- files %>%
    map(readPhenotypeSheet)
  
  locations <- phenoData %>%
    map_chr(~{.$Location})
  
  names(phenoData) <- locations
  
  phenoData <- phenoData %>%
    map(preparePhenotypeData) %>%
    bind_rows(.id = 'Location')
  
  phenoData <- phenoData %>%
    mutate(`Crown condition (%)` = crownCondition(`Missing crown (%)`,
                                                  `Crown transparency (%)`),
           `Crown volume (m^3)` = crownVolume(`Crown radius (m)`,
                                              `Total height (m)`,
                                              `Lower crown height (m)`,
                                              `Crown condition (%)`),
           `Bleed prevalence (%)` = bleedPrevalence(`Active bleed size (cm)`,
                                                    `Active bleeds`,
                                                    `Black staining size (cm)`,
                                                    `Black staining`,
                                                    `Diameter at breast height (m)`),
           `Agrilus exit hole density (m^-2)` = agrilusExitHoleDensity(`Agrilus exit holes`,`Diameter at breast height (m)`)
    )
  analysisTable <- makeAnalysisTable(phenoData)
  
  correctedPhenoData <- siteCorrection(phenoData) %>%
    mutate(`Crown condition (%)` = crownCondition(`Missing crown (%)`,
                                                  `Crown transparency (%)`),
           `Crown volume (m^3)` = crownVolume(`Crown radius (m)`,
                                              `Total height (m)`,
                                              `Lower crown height (m)`,
                                              `Crown condition (%)`),
           `Bleed prevalence (%)` = bleedPrevalence(`Active bleed size (cm)`,
                                                    `Active bleeds`,
                                                    `Black staining size (cm)`,
                                                    `Black staining`,
                                                    `Diameter at breast height (cm)`),
           `Agrilus exit hole density (m^-2)` = agrilusExitHoleDensity(`Agrilus exit holes`,`Diameter at breast height (cm)`)
    )
  
  correctedAnalysisTable <- makeAnalysisTable(correctedPhenoData)
  
  set.seed(1234)
  
  unsupervisedRF <- rf(correctedAnalysisTable,NULL,100)
  
  DIs <- unsupervisedRF %>%
    mds() %>%
    rename(PDI = `Dimension 1`,DAI = `Dimension 2`) %>%
    mutate(PDI = minMaxScale(PDI),
           DAI = 2 * minMaxScale(DAI) - 1) %>%
    bind_cols(correctedPhenoData %>%
                select(Location,`Tree No`,Status))
  return(list(DIs = DIs,unsupervisedRF = unsupervisedRF,data = analysisTable))
}