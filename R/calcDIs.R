#' @importFrom purrr map_chr
#' @importFrom readr read_csv
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom dplyr bind_cols
#' @export

calcDIs <- function(files,seed = 1234){
  
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
                                                    `Diameter at breast height (cm)`),
           `Agrilus exit hole density (m^-2)` = agrilusExitHoleDensity(`Agrilus exit holes`,`Diameter at breast height (cm)`)
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
  
  set.seed(seed)
  
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