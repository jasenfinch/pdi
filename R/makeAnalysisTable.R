#' Make analysis table
#' @description prepare data table ready for random forest analysis
#' @param phenoData tibble containing phenotype data
#' @importFrom purrr map_dfc
#' @export

makeAnalysisTable <- function(phenoData){
  analysisTable <- phenoData %>%
    select(-Location,-ID,-GPS,-Status,-`Tree No`)
  
  if ('ChosenGroup' %in% names(analysisTable)) {
    analysisTable <- analysisTable %>%
      select(-ChosenGroup)
  }
  
  analysisTable[,{map_dfc(analysisTable,is.character) %>% as.logical()}] <- analysisTable[, {map_dfc(analysisTable,is.character) %>% as.logical()}] %>%
    map_dfc(factor)
  
  return(analysisTable)
}