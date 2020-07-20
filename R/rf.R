#' rf
#' @description perform random forest repetitions
#' @param analysisTable tibble of phenotype data suitable for random forest analysis as returned by \code{preparePhenotypeData}
#' @param cls analysisTable column to use as response vector. NULL for unsupervised analyses.
#' @param nreps number of repetitions
#' @param seed random number seed
#' @importFrom randomForest randomForest
#' @importFrom magrittr set_names
#' @export

rf <- function(analysisTable, cls, nreps = 100, seed = 1234){
  set.seed(seed)
  map(1:nreps,~{
    randomForest(analysisTable,cls,proximity = T,importance  = TRUE)
  })
}

#' descriptorContributions
#' @description Calculate average descriptor contributions to random forest models.
#' @param rfModels list containing random forest models as returned by \code{rf()}
#' @details See \code{see ?randomForest::importance} for details on random forest importance metrics.
#' @importFrom randomForest importance
#' @importFrom dplyr summarise_all
#' @export

descriptorContributions <- function(rfModels){
  rfModels %>%
    map(~{
      .x %>%
        importance() %>%
        data.frame(check.names = FALSE) %>%
        tibble::rownames_to_column() %>%
        as_tibble() %>%
        rename(Descriptor = rowname)
    }) %>%
    set_names(1:length(.) %>%
                as.character()) %>%
    bind_rows(.id = 'rep') %>%
    group_by(Descriptor) %>%
    select(-rep) %>%
    summarise_all(mean)
}