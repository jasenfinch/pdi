#' mds
#' @description perform multidimensional scaling of random forest proximities
#' @param rfModels list containing random forest models as returned by \code{rf()}
#' @param dimensions number of dimensions to scale to
#' @importFrom tibble as_tibble
#' @importFrom stats cmdscale
#' @importFrom purrr map
#' @export

mds <- function(rfModels,dimensions = 2){
  rfModels %>%
    map(~{.$proximity %>%
        as_tibble(.name_repair = 'minimal') %>%
        rowid_to_column(var = 'Sample1') %>%
        gather('Sample2','Proximity',-Sample1)}) %>%
    bind_rows(.id = 'Iteration') %>%
    mutate(Sample2 = as.numeric(Sample2)) %>%
    group_by(Sample1,Sample2) %>%
    summarise(Proximity = mean(Proximity)) %>%
    spread(Sample2,Proximity) %>%
    ungroup() %>%
    select(-Sample1) %>%
    as.matrix() %>%
    {1 - .} %>%
    cmdscale(k = dimensions) %>%
    {suppressMessages(as_tibble(.,.name_repair = 'universal'))} %>%
    rename(`Dimension 1` = `...1`,`Dimension 2` = `...2`)
}