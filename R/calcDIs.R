#' Calculate Decline Indexes
#' @description Calculate Phenotypic Decline Index (PDI) and Decline Acuteness Index (DAI)
#' @param rfModels list containing random forest models
#' @param invertPDI invert the PDI scale? TRUE/FALSE
#' @param invertDAI invert the DAI scale? TRUE/FALSE
#' @importFrom magrittr %>%
#' @export

calcDIs <- function(rfModels, invertPDI = TRUE, invertDAI = TRUE){
  DIs <- rfModels %>%
    mds() %>%
    rename(PDI = `Dimension 1`,DAI = `Dimension 2`) %>%
    mutate(PDI = minMaxScale(PDI),
           DAI = minMaxScale(DAI) %>%
             {2 * . - 1})
  
  if (isTRUE(invertPDI)) {
    DIs <- DIs %>%
      mutate(PDI = PDI %>%
               {. * -1})
  }
  
  if (isTRUE(invertDAI)) {
    DIs <- DIs %>%
      mutate(DAI = DAI %>%
               {. * -1})
  }
  
  return(DIs)
}