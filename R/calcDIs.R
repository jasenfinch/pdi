#' Calculate Decline Indexes
#' @description Calculate Phenotypic Decline Index (PDI) and Decline Acuteness Index (DAI).
#' @param rfModels list containing random forest models as returned by \code{rf()}
#' @param invertPDI invert the PDI scale? TRUE/FALSE. Ignored if argument PDI is FALSE
#' @param invertDAI invert the DAI scale? TRUE/FALSE. Ignored if argument DAI is FALSE
#' @export

calcDIs <- function(rfModels, PDI = TRUE, DAI = TRUE, invertPDI = TRUE, invertDAI = TRUE){
  DIs <- rfModels %>%
    mds() %>%
    rename(PDI = `Dimension 1`,DAI = `Dimension 2`) %>%
    mutate(PDI = minMaxScale(PDI),
           DAI = minMaxScale(DAI) %>%
             {2 * . - 1})
  
  if (isFALSE(PDI)) {
    DIs <- DIs %>%
      select(-PDI)
  } else {
    if (isTRUE(invertPDI)) {
      DIs <- DIs %>%
        mutate(PDI = PDI %>%
                 {(. - 1) * -1})
    }
  }
  
  if (isFALSE(DAI)) {
    DIs <- DIs %>%
      select(-DAI)
  } else {
    if (isTRUE(invertDAI)) {
      DIs <- DIs %>%
        mutate(DAI = DAI %>%
                 {. * -1})
    }
  }
  
  return(DIs)
}