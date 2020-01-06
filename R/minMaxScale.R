#' minMaxScale
#' @description Variable min-max scaling.
#' @param vec vector of numbers to scale
#' @export

minMaxScale <- function(vec){
  (vec - min(vec)) / (max(vec) - min(vec))
}