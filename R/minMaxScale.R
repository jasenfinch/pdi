#' minMaxScale
#' @description variable min-max scaling
#' @export

minMaxScale <- function(vec){
  (vec - min(vec)) / (max(vec) - min(vec))
}