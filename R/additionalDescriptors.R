#' Crown condition
#' @description Calculate crown condition (%).
#' @param m missing crown (%)
#' @param t crown transparency (%)
#' @export

crownCondition <- function(m,t) {
  (100 - m) * (1 - t/100)
}

#' Estimated crown volume (m^3)
#' @description Calculate estimated crown volume.
#' @param r crown radius (m)
#' @param h total height (m)
#' @param l lower crown height (m)
#' @param c crown condition (%)
#' @export

crownVolume <- function(r,h,l,c) {
  (pi * r ^ 2 * c * (h - l)) / 200
}

#' Estimated bleed prevalence (%)
#' @description Calculate estimated bleed prevalence.
#' @param a average active bleed size (mm)
#' @param A number of active bleeds
#' @param b average black stain size
#' @param B number of black stains
#' @param d diameter at breast height (mm)
#' @param s height to which stem surveyed from the tree base (m)
#' @export

bleedPrevalence <- function(a,A,b,B,d,s = 3) {
  (a^2 * A + b^2 * B)/(s * d * pi)
}

#' Agrilus exit hole density (m^-2)
#' @description Calculate Agrilus biguttatus exit hole density.
#' @param h number of Agrilus exit holes
#' @param d diameter at breast height (mm)
#' @param s height to which stem surveyed from the tree base (m)
#' @export

agrilusExitHoleDensity <- function(h,d,s) {
  h/((s * d)/1000 * pi)
}