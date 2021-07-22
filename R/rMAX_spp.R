#' Rmax of a species
#'
#' @description
#' Calculate the theoretical Rmax of a species using the equation of Niel and Lebreton 2005.
#'
#' References.
#'
#' Niel, C., and J. Lebreton. 2005. Using demographic invariants to detect overharvested bird
#' populations from incomplete data. Conservation Biology 19:826–835.
#'
#' @param surv Adult survival. Single value between 0 and 1.
#' @param afr Age at first reproduction. Single integer value.
#'
#' @return the theoretical Rmax of the species
#' @export
#'
#' @examples
#' rMAX_spp(surv = 0.6, afr = 2)
#'
rMAX_spp <- function(surv, afr){
  ( ((surv*afr - surv + afr + 1) + sqrt((surv - surv*afr - afr - 1)^2 - (4*surv*(afr)^2))) / (2*afr) ) - 1
}


