#' Rmax value for a given species
#'
#' @description
#' Calculate the theoretical Rmax of a species using the equations of Niel and Lebreton 2005.
#' There are two different equation depending on whether the species is longlived or "short-lived".
#' Here, we consider a species to be "shortlived" if age at first reproduction <= 2 and adult survival <= 0.8.
#'
#' References.
#' Niel, C., and J. Lebreton. 2005. Using demographic invariants to detect overharvested bird
#' populations from incomplete data. Conservation Biology 19:826-835.
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

  lambdaMax <- ( ((surv*afr - surv + afr + 1) + sqrt((surv - surv*afr - afr - 1)^2 - (4*surv*(afr)^2))) / (2*afr) )
  rMAX_ll <- lambdaMax - 1

  lambdaMax <- function(x) (exp((afr+surv/(x-surv))^-1)) - x
  lambdaMax <- uniroot(lambdaMax, c(1,4))
  rMAX_sl <- lambdaMax[[1]] - 1

  if(afr <= 2 & surv <= 0.8){
    # "short-lived"
    rMAX <- rMAX_sl
  }else{
    # "long-lived"
    rMAX <- rMAX_ll
  }

  return(rMAX)
  #return(list(rMAX = rMAX, rMAX_longlived = rMAX_ll, rMAX_shortlived = rMAX_sl))
}


######################################################################################################


#' Theta value for a given species
#'
#' @description
#' Calculate the theoretical value of the theta parameter (= shape of the Density-dependence relationship) of a species using the equations of Johnson et al. 2012.
#' The equation is log(theta) = 1.129 - 1.824*rMAX
#'
#' References.
#' Johnson, F. A., M. A. H. Walters, and G. S. Boomer. 2012. Allowable levels of take for the trade in
#' Nearctic songbirds. Ecological Applications 22:1114-1130.
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
theta_spp <- function(rMAX_species) exp(1.129 - 1.824*rMAX_species)

