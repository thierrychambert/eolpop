##==============================================================================
##           Function to draw values from Gamma (Normal shape)                ==
##==============================================================================
#' Draw values from a Custom Gamma Distribution to force a Normal shape (symmetrical bell curve)
#'
#' @param n number of value to draw.
#' @param mu mean
#' @param sd standard deviation
#' @param r scaling factor. Used to ensure a Normal shape for small mean values.
#'
#' @return a vector of drawn values.
#' @export
#'
#' @examples
#' sample_gamma(n= 5, mu = 4, sd = 1.5)
#'
sample_gamma <- function(n, mu, sd, r=3) {
  if(mu <= 0){
    xx <- rep(0,n)
  }else{
    if(sd <= 0){
      xx <- rep(mu,n)
    }else{
      if(mu < 100){
        mu = mu*10^r
        sd = sd*10^r
        shape = (mu/sd)^2 ; shape
        scale = sd^2/mu ; scale
        xx = rgamma(n, shape = shape, scale = scale) / 10^r

      } else {

        shape = (mu/sd)^2
        scale = sd^2/mu
        xx = rgamma(n, shape = shape, scale = scale)
      }
    }
  }
  return(xx)
} # End function
################################################################################



