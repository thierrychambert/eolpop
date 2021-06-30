#' Infer Densisty-dependence Parameters
#'
#' infer_DD calculates the missing value of a parameter (either K or rMAX, whichever is NULL) in a density-dependence relationship.
#'
#' @param rMAX a number. Maximum (theoretical) population intrinsic rate of increase.
#' @param K a strictly positive number. Carrying capacity (= maximum size that the population can ever reach)
#' @param theta a strictly positive number. Parameter defining the shape of the density-dependence relationship.
#' The relationship is defined as : r <- rMAX*(1-(N/K)^theta)
#' Note lambda = r + 1
#' @param pop_size_current a strictly positive number. Current population size.
#' @param pop_growth_current a strictly positive number. Current population growth rate.
#'
#' @return a list of length 2, containing K and lambda_MAX.
#' @export
#'
#' @examples
#'## Infer K
#'infer_DD(rMAX = 0.15, K = NULL, theta = 1, pop_size_current = 200, pop_growth_current = 1.08)
#'## Infer rMAX
#'infer_DD(rMAX = NULL, K = 2000, theta = 1, pop_size_current = 200, pop_growth_current = 1.08)
#'
#'
infer_DD <- function(rMAX = NULL, K = NULL, theta = 1, pop_size_current, pop_growth_current){

  if(!is.null(rMAX)){
    # Infer K
    #rMAX = lambda_MAX - 1
    r_a = pop_growth_current - 1
    N_a = pop_size_current

    K <- (N_a/((1-(r_a/rMAX))^(1/theta))) %>% round

  }else{

    if(!is.null(K)){
      # Infer rMAX
      N_a = pop_size_current
      r_a = pop_growth_current - 1

      rMAX <- r_a/((1-(N_a/K))^theta)
      #lambda_MAX = rMAX + 1

    }else{

      stop("Either rMAX or K must be provided")

    } # end if 2
  } # end if 1

  return(list(rMAX = rMAX, K = K))

}  # END FUNCTION
################################################################################


