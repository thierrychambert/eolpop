#' Infer the rMAX
#'
#' @description
#' Calculates the rMAX of a density-dependence relationship, based on K and current population status (size and trend)
#'
#' @param K a strictly positive number. Carrying capacity (= maximum size that the population can ever reach)
#' @param pop_size_current a strictly positive number. Current population size.
#' @param pop_growth_current a strictly positive number. Current population growth rate.
#' @param rMAX the maximum rate of population growth.
#'
#' @return the theta value of a theta-logistic population growth model.
#' @export
#'
#'
infer_theta <- function(K, pop_size_current, pop_growth_current, rMAX){

  ## Infer rMAX
  N_a = pop_size_current
  r_a = pop_growth_current - 1

  theta <- log(1 - (r_a/rMAX)) / log(N_a/K)

  return(theta)

}  # END FUNCTION
################################################################################


