#' Infer the rMAX
#'
#' @description
#' Calculates the rMAX of a density-dependence relationship, based on K and current population status (size and trend)
#'
#' @param K a strictly positive number. Carrying capacity (= maximum size that the population can ever reach)
#' @param theta a strictly positive number. Parameter defining the shape of the density-dependence relationship.
#' The relationship is defined as : r <- rMAX*(1-(N/K)^theta)
#' Note lambda = r + 1
#' @param pop_size_current a strictly positive number. Current population size.
#' @param pop_growth_current a strictly positive number. Current population growth rate.
#' @param rMAX_theoretical the maximum value for rMAX, usually calculated from the function rMAX_spp. See ?rMAX_spp for details.
#'
#' @return the r_MAX value.
#' @export
#'
#' @examples
#'## Infer rMAX
#'infer_rMAX(K = 2000, theta = 1, pop_size_current = 200, pop_growth_current = 1.08)
#'
#'
infer_rMAX <- function(K, theta = 1, pop_size_current, pop_growth_current, rMAX_theoretical = Inf){

      ## Infer rMAX
      N_a = pop_size_current
      r_a = pop_growth_current - 1

      rMAX <- r_a/((1-(N_a/K))^theta)


      ## Take the minimum between this value and the theoretical species rMAX
      rMAX <- min(rMAX, rMAX_theoretical)

  return(rMAX)

}  # END FUNCTION
################################################################################


