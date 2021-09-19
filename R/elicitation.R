##### NOTES --------------------------------------------------------------------
## vals = elicited values (steps 1 to 3): min, best, max
# matrix: nrow = 3 ; ncol = nb of experts

## Cp = elicited probability coverage (step 4)
# vector: length = nb of experts


##==============================================================================
##  Function to estimate parameters (gamma) from expert elicitation table     ==
##==============================================================================

#' Function to estimate parameters from expert elicitation
#'
#' @param vals a matrix containing the elicited min, best and max value from a set of experts.
#'  One expert per column. ncol = number of expert elicited.
#' @param Cp a vector containing the elicited coverage probabilities from the same experts.
#' @param weights weights attributed to each expert
#' @param lower lower limit
#' @param upper upper limit
#'
#' @return a list
#' @export
#'
#' @import SHELF
#'
#' @examples
#'
#' ## Elicited values for 4 experts
#' E1 <- c(50, 70, 100)
#' E2 <- c(200, 240, 280)
#' E3 <- c(100, 180, 300)
#' E4 <- c(120, 160, 220)
#' vals <- matrix(cbind(E1, E2, E3, E4), nrow = 3, byrow = FALSE)
#'
#' ## Elicited coverage probabilities from the same 4 experts
#' Cp = c(0.80, 0.90, 0.90, 0.70)
#'
#' ## Weights attributed to each expert
#' weights = c(1, 0.2, 0.2, 0.1)
#'
#' ## Estimation
#' elicitation(vals, Cp, weights = weights)
#'
elicitation <- function(vals, Cp, weights = 1, lower = 0, upper = Inf){

  vals[vals == 0] <- min(min(vals[vals != 0])/100, 0.0001)

  # Get probability of quantiles
  probs <- sapply(X = Cp, FUN = function(Cp) c((1-Cp)/2, 0.5, Cp+(1-Cp)/2))

  # Fit
  fit_raw <- SHELF::fitdist(vals = vals, probs = probs, lower = lower, upper = upper, weights = weights)

  ## Extract values (gamma)
  shape_raw <- fit_raw$Gamma$shape
  rate_raw <- fit_raw$Gamma$rate

  ## Smoothing
  v_new <- seq(min(vals), max(vals), length.out = 10)[-1]
  p_new <- SHELF::plinearpool(fit = fit_raw, x = v_new, d = "gamma", w = 1)
  fit_smooth <- SHELF::fitdist(vals = v_new, probs = p_new, lower = lower, upper = upper, weights = 1)

  ## Extract values (gamma)
  shape_smooth <- fit_smooth$Gamma$shape
  rate_smooth <- fit_smooth$Gamma$rate
  # fit_smooth["Gamma"]

  return(list(fit_raw = fit_raw,
              shape_raw = shape_raw, rate_raw = rate_raw,
              mean_raw = shape_raw/rate_raw, var_raw = shape_raw/(rate_raw)^2,
              mode_raw = (shape_raw-1)/rate_raw,
              shape_smooth = shape_smooth, rate_smooth = rate_smooth,
              mean_smooth = shape_smooth/rate_smooth, var_smooth = shape_smooth/(rate_smooth)^2,
              mode_smooth = (shape_smooth-1)/rate_smooth
              )
         )

} # End of function
################################################################################







##==============================================================================
##             Function to plot the expert elicitation curves                 ==
##==============================================================================
#' Function to plot the expert elicitation curves
#'
#' @param out the output list from the elicitation function
#' @param ... additional graphical parameters passed onto the classic "plot" function
#'
#'
#' @return a plot of the estimated distirbution (elitation curves) of elicited parameters
#' @export
#'
#' @import graphics
#'
#' @examples
#' ## Elicited values for 4 experts
#' E1 <- c(50, 70, 100)
#' E2 <- c(200, 240, 280)
#' E3 <- c(100, 180, 300)
#' E4 <- c(120, 160, 220)
#' vals <- matrix(cbind(E1, E2, E3, E4), nrow = 3, byrow = FALSE)
#'
#' ## Elicited coverage probabilities from the same 4 experts
#' Cp = c(0.80, 0.90, 0.90, 0.70)
#'
#' ## Estimate
#' out <- elicitation(vals, Cp)
#'
#' ## Plot it
#' plot_elicitation(out)
#'
plot_elicitation <- function(out, ...){

  fit_raw = out$fit_raw
  n_experts <- nrow(fit_raw$Gamma)

  if(n_experts < 1){

  }else{
    if(n_experts == 1){
      xlims <- qgamma(p = c(0.0001,0.9999), shape = fit_raw$Gamma[, "shape"],
                      rate = fit_raw$Gamma[,"rate"])
      X_i <- seq(xlims[1], xlims[2], length.out = 200)
      Y_i <- dgamma(x = X_i, shape = fit_raw$Gamma[, "shape"], rate = fit_raw$Gamma[, "rate"])
      # Plot
      plot(x = X_i, y = Y_i, type = "l", col = "darkblue", lwd = 3, ...)

    }else{

      ## Extract predictions
      # Linear pool predicts (not smoothed)
      LP <- SHELF::linearPoolDensity(fit = fit_raw, d = "gamma", lpw = 1)
      xlim0 <- c(min(LP$x), max(LP$x))

      # Smoothed predicts
      Y_smooth <- dgamma(x = LP$x, shape = out$shape_smooth, rate = out$rate_smooth)

      # Individual expert predicts
      length.out <- 200
      Y_i <- X_i <- matrix(NA, nrow = n_experts, ncol = length.out)

      for(i in 1:nrow(fit_raw$Gamma)){
        xlims <- qgamma(p = c(0.0001,0.9999), shape = fit_raw$Gamma[i, "shape"],
                        rate = fit_raw$Gamma[i, "rate"])
        X_i[i,] <- seq(xlims[1], xlims[2], length.out = length.out)
        Y_i[i,] <- dgamma(x = X_i[i,], shape = fit_raw$Gamma[i, "shape"],
                          rate = fit_raw$Gamma[i, "rate"])
      }


      # Plot
      xlim <- qgamma(p = c(0.001,0.999), shape = out$shape_smooth, rate = out$rate_smooth)
      ylim = c(min(c(Y_i, Y_smooth, LP$y)), max(c(Y_i, Y_smooth, LP$y)))

      plot(x = LP$x, y = Y_smooth, type = "l", col = "darkblue", lwd = 3, ylim = ylim, xlim = xlim, ...)
      for(i in 1:n_experts) points(x = X_i[i,], y = Y_i[i,], type = "l", col = i, lwd = 1, lty = 2)

      # lengend
      legend(x = xlim[2], y = max(Y_smooth, Y_i), xjust = 1,
             legend = c(paste0("Expert #", 1:n_experts), "Estimation globale"),
             lty = c(rep(3, n_experts), 1), lwd = c(rep(1, n_experts), 3),
             col = c(1:n_experts, "darkblue"),
             text.col = c(1:n_experts, "darkblue"), text.font = c(rep(1, n_experts), 2),
             bty = "n", y.intersp = 1.5
      )

      # points(x = LP$x, y = LP$f, type = "l", col = "green", lwd = 3)

    }  # if 2
  } # if 1

  } # End of function
################################################################################





##==============================================================================
##              Function to sample the elicited distribution                  ==
##==============================================================================
#' Function to sample an expert elicited distribution
#'
#' @param n number of draws
#' @param out the output list from the elicitation function
#' @param smooth logical. If TRUE, uses the smoothed distribution, averaged across all experts.
#' @param expert a number telling which expert's distribution to draw the values from. Only implemented if smooth = FALSE.
#'
#' @return a vector of sample draws
#' @export
#'
#' @examples
#' ## Elicited values for 4 experts
#' E1 <- c(50, 70, 100)
#' E2 <- c(200, 240, 280)
#' E3 <- c(100, 180, 300)
#' E4 <- c(120, 160, 220)
#' vals <- matrix(cbind(E1, E2, E3, E4), nrow = 3, byrow = FALSE)
#'
#' ## Elicited coverage probabilities from the same 4 experts
#' Cp = c(0.80, 0.90, 0.90, 0.70)
#'
#' ## Estimate
#' out <- elicitation(vals, Cp)
#'
#' ## Draw values from a smoothed, averaged distribution
#' sample_elicitation(n=100, out = out)
#'
#' # Draw values from an indiviual expert's distribution
#' sample_elicitation(n=100, out = out, smooth = FALSE, expert = 3)
#'
sample_elicitation <- function(n, out, smooth = TRUE, expert = NULL){

  if(smooth){
    draws <- rgamma(n = n, shape = out$shape_smooth, rate = out$rate_smooth)
  }else{
    print(paste("Expert", expert))
    draws <- sampleFit(fit = out$fit_raw, n = n, expert = expert)[,"gamma"]
  } # end if

  return(draws)

} # End of function
################################################################################
