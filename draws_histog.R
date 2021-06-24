
##==============================================================================
##                         Check lambda draws                                 ==
##==============================================================================
draws_histog <- function(draws, mu, se){

  # Plot histogram
  h <- hist(draws, breaks = length(draws)/10, border = 0)

  # Theoretical Normal Curve
  par(new=T)
  curve(dnorm(x, mean=mu, sd=se), add=FALSE, lwd=3, col="darkblue",
        xlim = c(min(draws), max(draws)), axes = FALSE, xlab = "", ylab = "")

} # End function
################################################################################


# draws_histog(draws = run0$lambdas, mu = pop_growth_mean, se = pop_growth_se)
