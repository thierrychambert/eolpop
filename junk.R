rm(list = ls(all.names = TRUE))
b1 = 150
mu = 200
b2 = 325

vals = matrix(cbind(c(b1, mu, b2)), nrow = 3, byrow = FALSE)

out <- elicitation(vals = vals, Cp = 0.95)

plot_elicitation(out)
