##==============================================================================
##                           Plot trajectories                                ==
##==============================================================================

plot_it <- function(N){
  TH <- dim(N)[2]

  # Average trend
  N_avg <- apply(N, c(1,2,3), mean)

  # Color palette
  col_sc0 <- colorRampPalette(colors = c("black", "grey"))(nsim) %>% make_transparent(., percent = 85)
  col_h <- colorRampPalette(colors = c("red", "orange"))(nsim) %>% make_transparent(., percent = 85)

  # Initiate plot
  plot(x = 1:TH, y = colSums(N_avg[,,"sc0"]), 'l', col=1, lwd=3, ylim = c(0,max(colSums(N))),
       xlab = "Annee", ylab = "Taille de population (totale)", main=NULL)

  for(sim in 1:nsim) points(x = 1:TH, y = colSums(N[,,"sc0",sim]), 'l', col=col_sc0[sim])

  for(sim in 1:nsim) points(x = 1:TH, y = colSums(N[,,"sc1",sim]), 'l', col=col_h[sim])

  points(x = 1:TH, y = colSums(N_avg[,,"sc0"]), 'l', col=1, lwd=3)
  points(x = 1:TH, y = colSums(N_avg[,,"sc1"]), 'l', col="red", lwd=3)
} # End function



##==============================================================================
##                              Print impact                                  ==
##==============================================================================
print_it <- function(impact, lci, uci){
  #print(
    paste0("Impact sur la taille de population : ", round(impact, 2)*100, "%",
         " [", round(lci, 2)*100, "% ; ", round(uci, 2)*100, "%]")
  #)
} # End function
