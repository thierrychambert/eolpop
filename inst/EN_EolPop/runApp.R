rm(list = ls(all.names = TRUE))
#setwd( paste0(getwd(),"/inst/EN_eolpop/") )  ;  getwd()

source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)

