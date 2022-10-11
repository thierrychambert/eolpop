rm(list = ls(all.names = TRUE))
setwd( paste0(getwd(),"/inst/FR_eolpop_ANSI/") )
#setwd("../FR_eolpop_ANSI/")
getwd()

source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)

