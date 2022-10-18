rm(list = ls(all.names = TRUE))
setwd( paste0(getwd(),"/inst/FR_EolPop_ANSI/") )
#setwd("../EN_EolPop/")
#setwd("../FR_EolPop_ANSI/")
#setwd("../FR_EolPopUTF8/")

getwd()

source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)

