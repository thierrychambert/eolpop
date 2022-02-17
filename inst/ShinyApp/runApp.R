setwd( paste0(getwd(),"/inst/ShinyApp/") )
getwd()

source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)

