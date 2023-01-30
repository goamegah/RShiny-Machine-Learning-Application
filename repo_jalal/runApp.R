# app.R or runApp.R

source("ui.R")
source("server.R")
source("global.R")

shinyApp(ui = ui, server = server)