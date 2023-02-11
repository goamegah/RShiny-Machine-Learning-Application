# app.R or runApp.R
source("global.R")
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server,options=list(port=5001))