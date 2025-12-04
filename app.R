# Combined app file for shinyapps.io deployment

# Source UI and Server
source("ui.r")
source("server.r")

# Run the application
shinyApp(ui = ui, server = server)
