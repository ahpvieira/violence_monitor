library(shiny)

source("./ui/ui.R", encoding = "UTF-8")
source("./server/server.R", encoding = "UTF-8")

shinyApp(
      ui = ui,
      server = server
)