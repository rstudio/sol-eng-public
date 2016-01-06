library(rpivotTable)

ui <- bootstrapPage(
  rpivotTableOutput("pivot")
)

server <- function(input, output) {
  load('canadianElections.rda')
  output$pivot <- renderRpivotTable({
    rpivotTable(data = canadianElections,
                rows = c( "Province"),
                cols="Party",
                vals = "votes", 
                aggregatorName = "Sum", 
                rendererName = "Table",
                width="100%", height="500px")
  })
  
}

shinyApp(ui = ui, server = server)