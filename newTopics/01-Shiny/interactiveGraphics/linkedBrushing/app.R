require(shiny)
require(ggplot2)

ui <- basicPage(
  fluidRow(
    plotOutput("scatter1", brush = brushOpts(id = "brush"), height = "250px"),
    plotOutput("scatter2", height = "250px")
  )
)

server <- function(input, output) {
  
  output$scatter1 <- renderPlot({
    ggplot(mtcars, aes(disp, hp)) + 
      geom_point() + 
      ggtitle("Horsepower vs Displacement")
  })
  
  output$scatter2 <- renderPlot({
    brushed <- brushedPoints(mtcars, input$brush)
    ggplot(mtcars, aes(disp, mpg)) + 
      geom_point() +
      ggtitle("MPG vs Displacement") +
      geom_point(data = brushed, colour = "red", size = 4)
  })
  
}

shinyApp(ui, server)