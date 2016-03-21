library(ggplot2)
library(dplyr)
library(fasttime)
library(readr)

infile <- "../../data/serverLoad.txt"

ui <- shinyUI(
  fluidPage(
    titlePanel("Server Load"),
    plotOutput("plot")
  )
)

server <- function(input, output, session) {

  fileReaderData <- reactiveFileReader(
    intervalMillis = 500,
    session = session, 
    filePath = infile, 
    readFunc = read_csv, 
    col_names = c('dte', 'Load')
  )
  
  allData <- reactive({
    fileReaderData() %>%
      do(tail(., 20)) %>%
      mutate(Date = fastPOSIXct(dte))
  })
  
  output$plot <- renderPlot({
    ggplot(allData(), aes(x=Date, xend=Date, y=0, yend=Load)) + 
    geom_segment(color='darkblue', alpha = 0.75) +
      xlab("") + 
      ylab("Log10 Load") + 
      scale_y_log10()
  })

}

shinyApp(ui, server)