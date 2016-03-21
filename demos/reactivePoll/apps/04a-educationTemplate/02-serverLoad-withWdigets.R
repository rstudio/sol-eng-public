library(ggplot2)
library(dplyr)
library(fasttime)
library(readr)


infile <- '/home/nathan/sol-eng/presentations/201601-shinydev/randomWalkApp/data/randomWalk.txt'
fileReaderData <- read_csv(infile, c('dte', 'Load'))

calculateMovingAverage <- function(x, n) stats::filter(x, rep(1/n, n), sides = 2)

server <- function(input, output, session) {

  #fileReaderData <- reactiveFileReader(500, session, infile, read_csv, col_names = c('dte', 'Load'))

  allData <- reactive({
    fileReaderData %>%
      mutate(Date = fastPOSIXct(dte))
  })
  
  filterData <- reactive({
    allData() %>%
      filter(Date >= fastPOSIXct(input$dateRange[1])) %>%
      filter(Date < fastPOSIXct(input$dateRange[2]+1))
  })
  
  movingAverage <- reactive({
    if(input$ma)
      filterData() %>%
      mutate(MA = calculateMovingAverage(Load, input$ma_adjust))
    else
      filterData()
  })

  output$plot <- renderPlot({
    p1 <- ggplot(movingAverage(), aes(Date, Load)) + geom_line(color='darkgrey')
    if(input$ma) p1 <- p1 + geom_line(aes(Date, MA), size = 2, color='tomato')
    p1 <- p1 + xlab("") + ylab("")
    p1 <- p1 + scale_y_log10()
    p1
  })
  
  output$data <- renderDataTable({
    select(filterData(), Date, Load)
  })

}

ui <- shinyUI(
  fluidPage(
    titlePanel("Server Load"),
    tags$hr(),
    dateRangeInput("dateRange", "Select a date range", Sys.Date(), Sys.Date(), separator = 'through'),
    checkboxInput("ma", "Fit a moving average", FALSE),
    conditionalPanel(condition = "input.ma == true",
                     sliderInput(inputId = "ma_adjust",
                                 label = "Moving average adjustment",
                                 min = 1, max = 5000, value = 1000, step = 1.0)
    ),
    plotOutput("plot"),
    tags$hr(),
    dataTableOutput("data")
  )
)

shinyApp(ui, server)