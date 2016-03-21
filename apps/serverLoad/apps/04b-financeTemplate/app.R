library(ggplot2)
library(dplyr)
library(fasttime)
library(readr)

infile <- "../../data/serverLoad.txt"

calculateMovingAverage <- function(x, n) 
  stats::filter(x, rep(1/n, n), sides = 2)

ui <- htmlTemplate(
  
  filename = "index.html",
  
  calendar = dateRangeInput(
    inputId = "dateRange", 
    label = "Select a date range", 
    start = Sys.Date(), 
    end = Sys.Date(), 
    separator = 'through'
  ),
  
  checkBox = checkboxInput("ma", "Fit a moving average", FALSE),
  
  slider = conditionalPanel(
    condition = "input.ma == true",
    sliderInput(
      inputId = "ma_adjust",
      label = "Moving average adjustment",
      min = 1, max = 100, value = 5, step = 1.0,ticks = FALSE
    )
  ),
  
  plot = plotOutput("plot"),
  
  data = dataTableOutput("data")
  
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
      mutate(Date = fastPOSIXct(dte))
  })
  
  filterData <- reactive({
    allData() %>%
      filter(Date >= fastPOSIXct(input$dateRange[1])) %>%
      filter(Date < fastPOSIXct(input$dateRange[2]+1))
  })
  
  plotData <- reactive({
    if(input$ma){
      adj <- nrow(filterData()) * input$ma_adjust / 100
      filterData() %>%
        mutate(MA = calculateMovingAverage(Load, adj))
    }
    else
      filterData()
  })
  
  output$plot <- renderPlot({
    p1 <- ggplot(plotData(), aes(x=Date, xend=Date, y=0, yend=Load)) + 
      geom_segment(color='darkblue', alpha = 0.25) +
      xlab("") + 
      ylab("Log10 Load") + 
      scale_y_log10()
    if(input$ma) 
      p1 + geom_line(aes(Date, MA), size = 1.5, color = 'tomato', alpha = 0.75)
    else
      p1
  })
  
  output$data <- renderDataTable(
    select(filterData(), Date, Load),
    options = list(pageLength = 5, order = list(0, 'desc'))
  )
  
}

shinyApp(ui, server)