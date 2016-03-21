library(ggplot2)
library(dplyr)
library(fasttime)
library(readr)
library(shinydashboard)

infile <- "../../data/serverLoad.txt"

calculateMovingAverage <- function(x, n) 
  stats::filter(x, rep(1/n, n), sides = 2)

ui <- dashboardPage(
  skin = 'blue',
  dashboardHeader(title = "Server Load"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Calendar", tabName = "settings", icon = icon("calendar")),
      menuItem("Plot", tabName = "plot", icon = icon("image")),
      menuItem("Data", tabName = "data", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "settings",
        valueBoxOutput("valueBoxDate"),
        valueBoxOutput("valueBoxValue"),
        valueBoxOutput("valueBoxRecs"),
        dateRangeInput("dateRange", "Select a date range", Sys.Date(), Sys.Date(), separator = 'through')
      ),
      tabItem(
        tabName = "plot",
        fluidRow(
          box(
            'Time Series', width = 8, status = 'primary', plotOutput("plot")
            ),
          box(
            'Moving Average', width = 4, status='info',
            checkboxInput("ma", "Fit a moving average", FALSE),
            conditionalPanel(
              condition = "input.ma == true",
              sliderInput(
                inputId = "ma_adjust",
                label = "Moving average adjustment",
                 min = 1, max = 100, value = 5, step = 1.0)
              )
            )
          )
        ),
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            'Raw Data', width = 8, status='primary', dataTableOutput("data")
            )
          )
        )
      )
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
  
  output$valueBoxDate <- renderValueBox({
    valueBox(
      tail(plotData()[['Date']], 1), "Current Date and Time"
    )
  })
  
  output$valueBoxValue <- renderValueBox({
    valueBox(
      round(tail(plotData()[['Load']], 1), 3), "Current Value"
    )
  })
  
  output$valueBoxRecs <- renderValueBox({
    valueBox(
      format(nrow(plotData()), big.mark = ','), "Total Records Read"
    )
  })
  
}

shinyApp(ui, server)