library(stringr)
library(ggplot2)

server <- function(input, output, session) {

  convertTimestamp <- function(millisecs, origin = '1970-01-01', tz = 'US/Eastern'){
    as.POSIXct(as.numeric(millisecs) / 1000, origin = origin, tz = tz)
  }
  
  logfilename <- function() {
    dirPath <- '/usr/local/lib/rstudio-server/audit/r-console'
    allFiles <- list.files(dirPath, full.names = T)
    recentUpdate <- which.max(lapply(allFiles, function(x) file.info(x)[['mtime']]))
    path <- allFiles[recentUpdate]
    path
  }
  
  allInput <- reactiveFileReader(500, session, logfilename, read.table, 
                                 colClasses = c("NULL", "character")[c(1,1,1,2,2,1,2)], 
                                 sep = ',', header = T, strip.white=TRUE)
  
  tailData6 <- reactive({tail(allInput(), 6)})
  tailData1 <- reactive({tail(allInput(), 1)})
  headData1 <- reactive({head(allInput(), 1)})
  
  plotData <- reactive({
    data.frame(
      nchar = nchar(allInput()[['data']]),
      timestamp = convertTimestamp(allInput()[['timestamp']]),
      stringsAsFactors = F
    )
  })
  
  output$data <- renderTable({tailData6()})  
  output$nlines <- renderText({nrow(allInput())})
  
  output$timestamp <- renderText({
    alpha <- convertTimestamp(headData1()[['timestamp']])
    omega <- convertTimestamp(tailData1()[['timestamp']])
    paste(alpha, omega, sep = ' -- ')
  })
  
  output$plot <- renderPlot({
    ggplot(plotData(), aes(x=timestamp, y=nchar, xend=timestamp, yend=0)) +
      geom_segment(color='darkgrey') +
      scale_y_log10() +
      xlab('') + ylab('')
  })
  
}


ui <- shinyUI(fluidPage(
  titlePanel("Audit Dash"),
  fluidRow(
    column(6,
           h4("File")
    )),
  fluidRow(
    column(6, wellPanel(
      tableOutput("data")
    ))
  ),
  fluidRow(
    column(6,
           h4("Summary")
    )
  ),
  fluidRow(
    column(6, wellPanel(
      p("Total inputs"),
      verbatimTextOutput("nlines"),
      "Time Range",
      verbatimTextOutput("timestamp"),
      p("Number of characters per input"),
      plotOutput('plot', height = '200px')
    )))
))

shinyApp(ui, server)