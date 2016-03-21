library(RSQLite)
library(ggplot2)
library(dplyr)
library(fasttime)
library(readr)

infile <- "../../data/serverLoad.db"

ui <- shinyUI(
  fluidPage(
    titlePanel("Server Load"),
    plotOutput("plot")
  )
)

server <- function(input, output, session) {

  con <- dbConnect(SQLite(), infile)
  
  session$onSessionEnded(function() {
    dbClearResult(dbListResults(con)[[1]])
    dbDisconnect(con)
    })
  
  checkFunc <- function(){
    res <- dbSendQuery(con, "select max(id) from serverLoad")
    dbFetch(res)
  }
  
  valueFunc <- function(path){
    res <- dbSendQuery(con, "select * from serverLoad")
    dbFetch(res)
  }
  
  pollData <- reactivePoll(
    intervalMillis = 500, 
    session = session, 
    checkFunc = checkFunc, 
    valueFunc = valueFunc
    )

  allData <- reactive({
    pollData() %>%
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