########################################
### Predictive analalytics           ###
### Comparison of alternative models ###
### Shiny App                        ###
########################################

library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyr)
library(dplyr)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title = "Comparison of Model Methods", titleWidth = "100%"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
    selectInput('file', NULL, c('pred_lm.csv', 'pred_alt.csv')),
    box(plotOutput("density", height = "200px"), width = 6),
    box(plotOutput("fits", height = "200px"), width = 6),
    box(dataTableOutput("mse"), width = 12)
    ))
  )

server <- function(input, output) {
  out <- reactive({
    pred <- read.csv(input$file, row.names = 1)
    pred <- gather(pred, model, predicted, -actual)
    mse <- pred %>% 
      mutate(residual = actual - predicted) %>% 
      group_by(model) %>% 
      summarise(mse = sum(residual^2))
    list(pred = pred, mse = mse)
  })
  output$fits <- renderPlot(
    ggplot(out()[['pred']], aes(actual, predicted)) + 
      geom_abline(color = 'tomato') +
      stat_smooth(method = 'loess') +
      geom_point() + 
      facet_grid(~model) +
      ggtitle('Comparison of predicted values')
  )
  output$density <- renderPlot({
    ggplot(out()[['pred']], aes(actual - predicted, fill = model)) + 
      geom_density(alpha=0.25) +
      xlab('Residual') +
      ggtitle('Comparison of residuals')
  })
  output$mse <- renderDataTable({
    out()[['mse']]
  })
}

shinyApp(ui, server)
