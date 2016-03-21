library(shiny)
library(ggplot2)
library(tidyr)
library(shinydashboard)

# Input parameters (assumptions)
parms<-list(
  mem =             c(small = 0.50, medium = 5, large = 35),
  memMultiplier =   c(small = 5, medium = 4, large = 3),
  cpu =             c(small = 0.25, medium = 1, large = 4),
  cpuMultiplier =   c(small = 2, medium = 3, large = 4),
  hyperMultiplier = c(small = 0.75, medium = 0.75, large = 0.75),
  nodeMem =         c(micro = 16, small = 64, medium = 256, large = 512),
  nodeCPU =         c(micro = 2, small = 8, medium = 16, large = 32),
  nodeNames =       c("Micro", "Small", "Medium", "Large")
)

# Helper function: calculate number of instances based on inputs
calculateNodes <- function(x, vec) x %/% vec + (x %% vec > 0)

ui <- dashboardPage(

  title = "RStudio - Calc",
  
  dashboardHeader(
    title = "R Instance Calculator", titleWidth = "300px"
  ),
  
  dashboardSidebar(
    
    h3("1. Sessions"),
    p("How many active sessions do you need to support?"),
    numericInput("volume", NULL, 15, 1, 500),
    
    h3("2. Memory"),
    p("What is the percentage of small/medium/large sessions in terms of memory?"),
    sliderInput("memory", NULL, 0, 100, c(50, 85), ticks = FALSE),
    plotOutput("distPlot",height = "50px"),
    
    h3("3. Compute"),
    checkboxInput("compute","Do individual sessions use multiple cores?", FALSE),
    checkboxInput("hyper","Are you using hyper-threading?", FALSE),
    
    h3("4. High Availability"),
    p("What is your standard for high availability?"),
    sliderInput("ha", NULL, 0, 99, 50, ticks = FALSE),

    h3("5. Custom instance sizes"),
    checkboxInput("custom","Do you want to use a custom instance size?", FALSE),
    conditionalPanel(
      condition = "input.custom == true",
      numericInput("customMem", "Memory (GB)", 128, 2, 2048),
      numericInput("customCPU", "Compute (Cores)", 8 ,1, 64)
    ),
    
    br(),
    p("Copyright RStudio"),
    width = "300px"
    
  ),
  
  dashboardBody(
    
    p(strong("Instructions")),
    p("Estimate the amount of memory, cores, and instances that will be needed to 
      support your organization. Input the number of active sessions, the memory
      profile of sessions, and compute and high availability requirements."),

    fluidRow(widht = 12,
      valueBoxOutput("memoryReq", 6),
      valueBoxOutput("cpuReq", 6)
    ),
    
    fluidRow(
      box(
        width = 12, height = "300px", solidHeader = TRUE, status="primary",
        h3("Number of Instances Required", align="center"),
        plotOutput("nodePlot")
      )
    ),
    
    fluidRow(
      box(
        width = 12, status = "primary", align="center",
        h3("Detail of Instances", align = "center"),
        tableOutput("nodeFits")
      )
    ),
    
    p(strong("Disclaimer")),
    p("This calculator provides generalized estimates and cannot account for all
      aspects of your particular environment, use cases, and/or requirements.
      Estimates are calculated using assumptions that may or may not apply to your
      situation.")
    )
  
)

server <- shinyServer(function(input, output, session) {

  ##############################################
  ### Output memory distribution for sidebar
  ##############################################
  
  # Plot memory distribution for sidebar
  output$distPlot <- renderPlot({
    lo <- input$memory[1]
    hi <- input$memory[2]
    par(mar=c(0, 0, 0, 0))
    plot.new()
    plot.window(c(0, 100), c(0, 100))
    rect( 0, 0,  lo, 100, col = "goldenrod1")
    rect(lo, 0,  hi, 100, col = "goldenrod2")
    rect(hi, 0, 100, 100, col = "goldenrod3")
    if(lo > 10) text(lo / 2, 50, "Small\n<1GB")
    if(hi - lo > 15) text(lo / 2 + hi / 2, 50, "Medium\n1GB-10GB")
    if(hi < 90) text(hi / 2 + 50, 50, "Large\n>10GB")
  }, bg = "transparent")
  
  ##############################################
  ### Estimate cores and memory
  ##############################################
  
  # Calculate the memory distribution as a function of inputs
  profile <- reactive({
    c(input$memory[1], diff(input$memory), (100 - input$memory[2])) / 100
  })
  
  # Estimate memory
  memory <- reactive({
    lam <- parms$mem * parms$memMultiplier
    mu <- sum(profile() * lam)
    round(mu * input$volume)
  })
  
  # Output memory estimate
  output$memoryReq <- renderValueBox({
    valueBox(paste(memory(), "GB"), "Memory", icon("flash", lib = "glyphicon"))
  })
  
  # Estimate number of cores
  compute <- reactive({
    lam <- parms$cpu * 
      ifelse(input$compute, parms$cpuMultiplier, 1) * 
      ifelse(input$hyper, parms$hyperMultiplier, 1)
    mu <- sum(profile() * lam)
    round(mu * input$volume)
  })
  
  # Output number of cores
  output$cpuReq <- renderValueBox({
    valueBox(paste(compute(), "Cores"), "Cores", icon("th", lib = "glyphicon"))
  })

  ##############################################
  ### Estimate number of instances required
  ##############################################
  
  # Add custom instance sizes
  nodeParms <- reactive({
    nodeNames <- parms$nodeNames
    nodeMem <- parms$nodeMem 
    nodeCPU <- parms$nodeCPU
    if(input$custom) {
      nodeNames <- c(nodeNames, custom = "Custom")
      nodeMem <- c(nodeMem, custom = input$customMem) 
      nodeCPU <- c(nodeCPU, custom = input$customCPU)
    }
    list(nodeNames = nodeNames, nodeMem = nodeMem, nodeCPU = nodeCPU)
  })
  
  # Estimate instance sizes based on memory and cores
  nodeFits <- reactive({
    nodeparms <- nodeParms()
    mem <- calculateNodes(memory(), nodeparms[["nodeMem"]])
    cpu <- calculateNodes(compute(), nodeparms[["nodeCPU"]])
    instances <- pmax(mem, cpu)
    hainstances <- ceiling(instances * input$ha / 100)
    data.frame(
      Type = nodeparms[["nodeNames"]],
      Memory = nodeparms[["nodeMem"]],
      Cores = nodeparms[["nodeCPU"]],
      HA = hainstances,
      StandAlone = instances,
      Total = hainstances + instances)
  })
  
  # Plot instances
  output$nodePlot <- renderPlot({
    dat <- gather(nodeFits(), Instances, instances, HA, StandAlone)
    maxInstances <- max(dat$Total)
    ggplot(dat, aes(reorder(Type, length(Type) : 1), instances, fill = Instances)) + 
      geom_bar(stat="identity") +
      coord_flip() +
      scale_fill_manual(values=c("chartreuse4", "goldenrod")) +
      geom_hline(yintercept = 0:maxInstances, color = "white") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank()) +
      xlab("") + ylab("")
    }, height = 200)
  
  # Output table of instances
  output$nodeFits <- renderTable({
    nodeFits()
    }, 
    display = c("s", "s", "d", "d", "d", "d", "d"), 
    include.rownames = FALSE)
  
})

shinyApp(ui = ui, server = server)

