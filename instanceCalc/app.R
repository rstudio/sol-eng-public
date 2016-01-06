library(shiny)
library(ggplot2)
library(shinydashboard)

ui <- dashboardPage(
  title = "Calculator",
  dashboardHeader(
    title = "R Instance Calculator", titleWidth = "300px"
  ),
  dashboardSidebar(
    width = "300px",
    h3('1. Sessions'),
    p('How many active sessions do you need to support?'),
    numericInput("volume", NULL, 7, 1, 500),
    h3('2. Memory'),
    p('What is the percentage of small/medium/large sessions in terms of memory?'),
    sliderInput("memory",NULL, 0, 100, c(50,85), ticks = FALSE),
    plotOutput("distPlot",height="50px"),
    h3('3. Compute'),
    checkboxInput("compute","Do individual sessions use multiple cores?", FALSE),
    checkboxInput("hyper","Are you using hyper-threading?", FALSE),
    h3('4. Custom instance sizes'),
    checkboxInput("custom","Do you want to use a custom instance size?", FALSE),
    conditionalPanel(
      condition = "input.custom == true",
      numericInput("customMem", "Memory (GB)", 128, 2, 2048),
      numericInput("customCPU", "Compute (Cores)", 8 ,1, 64)
    ),
    br(),
    hr(),
    p('Copyright RStudio')
  ),
  dashboardBody(
    strong("Instructions"),
    p("Estimate the amount of memory, cores, and instances that will be needed to 
      support your organization. Input the number of active sessions, the memory
      profile of sessions, and compute requirements."),
    fluidRow(
      valueBoxOutput("memoryReq", 6),
      valueBoxOutput("cpuReq", 6)
    ),
    fluidRow(
      box(
        width = 12, height = "300px", solidHeader = TRUE, status="primary",
        h3('Number of Instances Required', align='center'),
        plotOutput("nodePlot")
      )
    ),
    fluidRow(
      box(
        width = 12, status = 'primary', align="center",
        h3("Detail of Instances", align = "center"),
        tableOutput("nodeFits")
      )
    )
    )
)

server <- shinyServer(function(input, output, session) {

  ##############################################
  ### Inputs and functions
  ##############################################
  
  # Calculate number of instances based on inputs
  calculateNodes <- function(x,vec) x%/%vec + (x%%vec > 0)
  
  # Input parameters
  parms<-list(memMultiplier=4,
              cpuMultiplier=2,
              hyperMultiplier=0.75,
              mem=c(small=0.50, med=5, large=30),
              cpu=c(small=0.25, med=1, large=2),
              nodeMem=c(micro=16, small=64, medium=256, large=512),
              nodeCPU=c(micro=2, small=8, medium=16, large=32),
              nodeNames=c("Micro", "Small", "Medium", "Large")
  )

  ##############################################
  ### Output memory distribution
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
    lam <- parms$mem * parms$memMult
    mu <- sum(profile() * lam)
    round(mu * input$volume)
  })
  
  # Output memory estimate
  output$memoryReq <- renderValueBox({
    valueBox(paste(memory(), 'GB'), "Memory", icon("flash", lib = "glyphicon"))
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
    valueBox(paste(compute(), 'Cores'), "Cores", icon("th", lib = "glyphicon"))
  })

  ##############################################
  ### Estimate number of instances required
  ##############################################
  
  # Organize instance sizes
  nodeParms <- reactive({
    nodeNames <- parms$nodeNames
    nodeMem <- parms$nodeMem 
    nodeCPU <- parms$nodeCPU
    if(input$custom) {
      nodeNames <- c(nodeNames, custom = 'Custom')
      nodeMem <- c(nodeMem, custom = input$customMem) 
      nodeCPU <- c(nodeCPU, custom = input$customCPU)
    }
    list(nodeNames = nodeNames, nodeMem = nodeMem, nodeCPU = nodeCPU)
  })
  
  # Estimate instance sizes based on memory and cores
  nodeFits <- reactive({
    mem <- calculateNodes(memory(), nodeParms()[['nodeMem']])
    cpu <- calculateNodes(compute(), nodeParms()[['nodeCPU']])
    setNames(pmax(mem, cpu), nodeParms()[['nodeNames']])
  })
  
  # Plot instances
  output$nodePlot <- renderPlot({
    dat <- data.frame(nodeNames = names(nodeFits()), nodeSizes = nodeFits())
    ggplot(dat, aes(reorder(nodeNames, length(nodeNames):1), nodeSizes)) + 
      geom_bar(stat='identity', fill='goldenrod') + 
      geom_hline(yintercept = 0:max(dat$nodeSizes), color = 'white') +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background = element_blank()) +
      coord_flip() + 
      xlab("") + ylab("")
  }, height = 200)
  
  # Output table of instances
  output$nodeFits <- renderTable({
    data.frame(
      Type = nodeParms()[['nodeNames']],
      Memory = nodeParms()[['nodeMem']], 
      Cores = nodeParms()[['nodeCPU']],
      Instances = nodeFits())
  }, display = c('s', 's', 'd', 'd', 'd'), include.rownames = FALSE)
  
})

shinyApp(ui = ui, server = server)

