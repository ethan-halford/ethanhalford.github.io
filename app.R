library(semantic.dashboard)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(dartRverse)
library(gbRd)
library(tools)
devtools::install_github("https://github.com/green-striped-gecko/dartR.captive/tree/dev_ethan")
withConsoleRedirect <- function(containerId, expr) {
  # Change type="output" to type="message" to catch stderr
  # (messages, warnings, and errors) instead of stdout.
  txt <- capture.output(results <- expr, type = "output")
  if (length(txt) > 0) {
    insertUI(paste0("#", containerId), where = "beforeEnd",
             ui = paste0(txt, "\n", collapse = "")
    )
  }
  results
}
gl.di
global <- new.env()
global$font <- "
                white-space:normal;
                font-family:'Lucida Sans Unicode','Lucida Grande',sans-serif;
                text-align:justify;
                font-size:14px;
                font-weight:normal;
                padding-left:10px;
                       padding-top:0px;
                       padding-right:10px;
                       padding-bottom:0px;
"


compute_data <- function(updateProgress = NULL) {
  # Create 0-row data frame which will be used to store data
  dat <- data.frame(x = numeric(0), y = numeric(0))
  
  for (i in 1:10) {
    Sys.sleep(0.25)
    
    # Compute new row of data
    new_row <- data.frame(x = rnorm(1), y = rnorm(1))
    
    # If we were passed a progress update function, call it
    if (is.function(updateProgress)) {
      text <- paste0("x:", round(new_row$x, 2), " y:", round(new_row$y, 2))
      updateProgress(detail = text)
    }
    
    # Add the new row of data
    dat <- rbind(dat, new_row)
  }
  
  dat
}



pig <- readRDS("pig.rds") %>%
  {position(.) <- 1:nLoc(.); .} %>%
  {chromosome(.) <- rep("1", nLoc(.));.}

sheep <- readRDS("sheep.rds") %>%
  {position(.) <- 1:nLoc(.); .} %>%
  {chromosome(.) <- rep("1", nLoc(.));.}

possum <- possums.gl[1:100] %>%
  {position(.) <- 1:nLoc(.); .} %>%
  {chromosome(.) <- rep("1", nLoc(.));.}%>%
  {. <- gl.subsample.loci(., n=100);.}


af <- gl.diagnostics.relatedness(possum, 
                           ref_variables = "ref_variables.csv", 
                           sim_variables = "sim_variables.csv",
                           cleanup = T, 
                           run_sim =T, 
                           which_tests = "wang",
                           IncludePlots = T,
                           rmseOut =T,
                           varOut = F,
                           numberIterations = 1, 
                           numberGenerations = 3,
                           includedPed = F
)





ui <- fluidPage(
  
  dashboard_page(
    title = "Relatedness Analysis", 
    dashboardHeader(logo_align = "left", 
                    titleWidth = "wide", 
                    logo_path = "USYD_logo.png", 
                    menu_button_label = "Hide/Show",
                    class = "ui top attached header"), 
    
    #Sidebar# 
    dashboardSidebar(
      size="", 
      color="green", 
      sidebarMenu(
        menuItem(
          tabName = "Welcome Jit!", 
          text=span(icon("map"), "Welcome", style="font-size:16px")
        ), 
        menuItem(
          tabName = "Relatedness", 
          text=span(icon("dna"), "Relatedness", style="font-size:16px")
        ), 
        menuItem(
          tabName = "Pedigrees", 
          text=span(icon("cow"), "Pedigrees", style="font-size:16px")
          
        ), 
        menuItem(
          tabName = "Simulations", 
          text=span(icon("computer"), "Simulations", style="font-size:16px")
        )
      )
    ), 

    dashboardBody(
      tabItem(tabName = "Welcome Jit!",
              splitLayout(
                box(
                  color ="green",
                  ribbon = FALSE,
                  collapsible = FALSE,
                  tags$pre(
                    style=global$font,
                                       p(
                      "Welcome to this interactive web application designed to
                   complement classes on population genetics at the University
                   of Sydney. This project aims to go beyond the conventional
                   methods of presenting information by providing an engaging
                   and dynamic experience for students. With the inclusion of
                   real-time computer simulations, this web app offers an
                   immersive learning experience that allows students to explore
                   different population genetic models, manipulate their
                   assumptions and parameters, and gain a deeper understanding
                   of complex population genetics concepts. Through interactive
                   exercises, students can move sliders, enter specific values,
                   or click buttons, making the learning process more fluid and
                   engaging.")
                    )
                  )
                )
              ), 
      
      tab_item(tabName = "Relatedness", 
               splitLayout(
                 box(
                   color ="green",
                   ribbon = FALSE,
                   collapsible = FALSE,
                   tags$pre(
                     style=global$font, 
                     p("gl.diagnostics.relatedness"), 
                     p("This function wraps a variety of methods for estimating relatedness, 
                       such that they can be directly compared for accuracy and precision. 
                       It also provides the ability to run the gl.sim function for a minimum 
                       of 3 generations, providing further functionality with regards to 
                       estimating gene flow and population dynamics. It supports multiple 
                       simulation back ends, correlation output, error checking, RMSE/variance 
                       summaries, and optional plotting.")
                   )
                 ),
                 box(
                   style=global$font,
                   height=100, 
                   collapsible = T, 
                   collapsed = T,
                   htmlOutput("literatureOutput")
                )
                 
               ),
               splitLayout(
                 box(), 
                 box()
               )
      ),
      
      tabItem("Pedigrees", 
              splitLayout(
                box(
                  color ="red",
                  ribbon = TRUE,
                  collapsible = FALSE,
                  style=global$font,
                    p("gl.diagnostics.relatedness"), 
                    p("We'll start with a brief introduction to uploading and analysisng 
                      data using a native pedigree - that is a pre-existing pedigree. To begin, 
                      you have the choice of uploading your own or using a pre-existing one. In this 
                      instance, the pre-existing pedigree is taken from a publicly avialable dataset 
                      of pig-breeding across X farms.")
                )
              ), 
              splitLayout(
                box(
                  p("Use a pre-selected datatset"),
                  selectInput("dataChoice", "",
                              choices = c("Pig", "Sheep")), 
                  br(), 
                  numericInput("NumInd", "# Individuals", value = 200), 
                  numericInput("NumLoc", "# Loci", value=300),
                  p("\nOr upload your own"), 
                  fileInput("pedigreeUp", "")
                ), 
                
                box(
                  p("Other potential inputs:"),
                  awesomeCheckbox("E9tick", "E9?", value = F), 
                  awesomeCheckbox("RMSEtick", "RMSE", value=F), 
                  awesomeCheckbox("Vartick", "Variance", value=F),
                  br(),
                  selectInput(
                    "testChoice", "Choose tests", c("trioml", "wang", "lynchli", "lynchrd", "ritland",
                                                      "quellergt", "dyadml"),
                    multiple = TRUE
                  ), 
                  br(), 
                  actionButton("plotGo", "Plot Out?")
                )
              ), 
              splitLayout(
                box(
                  selectInput("plot_selector", "Choose a plot:",
                              choices = NULL),
                  conditionalPanel(
                    condition = "input.plot_selector == 'RegPlot'",
                    plotOutput("RegPlot")
                  ),
                  conditionalPanel(
                    condition = "input.plot_selector == 'VarPlot'",
                    plotOutput("VarPlot")
                  ),
                  conditionalPanel(
                    condition = "input.plot_selector == 'RMSEPlot'",
                    plotOutput("RMSEPlot")
                  )
                )
              )
              
              ), 
      tabItem("Simulations", 
              splitLayout(
                box(
                  color ="red",
                  ribbon = TRUE,
                  collapsible = FALSE,
                  style=global$font,
                    p("Simulations"), 
                    p("We'll now move on to running simulations using gl.relatedness.diagnostics
                      and analysing the output - more specifically using the simulations as a 
                      benchmark for comparing methods of estimating relatedness.")
                  
                ), 
                box(
                  p("Use a pre-selected datatset"),
                  selectInput("dataChoice", "",
                              choices = c("Pig", "Sheep", "Possum", "Platypus")), 
                  numericInput("NumInd2", "# Individuals", value = 200), 
                  numericInput("NumLoc2", "# Loci", value=300),
                  p("\nOr upload your own"), 
                  fileInput("simUpload", "")
                )
              ), 
              splitLayout(
                box(
                  p("Edit variables"), 
                  textInput("NumGen", "# of Gen", value = 3),
                  br(), 
                  textInput("NumIter", "# of Iterations", value = 1),
                  br(), 
                  selectInput(
                    "testChoice2", "Choose tests", c("trioml", "wang", "lynchli", "lynchrd", "ritland",
                                                    "quellergt", "dyadml"), multiple = T
                  ), 
                  br(), 
                  actionButton("runSim", "Sim Go!")
                ), 
                box(
                  selectInput("iterSelect", "Iterations to display", 
                              choices = 1), 
                  actionButton("iterationChange", "Display")
                )
              ), 
              splitLayout(
                box(
                  p("Boxplot"), 
                  plotOutput("SimPlot1")
                  ), 
                box(
                  #pre(id = "console")
                  p("Density plot"),
                  plotOutput("SimPlot2")
                )
              )
      )
      
      )
    
    ),
  
  br(), 
  
  tags$footer(
    p(em("Developed by"), br("Ethan Halford")),
    align = "center",
    style = "bottom:0;
              width:100%;
              color:black;
              background-color:white;
              z-index: 1000;"
  )
) 


server <- function(input, output, session){
  output$photo <- renderImage({
    list(
      src = file.path("dartRlogo.png"), 
      contentType = "image/png", 
      width=259, 
      height=300
    )
  })

  ########## Relatedness Intro 
  output$help_output <- renderText({
    temp = Rd2HTML(gbRd::Rd_fun("gl.diagnostics.relatedness"),out = tempfile("docs"), 
                   dynamic=T)
    content = read_file(temp)
    file.remove(temp)
    content
  })
  
  output$literatureOutput <- renderText({
    temp = Rd2HTML(Rd_fun("reactive"),out = tempfile("docs"))
    content = readr::read_file(temp)
    file.remove(temp)
    content
  })

  
  ########## Pedigree
  dataDownload <- reactive({
    switch(input$dataChoice,
           "Pig" = pig, 
           "Sheep" = sheep)
  })
  
  locIndChange <- reactive({
    dataDownload()[1:input$NumInd] %>%
      {. <- gl.subsample.loci(.,input$NumLoc); .}
  })
  
  testChoice1 <- reactive({
    as.character(input$testChoice)
  })
  
  
  plotVals <- eventReactive(input$plotGo, {
    gl.diagnostics.relatedness(locIndChange(), 
                               which_tests = testChoice1(),
                               IncludePlots = T,
                               rmseOut = input$RMSEtick,
                               varOut = input$Vartick,
                               runE9 = input$E9tick,  
                               e9Path = "/Users/ethanhalford/Desktop/DArT_Coding/Luis_files", 
                               e9parallel = T,
                               nCores=8, 
                               includedPed = T
    ) 
  })
  
  observeEvent(input$plotGo, {
    selected <- c()
    if(input$E9tick == T){
      selected <- c(selected, "E9")
    }else if(input$RMSEtick == T){
      selected <- c(selected, "RMSE")
    }else if(input$Vartick == T){
      selected <- c(selected, "Var")
    }
    
    updateSelectInput(session, "plot_selector",
                      choices = selected) # Optionally set a default selected value
  })
  
    
  output$RegPlot <- eventReactive(input$plotGo,{
    renderPlot({
      plotVals()@plotList[[1]][[1]]
    })
  })
  
  output$RMSEPlot <- eventReactive(input$plotGo,{
    if(input$RMSEtick == T){
      renderPlot({
        plotVals()@corOutList@varPlot[[1]]
        })
    }else{
      NULL
    }
  })
  
  output$VarPlot <- eventReactive(input$plotGo,{
    if(input$Vartick == T){
      renderPlot({
        plotVals()@corOutList@rmsePlot[[1]]
      })
    }else{
      NULL
    }
  })
  
  
  ########## Simulation tab 
  simDataChoice <- reactive({
    switch(input$dataChoice,
           "Pig" = pig, 
           "Sheep" = sheep, 
           "Possum" = possum, 
           "Platypus" = platypus.gl
           )
  })
  
  iterReactive <- eventReactive(input$runSim,{
    as.numeric(input$NumIter)
  })
  
  genReactive <- eventReactive(input$runSim,{
    as.numeric(input$NumGen)
  })

  testReact <- reactive({
    as.character(input$testChoice2)
  })

  locIndChange2 <- reactive({
    simDataChoice()[1:input$NumInd2] %>%
      {. <- gl.subsample.loci(.,input$NumLoc2); .}
  })
  
  
  reactiveSim <- eventReactive(input$runSim, {
    gl.diagnostics.relatedness(locIndChange2(), 
                               ref_variables = "ref_variables.csv", 
                               sim_variables = "sim_variables.csv",
                               cleanup = T, 
                               run_sim =T, 
                               which_tests = testReact(),
                               IncludePlots = T,
                               rmseOut =F,
                               varOut = F,
                               numberIterations = iterReactive(), 
                               numberGenerations = genReactive(),
                               includedPed = F
    ) 
  })
  
  observe({
    updateSelectInput(session, "iterSelect", choices = c(1:iterReactive()))
    })

  
  iterLength <- eventReactive(input$iterationChange,{
    as.numeric(input$NumIter)
  })
  
  output$SimPlot1 <- renderPlot({
    reactiveSim()@plotList[[iterLength()]][[1]]
  })
  
  output$SimPlot2 <- renderPlot({
    reactiveSim()@plotList[[iterLength()]][[2]]
  })
  
}


shinyApp(ui, server)





