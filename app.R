library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(shinyjs)
# library(colourpicker)
library(shinythemes)

#### Functions ####

source("agecalculator.R")

recodechc <- function(col) {
  recode(col,
         "g" = "General Ability",
         "Gf/Gv" = "Fluid Reasoning and Visual Processing",
         "Gc" = "Comprehension-Knowledge",
         "Gc:CM" = "Communication Ability",
         "Gc:K0" = "General Verbal Information",
         "Gc:VL" = "Lexical Knowledge",
         "Gc:LD" = "Language Development",
         "Gc:LS" = "Listening Ability",
         "Gc:MY" = "Grammatical Sensitivity",
         "Gf" = "Fluid Reasoning",
         "Gf:I" = "Induction",
         "Gf:RG" = "General Sequential Reasoning",
         "Gf:RQ" = "Quantitative Reasoning",
         "Gf:RE" = "Reasoning Speed",
         "Gf:RP" = "Piagetian Reasoning",
         "Glr" = "Long-Term Storage and Retrieval",
         "Gl" = "Learning Efficiency",
         "Gr" = "Retrieval Fluency",
         "Gr:FF" = "Figural Fluency",
         "Gr:FE" = "Expressional Fluency",
         "Gr:FA" = "Associational Fluency",
         "Gr:SP" = "Alternative Solution Fluency",
         "Gr:FO" = "Creativity",
         "Gr:LA" = "Speed of Lexical Access",
         "Gr:FX" = "Figural Flexibility",
         "Gr:FI" = "Ideational Fluency",
         "Gr:FW" = "Word Fluency",
         "Gr:NA" = "Naming Facility",
         "Gl:M6" = "Free-recall Memory",
         "Gl:MA" = "Associative Memory",
         "Gl:MM" = "Meaningful Memory",
         "Gwm" = "Short-Term Working Memory",
         "Gwm:Wa" = "Auditory Short-Term Storage",
         "Gwm:Wv" = "Visual-Spatial Short-Term Storage",
         "Gwm:AC" = "Attentional Control",
         "Gwm:Wc" = "Working Memory Capacity",
         "Gv" = "Visual-Spatial Processing",
         "Gv:Vz" = "Visualization",
         "Gv:SR" = "Speeded Rotation",
         "Gv:IM" = "Imagery",
         "Gv:CS" = "Closure Speed",
         "Gv:CF" = "Flexibility of Closure",
         "Gv:MV" = "Visual Memory",
         "Gv:SS" = "Spatial Scanning",
         "Gv:PI" = "Serial Perceptual Integration",
         "Gv:LE" = "Length Estimation",
         "Gv:IL" = "Perceptual Illuisions",
         "Gv:PN" = "Perceptual Alterations",
         "Gv:P" = "Perceptual Speed",
         "Ga" = "Auditory Processing",
         "Ga:PC" = "Phonetic Coding",
         "Ga:US" = "Speech Sound Discrimination",
         "Ga:UR" = "Resistance ot Auditory Stimulus Distortion",
         "Ga:U8" = "Maintaining and Judging Rhythm",
         "Ga:UM" = "Memory for Sound Patterns",
         "Ga:U1U9" = "Musical Discrimination and Judgement",
         "Ga:UP" = "Absolute Pitch",
         "Ga:UL" = "Sound Localisation",
         "Gkn" = "Domain Specific Knowledge",
         "Gkn:K1" = "General Science Information",
         "Gkn:K2" = "Knowledge of Culture",
         "Gkn:MK" = "Mechanical Knowledge",
         "Gkn:KL" = "Foreign Language Proficiency",
         "Gkn:KF" = "Knowledge of Signing",
         "Gkn:LP" = "Skill in Lip Reading",
         "Grw" = "Reading and Writing",
         "Grw:RC" = "Reading Comprehension",
         "Grw:RD" = "Reading Decoding",
         "Grw:RS" = "Reading Speed",
         "Grw:WA" = "Writing Ability",
         "Grw:SG" = "Spelling Ability",
         "Grw:WS" = "Writing Speed",
         "Grw:EU" = "English Usage",
         "Gq" = "Quantiative Knowledge",
         "Gq:KM" = "Mathematical Knowledge",
         "Gq:A3" = "Mathematical Achievement",
         "Gs" = "Processing Speed",
         "Gs:P" = "Perceptual Speed",
         "Gs:Ps" = "Perceptual Speed Search",
         "Gs:Pc" = "Perceptual Speed Compare",
         "Gs:N" = "Number Facility",
         "Gs:RS" = "Reading Speed",
         "Gs:WS" = "Writing Speed",
         "ACH" = "Achievement"
  )
}

#### Load Data ####

# Load composites and prepare for Shiny
composites <- read_csv("composites.csv")
composites[is.na(composites)] <- 0
composites$name <- paste(composites$test, composites$composite, sep = " ")
names(composites)[names(composites) == 'composite'] <- 'scale'
composites$chcfull <- recodechc(composites$chc)

# Load subtests and prepare for Shiny
subtests <- read_csv("subtests.csv")
subtests[is.na(subtests)] <- 0
subtests$name <- paste(subtests$test, subtests$subtest, sep = " ")
names(subtests)[names(subtests) == 'subtest'] <- 'scale'
subtests$chcfull <- recodechc(subtests$chc)

# Load conners and prepare for shiny
conners <- read_csv("conners.csv")
conners[is.na(conners)] <- 0
conners$name <- paste(conners$form, conners$scale, sep = " ")

# Confidence Intervals
ci <- data.frame(ci = c(99.7,99.0,98.0,95.0,90.0,80.0,75.0,68.0), z = c(3,2.576,2.326,1.96,1.645,1.282,1.15,1))

#### User Interface ####

ui <- navbarPage(
  
  #### Initial JavaScript ####
  
  useShinyjs(),
  tags$script("$(document).on('shiny:connected', function(event) {var myWidth = $(window).width();Shiny.onInputChange('shiny_width',myWidth)});"),
  
  #### Title and Page Details ####
  
  title = "Psychology Test Visualisation", selected = "Home", fluid = TRUE, footer = includeHTML("footer.html"), 
  theme = shinytheme("flatly"),
  
  #### Home Page UI ####
  
  tabPanel("Home", 
           fluidRow(column(width = 11, offset = 1,
                           themeSelector(),
                           includeHTML("introduction.html"),
                           hr(),
                           includeHTML("assessment.html"))),
           fluidRow(column(width = 4, offset = 1,
                           textInput('clientName', "First Name", placeholder = "John"),
                           dateInput('assesseeDOB', "Date of Birth", max = Sys.Date() + 1, format = "dd/mm/yyyy", value = "2013-01-01")),
                    column(width = 3),
                    column(width = 3))),
  
  #### CHC UI ####
  
  tabPanel("CHC",
           fluidRow(column(width = 5, offset = 1,
                           dateInput("chcAxDate", "Date of Assessment", max = Sys.Date() + 1, format = "dd/mm/yyyy"),
                           textOutput("chcAge")),
                    column(width = 3,
                           numericInput("chcNAdditionalTests", "Number of Additional Tests",value = 0, min = 0, max = 5),
                           uiOutput("chcAdditionalAxInputs"))),
           fluidRow(column(width = 5, offset = 1,
                           hr(),
                           uiOutput("chcTests"),
                           uiOutput("chcComposites"),
                           uiOutput("chcSubtests"),
                           uiOutput("chcPlotOptions"),
                           uiOutput("chcColourOptions"),
                           uiOutput("executeCHCPlot")),
                    column(width = 3,
                           hr(),
                           uiOutput("chcCompositeScores")),
                    column(width = 3,
                           hr(),
                           uiOutput("chcSubtestScores"))),
           fluidRow(column(width = 10, offset = 1,
                           hr(),
                           uiOutput("chcPlotUI"),
                           downloadButton("downloadCHCPlot")
                           # hr(),
                           # tableOutput("chcDataTable")
                           ))),
  
  #### Conners-3 UI ####
  
  tabPanel("Conners-3",
           fluidRow(column(width = 3, offset = 1,
                           selectizeInput("connersForms", "Forms Administered", choices = unique(conners$form), multiple = TRUE, options = list(placeholder = "Form")),
                           uiOutput("connersAxDates"),
                           uiOutput("connersAge")),
                    column(width = 3,
                           uiOutput("connersInputs"),
                           textOutput("connersValues"),
                           textOutput("connersSEM")),
                    column(width = 3,
                           uiOutput("connersPlotOptions"),
                           uiOutput("connersColourOptions"),
                           uiOutput("executeConnersPlot"))),
           fluidRow(column(width = 10, offset = 1,
                           hr(),
                           plotOutput("connersPlot", width = "100%"),
                           downloadButton("downloadConnersPlot")
                           # hr(),
                           # tableOutput("connersDataTable"),
                           # uiOutput("connersClasses")
                           ))))

#### Server ####

server <- function(input, output, session) {
  
  # Set up plots reactive value
  plots <- reactiveValues()
  
  #### Client Name ####
  
  clientName <- reactive({
    if (is.null(input$clientName)) {return()}
    input$clientName
  })
  
  #### CHC Calculate Age ####
  
  chcAge <- reactive({
    as.character(floor(age_calc(input$assesseeDOB, input$chcAxDate, units = "years")))
  })
  
  output$chcAge <- renderText({
    if (clientName() == "") {
      paste("Assessee is ", chcAge(), " years old.")
    } else {
      paste(clientName(), " is ", chcAge(), " years old.") 
    }
  })
  
  #### CHC Additional Assessment Options ####
  
  chcAdditionalTests <- reactive({
    if (input$chcNAdditionalTests < 1) {
      FALSE
    } else if (is.null(input$chcNAdditionalTests)) {
      FALSE
    } else {
      TRUE
    }
  })
  
  chcRandomDates <- reactive({
    sample(2000:2017, input$chcNAdditionalTests)
  })
  
  output$chcAdditionalAxInputs <- renderUI({
    hide("chcPlot")
    hide("downloadCHCPlot")
    if (!chcAdditionalTests()) {
      return()
    } else {
      lapply(1:input$chcNAdditionalTests, function(i) {
        dateInput(paste("chcAssessmentYear",i+1,sep=""),"Assessment Date",value = paste(chcRandomDates()[i],"-01-01",sep=""),max = Sys.Date() + 1, format = "dd/mm/yyyy")})
    }
  })
  
  #### CHC Number of Assessment Dates ####
  
  chcNAssessments <- reactive({
    input$chcNAdditionalTests + 1
  })
  
  #### CHC Test Inputs ####
  
  output$chcTests <- renderUI({
    lapply(1:chcNAssessments(), function(i){
      if (i == 1) {
        label <- format(input$chcAxDate,"%Y")
      } else {
        label <- format(input[[paste("chcAssessmentYear",i,sep="")]], "%Y")
      }
      selectizeInput(paste("chcTests",i,sep=""),paste("Tests from ", label, sep = ""), width = "90%", choices = unique(rbind(composites,subtests)$test), multiple = TRUE, options = list(placeholder = "Test"))
    })
  })
  
  #### CHC Check Test Inputs ####
  
  chcTestsCheck <- reactive({
    check <- lapply(1:chcNAssessments(), function(i){
      chcTestsid <- paste("chcTests",i,sep="")
      if (is.null(input[[chcTestsid]]) || input[[chcTestsid]] == "") {FALSE} else {TRUE} 
    })
    if (length(check) > 0) {
      do.call(rbind,check)
      all(check)
    } else {
      check
    }
  })
  
  #### CHC Composite Selection ####
  
  output$chcComposites <- renderUI({
    if (chcTestsCheck()) {
      lapply(1:chcNAssessments(), function(i){
        if (i == 1) {
          label <- format(input$chcAxDate,"%Y")
        } else {
          label <-  format(input[[paste("chcAssessmentYear",i,sep="")]], "%Y")
        }
        chcTestsid <- paste("chcTests",i,sep="")
        availablecomposites <- subset(composites, test %in% input[[chcTestsid]])
        selectizeInput(paste("chcComposites",i,sep=""),paste("Composites from ", label, sep = ""), width = "90%", choices = availablecomposites$name, multiple = TRUE, options = list(placeholder = "Composite"))
      })
    }
  })
  
  #### CHC Subtest Selection ####
  
  output$chcSubtests <- renderUI({
    if (chcTestsCheck()) {
      lapply(1:chcNAssessments(), function(i){
        if (i == 1) {
          label <- format(input$chcAxDate,"%Y")
        } else {
          label <-  format(input[[paste("chcAssessmentYear",i,sep="")]], "%Y")
        }
        chcTestsid <- paste("chcTests",i,sep="")
        availablesubtests <- subset(subtests, test %in% input[[chcTestsid]])
        selectizeInput(paste("chcSubtests",i,sep=""),paste("Subtests from ", label, sep = ""), width = "90%", choices = availablesubtests$name, multiple = TRUE, options = list(placeholder = "Subtest"))
      })
    }
  })
  
  #### CHC Test Options ####
  
  chcSortOptions <- reactive({
    if (chcNAssessments() > 1) {
      c("Default", "Alphabetical", "Type", "CHC", "Value", "Values Reversed", "Year")
    } else {
      c("Default", "Alphabetical", "Type", "CHC", "Value", "Values Reversed")
    }
  })
  
  chcColourSelectionOptions <- reactive({
    if (chcNAssessments() > 1) {
      c("Type","Year & Type","Year")
    } else {
      c("Type")
    }
  })
  
  chcYearLabelOptions <- reactive({
    if (chcNAssessments() > 1) {
      c("Yes")
    } else {
      c("No", "Yes")
    }
  })
  
  output$chcPlotOptions <- renderUI({
    if (chcTestsCheck()) {
      tagList(
        radioButtons("chcConfidence", "Confidence Interval %", choices = ci$ci, inline = TRUE, selected = "95"),
        radioButtons("chcPlotType", "Plot Type", choices = c("Bar", "Line"), inline = TRUE),
        radioButtons("chcDataLabels", "Score Labels", choices = c("None", "Scores", "Scores + CI"), inline = TRUE),
        radioButtons("chcYearLabel", "Append Year of Assessment to Scale Labels", choices = chcYearLabelOptions(), inline = TRUE),
        #radioButtons("chcNorm", "Normal Curve", choices = c("No", "Yes"), inline = TRUE),
        radioButtons("chcOrganise", "Sort Graph", choices = chcSortOptions(), inline = TRUE),
        radioButtons("chcLabels", "CHC Labels", choices = c("No","Abbreviated","Full"), inline = TRUE),
        radioButtons("chcColourSetup", "Colour Scores", choices = chcColourSelectionOptions(), selected = "Type", inline = TRUE),
        sliderInput("chcPlotHeight", "Plot Height", min = 1, max = 5, value = 1, step = 1)
      )
    }
  })
  
  #### CHC Number of Colours ####
  
  chcNColours <- reactive({
    if (input$chcColourSetup == "Type") {
      2
    } else if (input$chcColourSetup == "Year & Type") {
      chcNAssessments()*2
    } else if (input$chcColourSetup == "Year") {
      chcNAssessments()
    }
  })
  
  #### CHC Colour Inputs ####
  
  output$chcColourOptions <- renderUI({
    if (chcTestsCheck()) {
      req(input$chcColourSetup)
      lapply(1:chcNColours(), function(i) {
        colourInput(paste("chcColour", i, sep = ""), paste("Colour", i, sep = " "), value = paste("rgb(",paste(sample(0:255, size = 3, replace = TRUE), collapse = ","),")", sep = ""))
      })
    }
  })
  
  # put colours into a vector
  chcColours <- reactive({
    chcColours <- lapply(1:chcNColours(), function(i) {
      colourinputid <- paste("chcColour",i,sep="")
      input[[colourinputid]]
    })
    unlist(chcColours, recursive = FALSE, use.names = TRUE)
  })
  
  #### CHC Plot Button ####
  
  output$executeCHCPlot <- renderUI({
    if (chcTestsCheck()) {
      actionButton("doCHCPlot", "Visualise")
    }
  })
  
  output$executeCHCTable <- renderUI({
    if(chcTestsCheck()) {
      actionButton("doCHCTable")
    }
  })
  

  
  #### CHC Composite and Subtest Inputs ####
  
  presentinputs <- function(score) {numericInput(score,score,value=100, min = 40, max = 160)}
  
  output$chcCompositeScores <- renderUI({
    if (chcTestsCheck()) {
      lapply(1:chcNAssessments(), function(i) {
        if (i == 1) {
          label <- format(input$chcAxDate,"%Y")
        } else {
          label <-  format(input[[paste("chcAssessmentYear",i,sep="")]], "%Y")
        }
        compositeinputid <- paste("chcComposites",i,sep="")
        if (is.null(input[[compositeinputid]])) {
          return()
        } else {
          lapply(1:length(input[[compositeinputid]]),function(z){presentinputs(paste(label,input[[compositeinputid]][z], sep = " "))})
        }
      })
    }
  })
  
  output$chcSubtestScores <- renderUI({
    if (chcTestsCheck()) {
      lapply(1:chcNAssessments(), function(i) {
        if (i == 1) {
          label <- format(input$chcAxDate,"%Y")
        } else {
          label <-  format(input[[paste("chcAssessmentYear",i,sep="")]], "%Y")
        }
        subtestinputid <- paste("chcSubtests",i,sep="")
        if (is.null(input[[subtestinputid]])) {
          return()
        } else {
          lapply(1:length(input[[subtestinputid]]),function(z){presentinputs(paste(label,input[[subtestinputid]][z], sep = " "))})
        }
      })
    }
  })
  
  #### CHC Composite Data ####
  
  chcCompositeData <- eventReactive(input$doCHCPlot, {
    cvalues <- lapply(1:chcNAssessments(), function (i) {
      if (i == 1)  {
        label <- format(input$chcAxDate,"%Y")
      } else {
        label <-  format(input[[paste("chcAssessmentYear",i,sep="")]], "%Y")
      }
      compositeinputid <- paste("chcComposites",i,sep="")
      if (!is.null(input[[compositeinputid]])) {
        cvalues <- lapply(input[[compositeinputid]],function(z){
          temp <- paste(label, z, sep = " ")
          data.frame(name = as.character(temp), value = input[[temp]], stringsAsFactors = FALSE)
        })
        cvalues <- do.call(rbind,cvalues)
      }
    })
    cvalues <- do.call(rbind,cvalues)
    cvalues <- separate(data = cvalues, col = "name", into = c("year","name"), sep = " ", extra = "merge")
    chcCompositeData <- subset(composites, name %in% unique(cvalues$name))
    if (!is.null(cvalues)) {
      chcCompositeData <- merge(chcCompositeData,cvalues)
    }
    return(chcCompositeData)
  })
  
  #### CHC Subtest Data ####
  
  chcSubtestsData <- eventReactive(input$doCHCPlot, {
    svalues <- lapply(1:chcNAssessments(), function (i) {
      if (i == 1)  {
        label <- format(input$chcAxDate,"%Y")
      } else {
        label <-  format(input[[paste("chcAssessmentYear",i,sep="")]], "%Y")
      }
      subtestinputid <- paste("chcSubtests",i,sep="")
      if (!is.null(input[[subtestinputid]])) {
        svalues <- lapply(input[[subtestinputid]],function(z){
          temp <- paste(label, z, sep = " ")
          data.frame(name = as.character(temp), value = input[[temp]], stringsAsFactors = FALSE)
        })
        svalues <- do.call(rbind,svalues)
      } 
    })
    svalues <- do.call(rbind,svalues)
    svalues <- separate(data = svalues, col = "name", into = c("year","name"), sep = " ", extra = "merge")
    chcSubtestsData <- subset(subtests, name %in% unique(svalues$name))
    if (!is.null(svalues)) {
      chcSubtestsData <- merge(chcSubtestsData,svalues)
    }
    return(chcSubtestsData)
  })
  
  #### CHC Composites Check ####
  
  chcCompositesCheck <- reactive({
    check <- lapply(1:chcNAssessments(), function(i){
      compositeinputid <- paste("chcComposites",i,sep="")
      if (is.null(input[[compositeinputid]]) || input[[compositeinputid]] == "") {FALSE} else {TRUE} })
    if (length(check) > 0) {
      do.call(rbind,check)
      any(check)
    } else {
      check
    }
  })
  
  #### CHC Subtests Check ####
  
  chcSubtestsCheck <- reactive({
    check <- lapply(1:chcNAssessments(), function(i){
      subtestinputid <- paste("chcSubtests",i,sep="")
      if (is.null(input[[subtestinputid]]) || input[[subtestinputid]] == "") {FALSE} else {TRUE} })
    if (length(check) > 0) {
      do.call(rbind,check)
      any(check)
    } else {
      check
    }
  })
  
  #### CHC Data ####
  
  chcData <- eventReactive(input$doCHCPlot, {
    
    # Select the data
    if (chcSubtestsCheck() && chcCompositesCheck()) {
      chcData <- rbind(chcCompositeData(), chcSubtestsData())
    } else if (chcSubtestsCheck()) {
      chcData <- chcSubtestsData()
    } else if (chcCompositesCheck()) {
      chcData <- chcCompositeData()
    } else {
      return()
    }  
    
    # select the columns
    chcData <- select(chcData, year, test, scale, name, type, chc, chcfull, value, sem = paste("sem", chcAge(), sep = ""))
    
    # Append the year of the assessment
    if (input$chcYearLabel == "Yes") {
      chcData <- unite(chcData,"name",c("year","name"), sep = " ", remove = FALSE)
    } else {
      chcData <- chcData
    }
    
    # Append CHC labels
    if(input$chcLabels == "No") {
      chcData <- chcData
    } else if (input$chcLabels == "Abbreviated") {
      chcData$name <- str_c(chcData$name, " (", chcData$chc, ") ", sep = "")
    } else if (input$chcLabels == "Full") {
      chcData$name <- str_c(chcData$name, " (", chcData$chc, ": ", chcData$chcfull, ") ", sep = "")
    } else {
      chcData <- chcData
    }
    
    # Organise data based on selection
    if (input$chcOrganise == "Default") {
      return(chcData)
    } else if (input$chcOrganise == "Alphabetical") {
      chcData$name <- factor(chcData$name, levels = chcData$name[order(chcData$scale, decreasing = TRUE)])
      return(chcData)
    } else if (input$chcOrganise == "Type") {
      chcData$name <- factor(chcData$name, levels = chcData$name[order(chcData$type)])
      return(chcData)
      # } else if (input$organise == "tchc") {
      #   chcData$name <- factor(chcData$name, levels = chcData$name[order(chcData$type, chcData$chc)])
      #   return(chcData)
    } else if (input$chcOrganise == "CHC") {
      chcData$name <- factor(chcData$name, levels = chcData$name[order(chcData$chc, decreasing = TRUE)])
      return(chcData)
    } else if (input$chcOrganise == "Value") {
      chcData$name <- factor(chcData$name, levels = chcData$name[order(chcData$value)])
      return(chcData)
    } else if (input$chcOrganise == "Values Reversed") {
      chcData$name <- factor(chcData$name, levels = chcData$name[order(chcData$value, decreasing = TRUE)])
      return(chcData)
    } else if (input$chcOrganise == "Year") {
      chcData$name <- factor(chcData$name, levels = chcData$name[order(chcData$year)])
      return(chcData)
    } else {
      return(chcData)
    }
  })
  
  #### CHC Data Table for Debugging ####
  
  # chcClasses <- reactive({
  #   req(chcData())
  #   apply(chcData(), 2, class)
  # })
  # 
  # output$chcDataTable <- renderTable(chcData())
  # output$chcClasses <- renderText(chcClasses())
  
  #### CHC Plot ####
  
  # Hide plot area upon loading
  hide("chcPlot")
  hide("downloadCHCPlot")
  
  # Show the plot
  observeEvent(input$doCHCPlot, {
    
    # if No data do not plot
    if (is.null(chcData())) {return()}
    
    # show the plot area and download button
    show("chcPlot")
    show("downloadCHCPlot")
    
    # set values ready for plot
    average <- c(90,110)
    mean <- 100
    sd <- 15
    data <- chcData()
    cname <- clientName()
    z <- as.numeric(subset(ci, ci %in% input$chcConfidence)$z)
    type <- input$chcPlotType
    labels <- input$chcDataLabels
    norm <- input$chcNorm
    ptitle <- if(cname == "") {"Assessment Results"} else {paste(cname, "'s Assessment Results", sep="")}
    caption <- "Plot created with Psychology Test Visualisation by Jake Kraska"
    colours <- chcColours()
    
    # plot the main data
    if (coloursetup == "Type") {
      p <- ggplot(data, aes(x = name, y = value, ymin = value- (sem*z), ymax = value + (sem*z), fill = type)) +
        scale_fill_manual(values = colours, name = "Scales")
    } else if (coloursetup == "Year & Type") {
      p <- ggplot(data, aes(x = name, y = value, ymin = value - (sem*z), ymax = value + (sem*z), fill = interaction(year, type, sep = " "))) +
        scale_fill_manual(values = colours, name = "Scales")
    } else if (coloursetup == "Year") {
      p <- ggplot(data, aes(x = name, y = value, ymin = value - (sem*z), ymax = value + (sem*z), fill = year)) +
        scale_fill_manual(values = colours, name = "Scales")
    } else {
      return()
    }
    
    # Add annotations and ranges
    p <- p + coord_flip() +
      geom_rect(xmin = -Inf, xmax = Inf, ymin = average[1], ymax = average[2], alpha = 0.5, fill = "grey50") +
      geom_rect(xmin = -Inf, xmax = Inf, ymin = mean-sd, ymax = average[1], alpha = 0.5, fill = "grey70") +
      geom_rect(xmin = -Inf, xmax = Inf, ymin = average[2], ymax = mean+sd, alpha = 0.5, fill = "grey70") +
      geom_hline(yintercept = mean) +
      geom_hline(yintercept = average[1]) +
      geom_hline(yintercept = mean - sd) +
      geom_hline(yintercept = average) +
      geom_hline(yintercept = mean + sd) +
      labs(title = ptitle, x = "Scale", y = "Score", caption = caption) +
      #annotate("text", y = mean, x = length(chcData()$name)+.75, label = "Average") +
      theme(plot.title = element_text(size = 16, face = "bold"),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14, face = "bold"))
    
    # set the scale of the plot
    #p <- p + scale_x_discrete(expand = expand_scale(mult = c(.1, .15)))
    
    if (max(data$value) + max(data$sem * z) > 150 | min(data$value) - max(data$sem * z) < 50) {
      p <- p + scale_y_continuous(breaks = c(40,50,60,70,80,90,100,110,120,130,140,150,160), limits = c(40,160), expand = c(0,0))
    } else if (max(data$value) + max(data$sem * z) > 140 | min(data$value) - max(data$sem * z) < 60) {
      p <- p + scale_y_continuous(breaks = c(50,60,70,80,90,100,110,120,130,140,150), limits = c(50,150), expand = c(0,0))
    } else if (max(data$value) + max(data$sem * z) > 130 | min(data$value) - max(data$sem * z) < 70) {
      p <- p + scale_y_continuous(breaks = c(60,70,80,90,100,110,120,130,140), limits = c(60,140), expand = c(0,0))
    } else {
      p <- p + scale_y_continuous(breaks = c(70,80,90,100,110,120,130), limits = c(70,130), expand = c(0,0))
    }
    
    # Alter plot based on type
    if (type == "Bar") {
      p <- p + geom_crossbar()
    } else if (type == "Line") {
      p <- p + geom_point(aes(colour = type)) +
        geom_errorbar(aes(colour = type), width = .5) +
        scale_color_manual(values=colours, name = "Scales")
    } else {
      p <- p
    }
    
    # Add data labels to plot
    if (labels == "None") {
      p <- p
    } else if (labels == "Scores") {
      p <- p + geom_text(aes(label = value), hjust = -.5)
    } else if (labels == "Scores + CI") {
      p <- p + 
        geom_text(aes(label = value), hjust = -.5) +
        geom_text(aes(label = round(value-(sem*z), digits = 0), y = value-(sem*z)), hjust = 1.25) + 
        geom_text(aes(label = round(value+(sem*z), digits = 0), y = value+(sem*z)), hjust = -.25)
    } else {
      p <- p
    }
    
    # Add normal curve
    # if (norm == "No") {
    #   p <- p
    # } else if (norm == "Yes") {
    #   p + stat_function(data = data.frame(), fun = dnorm, n = 1000, args = list(mean = 100, sd = 15))
    # } else {
    #   p <- p
    # }
    
    # print the plot
    plots$chcPlot <- p
  })
  
  #### CHC Print Plot ####
  
  chcPlotHeight <- reactive({
    req(chcData())
    input$chcPlotHeight * 200
  })
  
  output$chcPlotUI <- renderUI({
    req(chcPlotHeight())
    plotOutput("chcPlot", width = "100%", height = chcPlotHeight())
  })
  
  output$chcPlot <- renderPlot({
    req(chcPlotHeight())
    plots$chcPlot
  })
  
  output$downloadCHCPlot <- downloadHandler(
    filename = function(){paste("chcPlot",'.png',sep='')},
    content = function(file) {
      ggsave(file, plot = plots$chcPlot, width = input$shiny_width/320*5, height = chcPlotHeight()/320*5, dpi = 320)
    })
  
  #### Conners Form Ages ####
  
  output$connersAxDates <- renderUI({
    req(input$connersForms)
    lapply(input$connersForms, function(i){
      sliderInput(paste(i,"ConnersAge", sep = ""), paste("Age at Completion of", i, sep = " "), value = 6, min = 6, max = 18, step = 1)
    })
  })
  
  #### Conners Scale Inputs ####
  
  output$connersInputs <- renderUI({
    req(input$connersForms)
    lapply(input$connersForms, function(i){
      data <- filter(conners, form == i)
      lapply(data$name, function(name){
        numericInput(paste(name, "connersInput", sep = ""), paste(name, "Score", sep = " "), value = 50, min = 0, max = 100)
      })
    })
  })
  
  #### Conners Options ####
  
  output$connersPlotOptions <- renderUI({
    req(input$connersForms)
    tagList(
      radioButtons("connersConfidence", "Confidence Interval %", choices = ci$ci, inline = TRUE, selected = "95"),
      radioButtons("connersPlotType", "Plot Type", choices = c("Bar", "Line"), inline = TRUE),
      radioButtons("connersDataLabels", "Score Labels", choices = c("None", "Scores", "Scores + CI"), inline = TRUE),
      radioButtons("connersOrganise", "Sort Graph", choices = c("Type","Alphabetical","Value"),selected = "Type", inline = TRUE)
    )
  })
  
  #### Conners Colours ####
  
  # Present Colour Inputs
  output$connersColourOptions <- renderUI({
    req(input$connersForms)
    lapply(1:length(input$connersForms), function(i) {
      colourInput(paste("connersColour",i,sep=""),paste("Colour ",i,sep=""),value = paste("rgb(",paste(sample(0:255, size = 3, replace = TRUE), collapse = ","),")", sep = ""))
    })
  })
  
  # Put colours into a vector
  connersColours <- reactive({
    req(input$connersForms)
    connersColours <- lapply(1:length(input$connersForms), function(i) {
      input[[paste("connersColour",i,sep="")]]
    })
    unlist(connersColours, recursive = FALSE, use.names = TRUE)
  })
  
  #### Conners Data ####
  
  connersData <- eventReactive(input$doConnersPlot, {
    req(input$connersForms)
    connersData <- lapply(input$connersForms, function(i) {
      dataSubset <- filter(conners, form == i)
      connersData <- lapply(dataSubset$name, function(z){
        scaleInputName <- paste(z, "connersInput", sep = "")
        rowref <- which(dataSubset$name == z)
        data.frame(value = input[[scaleInputName]],
                   sem = dataSubset[[paste("sem",input[[paste(i,"ConnersAge",sep = "")]], sep = "")]][rowref],
                   name = z,
                   stringsAsFactors = FALSE
        )
      })
      do.call(rbind,connersData)
    })
    connersData <- do.call(rbind,connersData)
    connersScaleData <- subset(conners, form %in% input$connersForms)
    connersData <- merge(x = connersData, y = connersScaleData)
    connersData <- select(connersData, name, value, sem, form, scale)
    
    # Organise data based on selection
    if (input$connersOrganise == "Type") {
      return(connersData)
    } else if (input$connersOrganise == "Alphabetical") {
      connersData$name <- factor(connersData$name, levels = connersData$name[order(connersData$scale, decreasing = TRUE)])
      return(connersData)
    } else if (input$connersOrganise == "Value") {
      connersData$name <- factor(connersData$name, levels = connersData$name[order(connersData$value, decreasing = TRUE)])
      return(connersData)
    } else {
      return(connersData)
    }
  })
  
  #### Conners Data Table for Debugging ####
  
  # connersClasses <- reactive({
  #   req(connersData())
  #   apply(connersData(), 2, class)
  # })
  # 
  # output$connersDataTable <- renderTable(connersData())
  # output$connersClasses <- renderText(connersClasses())
  
  #### Conners Plot Button ####
  
  output$executeConnersPlot <- renderUI({
    req(input$connersForms)
    actionButton('doConnersPlot', "Visualise")
  })
  
  #### Conners Plot ####
  
  # hide plot and download button upon loading
  hide("connersPlot")
  hide("downloadConnersPlot")
  
  # Show the plot
  observeEvent(input$doConnersPlot, {
    
    # if No data do not plot
    req(connersData())
    
    # show the plot area and download button
    show("connersPlot")
    show("downloadConnersPlot")
    
    # set values ready for plot
    average <- c(40,60)
    mean <- 50
    sd <- 10
    data <- connersData()
    cname <- clientName()
    z <- as.numeric(subset(ci, ci %in% input$connersConfidence)$z)
    type <- input$connersPlotType
    labels <- input$connersDataLabels
    ptitle <- if(cname == "") {"Assessment Results"} else {paste(cname, "'s Assessment Results", sep="")}
    caption <- "Plot created with Psychology Test Visualisation by Jake Kraska"
    colours <- connersColours()
    
    # Plot main data
    p <- ggplot(data, aes(x = name, y = value, ymin = value - (sem * z), ymax = value + (sem * z), fill = form)) +
      scale_fill_manual(values = colours, name = "Form")
      
    # Add annotations and ranges
    p <- p + coord_flip() +
      geom_rect(xmin = -Inf, xmax = Inf, ymin = average[1], ymax = average[2], alpha = 0.5, fill = "grey50") +
      geom_rect(xmin = -Inf, xmax = Inf, ymin = mean - sd, ymax = average[1], alpha = 0.5, fill = "grey70") +
      geom_rect(xmin = -Inf, xmax = Inf, ymin = average[2], ymax = mean + sd, alpha = 0.5, fill = "grey70") +
      geom_hline(yintercept = mean) +
      geom_hline(yintercept = average[1]) +
      geom_hline(yintercept = mean - sd) +
      geom_hline(yintercept = average) +
      geom_hline(yintercept = mean + sd) +
      labs(title = ptitle, x = "Scale", y = "Score", caption = caption) +
      #annotate("text", y = mean, x = length(connersData()$name)+.75, label = "Average") +
      theme(plot.title = element_text(size = 16, face = "bold"),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14, face = "bold")) +
      scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100), limits = c(0,100), expand = c(0,0))
    
    # set the scale of the plot
    #p <- p + scale_x_discrete(expand = expand_scale(mult = c(.1, .15)))
    
    # Alter plot based on type
    if (type == "Bar") {
      p <- p + geom_crossbar()
    } else if (type == "Line") {
      p <- p + geom_point(aes(colour = form)) +
        geom_errorbar(aes(colour = form), width = .5) +
        scale_color_manual(values=colours, name = "Form")
    } else {
      p <- p
    }
    
    # Add data labels to plot
    if (labels == "None") {
      p <- p
    } else if (labels == "Scores") {
      p <- p + geom_text(aes(label = value), hjust = -.5)
    } else if (labels == "Scores + CI") {
      p <- p + 
        geom_text(aes(label = value), hjust = -.5) +
        geom_text(aes(label = round(value - (sem * z), digits = 0), y = value - (sem * z)), hjust = 1.25) + 
        geom_text(aes(label = round(value + (sem * z), digits = 0), y = value + (sem * z)), hjust = -.25)
    } else {
      p <- p
    }
    
    # print the plot
    plots$connersPlot <- p
  })
  
  #### conners Print Plot ####
  
  output$connersPlot <- renderPlot({ plots$connersPlot })
  
  output$downloadconnersPlot <- downloadHandler(
    filename = function(){paste("connersPlot",'.png',sep='')},
    content = function(file) {
      ggsave(file, plot = plots$connersPlot, width = 50, height = 50, units = "cm", dpi = 320)
    })
  
}

#### Run ####
shinyApp(ui = ui, server = server)