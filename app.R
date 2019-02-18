#### Load Libraries ####

library(shiny)
library(bsplus)
library(htmltools)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(shinyjs)
library(colourpicker)
library(shinythemes)

#### Functions ####

source("agecalculator.R")
source("recodechc.R")
source("convertscore.R")
source("presentinputs.R")
source("convertsem.R")
source("plotoptions.R")

#### Load Data ####

# Load composites and prepare for Shiny
composites <- read_csv("composites.csv")
composites[is.na(composites)] <- 0
composites$name <- paste(composites$test, composites$composite, sep = " ")
names(composites)[names(composites) == 'composite'] <- 'scale'
composites$chcfull <- RecodeCHC(composites$chc)

# Load subtests and prepare for Shiny
subtests <- read_csv("subtests.csv")
subtests[is.na(subtests)] <- 0
subtests$name <- paste(subtests$test, subtests$subtest, sep = " ")
names(subtests)[names(subtests) == 'subtest'] <- 'scale'
subtests$chcfull <- RecodeCHC(subtests$chc)

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
  use_bs_tooltip(),
  
  tags$script("$(document).on('shiny:connected', function(event) {var myWidth = $(window).width();Shiny.onInputChange('shiny_width',myWidth)});"),
  
  #### Title and Page Details ####
  
  title = "Psychology Test Visualisation", selected = "Home", fluid = TRUE, footer = includeHTML("footer.html"), 
  theme = shinytheme("flatly"),
  
  #### Home Page UI ####
  
  tabPanel("Home", 
           fluidRow(column(width = 4, offset = 1,
                           #themeSelector(),
                           includeHTML("introduction.html")),
                    column(width = 4, offset = 1,
                           tags$h3("Step 1: Assessee Details"),
                           tags$p("Provide details of the assessee below."),
                           textInput("clientName", "First Name", placeholder = "John") %>%
                             shinyInput_label_embed(
                               icon("question") %>%
                                 bs_embed_tooltip(
                                   title = "Input the client's name. This is optional. This is not stored on the server. If
                                   you provide a name it will be included in the visualisation's header. If you do not provide a name,
                                   then the plot will be titled 'Assessment Results'", placement = "right")
                               ),
                           dateInput('assesseeDOB', "Date of Birth", max = Sys.Date() + 1, format = "dd/mm/yyyy", value = "2013-01-01") %>% 
                             bs_embed_tooltip(title = "Input the assessee's DOB. This allows the application to calculate
                                              the appropriate confidence intervals for each visualisation.", placement = "right"),
                           radioButtons('assesseeGender', "Gender", choices = c("Female", "Male"), inline = TRUE) %>% 
                             bs_embed_tooltip(title = "Input the assessee's gender This allows the application to calculate
                                              the appropriate confidence intervals for the Conners visualisation.", placement = "right"),
                           tags$h3("Step 2: Select Visualisation Tools"),
                           tags$p("Use the navigation bar at the top of this page to select whether you want to visualise:"),
                           tags$ul(
                             tags$li("CHC data from cognitive ability and academic achievemnet tests"),
                             tags$li("Conners-3 data")
                           )
                    ))),
  
  #### CHC UI ####
  
  tabPanel("CHC",
           fluidRow(column(width = 3,
                           tags$h3("Step 3: Main Assessment"),
                           dateInput("chcAxDate", "Date of Assessment", format = "dd/mm/yyyy") %>% 
                             bs_embed_tooltip(title = "Input the date of the main assessment you want to visualise. This date
                                              may represent multiple tests. For example, you may have administered a WISC-V 
                                              and WJ IV.", placement = "right"),
                           tags$h3("Step 4: Additional Assessments"),
                           sliderInput("chcNAdditionalAx", "Number of Additional Assessments",value = 0, min = 0, max = 5, step = 1) %>%
                             shinyInput_label_embed(
                               icon("question") %>%
                                 bs_embed_tooltip(
                                   title = "Input the number of additional assessments if applicable. For example, if
                                   the client was assessed two years ago and you have a report that includes a WISC-V 
                                   and a WIAT-III, select 1 additional assessment.", placement = "right")),
                           uiOutput("chcAdditionalAxInputs"),
                           uiOutput("chcTests")),
                    column(width = 3,
                           uiOutput("chcComposites"),
                           uiOutput("chcCompositeScores")),
                    column(width = 3,
                           uiOutput("chcSubtests"),
                           uiOutput("chcSubtestScores")),
                    column(width = 3,
                           uiOutput("chcPlotOptions"),
                           uiOutput("chcColourOptions"),
                           uiOutput("executeCHCPlot"))),
           fluidRow(column(width = 10, offset = 1,
                           hr(),
                           uiOutput("chcPlotUI"),
                           downloadButton("downloadCHCPlot")
           ))),
  
  #### Conners-3 UI ####
  
  tabPanel("Conners-3",
           fluidRow(column(width = 3, offset = 1,
                           tags$h3("Step 3: Select Forms Administered"),
                           selectizeInput("connersForms", "Forms Administered", choices = unique(conners$form), multiple = TRUE, options = list(placeholder = "Form")) %>%
                             shinyInput_label_embed(
                               icon("question") %>%
                                 bs_embed_tooltip(
                                   title = "Select the forms of the Conners that were administered.", placement = "right")),
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
           ))),
  tabPanel("Changes",
           fluidRow(column(width = 5, offset = 1,
                           includeHTML("changes.html")),
                    column(width = 5, offset = 1,
                           includeHTML("plannedchanges.html")
           ))))

#### Server ####

server <- function(input, output, session) {
  
  #### Client Name ####
  
  clientName <- reactive({
    if (is.null(input$clientName)) {return()}
    input$clientName
  })
  
  #### Plots Reactive Values ####
  
  plots <- reactiveValues()
  
  #### CHC Number of Assessment Dates ####
  
  chcNAssessments <- reactive({
    input$chcNAdditionalAx + 1
  })
  
  #### CHC Main Assessment Age ####
  
  output$chcAxAge <- reactive({
    if (clientName() == "") {
      paste("Assessee was ", chcAges()[1], " years old.")
    } else {
      paste(clientName(), " was ", chcAges()[1], " years old.") 
    }
  })
  
  #### CHC Additional Date Inputs ####
  
  chcRandomDates <- reactive({
    min <- format(input$assesseeDOB,"%Y")
    min <- as.numeric(min) + 1
    max <- format(Sys.Date(), "%Y")
    max <- as.numeric(max) - 1
    sample(min:max, input$chcNAdditionalAx)
  })
  
  output$chcAdditionalAxInputs <- renderUI({
    hide("chcPlot")
    hide("downloadCHCPlot")
    if (chcNAssessments() > 1) {
      lapply(1:input$chcNAdditionalAx, function(i) {
        dateInput(paste0("chcAssessmentDate",i+1),"Assessment Date",value = paste0(chcRandomDates()[i],"-01-01"), max = Sys.Date() + 1, format = "dd/mm/yyyy") %>% 
          bs_embed_tooltip(title = "Input the date of the additional assesssment you would like to visualise. As with the main assessment date, 
                           this date may represent multiple tests within an assessment.", placement = "right")
      })
    }
  })
  
  #### CHC Calculate Age ####
  
  chcAges <- reactive({
    req(input$assesseeDOB,input$chcAxDate,1,input$chcNAdditionalAx)
    if (chcNAssessments() == 1) {
      floor(age_calc(input$assesseeDOB, input$chcAxDate))
    } else if (chcNAssessments() > 1) {
      firstage <- floor(age_calc(input$assesseeDOB, input$chcAxDate))
      extraages <- sapply(1:input$chcNAdditionalAx, function(i) {
        floor(age_calc(input$assesseeDOB, input[[paste0("chcAssessmentDate",i+1)]]))
      })
      c(firstage,extraages)
    } else {
      stop("Number of assessments is not a positive integer.")
    }
  })
  
  
  #### CHC Test Inputs ####
  
  output$chcTests <- renderUI({
    req(chcNAssessments(),chcAges())
    tagList(
      tags$h3("Step 5: Select Tests"),
      tags$p("Select the tests that you administered as part of this assessment.
             If you indicated that there were additional assessments, input boxes 
             will be avaiable for those dates."),
      lapply(1:chcNAssessments(), function(i){
        if (i == 1) {
          label <- format(input$chcAxDate,"%Y")
        } else {
          label <- format(input[[paste0("chcAssessmentDate",i)]], "%Y")
        }
        choices <- rbind(composites,subtests)
        choices <- filter(choices, chcAges()[i] >= minage, chcAges()[i] <= maxage)
        selectizeInput(paste0("chcTests",i),paste0("Tests from ", label), choices = unique(choices$test), multiple = TRUE, options = list(placeholder = "Test")) %>%
          shinyInput_label_embed(
            icon("question") %>%
              bs_embed_tooltip(
                title = paste0("Select the tests administered as part of the assessment conducted in ", label), placement = "right"))
      })
    )
  })
  
  #### CHC Check Test Inputs ####
  
  # Check to see if all assessment fields have at least one test
  chcTestsCheck <- reactive({
    check <- sapply(1:chcNAssessments(), function(i){
      chcTestsID <- paste0("chcTests",i)
      if (is.null(input[[chcTestsID]]) || input[[chcTestsID]] == "") {FALSE} else {TRUE} 
    })
    all(check)
  })
  
  #### CHC Composite Selection ####
  
  output$chcComposites <- renderUI({
    if (chcTestsCheck()) {
      tagList(
        tags$h3("Step 6: Select Composites to Visualise"),
        lapply(1:chcNAssessments(), function(i){
          if (i == 1) {
            label <- format(input$chcAxDate,"%Y")
          } else {
            label <-  format(input[[paste0("chcAssessmentDate",i)]], "%Y")
          }
          chcTestsID <- paste0("chcTests",i)
          availablecomposites <- subset(composites, test %in% input[[chcTestsID]])
          selectizeInput(paste0("chcComposites",i),paste0("Composites from ", label), width = "100%",  choices = availablecomposites$name, multiple = TRUE, options = list(placeholder = "Composite")) %>%
            shinyInput_label_embed(
              icon("question") %>%
                bs_embed_tooltip(
                  title = paste0("Select the composites administered as part of the testing conducted in ", label), placement = "right"))
        })
      )
    }
  })
  
  #### CHC Subtest Selection ####
  
  output$chcSubtests <- renderUI({
    if (chcTestsCheck()) {
      tagList(
        tags$h3("Step 7: Select Subtests to Visualise"),
        lapply(1:chcNAssessments(), function(i){
          if (i == 1) {
            label <- format(input$chcAxDate,"%Y")
          } else {
            label <-  format(input[[paste0("chcAssessmentDate",i)]], "%Y")
          }
          chcTestsID <- paste0("chcTests",i)
          availablesubtests <- subset(subtests, test %in% input[[chcTestsID]])
          selectizeInput(paste0("chcSubtests",i),paste0("Subtests from ", label), width = "100%", choices = availablesubtests$name, multiple = TRUE, options = list(placeholder = "Subtest"))%>%
            shinyInput_label_embed(
              icon("question") %>%
                bs_embed_tooltip(
                  title = paste0("Select the subtests administered as part of the testing conducted in ", label), placement = "right"))
        })
      )
    }
  })
  
  #### CHC Test Options ####
  
  # If more than 1 assessment then add the sort by year option
  chcSortOptions <- reactive({
    if (chcNAssessments() > 1) {
      c("Default", "Alphabetical", "Type", "CHC", "Value", "Values Reversed", "Year")
    } else {
      c("Default", "Alphabetical", "Type", "CHC", "Value", "Values Reversed")
    }
  })
  
  # If more than 1 assessment then add ability to break up categories by colours
  chcColourSelectionOptions <- reactive({
    if (chcNAssessments() > 1) {
      c("Year", "Year & Type", "Type")
    } else {
      c("Type")
    }
  })
  
  # If 1 assessment date, then allow user to not append year to labels
  chcYearLabelOptions <- reactive({
    if (chcNAssessments() > 1) {
      c("No", "Yes")
    } else {
      c("No")
    }
  })
  
  # Present the plot customisation options
  output$chcPlotOptions <- renderUI({
    if (chcTestsCheck()) {
      PlotOptions(tool = "chc", 
                  sortoptions = chcSortOptions(), 
                  colourselection = chcColourSelectionOptions(), 
                  yearlabels = chcYearLabelOptions(),
                  confidence = TRUE,
                  bartype = TRUE,
                  datalabels = TRUE,
                  yearlabel = TRUE,
                  normalcurve = FALSE,
                  presentationtype = TRUE,
                  organise = TRUE,
                  chclabels = TRUE,
                  coloursetup = TRUE,
                  plotheight = TRUE)
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
      actionButton("doCHCPlot", "Step 9: Visualise")
    }
  })
  
  #### CHC Composite Inputs ####
  
  output$chcCompositeScores <- renderUI({
    if (chcTestsCheck()) {
      lapply(1:chcNAssessments(), function(i) {
        if (i == 1) {
          label <- format(input$chcAxDate,"%Y")
        } else {
          label <-  format(input[[paste0("chcAssessmentDate",i)]], "%Y")
        }
        compositeinputid <- paste0("chcComposites",i)
        if (is.null(input[[compositeinputid]])) {
          return()
        } else {
          lapply(1:length(input[[compositeinputid]]), function(z) {
            type <- paste(select(filter(composites, name == input[[compositeinputid]][z]), scoretype))
            PresentInputs(type, paste(label, input[[compositeinputid]][z], sep = " "))
          })
        }
      })
    }
  })
  
  #### CHC Subtest Inputs ####
  
  output$chcSubtestScores <- renderUI({
    if (chcTestsCheck()) {
      lapply(1:chcNAssessments(), function(i) {
        if (i == 1) {
          label <- format(input$chcAxDate,"%Y")
        } else {
          label <-  format(input[[paste0("chcAssessmentDate",i)]], "%Y")
        }
        subtestinputid <- paste0("chcSubtests",i)
        if (is.null(input[[subtestinputid]])) {
          return()
        } else {
          lapply(1:length(input[[subtestinputid]]), function(z) {
            type <- paste(select(filter(subtests, name == input[[subtestinputid]][z]), scoretype))
            PresentInputs(type, paste(label, input[[subtestinputid]][z], sep = " "))
          })
        }
      })
    }
  })
  
  #### CHC Composite Data ####
  
  chcCompositeData <- eventReactive(input$doCHCPlot, {
    cvalues <- lapply(1:chcNAssessments(), function (i) {
      label <- ifelse(i==1, format(input$chcAxDate,"%Y"), format(input[[paste("chcAssessmentDate",i,sep="")]], "%Y"))
      compositeinputid <- paste("chcComposites",i,sep="")
      if (!is.null(input[[compositeinputid]])) {
        cvalues <- lapply(input[[compositeinputid]], function(z) {
          type <- paste(select(filter(composites, name == z), scoretype))
          sem <- paste(select(filter(composites, name == z), paste0("sem",chcAges()[i])))
          name <- paste(label, z, sep = " ")
          data.frame(name = name, 
                     value = ConvertScore(score = input[[name]], fromtype = type, totype = input$chcPresentationType), 
                     sem = ConvertSEM(sem = sem, fromtype = type, totype = input$chcPresentationType),
                     stringsAsFactors = FALSE)
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
      label <- ifelse(i==1, format(input$chcAxDate,"%Y"), format(input[[paste("chcAssessmentDate",i,sep="")]], "%Y"))
      subtestinputid <- paste("chcSubtests",i,sep="")
      if (!is.null(input[[subtestinputid]])) {
        svalues <- lapply(input[[subtestinputid]],function(z){
          type <- paste(select(filter(subtests, name == z), scoretype))
          sem <- paste(select(filter(subtests, name == z), paste0("sem",chcAges()[i])))
          name <- paste(label, z, sep = " ")
          data.frame(name = name, 
                     value = ConvertScore(score = input[[name]], fromtype = type, totype = input$chcPresentationType),
                     sem = ConvertSEM(sem, fromtype = type, totype = input$chcPresentationType),
                     stringsAsFactors = FALSE)
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
    check <- sapply(1:chcNAssessments(), function(i){
      compositeinputid <- paste("chcComposites",i,sep="")
      if (is.null(input[[compositeinputid]]) || input[[compositeinputid]] == "") {FALSE} else {TRUE} 
    })
    any(check)
  })
  
  #### CHC Subtests Check ####
  
  chcSubtestsCheck <- reactive({
    check <- sapply(1:chcNAssessments(), function(i){
      subtestinputid <- paste("chcSubtests",i,sep="")
      if (is.null(input[[subtestinputid]]) || input[[subtestinputid]] == "") {FALSE} else {TRUE} 
    })
    any(check)
    
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
    chcData <- select(chcData, year, test, scale, name, type, chc, chcfull, value, sem)
    
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
    if (input$chcPresentationType == "z") {
      average <- c(-1,1)
      mean <- 0
      sd <- 1
      breaks <- 1
    } else if (input$chcPresentationType == "scaled") {
      average <- c(8,12)
      mean <- 10
      sd <- 3
      breaks <- 2
    } else if (input$chcPresentationType == "standard") {
      average <- c(90,110)
      mean <- 100
      sd <- 15
      breaks <- 10
    }
    
    data <- chcData()
    cname <- clientName()
    z <- as.numeric(subset(ci, ci %in% input$chcConfidence)$z)
    type <- input$chcPlotType
    labels <- input$chcDataLabels
    norm <- input$chcNorm
    ptitle <- if(cname == "") {"Assessment Results"} else {paste(cname, "'s Assessment Results", sep="")}
    caption <- "Plot created with Psychology Test Visualisation by Jake Kraska"
    colours <- chcColours()
    coloursetup <- input$chcColourSetup
    chcErrorWidth <- input$chcErrorWidth
    chcLineThickness <- input$chcLineThickness
    chcPointSize <- input$chcPointSize
    chcPointShape <- input$chcPointShape
    
    # Set up the plot scale
    maxDataValue <- max(data$value) + max(data$sem * z)
    minDataValue <- min(data$value) - max(data$sem * z)
    
    if (maxDataValue > mean + (4*sd) | minDataValue < mean - (4*sd)) {
      max <- 5 * sd
    } else if (maxDataValue > mean+(3*sd) | minDataValue < mean-(3*sd)) {
      max <- 4 * sd
    } else if (maxDataValue > mean+(2*sd) | minDataValue < mean-(2*sd)) {
      max <- 3 * sd
    } else {
      max <- 2 * sd
    }
    
    limits <- c(mean - max, mean + max)
    nBreaks <- max/breaks
    breaksLow <- sapply(nBreaks:1, function(i) {
      mean - (i*breaks)
    })
    breaksMiddle <- mean
    breaksHigh <- sapply(1:nBreaks, function(i) {
      mean + (i*breaks)
    })
    breaks <- c(breaksLow,breaksMiddle,breaksHigh)
    
    # set up colours
    if (coloursetup == "Type") {
      Categories <- data$type
    } else if (coloursetup == "Year & Type") {
      Categories <- interaction(data$year, data$type, sep = " ")
    } else if (coloursetup == "Year") {
      Categories <- data$year
    } else {
      return()
    }
    
    # plot the main data
    p <- ggplot(data, aes(x = name, y = value, ymin = value - (sem*z), ymax = value + (sem*z)))
    
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
    
    # Place the breaks and limits on the plot
    p <- p + scale_y_continuous(breaks = breaks, limits = limits, expand = c(0,0))
    
    # Alter plot based on type
    if (type == "Bar") {
      p <- p + geom_crossbar(aes(fill = Categories)) +
        scale_fill_manual(values = colours, name = "Categories")
    } else if (type == "Line") {
      p <- p + geom_point(aes(colour = Categories), size = chcPointSize, shape = chcPointShape) +
        geom_errorbar(aes(colour = Categories), size = chcLineThickness, width = chcErrorWidth) +
        scale_color_manual(values = colours, name = "Categories")
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
    tagList(
      tags$h3("Step 4: Select Age of Client"),
      lapply(input$connersForms, function(i){
        sliderInput(paste(i,"ConnersAge", sep = ""), paste("Age at Completion of", i, sep = " "), value = 6, min = 6, max = 18, step = 1)
      })
    )
  })
  
  #### Conners Scale Inputs ####
  
  output$connersInputs <- renderUI({
    req(input$connersForms)
    tagList(
      tags$h3("Step 5: Input Scores"),
      lapply(input$connersForms, function(i){
        data <- filter(conners, form == i)
        lapply(data$name, function(name){
          numericInput(paste(name, "connersInput", sep = ""), paste(name, "Score", sep = " "), value = 50, min = 0, max = 100)
        })
      })
    )
  })
  
  #### Conners Options ####
  
  output$connersPlotOptions <- renderUI({
    req(input$connersForms)
    PlotOptions(tool = "conners", 
                sortoptions = c("Type", "Alphabetical", "Value"), 
                confidence = TRUE,
                bartype = TRUE, 
                datalabels = TRUE, 
                organise = TRUE,
                plotheight = TRUE)
  })
  
  #### Conners Colours ####
  
  # Present Colour Inputs
  output$connersColourOptions <- renderUI({
    req(input$connersForms)
    lapply(1:length(input$connersForms), function(i) {
      colourInput(paste0("connersColour",i),paste0("Colour ",i),value = paste0("rgb(",paste(sample(0:255, size = 3, replace = TRUE), collapse = ","),")"))
    })
  })
  
  # Put colours into a vector
  connersColours <- reactive({
    req(input$connersForms)
    connersColours <- lapply(1:length(input$connersForms), function(i) {
      input[[paste0("connersColour",i)]]
    })
    unlist(connersColours, recursive = FALSE, use.names = TRUE)
  })
  
  #### Conners Data ####
  
  connersData <- eventReactive(input$doConnersPlot, {
    gender <- if (input$assesseeGender == "Male") {"m"} else if (input$assesseeGender == "Female") {"f"} else {stop("Error in Gender.")}
    req(input$connersForms)
    connersData <- lapply(input$connersForms, function(form) {
      dataSubset <- filter(conners, form == form)
      connersData <- lapply(dataSubset$name, function(name){
        scaleInputName <- paste0(name, "connersInput")
        rowref <- which(dataSubset$name == name)
        data.frame(value = input[[scaleInputName]],
                   sem = dataSubset[[paste0(gender,"sem",input[[paste(form,"ConnersAge",sep = "")]])]][rowref],
                   name = name,
                   stringsAsFactors = FALSE
        )
      })
      do.call(rbind,connersData)
    })
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
  
  #### Conners Plot Button ####
  
  output$executeConnersPlot <- renderUI({
    req(input$connersForms)
    actionButton('doConnersPlot', "Step 7: Visualise")
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
    connersErrorWidth <- input$connersErrorWidth
    connersLineThickness <- input$connersLineThickness
    connersPointSize <- input$connersPointSize
    connersPointShape <- input$connersPointShape
    
    # Plot main data
    p <- ggplot(data, aes(x = name, y = value, ymin = value - (sem * z), ymax = value + (sem * z)))
    
    
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
    
    # Alter plot based on type
    if (type == "Bar") {
      p <- p + geom_crossbar(aes(fill = form)) +
        scale_fill_manual(values = colours, name = "Form")
    } else if (type == "Line") {
      p <- p + geom_point(aes(colour = form), size = connersPointSize, shape = connersPointShape) +
        geom_errorbar(aes(colour = form), size = connersLineThickness, width = connersErrorWidth) +
        scale_color_manual(values = colours, name = "Form")
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
  
  #### Close Session on Browser Exit ####
  
  session$onSessionEnded(stopApp)
  
}

#### Run ####
shinyApp(ui = ui, server = server)