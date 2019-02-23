PlotOptions <- function(tool = NULL, 
                        sortoptions = c("Default", "Alphabetical"), 
                        colourselection = c("Type (ACH/COG)"),
                        yearlabels = c("Yes"),
                        ranges = c("No", "Average"),
                        confidence = FALSE,
                        bartype = FALSE,
                        datalabels = FALSE,
                        yearlabel = FALSE,
                        normalcurve = FALSE,
                        presentationtype = FALSE,
                        rangevis = FALSE,
                        organise = FALSE,
                        chclabels = FALSE,
                        coloursetup = FALSE,
                        plotheight = FALSE) {
  
  # Presents a variety of options for customising the plot
  #
  # Args:
  #   tool: string of the name of the tool you want options for. This string will be added to ids.
  #   sortoptions: a vector of strings labelling different ways to sort the plot, default is "Default" and "Alphabetical"
  #   colourselection: a vector of strings labelling different ways to colour the plot, default is "Type"
  #   yearlabels: a vector to determine if options should be provided for year labels, default is "No" and "Yes"
  #   ranges: a vector to determine if options should be provided for ranges, default is "No" and "Average"
  #   confidence: boolean to determine if confidnece interval options should be available, default is FALSE
  #   bartype: boolean to determine bar type options should be available, default is FALSE
  #   datalabels: boolean to determine if data label options should be available, default is FALSE
  #   yearlabel: boolean to determine if year label options should be available, default is FALSE
  #   normalcurve: boolean to determine if a normal curve option should be available, default is FALSE
  #   presentationtype: boolean to determine if plot and scores should be transformed (e.g. z, standard score, etc), default is FALSE
  #   rangvis: boolean to determine if plot should have range visualisaitons options, default is FALSE
  #   organise: boolean to determine if organisation options should be available, default is FALSE
  #   chclabels: boolean to determine if chclabel options should be available (suitable only for CHC data), default is false
  #   coloursetup: boolean to determine if colour options should be available, default is false
  #   plotheight: boolean to determine if height options should be available, default is false
  # Returns:
  #   Shiny input options for plots
  
  # Check for missing arguments
  if(missing(tool)) {stop("Tool argument not provided.")}
  
  # Check for incorrect arguments
  if(is.logical(confidence) == FALSE) {stop("Only logical argument accepted for confidence.")}
  if(is.logical(bartype) == FALSE) {stop("Only logical argument accepted for bartype")}
  if(is.logical(datalabels) == FALSE) {stop("Only logical argument accepted for datalabels")}
  if(is.logical(yearlabel) == FALSE) {stop("Only logical argument accepted for yearlabel")}
  if(is.logical(normalcurve) == FALSE) {stop("Only logical argument accepted for normalcurve")}
  if(is.logical(presentationtype) == FALSE) {stop("Only logical argument accepted for presentationtype")}
  if(is.logical(organise) == FALSE) {stop("Only logical argument accepted for organise")}
  if(is.logical(chclabels) == FALSE) {stop("Only logical argument accepted for chclabels")}
  if(is.logical(coloursetup) == FALSE) {stop("Only logical argument accepted for coloursetup")}
  if(is.logical(plotheight) == FALSE) {stop("Only logical argument accepted for plotheight")}
  
  # Determine which options to show
  option1 <- if(confidence == TRUE) {
    ci <- data.frame(ci = c(99.7,99.0,98.0,95.0,90.0,80.0,75.0,68.0), z = c(3,2.576,2.326,1.96,1.645,1.282,1.15,1))
    selectInput(paste0(tool,"Confidence"), "Confidence Interval %", choices = ci$ci, selected = "95", multiple = FALSE) %>% 
      shinyInput_label_embed(
        icon("question") %>%
          bs_embed_tooltip(title = "This option allows you to change the confidence interval of the bars. This relies on the 
                       appropriate normative data for the nationality you have selected.", placement = "left"))
  } else {
    NULL
  }
  
  option2 <- if(bartype == TRUE) {
    selectInput(paste0(tool,"PlotType"), "Plot Type", choices = c("Bar", "Line"), selected = "Bar", multiple = FALSE) %>%
      shinyInput_label_embed(
        icon("question") %>%
          bs_embed_tooltip(title = "Bar: Visualises confidence intervals as solid bars with the score
                       represented by a thick line in the middle. Line: Visualises confidence intervals
                       as lines with vertical ends and a symbol in the centre as a score.", placement = "left"))
  } else {
    NULL
  }
  
  option3 <- if(bartype == TRUE) {
    conditionalPanel("input.chcPlotType == 'Line'", 
                     sliderInput(paste0(tool,"ErrorWidth"), "Error Bar Width", max = 1, min = .1, step = .1, value = .5) %>%
                       shinyInput_label_embed(
                         icon("question") %>%
                           bs_embed_tooltip(title = "Changes the width of the line representing the minimum and maximum of the confidence interval.", placement = "left")),
                     sliderInput(paste0(tool,"LineThickness"), "Line Thickness", max = 3, min = .2, step = .2, value = 1) %>% 
                       shinyInput_label_embed(
                         icon("question") %>%
                           bs_embed_tooltip(title = "Changes the thickness of the line.", placement = "left")),
                     sliderInput(paste0(tool,"PointSize"), "Point Size", max = 4, min = .2, step = .2, value = 2 )%>% 
                       shinyInput_label_embed(
                         icon("question") %>%
                           bs_embed_tooltip(title = "Changes the size of the point that represents the score in the middle of the line.", placement = "left")),
                     sliderInput(paste0(tool,"PointShape"), "Point Shape", max = 25, min = 0, step = 1, value = 16) %>%
                       shinyInput_label_embed(
                         icon("question") %>%
                           bs_embed_tooltip(title = "Changes the symbol that represents the score. There are 25 symbols available.", placement = "left"))
    )
  } else {
    NULL
  }
  
  option4 <- if(bartype == TRUE) {
    selectInput(paste0(tool,"DataLabels"), "Score Labels", choices = c("None", "Scores", "Scores + CI"), selected = "None", multiple = FALSE) %>% 
      shinyInput_label_embed(
        icon("question") %>%
          bs_embed_tooltip(title = "None: No scores will be represented on the visualisation. Scores: Only the main scores will be
                       represented on the visualisation. Scores + CI: Both the scores and the ends of the confidence intervals
                       will be represented on the visualisation.", placement = "left"))
  } else {
    NULL
  }
  
  option5 <- if(yearlabel == TRUE) {
    selectInput(paste0(tool,"YearLabel"), "Append Year of Assessment to Scale Labels", choices = yearlabels, multiple = FALSE) %>% 
      shinyInput_label_embed(
        icon("question") %>%
          bs_embed_tooltip(title = "Select whether the year the test was administered should be included in the scale name.", placement = "left"))
  } else {
    NULL
  }
  
  option6 <- if(normalcurve == TRUE) {
    selectInput(paste0(tool,"Norm"), "Normal Curve", choices = c("No", "Yes"), multiple = FALSE) %>% 
      shinyInput_label_embed(
        icon("question") %>%
          bs_embed_tooltip(title = "Select whether you want a normal curve plotted behind the visualisation.", placement = "left"))
  } else {
    NULL
  }
  
  option7 <- if(presentationtype == TRUE) {
    scoreTypes <- c("Z" = "z","Standard Score" = "standard", "Scaled Score" = "scaled", "T-Score" = "t-score")
    selectInput(paste0(tool,"PresentationType"), "Score Type to Plot", choices = scoreTypes, selected = "standard", multiple = FALSE) %>% 
      shinyInput_label_embed(
        icon("question") %>%
          bs_embed_tooltip(title = "Changes the scale of the plot and transforms all scores and confidence intervals appropriately. Z scores have a mean
                       of 0 and SD of 1, Standard Scores have a mean of 100 and SD of 15, Scaled Scores have a mean of 10 and SD of 2, and T-Scores
                       have a mean of 50 and SD of 10.", placement = "left"))
  } else {
    NULL
  }
  
  option8 <- if(organise == TRUE) {
    selectInput(paste0(tool,"Organise"), "Sort Graph", choices = sortoptions) %>% 
      shinyInput_label_embed(
        icon("question") %>%
          bs_embed_tooltip(title = "Determines what order the scales are presented in.", placement = "left"))
  } else {
    NULL
  }
  
  option9 <- if(chclabels == TRUE) {
    selectInput("chcLabels", "CHC Labels", choices = c("No","Name","Acronym","Name + Acronym"), multiple = FALSE) %>% 
      shinyInput_label_embed(
        icon("question") %>%
          bs_embed_tooltip(title = "Select whether you want the CHC factor names added to the scale names for each composite/subtest.", placement = "left"))
  } else {
    NULL
  }
  
  option10 <- if(coloursetup == TRUE) {
    selectInput(paste0(tool,"ColourSetup"), "Colour Scores", choices = colourselection, multiple = FALSE) %>% 
      shinyInput_label_embed(
        icon("question") %>%
          bs_embed_tooltip(title = "Determine what categories you want the visualisation to be represented by different colours.", placement = "left"))
  } else {
    NULL
  }
  
  option11 <- if(plotheight == TRUE) {
    sliderInput(paste0(tool,"PlotHeight"), "Plot Size", min = 1, max = 5, value = 3, step = 1) %>% 
      shinyInput_label_embed(
        icon("question") %>%
          bs_embed_tooltip(title = "Change the height of the plot to fit more or less scales.", placement = "left"))
  } else {
    NULL
  }
  
  option12 <- if(rangevis == TRUE) {
    selectInput(paste0(tool,"RangeVisualisation"), "Range Visualisation", choices = ranges, multiple = FALSE, selected = "No") %>%
      shinyInput_label_embed(
        icon("question") %>%
          bs_embed_tooltip(title = "Visualise each score range using these options.", placement = "left"))
  } else {
    NULL
  }
  
  # Put options into a taglist
  tagList(
    tags$h3("Step 8: Plot Options"),
    tags$hr(),
    tags$p("These options change the presentation of the visualisation."),
    option1, 
    option2, 
    option3, 
    option4, 
    option5, 
    option6, 
    option7,
    option12,
    option8, 
    option9, 
    option10, 
    option11
  )
}
