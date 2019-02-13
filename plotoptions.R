PlotOptions <- function(tool, 
                        sortoptions = c("Default", "Alphabetical"), 
                        colourselection = c("Type"),
                        yearlabels = c("No", "Yes"),
                        confidence = FALSE,
                        bartype = FALSE,
                        datalabels = FALSE,
                        yearlabel = FALSE,
                        normalcurve = FALSE,
                        presentationtype = FALSE,
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
  #   confidence: boolean to determine if confidnece interval options should be available, default is FALSE
  #   bartype: boolean to determine bar type options should be available, default is FALSE
  #   datalabels: boolean to determine if data label options should be available, default is FALSE
  #   yearlabel: boolean to determine if year label options should be available, default is FALSE
  #   normalcurve: boolean to determine if a normal curve option should be available, default is FALSE
  #   presentationtype: boolean to determine if plot and scores should be transformed (e.g. z, standard score, etc), default is FALSE
  #   organise: boolean to determine if organisation options should be available, default is FALSE
  #   chclabels: boolean to determine if chclabel options should be available (suitable only for CHC data), default is false
  #   coloursetup: boolean to determine if colour options should be available, default is false
  #   plotheight: boolean to determine if height options should be available, default is false
  
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
    radioButtons(paste0(tool,"Confidence"), "Confidence Interval %", choices = ci$ci, inline = TRUE, selected = "95")
  } else {
    NULL
  }
  
  option2 <- if(bartype == TRUE) {
    radioButtons(paste0(tool,"PlotType"), "Plot Type", choices = c("Bar", "Line"), inline = TRUE)
  } else {
    NULL
  }
  
  option3 <- if(bartype == TRUE) {
    conditionalPanel("input.chcPlotType == 'Line'", 
                     sliderInput(paste0(tool,"ErrorWidth"), "Error Bar Width", max = 1, min = .1, step = .1, value = .5),
                     sliderInput(paste0(tool,"LineThickness"), "Line Thickness", max = 3, min = .2, step = .2, value = 1),
                     sliderInput(paste0(tool,"PointSize"), "Point Size", max = 4, min = .2, step = .2, value = 2),
                     sliderInput(paste0(tool,"PointShape"), "Point Shape", max = 25, min = 0, step = 1, value = 16)
    )
  } else {
    NULL
  }
  
  option4 <- if(bartype == TRUE) {
    radioButtons(paste0(tool,"DataLabels"), "Score Labels", choices = c("None", "Scores", "Scores + CI"), inline = TRUE)
  } else {
    NULL
  }
  
  option5 <- if(yearlabel == TRUE) {
    radioButtons(paste0(tool,"YearLabel"), "Append Year of Assessment to Scale Labels", choices = yearlabels, inline = TRUE)
  } else {
    NULL
  }
  
  option6 <- if(normalcurve == TRUE) {
    radioButtons(paste0(tool,"Norm"), "Normal Curve", choices = c("No", "Yes"), inline = TRUE)
  } else {
    NULL
  }
  
  option7 <- if(presentationtype == TRUE) {
    scoreTypes <- c("Z" = "z","Standard Score" = "standard", "Scaled Score" = "scaled", "T Score" = "t")
    radioButtons(paste0(tool,"PresentationType"), "Score Type to Plot", choices = scoreTypes, selected = "standard", inline = TRUE)
  } else {
    NULL
  }
  
  option8 <- if(organise == TRUE) {
    selectInput(paste0(tool,"Organise"), "Sort Graph", choices = sortoptions)
  } else {
    NULL
  }
  
  option9 <- if(chclabels == TRUE) {
    radioButtons("chcLabels", "CHC Labels", choices = c("No","Abbreviated","Full"), inline = TRUE)
  } else {
    NULL
  }
  
  option10 <- if(coloursetup == TRUE) {
    radioButtons(paste0(tool,"ColourSetup"), "Colour Scores", choices = colourselection, selected = "Type", inline = TRUE)
  } else {
    NULL
  }
  
  option11 <- if(plotheight == TRUE) {
    sliderInput(paste0(tool,"PlotHeight"), "Plot Size", min = 1, max = 5, value = 3, step = 1)
  } else {
    NULL
  }
  
  # Put options into a taglist
  tagList(
    option1, option2, option3, option4, option5, option6, option7, option8, option9, option10, option11
  )
}
