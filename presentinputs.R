PresentInputs <- function(type, score) {
  # Present numeric input fields of the required type
  #
  # Args:
  #   type: specify the type of score: "z", "scaled", "standard" or "t"
  #   score: A string that will act as the id and label for the numeric input
  #
  # Returns:
  #   A shiny style numeric input field with a minimum and maximum relevant
  #   to the appropriate score
  
  if (type == "z") {
    value <- 0
    min <- -5
    max <- 5
  } else if (type == "scaled") {
    value <- 10
    min <- 1
    max <- 19
  } else if (type == "standard") {
    value <- 100
    min <- 140
    max <- 160
  } else if (type == "t") {
    value <- 50
    min <- 0
    max <- 100
  } else {
    stop("Invalid type of standardised score provided")
  }
  
  numericInput(score, score, value = value, min = min, max = max)
  
}