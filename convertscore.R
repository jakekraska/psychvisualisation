ConvertScore <- function(score, fromtype, totype, round = 0) {
  # Converts standardised scores between z score,
  # scaled score, standard score and T-score
  #
  # Args:
  #   score: a score from a psychological test
  #   fromtype: specify the type of score: "z", "scaled", "standard" or "t"
  #   toscore: specify the type of score: "z", "scaled", "standard" or "t"
  #   round: specify to how many digits the score should be rounded, default is 0
  # Returns:
  #   The score in a converted standardised score type
  
  # Check for missing arguments
  if(missing(score)) {stop("Score argument not provided.")}
  if(missing(fromtype)) {stop("fromtype argument not provided.")}
  if(missing(totype)) {stop("totype argument not provided.")}
  
  # Convert all scores from their type to a z score
  if (fromtype == "z") {
    score <- score
  } else if  (fromtype == "scaled") {
    score <- (score-10)/3
  } else if (fromtype == "standard") {
    score <- (score-100)/15
  } else if (fromtype == "t") {
    score <- (score-50)/10
  } else {
    stop("fromtype is specified incorrectly")
  }
  
  # Convert the calculated score to the desired type
  if (totype == "z") {
    score <- (score*1)+0
  } else if (totype == "scaled") {
    score <- (score*3)+10
  } else if (totype == "standard") {
    score <- (score*15)+100
  } else if (totype == "t") {
    score <- (score*10)+50
  } else {
    stop("totype is specified incorrectly")
  }
  
  # Round the score to the appropriate number of digits
  if (!is.numeric(round) || is.null(round) || round < 0 || round > 10 || round %% 1 != 0) {
    stop("round must be an integer between 0 and 10")
  } else {
    score <- round(score, digits = round)
  }
  
  return(score)
}