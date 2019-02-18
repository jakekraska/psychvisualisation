ConvertSEM <- function(sem, fromtype, totype) {
  # Converts SEM scores based on the type of standardised score
  #
  # Args:
  #   sem: a sem from a psychological test
  #   fromtype: specify the type of sem: "z", "scaled", "standard" or "t"
  #   toscore: specify the type of sem: "z", "scaled", "standard" or "t"
  # Returns:
  #   The sem in a converted standardised sem type
  
  # Check for missing arguments
  if(missing(sem)) {stop("sem argument not provided.")}
  if(missing(fromtype)) {stop("fromtype argument not provided.")}
  if(missing(totype)) {stop("totype argument not provided.")}
  
  sem <- as.numeric(sem)
  
  # Determine n
  if (fromtype == "z") {
    sd <- 1
  } else if (fromtype == "scaled") {
    sd <- 3
  } else if (fromtype == "standard") {
    sd <- 15
  } else if (fromttype == "t-score") {
    sd <- 10
  } else {
    stop("fromtype is specified incorrectly")
  }
  
  n <- (sd/sem)^2
  
  # Determine new SEM
  if (totype == "z") {
    sd <- 1
  } else if (totype == "scaled") {
    sd <- 3
  } else if (totype == "standard") {
    sd <- 15
  } else if (totype == "t-score") {
    sd <- 10
  } else {
    stop("totype is specified incorrectly")
  }
  
  sem <- sd/sqrt(n)
  return(sem)

}