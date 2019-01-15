age_calc <- function(from, to) {
  
  from_lt <- as.POSIXlt(from, tryFormats = c("%Y-%m-%d","%d/%m/%Y"))
  to_lt <- as.POSIXlt(to, tryFormats = c("%Y-%m-%d","%d/%m/%Y"))
  
  age <- to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon | (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday), age - 1, age)
}