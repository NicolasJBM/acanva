#' @name dbl
#' @title Format as numeric
#' @author Nicolas Mangin
#' @description Function formating as a number with the specified number of digits.
#' @param x Double or Integer. Number to be properly formatted.
#' @param decimals Integer. Number of digits.
#' @param negpar Logical. Whether negative numbers should be written between parentheses.
#' @param marks Character. Characters for thousands separator and decimal point.
#' @return Character. Formatted number.
#' @export

dbl <- function(x = 1.234, decimals = 2, negpar = FALSE, marks = c(",",".")) {
  
  base::stopifnot(
    base::is.numeric(x),
    base::is.numeric(decimals),
    decimals >= 0
  )
  
  x <- base::round(x, decimals)
  
  nbr <- base::length(x)
  z <- base::character(nbr)
  
  for (i in base::seq_len(nbr)){
    
    if (base::is.na(x[i])) {
      
      z[i] <- "  "
      
    } else {
      
      if (negpar & x[i] < 0){
      
        z[i] <- base::format(
          abs(x[i]), big.mark = marks[1],
          decimal.mark = marks[2],
          scientific = F,
          nsmall = decimals
        )
        z[i] <- base::paste0("(", z[i], ")")
        
      } else 
      
      z[i] <- base::format(
        x[i], big.mark = marks[1],
        decimal.mark = marks[2],
        scientific = F,
        nsmall = decimals
      )
      
    }
    
    
  }
  
  return(z)
}
  
