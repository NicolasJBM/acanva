#' @name pct
#' @title Format percentages
#' @author Nicolas Mangin
#' @description Function transforming numbers between 0 and 1 in percentages.
#' @param x Double or Integer. Number to be properly formatted.
#' @param digits Integer. Number of digits.
#' @param negpar Logical. Whether negative numbers should be written between parentheses.
#' @param sign Logical. Whether the sign should be displayed (if negpar is FALSE)
#' @param symb   Logical. Whether the % symbol shour be added.
#' @param marks Character. Characters for thousands separator and decimal point.
#' @return Character. Formated number.
#' @export

pct <- function(x = 0.12345, digits = 2, negpar = FALSE, sign = FALSE, symb = FALSE, marks = c(",",".")) {
  
  stopifnot(
    is.numeric(x),
    is.numeric(digits),
    digits >= 0
  )
  
  nbr <- base::length(x)
  z <- base::character(nbr)
  
  for (i in base::seq_len(nbr)){
    if (base::is.na(x[i])) {
      z[i] <- NA
    } else {
      
      
      if (negpar & x[i] < 0){
        
        z[i] <- base::format(
          base::abs(x[i]) * 100,
          big.mark = marks[1],
          decimal.mark = marks[2],
          scientific = F,
          digits = digits,
          nsmall = 2
        )
        
        if (symb){
          z[i] <- base::paste0(z[i], "%")
        }
        
        z[i] <- base::paste0("(", z[i], ")")
        
      } else {
        
        z[i] <- base::format(
          x[i] * 100,
          big.mark = marks[1],
          decimal.mark = marks[2],
          scientific = F,
          digits = digits,
          nsmall = 2
        )
        
        if (sign) {
          if (x[i] > 0){
            z[i] <- base::paste0("+", z[i])
          }
        }
        
        if (symb){
          z[i] <- base::paste0(z[i], "%")
        }
        
      }
      
    }
  }
  
  return(z)
} 
