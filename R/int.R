#' @name int
#' @title Format as integer
#' @author Nicolas Mangin
#' @description Function formating as an integer.
#' @param x   Double or Integer. Number to be properly formatted.
#' @param marks Character. Characters for thousands separator and decimal point.
#' @return Character. Formatted number.
#' @importFrom dplyr case_when
#' @export

int <- function(x = 1.234, marks = c(",",".")) {
  
  base::stopifnot(base::is.numeric(x))
  
  nbr <- base::length(x)
  z <- base::character(nbr)
  
  for (i in base::seq_len(nbr)){
    if (base::is.na(x[i])) {
      z[i] <- NA
    } else {
      z[i] <- base::format(
        base::round(x[i],0),
        big.mark = marks[1],
        decimal.mark = marks[2],
        scientific = F
      )
    }
  }
  
  return(z)
}
