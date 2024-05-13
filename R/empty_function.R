#' @name empty_function
#' @title Template for a function
#' @author Nicolas Mangin
#' @description Write your new function based ion this template.
#' @param x Numeric. Specify the characteristics (e.g. class) and definition of your input here.
#' @return Numeric. Specify the characteristics (e.g. class) and definition of your output here.
#' @export

empty_function <- function(x) {
  
  base::stopifnot(base::is.numeric(x))
  
  y <- x+1
  
  return(y)
}
  
