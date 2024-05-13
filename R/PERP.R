#' @name PERP
#' @title Compute a perpetuity
#' @author Nicolas Mangin
#' @description Compute the value of a perpetuity at the beginning of this perpetuity.
#' @param RATE Numeric. Periodic rate of return
#' @param GROWTH Numeric
#' @param PMT Numeric
#' @return Numeric. Present value of a perpetiuity
#' @export


PERP <- function(
    RATE = 0,
    GROWTH = 0,
    PMT = 0
){
  if (GROWTH == 0) PMT/RATE else (PMT*(1+GROWTH))/(RATE-GROWTH)
}
