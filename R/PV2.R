#' @name PV2
#' @title Present value
#' @author Nicolas Mangin
#' @description Wrapper emulating the MS Excel financial math function to compute the present value of an annuity.
#' @param RATE Numeric. Periodic rate of return
#' @param NPER Integer. Number of periods.
#' @param PMT Numeric. Cash flow at every period.
#' @param FV Numeric. Cash flow at the end of all the periods.
#' @param type Integer. 0 if payment at the end of the period or 1 if payment at the beginning of the period.
#' @return Numeric. Present value of an annuity.
#' @export


PV2 <- function(
    RATE = 0,
    NPER = 0,
    PMT = 0,
    FV = 0,
    type = 0
){
  PV1 <- FV / (1+RATE)^NPER
  PV2 <- PMT * (1-(1+RATE)^-NPER)/RATE
  if (type == 1) PV2 <- PV2*(1+RATE)
  PV1 + PV2
}


