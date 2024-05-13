#' @name FV2
#' @title Future Value
#' @author Nicolas Mangin
#' @description Wrapper emulating the MS Excel financial math function to compute a future value of an annuity.
#' @param RATE Numeric. Periodic rate of return
#' @param NPER Integer. Number of periods.
#' @param PMT Numeric. Cash flow at every period.
#' @param PV Numeric. Cash flow at the beginning of all the periods.
#' @param type Integer. 0 if payment at the end of the period or 1 if payment at the beginning of the period.
#' @return Numeric. Future value of an annuity.
#' @export


FV2 <- function(
    RATE = 0,
    NPER = 0,
    PMT = 0,
    PV = 0,
    type = 0
){
  FV1 <- PV * (1+RATE)^NPER
  FV2 <- PMT * ((1+RATE)^NPER-1)/RATE
  if (type == 1) FV2 <- FV2*(1+RATE)
  FV1 + FV2
}
