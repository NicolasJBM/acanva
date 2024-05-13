#' @name FV
#' @title Future Value
#' @author Nicolas Mangin
#' @description Wrapper emulating the MS Excel financial math function to compute a future value of an annuity.
#' @param RATE Numeric. Periodic rate of return
#' @param NPER Integer. Number of periods.
#' @param PMT Numeric. Cash flow at every period.
#' @param PV Numeric. Cash flow at the beginning of all the periods.
#' @param type Integer. 0 if payment at the end of the period or 1 if payment at the beginning of the period.
#' @return Numeric. Future value of an annuity.
#' @importFrom FinCal fv
#' @export


FV <- function(
    RATE = 0,
    NPER = 0,
    PMT = 0,
    PV = 0,
    type = 0
){
  FinCal::fv(RATE,NPER,PV,PMT,type)
}
