#' @name PMT
#' @title Annuity
#' @author Nicolas Mangin
#' @description Wrapper emulating the MS Excel financial math function to compute an annuity.
#' @param RATE Numeric. Periodic rate of return/
#' @param NPER Integer. Number of periods.
#' @param PV Numeric. Cash flow at the beginning of all the periods.
#' @param FV Numeric. Cash flow at the end of all the periods.
#' @param type Integer. 0 if payment at the end of the period or 1 if payment at the beginning of the period.
#' @return Numeric. Constant periodic payment necessary to maintain equivalance.
#' @importFrom FinCal pmt
#' @export


PMT <- function(
    RATE = 0,
    NPER = 0,
    PV = 0,
    FV = 0,
    type = 0
){
  FinCal::pmt(RATE,NPER,PV,FV,type)
}
