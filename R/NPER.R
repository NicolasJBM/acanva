#' @name NPER
#' @title Number of periods
#' @author Nicolas Mangin
#' @description Wrapper emulating the MS Excel financial math function to compute number of periods necessary to achieve a FV given the PV, PMT, and periodic rate of return.
#' @param RATE Numeric. Periodic rate of return
#' @param PMT Numeric. Cash flow at every period.
#' @param PV Numeric. Cash flow at the beginning of all the periods.
#' @param FV Numeric. Cash flow at the end of all the periods.
#' @param type Integer. 0 if payment at the end of the period or 1 if payment at the beginning of the period.
#' @return Numeric. Numpber of periods
#' @importFrom FinCal n.period
#' @export


NPER <- function(
    RATE = 0,
    PMT = 0,
    PV = 0,
    FV = 0,
    type = 0
){
  FinCal::n.period(RATE,PV,FV,PMT,type)
}
