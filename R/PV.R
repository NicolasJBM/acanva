#' @name PV
#' @title Present value
#' @author Nicolas Mangin
#' @description Wrapper emulating the MS Excel financial math function to compute the present value of an annuity.
#' @param RATE Numeric. Periodic rate of return
#' @param NPER Integer. Number of periods.
#' @param PMT Numeric. Cash flow at every period.
#' @param FV Numeric. Cash flow at the end of all the periods.
#' @param type Integer. 0 if payment at the end of the period or 1 if payment at the beginning of the period.
#' @return Numeric. Present value of an annuity.
#' @importFrom FinCal pv
#' @export


PV <- function(
    RATE = 0,
    NPER = 0,
    PMT = 0,
    FV = 0,
    type = 0
){
  FinCal::pv(RATE,NPER,FV,PMT,type)
}
