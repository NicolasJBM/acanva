#' @name NPV
#' @title Net Present Value
#' @description Wrapper emulating the MS Excel financial math function to compute the Net Present Value of a set of periodc cash flows given the periodic exchange rate.
#' @param RATE Numeric. Periodic rate of return
#' @param CF Numeric vector. Set of periodic cash flows.
#' @return Numeric. Specify the characteristics (e.g. class) and definition of your output here.
#' @importFrom FinCal npv
#' @export


NPV <- function(
    RATE = 0,
    CF = 0
){
  FinCal::npv(RATE,CF)
}
