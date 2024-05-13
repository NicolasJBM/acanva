#' @name compute_total_share_returns
#' @title Compute rates of return
#' @author Nicolas Mangin
#' @description Compute rates of return on shares
#' @param base_total_returns Tibble. cik, date, period, adjusted_price, dividend, cashflow.
#' @return Tibble. cik, id, date, value
#' @seealso [prepare_total_share_returns()]
#' @importFrom FinCal irr
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom lubridate as_date
#' @importFrom purrr map_chr
#' @importFrom purrr map_dbl
#' @importFrom purrr map_lgl
#' @importFrom tidyr nest
#' @export


compute_total_share_returns <- function(base_total_returns){

  cik <- NULL
  period <- NULL
  data <- NULL
  keep <- NULL
  id <- NULL
  value <- NULL


  base_total_returns |>
    dplyr::arrange(cik, period, date) |>
    dplyr::group_by(cik, period) |>
    tidyr::nest() |>
    dplyr::mutate(keep = purrr::map_lgl(data, function(x){
      x$cashflow[[1]] < 0 & x$cashflow[[base::length(x$cashflow)]] > 0
    })) |>
    dplyr::filter(keep) |>
    dplyr::mutate(
      value = purrr::map_dbl(data, function(x){
        FinCal::irr(x$cashflow)*(base::as.integer(lubridate::as_date(x$date[base::length(x$date)]) - lubridate::as_date(x$date[1])))
      }),
      date = purrr::map_chr(data, function(x) x$date[base::length(x$date)]),
      id = "R"
    ) |>
    dplyr::ungroup() |>
    dplyr::select(cik, id, date, value) |>
    dplyr::mutate(value = base::round(value, 2))
}

