#' @name prepare_total_share_returns
#' @title Prepare market data
#' @author Nicolas Mangin
#' @description Prepare market data to compute rates of return on shares.
#' @param start Character. Beginning date (YYYY-MM-DD)
#' @param end Character. Ending date (YYYY-MM-DD)
#' @return Tibble. cik, date, period, adjusted_price, dividend, cashflow.
#' @seealso [compute_total_share_returns()]
#' @importFrom dplyr arrange
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr full_join
#' @importFrom dplyr group_by
#' @importFrom dplyr lag
#' @importFrom dplyr lead
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom tidyr replace_na
#' @export


prepare_total_share_returns <- function(start = "2018-01-01", end = "2023-12-31"){

  cik <- NULL
  reportDate <- NULL
  period <- NULL
  adjusted_price <- NULL
  dividend <- NULL
  cashflow <- NULL

  acanva::filings |>
    dplyr::select(cik, date = reportDate) |>
    base::unique() |>
    dplyr::mutate(reportDate = 1) |>
    dplyr::filter(date >= start, date <= end) |>
    dplyr::full_join(acanva::market_data, by = c("cik","date")) |>
    tidyr::replace_na(base::list(reportDate = 0)) |>
    dplyr::arrange(cik, date) |>
    dplyr::group_by(cik) |>
    dplyr::mutate(period = base::cumsum(reportDate) - reportDate) |>
    stats::na.omit() |>
    dplyr::mutate(reportDate = period - dplyr::lag(period)) |>
    tidyr::replace_na(base::list(reportDate = 0)) |>
    dplyr::mutate(purchasing_price = dplyr::lead(adjusted_price) + dplyr::lead(dividend)) |>
    stats::na.omit() |>
    dplyr::mutate(
      before_reporting = dplyr::lead(reportDate),
      cashflow = dplyr::case_when(
        reportDate == 1 ~ -adjusted_price,
        before_reporting == 1 ~ purchasing_price + dividend,
        TRUE ~ dividend
      )
    ) |>
    dplyr::mutate(cashflow = base::round(cashflow, 2)) |>
    dplyr::select(cik, date, period, adjusted_price, dividend, cashflow) |>
    dplyr::filter(period > 0)
}

