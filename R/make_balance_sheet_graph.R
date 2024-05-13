#' @name make_balance_sheet_graph
#' @title make a balance sheet graph
#' @author Nicolas Mangin
#' @description Create a visual representation of a balance sheet.
#' @param company_id Character. CIK code of the company.
#' @param date_id Character. ISO code for the end of the period: "YYYY-MM-DD"
#' @param language Character. ISO2 code for the language.
#' @param label_size Integer. Size of labels.
#' @return ggplot visualization of a balance sheet.
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr group_by
#' @importFrom dplyr lag
#' @importFrom dplyr mutate
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_void
#' @importFrom tidyr replace_na
#' @export


make_balance_sheet_graph <- function(company_id = NA, date_id, language = "en", label_size = 4){

  type <- NULL
  value <- NULL
  position <- NULL
  lagposition <- NULL
  label <- NULL
  category <- NULL
  code <- NULL
  fillcolor <- NULL
  fontcolor <- NULL
  statement <- NULL

  base::stopifnot(company_id %in% base::unique(acanva::statements$cik))

  if (language == "fr"){
    slctstatement <- "Bilan"
  } else slctstatement <- "Balance Sheet"
  
  balancesheet <- acanva::compute_financial_data(company_id, language) |>
    dplyr::filter(source == "statements", statement == slctstatement, date == date_id)

  base::stopifnot(base::nrow(balancesheet) > 0)

  balancesheet <- balancesheet |>
    dplyr::select(type, label = category, code, fillcolor, fontcolor, value) |>
    dplyr::group_by(type, label, fillcolor, fontcolor) |>
    dplyr::summarise(code = base::mean(code), value = base::sum(value), .groups = "drop") |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(code)) |>
    dplyr::group_by(type) |>
    dplyr::mutate(position = base::cumsum(value)) |>
    dplyr::mutate(lagposition = dplyr::lag(position)) |>
    tidyr::replace_na(base::list(lagposition = 0)) |>
    dplyr::mutate(
      position = (position + lagposition)/2,
      type = base::factor(type)
    )

  balancesheet$type <- stats::reorder(balancesheet$type, balancesheet$code, FUN = mean)

  balancesheet |>
    ggplot2::ggplot(ggplot2::aes(x = type, y = value)) +
    ggplot2::geom_bar(stat = "identity", color = "black", width=0.98, fill = balancesheet$fillcolor) +
    ggplot2::geom_text(ggplot2::aes(label = label, y = position), size = label_size, color = balancesheet$fontcolor) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
}
