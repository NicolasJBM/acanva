#' @name format_financial_data
#' @title Prepare computed financial data
#' @author Nicolas Mangin
#' @description Take financial data and format them in the proper language for subsequent uses.
#' @param financial_data Tibble cik code of the company.
#' @param language Character. ISO2 code of the langiage to use ("en" or "fr")
#' @return Tibble with formated financial statements.
#' @importFrom dplyr arrange
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom tibble tibble
#' @export

format_financial_data <- function(financial_data, language = "en"){

  id <- NULL
  code <- NULL
  statement_fr <- NULL
  type_fr <- NULL
  id_fr <- NULL
  label_fr <- NULL
  fillcolor <- NULL
  fontcolor <- NULL
  statement <- NULL
  type <- NULL
  subtype <- NULL
  category <- NULL
  id_en <- NULL
  label <- NULL
  cik <- NULL
  ticker <- NULL
  name <- NULL
  sector <- NULL
  altid <- NULL
  value <- NULL
  subtype_fr <- NULL
  category_fr <- NULL

  if (language == "fr"){
    classifications <- acanva::classifications |>
      dplyr::select(
        id, code, statement = statement_fr, type = type_fr, subtype = subtype_fr, category = category_fr
      )
    definitions <- acanva::definitions |>
      dplyr::select(
        id, altid = id_fr, label = label_fr, fillcolor, fontcolor
      )
  } else {
    classifications <- acanva::classifications |>
      dplyr::select(
        id, code, statement, type, subtype, category
      )
    definitions <- acanva::definitions |>
      dplyr::mutate(id_en = id) |>
      dplyr::select(
        id, altid = id_en, label, fillcolor, fontcolor
      )
  }

  if (base::any(base::unique(financial_data$cik) %in% acanva::corporations$cik)){
    corporations <- acanva::corporations |>
      dplyr::select(cik, ticker, name, sector)
  } else {
    corporations <- tibble::tibble(
      cik = base::unique(financial_data$cik),
      ticker = NA,
      ame = base::unique(financial_data$cik),
      sector = NA
    )
  }

  financial_data <- financial_data |>
    dplyr::left_join(corporations, by = "cik") |>
    dplyr::left_join(classifications, by = "id") |>
    dplyr::left_join(definitions, by = "id") |>
    dplyr::select(cik, ticker, name, sector, date, source, statement, type, subtype, category, id, altid, fillcolor, fontcolor, code, label, value) |>
    dplyr::arrange(cik, date, code)

  return(financial_data)
}
