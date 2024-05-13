#' @name make_journal_entry
#' @title Create a journal entry
#' @author Nicolas Mangin
#' @description Create an HTML journal entry.
#' @param transaction Tibble. Columns: "side" for D or C,"id" for the account,"amount", and "hide"; Rows: every impact on each account.
#' @param date Character. Transaction date or number.
#' @param label Character. Transaction label/title.
#' @param language Character. ISO2 code of the language.
#' @return HTML journal entry
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom kableExtra cell_spec
#' @importFrom kableExtra column_spec
#' @importFrom kableExtra kable_styling
#' @importFrom kableExtra kbl
#' @importFrom kableExtra row_spec
#' @importFrom purrr map2_chr
#' @importFrom tibble tibble
#' @importFrom tibble tribble
#' @export

make_journal_entry <- function(
    transaction = tibble::tribble(
      ~side, ~id, ~amount, ~hide,
      "D","CASH",50,FALSE,
      "D","AR",100,TRUE,
      "D","COS",80,FALSE,
      "C","REV",150,FALSE,
      "C","FP",80,FALSE
    ),
    date = base::as.character(base::Sys.Date()),
    label = "Sale of finished products",
    language = "en"
){
  base::stopifnot(base::sum(transaction$amount[transaction$side == "D"]) == base::sum(transaction$amount[transaction$side == "C"]))

  CDT <- NULL
  DBT <- NULL
  account <- NULL
  code <- NULL
  extracss <- NULL
  fillcolor <- NULL
  fontcolor <- NULL
  id <- NULL
  id_fr <- NULL
  label_fr <- NULL

  if (language == "fr"){
    attributes <- acanva::definitions |>
      dplyr::left_join(dplyr::select(acanva::classifications, id, code), by = "id") |>
      dplyr::select(id = id_fr, code, account = label_fr, fillcolor, fontcolor)
    marks <- c(" ",",")
    variables <- c("Date","Code","Compte","Debit","Credit")
  } else {
    attributes <- acanva::definitions |>
      dplyr::left_join(dplyr::select(acanva::classifications, id, code), by = "id") |>
      dplyr::select(id, code, account = label, fillcolor, fontcolor)
    marks <- c(",",".")
    variables <- c("Date","Code","Account","Debit","Credit")
  }

  locdbl <- function(x) acanva::dbl(x, marks = marks)

  entry <- transaction |>
    dplyr::left_join(attributes, by = "id") |>
    dplyr::mutate(extracss = base::paste0("background-color:", fillcolor, "; color:", fontcolor,";")) |>
    dplyr::mutate(
      date = "",
      code = purrr::map2_chr(code, extracss, function(x,y) kableExtra::cell_spec(x, extra_css = y)),
      account = dplyr::case_when(
        side == "D" ~ kableExtra::cell_spec(account, extra_css = "display:block;padding-left:10px;"),
        TRUE ~ kableExtra::cell_spec(account, extra_css = "display:block;padding-left:75px;")
      ),
      DBT = dplyr::case_when(
        hide == TRUE & side == "D" ~ "?",
        side == "D" ~ locdbl(amount),
        TRUE ~ ""
      ),
      CDT = dplyr::case_when(
        hide == TRUE & side == "C" ~ "?",
        side == "C" ~ locdbl(amount),
        TRUE ~ ""
      )
    ) |>
    dplyr::select(date, code, account, DBT, CDT) |>
    dplyr::bind_rows(
      tibble::tibble(
        date = "",
        code = "",
        account = kableExtra::cell_spec(label, bold = TRUE, extra_css = "text-align:center !important;"),
        DBT = "", CDT = ""
      )
    )

  entry$date[1] <- date
  base::names(entry) <- variables

  rn <- base::nrow(entry)

  entry |>
    kableExtra::kbl(format = "html", align = "lclrr", escape = FALSE) |>
    kableExtra::kable_styling(bootstrap_options = "striped") |>
    kableExtra::column_spec(c(4,5), width = "110px", extra_css = "border-left:dashed 1px black;") |>
    kableExtra::row_spec(0, extra_css = "border-bottom:solid 2px black;") |>
    kableExtra::row_spec(rn, extra_css = "border-bottom:solid 2px black;")

}
