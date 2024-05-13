#' @name make_t_account
#' @title Create a T-account
#' @author Nicolas Mangin
#' @description Create an HTML T account.
#' @param transactions Tibble. Columns: "dbtid","debit","credit","cdtid","hide_debit","hide_credit"; Rows: initial balance if permanent account and then one per transaction.
#' @param account Character. Id of the account displayed.
#' @param balance Logical.Whether balances should be computed and appended.
#' @param language Character. ISO2 code of the language.
#' @return HTML T account.
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr select
#' @importFrom kableExtra add_header_above
#' @importFrom kableExtra column_spec
#' @importFrom kableExtra kable_styling
#' @importFrom kableExtra kbl
#' @importFrom kableExtra row_spec
#' @importFrom stringr str_replace_all
#' @importFrom tibble tibble
#' @importFrom tidyr replace_na
#' @importFrom tidyr unite
#' @export


make_t_account <- function(
    transactions = tibble::tibble(
      dbtid =  c(NA,"(1)","(2)","(3)"),
      debit = c(50, 10, 50, 75),
      credit = c(NA, 90, NA, 20),
      cdtid =  c(NA,"(4)",NA,"(5)"),
      hide_debit = c(NA,NA,"x",NA),
      hide_credit = c(NA,"y",NA,NA)
    ),
    account = "CASH",
    balance = TRUE,
    language = "en"
){

  cdtid <- NULL
  code <- NULL
  credit <- NULL
  dbtid <- NULL
  debit <- NULL
  fillcolor <- NULL
  fontcolor <- NULL
  id <- NULL
  label <- NULL
  type <- NULL
  id_fr <- NULL
  label_fr <- NULL

  if (language == "fr"){
    attributes <- acanva::definitions |>
      dplyr::left_join(dplyr::select(acanva::classifications, id, code), by = "id") |>
      dplyr::select(id = id_fr, code, label = label_fr, fillcolor, fontcolor)
    marks <- c(" ",",")
  } else {
    attributes <- acanva::definitions |>
      dplyr::left_join(dplyr::select(acanva::classifications, id, code), by = "id") |>
      dplyr::select(id, code, label, fillcolor, fontcolor)
    marks <- c(",",".")
  }

  locdbl <- function(x) acanva::dbl(x, marks = marks)

  account_info <- attributes |>
    dplyr::filter(id == account) |>
    dplyr::mutate(
      type = dplyr::case_when(
        code < 40000 ~ "permanent",
        TRUE ~ "temporary"
      )
    ) |>
    tidyr::unite(label, code, label, sep = " - ") |>
    dplyr::select(label, type, fillcolor, fontcolor)

  total_debit <- base::sum(transactions$debit, na.rm = TRUE)
  total_credit <- base::sum(transactions$credit, na.rm = TRUE)

  if (balance){
    if (total_debit > total_credit) {
      dbtbal <- total_debit - total_credit
      cdtbal <- NA
    } else if (total_debit < total_credit) {
      dbtbal <- NA
      cdtbal <- total_credit - total_debit
    } else {
      dbtbal <- 0
      cdtbal <- 0
    }

    ballab <- base::ifelse(account_info$type == "permanent", "End.", "Bal.")

    append <- tibble::tibble(
      dbtid = base::ifelse(!base::is.na(dbtbal), ballab, "--"),
      debit = dbtbal,
      credit = cdtbal,
      cdtid =  base::ifelse(!base::is.na(cdtbal), ballab, "--"),
      hide_debit = NA,
      hide_credit = NA
    )

    transactions <- transactions |>
      dplyr::bind_rows(append)
  }

  if (account_info$type == "permanent"){
    if (!base::is.na(transactions$debit[1])) transactions$dbtid[1] <- "Beg."
    if (!base::is.na(transactions$credit[1])) transactions$cdtid[1] <- "Beg."
  }

  t_account <- transactions |>
    dplyr::mutate(
      debit = dplyr::case_when(
        !base::is.na(hide_debit) ~ hide_debit,
        !base::is.na(debit) ~ locdbl(debit),
        TRUE ~ "--"
      ),
      credit = dplyr::case_when(
        !base::is.na(hide_credit) ~ hide_credit,
        !base::is.na(credit) ~ locdbl(credit),
        TRUE ~ "--"
      )
    ) |>
    dplyr::select(dbtid, debit, credit, cdtid) |>
    tidyr::replace_na(base::list(dbtid = "--", debit = "--", credit = "--", cdtid = "--"))

  rn <- base::nrow(t_account)

  tacc <- t_account |>
    dplyr::mutate_all(function(x) stringr::str_replace_all(x, "--", "<br>")) |>
    kableExtra::kbl(format = "html", align = "rr", escape = FALSE) |>
    kableExtra::kable_styling(bootstrap_options = "striped") |>
    kableExtra::add_header_above(
      base::data.frame(name = account_info$label[1], col = 4),
      color = account_info$fontcolor[1],
      background = account_info$fillcolor[1],
    ) |>
    kableExtra::row_spec(0, extra_css = base::paste0(
      "font-size:0;line-height:0;background-color:", account_info$fillcolor[1], ";","color:#00000000;"
    )) |>
    kableExtra::row_spec(0, extra_css = "border-bottom:solid 5px black;") |>
    kableExtra::row_spec(0, extra_css = "line-height:0px !important;") |>
    kableExtra::column_spec(2, extra_css = "border-right:solid 5px black;") |>
    kableExtra::column_spec(c(2,3), extra_css = "width:40% !important;") |>
    kableExtra::column_spec(c(1,4), extra_css = "width:8% !important;") |>
    kableExtra::row_spec(1:rn, extra_css = "line-height:1em;")

  if (account_info$type == "permanent"){
    tacc <- tacc |>
      kableExtra::row_spec(1, extra_css = "border-bottom:solid 1px black;", bold = TRUE)
  }

  if (balance) {
    tacc <- tacc |>
      kableExtra::row_spec(rn, extra_css = "border-top:solid 1px black;", bold = TRUE)
  }

  tacc
}
