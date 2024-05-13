#' @name compute_financial_data
#' @title Create financial data set
#' @author Nicolas Mangin
#' @description Create a complete set of statements, aggregates and ratios for the selected company in the chosen language.
#' @param dataset Character or tibble. Either the cik code of the company or a simulated dataset.
#' @param type Character. Whether the dataset should retrieved from the cik code ("observation") or given ("simulation")
#' @return Tibble with financial data
#' @importFrom broom tidy
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr everything
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr lag
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr slice_tail
#' @importFrom dplyr starts_with
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom purrr map
#' @importFrom purrr map_dbl
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove_all
#' @importFrom tidyr nest
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr unite
#' @importFrom tidyr unnest
#' @export

compute_financial_data <- function(dataset = NA, type = "observation"){

  base::stopifnot(!base::is.na(dataset), type %in% c("observation","simulation"))

  AP_A <- NULL
  AR_A <- NULL
  CAR <- NULL
  CASH_L <- NULL
  CASH_P <- NULL
  CA_L <- NULL
  CE_A <- NULL
  CFCR <- NULL
  CFIA_L <- NULL
  CFOA_L <- NULL
  CL_L <- NULL
  COL_L <- NULL
  COS_L <- NULL
  CUR <- NULL
  DAR <- NULL
  DDO <- NULL
  DER <- NULL
  DFL <- NULL
  DIO <- NULL
  DOL <- NULL
  DPO <- NULL
  DPS <- NULL
  DRO <- NULL
  DY <- NULL
  EBIT_L <- NULL
  EBT_L <- NULL
  EPS <- NULL
  EQ <- NULL
  FATR <- NULL
  FCF <- NULL
  FCF_L <- NULL
  FCF_P <- NULL
  GMR <- NULL
  GM_L <- NULL
  IBD_A <- NULL
  ICR <- NULL
  ICTR <- NULL
  IC_A <- NULL
  INT_L <- NULL
  INV_A <- NULL
  Kd <- NULL
  Kdn <- NULL
  LOR <- NULL
  MTBR <- NULL
  NCA_A <- NULL
  NCOL_L <- NULL
  NCSO <- NULL
  NCSO_A <- NULL
  NFWC_L <- NULL
  NGD <- NULL
  NI <- NULL
  NI_L <- NULL
  NMR <- NULL
  NOR <- NULL
  NOPAT <- NULL
  OCD <- NULL
  OCS <- NULL
  OI_L <- NULL
  OI_P <- NULL
  OWI_A <- NULL
  P <- NULL
  L <- NULL
  PAIDIV <- NULL
  PC_FCF <- NULL
  PC_OI <- NULL
  PC_REV <- NULL
  PER <- NULL
  QA_L <- NULL
  QR <- NULL
  RCD <- NULL
  REV_L <- NULL
  REV_P <- NULL
  ROCE <- NULL
  ROE <- NULL
  ROIC <- NULL
  ROL <- NULL
  SE <- NULL
  SE_L <- NULL
  SP <- NULL
  TAX_L <- NULL
  TA_L <- NULL
  TL_L <- NULL
  UDR_A <- NULL
  WCS <- NULL
  WC_L <- NULL
  adjusted_price <- NULL
  aggregate <- NULL
  amount <- NULL
  category_fr <- NULL
  cik <- NULL
  close_date <- NULL
  data <- NULL
  estimate <- NULL
  id <- NULL
  included <- NULL
  lagncso <- NULL
  period <- NULL
  simulation <- NULL
  subtype_fr <- NULL
  value <- NULL
  NOMR <- NULL

  if (type == "observation") {
    statements <- acanva::statements |>
      dplyr::filter(cik == dataset) |>
      dplyr::select(cik, date, id, value = amount)
  } else {
    statements <- dataset |>
      dplyr::select(cik, date, id, value = amount)
  }

  statements <- statements |>
    dplyr::group_by(cik, date, id) |>
    dplyr::summarise(value = base::sum(value), .groups = "drop") |>
    tidyr::pivot_wider(names_from = "date", values_from = value, values_fill = 0) |> # Necessary for appearing/disappearing accounts
    tidyr::pivot_longer(cols = dplyr::starts_with("20"), names_to = "date", values_to = "value")

  # Compute aggregates

  aggregates <- statements |>
    dplyr::select(cik, date, id, L = value) |>
    dplyr::group_by(cik, date, id) |>
    dplyr::summarise(L = base::sum(L), .groups = "drop") |>
    dplyr::arrange(cik, id, date) |>
    dplyr::group_by(cik, id) |>
    dplyr::mutate(P = dplyr::lag(L)) |>
    dplyr::mutate(A = (L + P)/2) |>
    dplyr::ungroup() |>
    tidyr::pivot_longer(cols = c("L","P","A"), names_to = "period", values_to = "value") |>
    stats::na.omit() |>
    dplyr::left_join(acanva::aggregations, by = "id") |>
    stats::na.omit() |>
    dplyr::select(cik, date, period, value, id, dplyr::everything())

  aggregates <- aggregates |>
    tidyr::pivot_longer(cols = base::names(aggregates)[-c(1:5)], names_to = "aggregate", values_to = "included") |>
    dplyr::filter(included != 0) |>
    dplyr::mutate(value = value * included) |>
    dplyr::group_by(cik, date, period, aggregate) |>
    dplyr::summarise(value = base::sum(value), .groups = "drop") |>
    tidyr::unite("aggregate", aggregate, period, sep = "_") |>
    tidyr::pivot_wider(names_from = "aggregate", values_from = value, values_fill = 0)

  missing <- base::setdiff(
    c(
      base::paste(base::names(acanva::aggregations)[-1], c("L"), sep = "_"),
      base::paste(base::names(acanva::aggregations)[-1], c("P"), sep = "_"),
      base::paste(base::names(acanva::aggregations)[-1], c("A"), sep = "_")
    ),
    base::names(aggregates)[-c(1:2)]
  )

  for (miss in missing){
    aggregates[,miss] <- 0
  }

  aggregates <- aggregates |>
    dplyr::mutate(
      NOGL_L = dplyr::case_when(
        EBT_L < 0 & TAX_L < 0 ~ NOGL_L + TAX_L,
        TRUE ~ NOGL_L
      ),
      EBIT_L = dplyr::case_when(
        EBT_L < 0 & TAX_L < 0 ~ EBIT_L + TAX_L,
        TRUE ~ EBIT_L
      ),
      EBT_L = dplyr::case_when(
        EBT_L < 0 & TAX_L < 0 ~ EBT_L + TAX_L,
        TRUE ~ EBT_L
      ),
      TAX_L = dplyr::case_when(
        EBT_L < 0 & TAX_L < 0 ~ 0,
        TRUE ~ TAX_L
      ),
      CASHBEG_L = CASH_P,
      CASHEND_L = CASH_L
    )

  # Compute ratios

  ratios <- aggregates |>
    dplyr::mutate(
      Kd = -INT_L / IBD_A,
      t = -TAX_L / EBT_L,
      Kdn = (-INT_L / IBD_A) * (1-(-TAX_L / EBT_L)),
      RCD = -INT_L * (1-(-TAX_L / EBT_L))
    ) |>
    dplyr::mutate(
      NOPAT = EBIT_L * (1-t),
      FCF = CFOA_L - INT_L * (1-(-TAX_L / EBT_L)) + CFIA_L
    ) |>
    dplyr::mutate(
      ROE = NI_L / OWI_A,
      ROIC = NOPAT / IC_A,
      ROCE = NOPAT / CE_A
    ) |>
    dplyr::mutate(
      DFL = base::round((ROIC - Kdn) * IBD_A / OWI_A, 4)
    ) |>
    dplyr::mutate(
      NOMR = NOPAT / REV_L,
      GMR = GM_L / REV_L,
      NMR = NI_L / REV_L,
      EQ = CFOA_L / NI_L,

      ICTR = REV_L/IC_A,
      FATR = REV_L / NCA_A,
      DRO = AR_A / (REV_L/365),
      DIO = base::ifelse(COS_L == 0, 0, INV_A / (-COS_L/365)),
      DPO = base::ifelse(COS_L == 0, 0, AP_A / (-COS_L/365)),
      DDO = UDR_A / (REV_L/365),
      OCD = DRO + DIO - DPO - DDO,
      OCS = 365 / (1 + DRO + DIO + DPO + DDO),

      CAR = CASH_L / CL_L,
      QR = QA_L / CL_L,
      CUR = CA_L / CL_L,

      DER = TL_L / SE_L,
      ICR = -EBIT_L/INT_L,
      CFCR = TL_L / CFOA_L
    ) |>
    dplyr::mutate(
      NGD = ROIC - Kdn,
      LOR = IBD_A / OWI_A,
      WCS = (WC_L - NFWC_L) / base::abs(NFWC_L),
      DAR = TL_L / TA_L,
      ROL = (COL_L + NCOL_L) / TA_L
    )

  # Compute coefficients

  coefficients <- ratios |>
    dplyr::select(cik, date, FCF_L = FCF) |>
    dplyr::left_join(
      dplyr::select(aggregates, cik, date, REV_L, REV_P, OI_L, OI_P),
      by = c("cik","date")
    ) |>
    dplyr::arrange(date) |>
    dplyr::mutate(FCF_P = dplyr::lag(FCF_L)) |>
    dplyr::mutate(
      PC_REV = (REV_L - REV_P)/ REV_P,
      PC_FCF = (FCF_L - FCF_P) / FCF_P,
      PC_OI = (OI_L - OI_P) / OI_P,
      DOL = PC_OI / PC_REV
    ) |>
    dplyr::filter(base::is.finite(DOL)) |>
    #dplyr::slice_tail(n = 5) |>
    dplyr::group_by(cik) |>
    tidyr::nest() |>
    dplyr::mutate(
      PC_REV_LT = purrr::map_dbl(data, function(x) base::mean(x$PC_REV)),
      PC_FCF_LT = purrr::map_dbl(data, function(x) base::mean(x$PC_FCF)),
      CV_REV = purrr::map_dbl(data, function(x) stats::sd(x$REV_L)/base::mean(x$REV_L)),
      CV_FCF = purrr::map_dbl(data, function(x) stats::sd(x$FCF_L)/base::mean(x$FCF_L)),
      DOL_LT = purrr::map_dbl(data, function(x) {
        stats::glm(PC_OI ~ 0 + PC_REV, data = x) |>
          broom::tidy() |>
          dplyr::select(estimate) |>
          base::unlist() |> base::as.numeric()
      }),
      data = purrr::map(data, function(x) dplyr::select(x, date, PC_REV, PC_FCF, DOL))
    ) |>
    tidyr::unnest(data)

  # Compute market data

  equity <- aggregates |>
    dplyr::select(cik, date, SE = SE_L)

  earnings <- aggregates |>
    dplyr::select(cik, date, NI = NI_L)

  dividends <- statements |>
    dplyr::filter(id == "PAIDIV") |>
    dplyr::mutate(PAIDIV = -value) |>
    dplyr::select(cik, date, PAIDIV)

  avg_ncso <- statements |>
    dplyr::filter(id == "NCSO") |>
    dplyr::arrange(date) |>
    dplyr::filter(value > 0) |>
    dplyr::mutate(lagncso = dplyr::lag(value)) |>
    stats::na.omit() |>
    dplyr::mutate(NCSO_A = (value+lagncso)/2) |>
    dplyr::select(cik, date, NCSO = value, NCSO_A)

  if (type == "observation"){

    prices <- acanva::market_data |>
      dplyr::filter(cik == dataset) |>
      dplyr::select(cik, date, SP = adjusted_price) |>
      dplyr::mutate(close_date = stringr::str_remove_all(date, "-..$")) |>
      dplyr::group_by(close_date) |>
      dplyr::filter(date == base::max(date)) |>
      dplyr::ungroup() |>
      dplyr::select(-date)

    market_data <- avg_ncso |>
      dplyr::left_join(equity, by = c("cik","date")) |>
      dplyr::left_join(earnings, by = c("cik","date")) |>
      dplyr::left_join(dividends, by = c("cik","date")) |>
      dplyr::mutate(close_date = stringr::str_remove_all(date, "-..$")) |>
      dplyr::left_join(prices, by = c("cik","close_date"))

  } else {

    prices <- simulation |>
      dplyr::filter(id == "SP") |>
      stats::na.omit() |>
      dplyr::select(cik, date, SP = amount)

    market_data <- avg_ncso |>
      dplyr::left_join(equity, by = c("cik","date")) |>
      dplyr::left_join(earnings, by = c("cik","date")) |>
      dplyr::left_join(dividends, by = c("cik","date")) |>
      dplyr::left_join(prices, by = c("cik","date"))
  }

  market_data <- market_data |>
    dplyr::mutate(
      EPS = NI / NCSO_A,
      DPS = PAIDIV / NCSO_A
    ) |>
    dplyr::mutate(
      DPS = dplyr::case_when(
        DPS > 100000 ~ DPS/1000000,
        DPS > 100 ~ DPS/1000,
        TRUE ~ DPS
      ),
      EPS = dplyr::case_when(
        EPS > 100000 ~ EPS/1000000,
        EPS > 100 ~ EPS/1000,
        TRUE ~ EPS
      )
    ) |>
    dplyr::mutate(
      MTBR = (SP * NCSO)/SE,
      PER = SP / EPS,
      DY = DPS / SP
    ) |>
    dplyr::select(cik, date, SP, MTBR, EPS, PER, DPS, DY) |>
    tidyr::pivot_longer(
      cols = c("SP","MTBR","EPS","PER","DPS","DY"),
      names_to = "id", values_to = "value"
    ) |>
    dplyr::select(cik, date, id, value) |>
    stats::na.omit()

  # Format statements, agregates, ratios, coefficients, and market data

  statements <- dplyr::mutate(statements, source = "statements") |>
    dplyr::select(cik, date, source, id, value)

  aggregates <- aggregates |>
    tidyr::pivot_longer(cols = base::names(aggregates)[-c(1:2)], names_to = "id", values_to = "value") |>
    dplyr::filter(stringr::str_detect(id, "_L$")) |>
    dplyr::mutate(
      id = stringr::str_remove_all(id, "_L$"),
      source = "aggregates"
    ) |>
    dplyr::select(cik, date, source, id, value)

  ratios <- ratios |>
    dplyr::select(
      cik, date,
      NOPAT, t, Kd, Kdn, RCD,
      ROE, ROIC, DFL, NGD, LOR, ROCE,
      NOMR, GMR, NMR, EQ,
      ICTR, FATR, OCD, DRO, DIO, DPO, DDO, OCS,
      WCS, CAR, QR, CUR,
      DER, DAR, ROL, ICR, CFCR, FCF
    ) |>
    tidyr::pivot_longer(
      cols = c(
        NOPAT, t, Kd, Kdn, RCD,
        ROE, ROIC, DFL, NGD, LOR, ROCE,
        NOMR, GMR, NMR, EQ,
        ICTR, FATR, OCD, DRO, DIO, DPO, DDO, OCS,
        WCS, CAR, QR, CUR,
        DER, DAR, ROL, ICR, CFCR, FCF
      ),
      names_to = "id", values_to = "value"
    ) |>
    dplyr::mutate(
      value = base::round(value, 4),
      source = "ratios"
    ) |>
    dplyr::filter(base::is.finite(value)) |>
    dplyr::select(cik, date, source, id, value)

  coefficients <- coefficients |>
    tidyr::pivot_longer(cols = base::names(coefficients)[-c(1:2)], names_to = "id", values_to = "value") |>
    dplyr::mutate(source = "coefficients") |>
    stats::na.omit() |>
    dplyr::select(cik, date, source, id, value)

  market_data <- market_data |>
    dplyr::bind_rows(dplyr::filter(acanva::cost_of_capital, cik == dataset)) |>
    dplyr::mutate(source = "market") |>
    dplyr::select(cik, date, source, id, value)

  financial_data <- statements |>
    dplyr::bind_rows(aggregates) |>
    dplyr::bind_rows(ratios) |>
    dplyr::bind_rows(coefficients) |>
    dplyr::bind_rows(market_data) |>
    dplyr::arrange(cik, date)

  return(financial_data)
}
