#' @name simulate_statements_fin
#' @title Create income statement and average balance sheet
#' @author Nicolas Mangin
#' @description Create an income statement and an average balance sheet leading to desired ratios
#' @param start List.
#' @param end List.
#' @param company Character.
#' @param year Integer.
#' @param parval Numeric.
#' @param Ke Numeric.
#' @return Tibble.
#' @importFrom tibble tibble
#' @importFrom tibble tribble
#' @importFrom tibble rownames_to_column
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr case_when
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @export


simulate_statements_fin <- function(start, end, company = "A", year, parval = 0.1, Ke = c(0.1,0.1)){

  AP <- NULL
  APIC <- NULL
  AR <- NULL
  CASH <- NULL
  COMSTK <- NULL
  COS <- NULL
  DEPR <- NULL
  INT <- NULL
  INV <- NULL
  LTD <- NULL
  OAL <- NULL
  SGNA <- NULL
  PDE <- NULL
  PPE <- NULL
  RE <- NULL
  TAX <- NULL
  UDR <- NULL
  amount <- NULL
  cik <- NULL
  id <- NULL
  label <- NULL
  reportDate <- NULL
  report_year <- NULL
  var1 <- NULL
  var2 <- NULL
  year_1 <- NULL
  year_2 <- NULL
  year_3 <- NULL


  is <- tibble::tibble(
    id = base::names(start$income_statement),
    year_2 = base::as.numeric(start$income_statement[1,]),
    year_3 = base::as.numeric(end$income_statement[1,])
  )

  bs <- tibble::tibble(
    id = base::names(start$balance_sheet),
    start = base::as.numeric(start$balance_sheet[1,]),
    end = base::as.numeric(end$balance_sheet[1,])
  ) |>
    dplyr::mutate(year_2 =  base::round((end+start)/(2.1-stats::runif(1)/5),2)) |>
    dplyr::mutate(year_2 = dplyr::case_when(
      id %in% c("COMSTK","APIC") ~ base::round((end+start)/2,0),
      TRUE ~ year_2
    )) |>
    dplyr::mutate(
      year_1 = start + (start-year_2),
      year_3 = end + (end-year_2)
    ) |>
    dplyr::select(id, year_1, year_2, year_3)

  bs$year_1[1] <- base::sum(bs$year_1[6:12]) - base::sum(bs$year_1[2:5])
  bs$year_2[1] <- base::sum(bs$year_2[6:12]) - base::sum(bs$year_2[2:5])
  bs$year_3[1] <- base::sum(bs$year_3[6:12]) - base::sum(bs$year_3[2:5])

  prep_cfs <- bs |>
    dplyr::mutate(
      var1 = year_2 - year_1,
      var2 = year_3 - year_2
    ) |>
    dplyr::select(id, var1, var2)

  CFOA_1 <- is$year_2[11] + is$year_2[6] -
    prep_cfs$var1[2] - prep_cfs$var1[3] - prep_cfs$var1[4] +
    prep_cfs$var1[6] + prep_cfs$var1[7] + prep_cfs$var1[8]
  CFIA_1 <- -prep_cfs$var1[5]-is$year_2[6]
  CHG_PIC_1 <- prep_cfs$var1[10]+prep_cfs$var1[11]
  PAIDIV_1 <- prep_cfs$var1[12]-is$year_2[11]
  CFFA_1 <- prep_cfs$var1[9]+CHG_PIC_1+PAIDIV_1

  CFOA_2 <- is$year_3[11] + is$year_3[6] -
    prep_cfs$var2[2] - prep_cfs$var2[3] - prep_cfs$var2[4] +
    prep_cfs$var2[6] + prep_cfs$var2[7] + prep_cfs$var2[8]
  CFIA_2 <- -prep_cfs$var2[5]-is$year_3[6]
  CHG_PIC_2 <- prep_cfs$var2[10]+prep_cfs$var2[11]
  PAIDIV_2 <- prep_cfs$var2[12]-is$year_3[11]
  CFFA_2 <- prep_cfs$var2[9]+CHG_PIC_2+PAIDIV_2

  CASHVAR_1 <- CFOA_1 + CFIA_1 + CFFA_1
  CASHVAR_2 <- CFOA_2 + CFIA_2 + CFFA_2

  cfs <- tibble::tribble(
    ~id, ~"year_2", ~"year_3",
    "NI_CFS",is$year_2[11],is$year_3[11],
    "CNCL_DEPR",is$year_2[6],is$year_3[6],
    "CHG_AR",-prep_cfs$var1[2],-prep_cfs$var2[2],
    "CHG_INV",-prep_cfs$var1[3],-prep_cfs$var2[3],
    "CHG_PDE",-prep_cfs$var1[4],-prep_cfs$var2[4],
    "CHG_AP",prep_cfs$var1[6],prep_cfs$var2[6],
    "CHG_UDR",prep_cfs$var1[7],prep_cfs$var2[7],
    "CHG_OAL",prep_cfs$var1[8],prep_cfs$var2[8],
    "TCFOA",CFOA_1,CFOA_2,
    "NET_CHG_PPE",CFIA_1,CFIA_2,
    "TCFIA",CFIA_1,CFIA_2,
    "CHG_LTD",prep_cfs$var1[9],prep_cfs$var2[9],
    "CHG_PIC",CHG_PIC_1,CHG_PIC_2,
    "PAIDIV",PAIDIV_1,PAIDIV_2,
    "TCFFA",CFFA_1,CFFA_2,
    "CASHVAR",CASHVAR_1,CASHVAR_2,
    "CASHBEG",bs$year_1[1],bs$year_2[1],
    "CASHEND",bs$year_2[1],bs$year_3[1]
  )

  is <- is |>
    tidyr::pivot_longer(cols = c("year_2","year_3"), names_to = "tmpyear", values_to = "amount") |>
    dplyr::mutate(
      report_year = dplyr::case_when(
        tmpyear == "year_1" ~ year - 2,
        tmpyear == "year_2" ~ year - 1,
        TRUE ~ year
      )
    ) |>
    dplyr::select(report_year, id, amount) |>
    tidyr::pivot_wider(names_from = "id", values_from = "amount") |>
    tibble::column_to_rownames("report_year") |>
    dplyr::mutate(COS = -COS, SGNA = -SGNA, DEPR = -DEPR, INT = -INT, TAX = -TAX)

  bs <- bs |>
    tidyr::pivot_longer(cols = c("year_1","year_2","year_3"), names_to = "tmpyear", values_to = "amount") |>
    dplyr::mutate(
      report_year = dplyr::case_when(
        tmpyear == "year_1" ~ year - 2,
        tmpyear == "year_2" ~ year - 1,
        TRUE ~ year
      )
    ) |>
    dplyr::select(report_year, id, amount) |>
    tidyr::pivot_wider(names_from = "id", values_from = "amount") |>
    tibble::column_to_rownames("report_year")

  assets <- dplyr::select(bs,PPE,PDE,INV,AR,CASH)
  liabilities <- dplyr::select(bs,AP,UDR,OAL,LTD)
  equity <- dplyr::select(bs,COMSTK,APIC,RE)

  cfs <- cfs |>
    tidyr::pivot_longer(cols = c("year_2","year_3"), names_to = "tmpyear", values_to = "amount") |>
    dplyr::mutate(
      report_year = dplyr::case_when(
        tmpyear == "year_1" ~ year - 2,
        tmpyear == "year_2" ~ year - 1,
        TRUE ~ year
      )
    ) |>
    dplyr::select(report_year, id, amount) |>
    tidyr::pivot_wider(names_from = "id", values_from = "amount") |>
    tibble::column_to_rownames("report_year")

  cash_flow_statement <- cfs

  IBD_2 <- base::sum(liabilities$LTD[1:2])/2
  IBD_3 <- base::sum(liabilities$LTD[2:3])/2
  t <- -base::round(is$TAX/is$EBT,4)
  Kdn <- base::round(-is$INT/c(IBD_2,IBD_3),4) * (1-t)
  OWI_2 <- base::sum(equity[1:2,])/2
  OWI_3 <- base::sum(equity[2:3,])/2
  WD_2 <- IBD_2/(IBD_2+OWI_2)
  WE_2 <- OWI_2/(IBD_2+OWI_2)
  WD_3 <- IBD_3/(IBD_3+OWI_3)
  WE_3 <- OWI_3/(IBD_3+OWI_3)
  WACC_2 <- Ke[1] * WE_2 + Kdn[1] * WD_2
  WACC_3 <- Ke[2] * WE_3 + Kdn[2] * WD_3
  VALDIV <- -c(PAIDIV_1,PAIDIV_2) * (1+0.01)/(Ke-0.01)
  FCF <- (cfs$TCFOA + cfs$TCFIA - is$INT * (1-t))
  VALFCF <- FCF * (1+0.01)/(Ke-0.01)
  VAL <- c((VALDIV[1] + VALFCF[1])/2, (VALDIV[2] + VALFCF[2])/2)
  shares <- bs$COMSTK/parval
  CSO <- c((shares[1] + shares[2])/2, (shares[2] + shares[3])/2)
  EPS <- base::round(is$NI/CSO,4)
  is$EPS <- EPS
  SP <- base::round(VAL / c(shares[2], shares[3]),2)
  PER <- SP/EPS
  DPS <- c(-PAIDIV_1, -PAIDIV_2) / CSO
  DY <- base::round(DPS/SP,4)

  income_statement <- is

  market_data <- base::data.frame(
    DCLPREFDIV = 0,
    DCLCOMDIV = c(NA, -PAIDIV_1, -PAIDIV_2),
    Ke = c(NA,Ke),
    Kdn = c(NA,Kdn),
    WACC = base::round(c(NA, WACC_2, WACC_3),4),
    FCF = c(NA,FCF),
    PER = c(NA,PER),
    DPS = base::round(c(NA,DPS),4),
    DY = c(NA, DY),
    NCSO = shares,
    SP = c(NA,SP)
  )
  base::row.names(market_data) <- base::row.names(assets)


  assets <- assets |>
    tibble::rownames_to_column("date") |>
    tidyr::pivot_longer(cols = c("PPE", "PDE", "INV", "AR", "CASH"), names_to = "id", values_to = "amount")

  liabilities <- liabilities |>
    tibble::rownames_to_column("date") |>
    tidyr::pivot_longer(cols = c("AP", "UDR", "OAL", "LTD"), names_to = "id", values_to = "amount")

  equity <- equity |>
    tibble::rownames_to_column("date") |>
    tidyr::pivot_longer(cols = c("COMSTK", "APIC", "RE"), names_to = "id", values_to = "amount")

  income_statement <- income_statement |>
    tibble::rownames_to_column("date") |>
    tidyr::pivot_longer(cols = c("REV", "COS", "GM", "SGNA", "EBITDA", "DEPR", "EBIT", "INT", "EBT", "TAX", "NI", "EPS"), names_to = "id", values_to = "amount")

  cash_flow_statement <- cash_flow_statement |>
    tibble::rownames_to_column("date") |>
    tidyr::pivot_longer(cols = c("NI_CFS","CNCL_DEPR","CHG_AR","CHG_INV","CHG_PDE","CHG_AP","CHG_UDR","CHG_OAL","TCFOA","NET_CHG_PPE","TCFIA","CHG_LTD","CHG_PIC","PAIDIV","TCFFA","CASHVAR","CASHBEG","CASHEND"), names_to = "id", values_to = "amount")

  market_data <- market_data |>
    tibble::rownames_to_column("date") |>
    tidyr::pivot_longer(cols = c("DCLPREFDIV", "DCLCOMDIV", "Ke", "Kdn", "WACC","FCF","PER","DPS","DY","NCSO","SP"), names_to = "id", values_to = "amount")

  simulated_data <- dplyr::bind_rows(assets, liabilities, equity, income_statement, cash_flow_statement, market_data) |>
    dplyr::mutate(
      cik = company,
      reportDate = date
    ) |>
      dplyr::left_join(dplyr::select(acanva::definitions, id, label), by = "id") |>
      dplyr::select(cik, date, reportDate, id, label, amount)

  return(simulated_data)
}
