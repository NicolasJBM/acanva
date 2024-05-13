#' @name simulate_statements_ini
#' @title Create income statement and average balance sheet
#' @author Nicolas Mangin
#' @description Create an income statement and an average balance sheet leading to desired ratios
#' @param ROE Numeric.
#' @param DFL Numeric.
#' @param NOMR Numeric.
#' @param NOPAT Numeric.
#' @param t Numeric.
#' @param SOD Numeric vector equal to 1
#' @param Kd Numeric.
#' @param NCOA Numeric.
#' @param DRO Numeric.
#' @param DIO Numeric.
#' @param DPO Numeric.
#' @param DDO Numeric.
#' @param CASH Numeric.
#' @param PIC Numeric.
#' @return List.
#' @importFrom tibble tibble
#' @importFrom dplyr mutate_all
#' @export



simulate_statements_ini <- function(
    ROE = 0.20,
    DFL = 0.04,
    NOMR = 0.10,
    NOPAT = 365*100,
    t = 0.25,
    SOD = c(0.2,0.5,0.3), # Cost of Sales, Operating expenses, Depreciation
    Kd = 0.05,
    NCOA = 0.9,
    DRO = 20,
    DIO = 60,
    DPO = 45,
    DDO = 0,
    CASH = 5000,
    PIC = 90000
){

  base::stopifnot(base::sum(SOD) == 1)

  Kdn <- Kd * (1-t)
  ROIC <- ROE - DFL
  ICTR <- ROIC/NOMR
  REV <- NOPAT/NOMR
  IC <- REV/ICTR
  ICS <- DFL / (ROIC - Kdn)
  SE <- IC / (1+ICS)
  COMSTK <- PIC * 0.1
  APIC <- PIC - COMSTK
  RE <- SE - COMSTK - APIC
  LTD <- IC - SE
  EBIT <- NOPAT/(1-t)
  SGNA <- REV - NOPAT/(1-t)
  tmpSGNA <- SGNA * SOD
  COS <- base::round(tmpSGNA[1],0)
  DEPR <- base::round(tmpSGNA[3],0)
  SGNA <- SGNA - COS - DEPR
  GM <- REV - COS
  GMR <- GM/REV
  EBITDA <- GM - SGNA
  INT <- LTD * Kd
  EBT <- EBIT - INT
  TAX <- EBT * t
  NI <- EBT - TAX
  IC <- REV/ICTR
  NCOA <- IC * NCOA
  COS <- REV - GM
  AR <- DRO * REV / 365
  INV <- DIO * COS / 365
  PDE <- REV * 0.01
  AP <- DPO * COS / 365
  UDR <- DDO * REV / 365

  OAL <- NCOA + PDE + INV + AR + CASH - SE - LTD - AP - UDR

  is <- tibble::tibble(
    REV = REV,
    COS = COS,
    GM = GM,
    SGNA = SGNA,
    EBITDA = GM - SGNA,
    DEPR = DEPR,
    EBIT = EBIT,
    INT = INT,
    EBT = EBT,
    TAX = TAX,
    NI = NI
  ) |>
    dplyr::mutate_all(base::round, digits = 2)

  bs <- tibble::tibble(
    CASH = CASH,
    AR = AR,
    INV = INV,
    PDE = PDE,
    PPE = NCOA,
    AP = AP,
    UDR = UDR,
    OAL = OAL,
    LTD = LTD,
    COMSTK = COMSTK,
    APIC = APIC,
    RE = RE
  ) |>
    dplyr::mutate_all(base::round, digits = 2)

  bs$CASH <- bs$COMSTK + bs$APIC + bs$RE + bs$LTD + bs$OAL + bs$UDR + bs$AP - bs$PPE - bs$PDE - bs$INV - bs$AR

  return(base::list(income_statement = is, balance_sheet = bs))
}
