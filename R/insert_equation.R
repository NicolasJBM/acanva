#' @name insert_equation
#' @title Insert equation
#' @author Nicolas Mangin
#' @description Function formating and inserting an equation from a database of definitions.
#' @param acronym Character. English identifiers for the concept.
#' @param language Character. Lowercase ISO2 code for a language. 
#' @return Character. Formatted equation.
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stringr fixed
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_split
#' @importFrom tibble tibble
#' @importFrom tidyr unite
#' @export


insert_equation <- function(acronym = "ROE", language = "en") {
  
  id <- NULL
  term <- NULL
  label <- NULL
  
  if (language == "fr"){
    slctdefinitions <- dplyr::select(acanva::definitions, id = "id_fr", label = "label_fr", equation = "equation_fr")
    Where <- "Dans laquelle "
    is <- " est "
  } else {
    slctdefinitions <- dplyr::select(acanva::definitions, id, label, equation)
    Where <- "Where "
    is <- " is "
  }
  
  notion <- dplyr::filter(slctdefinitions, id == acronym)
  equation <- stringr::fixed(notion$equation[1])
  terms <- stringr::str_remove_all(equation, "times|frac\\{|sum\\{|overline\\{|\\=|\\+|\\-")
  terms <- stringr::str_replace_all(terms, "[^a-zA-Z_{}\\^]", " ")
  terms <- stringr::str_replace_all(terms, "\\s+", " ")
  terms <- base::trimws(terms)
  terms <- stringr::str_replace_all(terms, "\\}\\{", " ")
  terms <- base::unique(base::as.character(stringr::str_split(terms, "\\s+", simplify = TRUE)))
  terms <- stringr::str_remove_all(terms, "^\\{")
  terms <- stringr::str_remove_all(terms, "\\}\\}.$")
  terms <- stringr::str_remove_all(terms, "\\}.$")
  for (i in base::seq_len(base::length(terms))){
    if (!stringr::str_detect(terms[i], "\\{")) terms[i] <- stringr::str_remove(terms[i], "\\}$")
  }
  ids <- stringr::str_remove_all(terms, "[_{}]")
  terms <- tibble::tibble(id = terms, term = terms) |>
    dplyr::left_join(slctdefinitions, by = "id") |>
    dplyr::mutate(id = base::factor(id, levels = terms)) |>
    dplyr::arrange(id) |>
    dplyr::mutate(term = terms) |>
    dplyr::mutate(term = base::paste0("$", base::as.character(term), "$")) |>
    dplyr::filter(!base::is.na(label)) |>
    tidyr::unite("terms", term, label, sep = is)
  
  base::writeLines(base::paste0(
    "$$\n\\begin{aligned}\n",
    stringr::str_replace_all(stringr::str_remove_all(equation, "^\\$|\\$$"), "=", "& = "),
    '\n\\end{aligned}\n$$\n\n<span style="font-size: 0.6em;">*',
    Where, base::paste(terms$terms, collapse = ", "), ".*</span>"
  ))
}
