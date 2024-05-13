#' @name insert_definition
#' @title Insert definition
#' @author Nicolas Mangin
#' @description Function formating and inserting a definition from a database of definitions.
#' @param acronym Character. English identifiers for the concept.
#' @param language Character. Lowercase ISO2 code for a language. 
#' @return Character. Formatted definition.
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @export

insert_definition <- function(acronym = "ROE", language = "en") {
  
  id <- NULL
  definition <- NULL
  label <- NULL
  
  definitions <- acanva::definitions
  
  if (language == "fr"){
    slctdefinitions <- dplyr::select(definitions, id = "id_fr", label = "label_fr", definition = "definition_fr")
  } else {
    slctdefinitions <- dplyr::select(definitions, id, label, definition)
  }
  
  notion <- dplyr::filter(slctdefinitions, id == acronym)
  
  base::writeLines(base::paste0(
    "> ", notion$definition[1]
  ))
}
