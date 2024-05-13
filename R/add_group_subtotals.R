#' @name add_group_subtotals
#' @title Add group subtotals
#' @author Nicolas Mangin
#' @description Add group subtotals to a dataframe or tibble.
#' @param x Dataframe or tibble. Table to which subtotals will be added based on the variable labelled "group".
#' @return Dataframe with subtotals added after each group.
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise_if
#' @importFrom dplyr select
#' @importFrom dplyr bind_rows
#' @export


add_group_subtotals <- function(x){
  group <- NULL
  base::stopifnot("group" %in% base::names(x))
  x <- dplyr::mutate(x, group = base::factor(group, levels = base::unique(x$group)))
  x <- base::split(x, x$group)
  x <- base::lapply(x, function(y){
    z <- y |>
      dplyr::group_by(group) |>
      dplyr::summarise_if(base::is.numeric, base::sum, na.rm = TRUE)
    y <- dplyr::select(y, -group)
    base::names(z) <- base::names(y)
    dplyr::bind_rows(y,z)
  })
  dplyr::bind_rows(x)
}
