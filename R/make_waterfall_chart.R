#' @name make_waterfall_chart
#' @title Make waterfall charts
#' @author Nicolas Mangin
#' @description Make a waterfall chart to visualize changes leading from a beginning value to an ending value.
#' @param changes Tibble. Starting value, changes, and ending value.
#' @param steps Vector of integers. Rows in which their is an intermediary balance.
#' @param label_size Double. Size of labels
#' @param decimals Integer. Number of decimals.
#' @param negpar Logical Whether negative numbers should be between parentheses.
#' @param marks Character vector. Separators for thousands and decimals
#' @return ggplot2 Waterfall chart.
#' @importFrom dplyr case_when
#' @importFrom dplyr lag
#' @importFrom dplyr mutate
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 geom_rect
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 theme
#' @importFrom ggrepel geom_label_repel
#' @importFrom purrr map2_dbl
#' @export


make_waterfall_chart <- function(
  changes,
  steps = NA,
  label_size = 2.75,
  decimals = 0,
  negpar = TRUE,
  marks = c(",",".")
){
  
  amount <- NULL
  label <- NULL
  type <- NULL
  cumulating <- NULL
  cumulated <- NULL
  start <- NULL
  nbr <- NULL
  position <- NULL
  variations <- NULL
  
  base::stopifnot(
    "amount" %in% base::names(changes),
    "fillcolor" %in% base::names(changes),
    "fontcolor" %in% base::names(changes),
    "label" %in% base::names(changes)
  )
  
    beg <- 1
    end <- base::nrow(changes)
    
    changes <- changes |>
      dplyr::mutate(
        nbr = seq_along(amount),
        type = dplyr::case_when(
          amount >= 0 ~ "Increasing",
          amount < 0  ~ "Decreasing"
        )
      ) |>
      dplyr::mutate(type = dplyr::case_when(
        nbr %in% c(beg, steps, end) ~ "Balance",
        TRUE ~ type
      )) |>
      dplyr::mutate(
        cumulating = dplyr::case_when(nbr %in% steps ~ 0, TRUE ~ amount)
      ) |>
      dplyr::mutate(
        label = base::factor(label, levels = changes$label),
        type = base::factor(type, levels = c("Balance","Increasing","Decreasing")),
        cumulated = cumsum(cumulating)
      ) |>
      dplyr::mutate(lag = dplyr::lag(cumulated)) |>
      dplyr::mutate(
        start = dplyr::case_when(
          type == "Balance" ~ 0,
          type == "Increasing" ~ lag,
          TRUE ~ cumulated
        ),
        end = dplyr::case_when(
          type == "Balance" ~ amount,
          type == "Increasing" ~ cumulated,
          TRUE ~ lag
        )
      ) |>
      dplyr::mutate(
        position = purrr::map2_dbl(end, start, function(x,y) (x+y)/2),
        variations = dbl(amount, decimals = decimals, negpar = negpar, marks = marks)
      )
    
    changes |>
      ggplot2::ggplot(ggplot2::aes(x = label, xmin = nbr - 0.45, xmax = nbr + 0.45, ymin = end, ymax = start)) +
      ggplot2::geom_rect(fill = changes$fillcolor, color = changes$fontcolor) +
      ggplot2::theme_void() +
      ggplot2::theme(
        legend.position="none",
        panel.background = ggplot2::element_rect(fill = "white"),
        panel.grid.major = ggplot2::element_line(colour = "grey"),
        axis.text.x = ggplot2::element_text(angle = 60, vjust = 0.5, hjust=0.5)
      ) +
      ggrepel::geom_label_repel(
        ggplot2::aes(x = label, y = position, label = variations),
        size = label_size, color = changes$fontcolor, fill = changes$fillcolor
      )
}
