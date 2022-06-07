#' Custom proportion bar plot
#'
#' This function will create a custom proportion bar plot
#'
#' @import ggplot2
#' @import dplyr
#' @import scales
#'
#' @param data A data frame
#' @param x X axis variable
#' @param var Filter variable
#' @param xlab X axis title
#' @param ylab Y axis title
#' @param title boxplot title
#'
#' @examples
#' bar_prop_plot(data, Gender, IsWorking)
#'
#' @export
bar_prop_plot <- function(data, x, var, xlab = "", ylab = "", title = ""){
  data |>
    dplyr::group_by({{x}}, {{var}}) |>
    dplyr::summarize(n = dplyr::n()) |>
    dplyr::mutate(pct = n/sum(n),
                  rotulo = scales::percent(pct)) |>
    dplyr::arrange({{var}}) |>
    ggplot2::ggplot(mapping = ggplot2::aes(x = {{x}},
                                           y = pct,
                                           fill = {{var}})) +
    ggplot2::geom_bar(stat = "identity",
                      position = "fill") +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, .2),
                                label = scales::percent) +
    ggplot2::geom_text(ggplot2::aes(label = rotulo),
                       size = 6,
                       position = ggplot2::position_stack(vjust = 0.5))  +
    ggplot2::scale_fill_brewer(palette = "Set1") +
    ggplot2::labs(x = xlab, y = ylab,
                  title = title) + ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "right")  +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size=14, face="bold"),
                   text = ggplot2::element_text(size=15))
}
