#' Custom boxplot
#'
#' This function will create a custom boxplot
#'
#' @import ggplot2
#'
#' @param data A data frame
#' @param x A variable
#' @param y Another variable
#' @param xlab X axis title
#' @param ylab Y axis title
#' @param title boxplot title
#'
#' @examples
#' bxplt(data, "Gender", "Age")
#'
#' @export
bxplt <- function(data, x, y, xlab = "", ylab = "", title = ""){
  data |>
    ggplot2::ggplot(mapping = ggplot2::aes(y = {{y}} ,
                                           x = {{x}})) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(x = xlab,
                  y = ylab,
                  title = title)+
    ggplot2::theme_classic()+
    ggplot2::theme(legend.position = "none", plot.title = ggplot2::element_text(hjust = 0.5,
                   size=14, face="bold"), text = ggplot2::element_text(size=15))
}
