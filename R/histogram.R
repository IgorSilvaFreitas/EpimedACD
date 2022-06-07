#' Custom histogram plot
#'
#' This function will create a custom histogram
#'
#' @import dplyr
#' @import ggplot2
#'
#' @param data A data frame
#' @param var A variable to the data be filtered from
#' @param x The variable to build the histogram
#' @param filter The label to filter the var parameter
#' @param xlab X axis title
#' @param ylab Y axis title
#' @param title histogram title
#'
#' @examples
#' histogram(data, "Gender", "Age", "Female")
#'
#' @export
histogram <- function(data, var, x, filter, xlab="", ylab="", title=""){
  data <- filter(data, data[[var]] == filter)
  h1 <- ggplot(data, mapping = aes(x = data[[x]])) +
    geom_histogram(fill="orange", col='White')+
    geom_vline(xintercept = mean(data[[x]])) +
    labs(y= ylab, x= xlab,
         title= title) +
    theme_classic()+ theme(legend.position = "none", plot.title = element_text(hjust = 0.5,
    size=14, face="bold"), text = element_text(size=15))

  return(h1)
}
