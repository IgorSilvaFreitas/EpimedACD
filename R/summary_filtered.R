#' Custom summary data frame
#'
#' This function will create a summary data frame by categories
#'
#' @import dplyr
#'
#' @param data A data frame
#' @param var A variable to the data be filtered from
#' @param x The variable of interest
#' @param row.names A vector with row names ordered
#'
#' @examples
#' summary_filtered(data, Gender, "Age", c("Male","Female"))
#'
#' @export
summary_filtered <- function(data, var, x, row.names){
  summary <- data |>
    dplyr::group_by({{var}}) |>
    dplyr::summarise(Min = min({{x}}),
                     Median = median({{x}}),
                     Mean = mean({{x}}),
                     Max = max({{x}}))


  table_summary <- summary[,-1]
  rownames(table_summary) <- row.names
  return(table_summary)
}
