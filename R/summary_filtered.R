#' Custom histogram plot
#'
#' This function will create a custom histogram
#'
#' @import dplyr
#'
#' @param data A data frame
#' @param var A variable to the data be filtered from
#' @param x The variable of interest
#' @param row.names A vector with row names ordered
#'
#' @examples
#' histogram(data, Gender, "Age", c("Male","Female"))
#'
#' @export
summary_filtered <- function(data, var, x, row.names){
  summary <- data |>
    dplyr::group_by({{var}}) |>
    dplyr::summarise(Min = min({{x}}),
                     Median = median({{x}}),
                     Mean = mean({{x}}),
                     Max = max({{x}}))


  tabel_summary <- summary[,-1]
  rownames(tabel_summary) <- row.names
  return(tabel_summary)
}
