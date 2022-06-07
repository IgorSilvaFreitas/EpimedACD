#' Decode treatment function
#'
#' This function can decode a 2 labels variable. The variable must be character
#'
#' @import dplyr
#'
#' @param data A data frame
#' @param col A column you wish to decode
#' @param newvar The name of the new column (it can be the same name of the original variable)
#' @param code1 The first label in use
#' @param code2 The second label in use
#' @param decode1 The label you want to replace the first coded label
#' @param decode2 The label to replace the remaining coded label
#'
#' @examples
#' treat(data, Gender, "Gender", "F", "M", "Female", "Male")
#'
#' @export
treat <- function(data, col, newvar, code1, code2, decode1, decode2){
  data <- data |>
    dplyr::mutate(!!newvar := factor({{col}},
                                     levels = c(code1, code2),
                                     labels = c(decode1, decode2)), .keep="unused")

  return(data)
}
