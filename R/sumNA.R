#' Sum missing data
#'
#' This function will return the number of missing data on a certain column of a data frame
#'
#'
#' @param variable A data frame and it's column
#'
#' @examples
#' sum_na(data$Gender)
#'
#' @export
sum_na <- function(variable){
  na <- sum(is.na(variable))
  return(na)
}
