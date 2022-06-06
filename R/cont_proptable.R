#' Creates a contingency table
#'
#' This function will return a contingency table of column proportion from 2 discrete variables
#'
#'
#' @param col1 A data frame and it's first column
#' @param col2 A data frame and it's second column
#'
#' @examples
#' cont_prop.table(data$variable1, data$variable2)
#'
#' @export
cont_prop.table <- function(col1, col2){
  cont <- ftable(prop.table(table(col1, col2),2))
  return(cont)
}
