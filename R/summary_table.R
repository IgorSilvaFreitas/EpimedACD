#' Custom summary data frame
#'
#' This function will create a summary table
#'
#' @param datavar A data frame and it's column of interest
#'
#' @examples
#' summary_table(data$Gender)
#'
#' @export
summary_table <- function(datavar){
  Frequency <- table(datavar)

  table<- rbind(Frequency)
  return(table)
}
