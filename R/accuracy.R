#' logit model accuracy
#'
#' This function will calculate the accuracy of a logit model
#'
#' @import dplyr
#'
#' @param data A data frame
#' @param y The variable of interest
#' @param prob The predicted probability column
#' @param p1 The minimum probability to consider label1 true
#' @param p2 The maximum probability to consider label1 true
#' @param filter A label of interest to filter from y
#' @param label1 First label to compare with y labels
#' @param label2 Second label to compare with y labels
#'
#' @examples
#' accuracy(data = data, y = "y", prob = "x", p1 = 0.5, filter = "label1", label1 = "label1", label2 = "label2")
#'
#' @export
accuracy <- function(data, y, prob, p1, p2=1, filter, label1, label2){

  data <- dplyr::filter(data, data[[y]] == filter)

  ac <- mean(ifelse(data[[prob]] > p1 & data[[prob]] <= p2, label1, label2) == data[[y]])

  return(ac)
}
