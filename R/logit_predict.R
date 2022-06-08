#' Model predictions
#'
#' This function will return the data frame with an extra column, which is the predictions
#'
#'
#' @param data A data frame
#' @param model A model
#'
#' @examples
#' logit_predict(data, y ~ x1 + ... + xn)
#'
#' @export
logit_predict <- function(data, model){
  ## Calculating probability
  prob <- model |> predict(data, type = "response")
  data$Predict <- prob
  return(data)
}
