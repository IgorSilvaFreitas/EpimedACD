#' Logit model
#'
#' This function will return a logit model
#'
#'
#' @param data A data frame to train the model
#' @param formula A data frame and it's second column
#'
#' @examples
#' logit_model(data, y ~ x1 + ... + xn)
#'
#' @export
logit_model <- function(data, formula){

  # creating a generalized linear model
  model <- glm(formula,
               data = data, family = binomial("logit"))

  return(model)
}
