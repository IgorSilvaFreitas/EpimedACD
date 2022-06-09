#' Custom markdown table
#'
#' This function will create a custom model coefficients table
#'
#' @import kableExtra
#'
#' @param dmodel A logit model
#' @param title Title of the table
#'
#' @examples
#' coef_table(model)
#'
#' @export
coef_table <- function(model, title=""){

  model <- summary(model)
  coef <- model[["coefficients"]]
  chance <- exp(coef[,"Estimate"])
  chance
  coef <- cbind(coef[,1], chance, coef[,2:4])
  EpimedACD::rmd_table(round(coef, 4), title)
}
