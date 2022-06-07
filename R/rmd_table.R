#' Custom markdown table
#'
#' This function will create a custom table
#'
#' @import kableExtra
#'
#' @param datavar A data frame
#' @param title Title of the table
#'
#' @examples
#' summary_table(data$Gender)
#'
#' @export
rmd_table <- function(data, title=""){
  data |>
    kableExtra::kable(caption = title)  |>
    kableExtra::kable_classic(full_width = F)  |>
    kableExtra::kable_styling(latex_options = "HOLD_position")
}
