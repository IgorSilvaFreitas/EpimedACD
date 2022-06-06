#' Decode treatment function
#'
#' This function can decode a 2 labels variable. The variable must be character
#'
#'
#' @param data A data frame
#' @param col A column you wish to decode
#' @param code1 The first label in use
#' @param code2 The second label in use
#' @param decode1 The label you want to replace the first coded label
#' @param decode2 The label to replace the remaining coded label
#'
#' @examples
#' treat(data, "Gender", "F", "M", "Female", "Male")
#'
#' @export
treat <- function(data, col, code1, code2, decode1, decode2){
  for (i in 1:nrow(data)) {
    if(is.na(data[i,col])==TRUE){
    }else{
      if(data[i,col]==code1){
        data[i,col] <- decode1
      }else{
        if(data[i,col]==code2){
          data[i,col] <- decode2
        }
      }
    }
  }

  return(data)
}
