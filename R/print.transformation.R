#' Print object of type transformation
#' 
#' @param trans an object of type transformation with the estimates
#' @keywords internal


print.transformation <- function(trans, ...){
  cat(trans$family, "Transformation")
  cat("\nlambdahat:\n")
  print(trans$lambdahat)
  cat("\nloglike:\n")
  print(trans$llike)
  invisible(trans)
}