#' Print object of type trafo_mod
#' 
#' @param x an object of type trafo_mod.
#' @param ... other parameters that can be passed to the function.
#' @export

print.trafo_mod <- function(x, ...){
  cat("Applied transformation \n")
  cat("Transformation: ",x$trafo," \n")
  if (x$trafo != "log") {
    cat("Estimation method: ", x$method, " \n")
  }
  cat("Optimal Parameter: ", x$lambdahat, " \n")
  cat("\n")
  cat("Transformed model \n")
  print(x$trafo_mod)
 invisible(x)
}