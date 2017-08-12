#' Print object of type trafo
#' 
#' @param x an object of type trafo.
#' @param ... other parameters that can be passed to the function.
#' @export

print.trafo <- function(x, ...){
  cat(x$family, "Transformation \n")
  cat("\n")
  cat("Estimation method: ", x$method, " \n")
  cat("Optimal parameter: ", x$lambdahat, " \n")
  if (x$method == "ml" | x$method == "reml") {
    cat("Loglike: ",x$measoptim,"\n") 
  } else if (x$method == "skew" | x$method == "pskew" ) {
    cat("Skewness: ",x$measoptim,"\n")
  } else if (x$method == "div.ks" | x$method == "div.cvm" | 
             x$method == "div.kl") {
    cat("Divergence: ", x$measoptim,"\n")
  } 
 invisible(x)
}