#' Print object of type transformation
#' 
#' @param x an object of type trafo.
#' @param ... other parameters that can be passed to the function.
#' #@keywords internal
#' @export

print.trafo <- function(x, ...){
cat(x$family, "Transformation \n")
cat("\nlambdahat: ", x$lambdahat)
if (x$method == "ml" | x$method == "reml") {
cat("\nloglike: ",x$measoptim,"\n") 
} else if (x$method == "skew" | x$method == "pskew" ) {
cat("\nskewness: ",x$measoptim,"\n")
} else if (x$method == "div.ks" | x$method == "div.cvm" | 
           x$method == "div.kl") {
cat("\ndivergence: ",x$measoptim,"\n")
}  
 invisible(x)
}