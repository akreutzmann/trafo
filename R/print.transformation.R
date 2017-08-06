#' Print object of type transformation
#' 
#' @param x an object of type transformation
#' #@keywords internal
#' @export

print.transformation <- function(x, ...){
cat(x$family, "Transformation \n")
cat("\nlambdahat: ", x$lambdahat)
if (x$method == "ml" | x$method == "reml") {
cat("\nloglike: ",-x$optmeas,"\n") 
} else if (x$method == "skew" | x$method == "pskew" ) {
cat("\nskewness: ",x$optmeas,"\n")
} else if (x$method == "div.ks" | x$method == "div.cvm" | 
           x$method == "div.kl") {
cat("\ndivergence: ",x$optmeas,"\n")
}  
 invisible(x)
}