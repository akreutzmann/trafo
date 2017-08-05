#' Print object of type transformation
#' 
#' @param trans an object of type transformation with the estimates
#' #@keywords internal
#' @export

print.transformation <- function(trans, ...){
cat(trans$family, "Transformation \n")
cat("\nlambdahat: ", trans$lambdahat)
if (trans$method == "ml" | trans$method == "reml") {
cat("\nloglike: ",trans$optmeas,"\n") 
} else if (trans$method == "skew" | trans$method == "pskew" ) {
cat("\nskewness: ",trans$optmeas,"\n")
} else if (trans$method == "div.ks" | trans$method == "div.cvm" | 
           trans$method == "div.kl") {
cat("\ndivergence: ",trans$optmeas,"\n")
}  
 invisible(trans)
}