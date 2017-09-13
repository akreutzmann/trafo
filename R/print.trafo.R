#' Print object of type trafo
#' 
#' @param x an object of type trafo.
#' @param ... other parameters that can be passed to the function.
#' @export

print.trafo <- function(x, ...){

 
  cat(x$family, "Transformation \n")
  if (inherits(x, "oneparam")) {
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
  }
  cat("\n")
  cat("Summary of transformed variables \n")
  print(summary(as.numeric(x$yt)))

   
 invisible(x)
}



#' Data frame with transformed variables
#' 
#' @param x an object of type trafo.
#' @param row.names	NULL or a character vector giving the row names for the 
#' data frame. Missing values are not allowed.
#' @param optional	logical. If TRUE, setting row names and converting column 
#' names (to syntactic names: see make.names) is optional. Note that all of R's 
#' base package as.data.frame() methods use optional only for column names 
#' treatment, basically with the meaning of 
#' data.frame(*, check.names = !optional)
#' @param model_obj an object of a fitted model. In this version only objects
#' of class lm can be included.
#' @param ... other parameters that can be passed to the function.
#' @export

as.data.frame.trafo <- function(x, row.names = NULL, optional = FALSE, ...) {
  
  formula <- NULL
  
  if (inherits(x$object, "lm")) {
    data <- x$object$model 
    transformed_dependent <- paste0(as.character(formula(x$object$terms)[2]), "t")
    data[, transformed_dependent] <- x$yt
    
    data <- as.data.frame(data, row.names = row.names, optional = optional, ...)
  } else if (inherits(x$object, "lme")) {
   data <- x$object$data
   transformed_dependent <- paste0(as.character(formula(x$object$terms)[2]), "t")
   data[, transformed_dependent] <- as.numeric(x$yt)
   
   data <- as.data.frame(data, row.names = row.names, optional = optional, ...)
  }
  return(data)
}
