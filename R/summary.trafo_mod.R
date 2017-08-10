#' Summary for models with untransformed and transformed dependent variable
#'
#' Information about the transformed data and model and components of an 
#' transformation object are extracted. The returned object is suitable for 
#' printing with the print.summary.transformation method.
#' 
#' @param x an object of type \code{trafo_mod}
#' @param ... additional arguments that are not used in this method
#' @return an object of class \code{summary.transformation}
#' @keywords internal
#' @export

summary.trafo_mod <- function(object, ...) {
  
  trafo <- object$trafo
  method <- object$method
  lambdahat <- object$lambdahat
  
  
  if (class(object$orig_mod) == "lm") {
    # Summary of original model
    orig_sum <- summary(object$orig_mod)
    
    
    # Summary of transformed model
    trafo_sum <- summary(object$trafo_mod) 
    trafo_sum$coefficients <- as.matrix(trafo_sum$coefficients[, 1])
    colnames(trafo_sum$coefficients) <- c("Estimate")
  } else if (class(object$orig_mod) == "lme") {
    # Summary of original model
    orig_sum <- summary(object$orig_mod)
    
    
    # Summary of transformed model
    trafo_sum <- summary(object$trafo_mod) 
    trafo_sum$coefficients <- as.matrix(trafo_sum$coefficients[, 1])
    colnames(trafo_sum$coefficients) <- c("Estimate")
    
  }
  
  sum_out <- list(trafo = trafo, 
                  method = method, 
                  lambdahat = lambdahat, 
                  orig_sum = orig_sum, 
                  trafo_sum = trafo_sum)
  
  class(sum_out) <- "summary.trafo_mod"
  
  return(sum_out)
}



#' Print summary trafo
#'
#' prints objects to be shown in the summary function for objects of 
#' type \code{trafo_mod}
#' @param x an object of type \code{summary.trafo_mod}
#' @keywords internal
#' @export

print.summary.trafo_mod <- function(x, ...) {
  
  cat("Applied transformation \n")
  cat("Transformation: ",x$trafo," \n")
  cat("Estimation method: ", x$method, " \n")
  cat("Optimal Parameter: ", x$lambdahat, " \n")
  cat("\n")
  cat("Summary of transformed model \n")
  print(x$trafo_sum)
  cat("Note that the standard errors are missing due to the lack of methods 
      for correct standard errors in transformed models. \n")
  cat("\n")
  cat("Press [enter] to continue or type in [q] to quit" )
  line <- readline()
  if(substr(line, 1, 1) != "q" & substr(line, 1, 1) != "Q") {
    cat("Summary of original model \n")
    print(x$orig_sum) 
  }
  invisible(x)
}




