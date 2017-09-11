#' Summary for models with untransformed and transformed dependent variable
#'
#' Information about the transformed data and model and components of an 
#' transformation object are extracted. The returned object is suitable for 
#' printing with the print.summary.transformation method.
#' 
#' @param object an object of type \code{trafo_mod}
#' @param ... additional arguments that are not used in this method
#' @return an object of class \code{summary.transformation}
#' @export

summary.compare_trafo <- function(object, ...) {
  
  formula <- NULL
  
  trafos <- object$trafos
  method <- object$method
  lambdahat <- object$lambdahat
  param <- object$param
  
  modOne <- object$trafoOne
  modOne$name <- trafos[[1]]
  modTwo <- object$trafoTwo
  modTwo$name <- trafos[[2]]
  
  sums <- summary_internal(modOne = modOne, modTwo = modTwo, 
                           compare = TRUE, std = object$std)
  
  
  sum_out <- list(trafo = trafos, 
                  method = method, 
                  lambdahat = lambdahat, 
                  trafoOne_sum  = sums$modOne_sum, 
                  trafoTwo_sum  = sums$modTwo_sum,
                  std = object$std)
  
  class(sum_out) <- "summary.compare_trafo"
  
  return(sum_out)
}



#' Print summary trafo
#'
#' prints objects to be shown in the summary function for objects of 
#' type \code{trafo_mod}
#' @param x an object of type \code{summary.trafo_mod}
#' @param ... additional arguments that are not used in this method
#' @export

print.summary.compare_trafo <- function(x, ...) {
  
  cat("Summary of model with ", x$trafo[[1]], "\n")
  print(x$trafoOne_sum)
  cat("\n")
  cat("Summary of model with ", x$trafo[[2]], "\n")
  print(x$trafoTwo_sum)
  cat("\n")
  if (x$std == TRUE) {
    cat("Note that the standard errors are missing due to the lack of methods 
      for correct standard errors in transformed models using standardized
        transformation. \n")
  }
  invisible(x)
}




