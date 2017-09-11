#' Diagnostics for two differently transformed models
#'
#' Information about the transformed data and model and components of an 
#' transformation object are extracted. The returned object is suitable for 
#' printing with the print.summary.transformation method.
#' 
#' @param object an object of type \code{compare_trafo}
#' @param ... additional arguments that are not used in this method
#' @return an object of class \code{summary.transformation}
#' @importFrom moments skewness kurtosis
#' @importFrom lmtest bptest
#' @export

diagnostics.compare_trafo <- function(object, ...) {
  
  formula <- NULL
  
  trafos <- object$trafos
  method <- object$method
  lambdahat <- object$lambdahat
  param <- object$param
  
  modOne <- object$trafoOne
  modOne$name <- trafos[[1]]
  modTwo <- object$trafoTwo
  modTwo$name <- trafos[[2]]
  
  diagnose <- diagnostics_internal(modOne = modOne, modTwo = modTwo)
  
  diagnose_out <- list(trafo = trafos, 
                       method = method, 
                       lambdahat = lambdahat, 
                       param = param, 
                       std = object$std,
                       norm_resid = diagnose$norm_resid, 
                       norm_ranef = diagnose$norm_ranef, 
                       hetero = diagnose$hetero)

  class(diagnose_out) <- "diagnostics.compare_trafo"
  
  return(diagnose_out)
}



#' Print diagnostics trafo
#'
#' prints objects to be shown in the summary function for objects of 
#' type \code{trafo_mod}
#' @param x an object of type \code{diagnostics.trafo_mod}
#' @param ... additional arguments that are not used in this method
#' @export

print.diagnostics.compare_trafo <- function(x, ...) {
  # 
  cat("Diagnostics of two transformed models \n")
  cat("\n")
  cat("Transformations: ",x$trafo[[1]], "and",x$trafo[[2]],"\n")
  cat("Estimation methods: ", x$method[[1]], "and", x$method[[2]], " \n")
  cat("Optimal Parameters: ", x$lambdahat[[1]], "and", x$lambdahat[[2]]," \n")
  cat("\n")
  cat("Residual diagnostics:\n")
  
  cat("\n")
  cat("Normality:\n")
  cat("Pearson residuals:\n")
  print(x$norm_resid)
  if (!is.null(x$norm_ranef)) {
    cat("Standardized random effects:\n")
    print(x$norm_ranef) 
  }
  cat("\n")
  cat("Heteroscedasticity:\n")
  print(x$hetero)
  cat("\n")
  invisible(x)
}




