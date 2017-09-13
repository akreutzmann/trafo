#' Diagnostics for models with untransformed and transformed dependent variable
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

diagnostics.trafo_mod <- function(object, ...) {
  
  formula <- NULL
  
  trafo <- object$trafo
  method <- object$method
  lambdahat <- object$lambdahat
  if (inherits(object, "woparam")) {
  param <- "woparam"
  } else if (inherits(object, "oneparam")) {
    param <- "oneparam"
  }
  modOne <- object$orig_mod
  modOne$name <- "Untransformed model"
  modTwo <- object$trafo_mod
  modTwo$name <- "Transformed model"
  
  diagnose <- diagnostics_internal(modOne = modOne, modTwo = modTwo)
  
  diagnose_out <- list(trafo = trafo, 
                       method = method, 
                       lambdahat = lambdahat, 
                       param = param, 
                       std = object$std,
                       norm_resid = diagnose$norm_resid, 
                       norm_ranef = diagnose$norm_ranef, 
                       hetero = diagnose$hetero)

  class(diagnose_out) <- "diagnostics.trafo_mod"
  
  return(diagnose_out)
}



#' Print diagnostics trafo
#'
#' prints objects to be shown in the summary function for objects of 
#' type \code{trafo_mod}
#' @param x an object of type \code{diagnostics.trafo_mod}
#' @param ... additional arguments that are not used in this method
#' @export

print.diagnostics.trafo_mod <- function(x, ...) {
  # 
  cat("Diagnostics: Untransformed vs transformed model \n")
  cat("\n")
  cat("Transformation: ",x$trafo," \n")
  if (x$param == "oneparam") {
     cat("Estimation method: ", x$method, " \n")
     cat("Optimal Parameter: ", x$lambdahat, " \n")
   }
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
  if (!is.null(x$hetero)) {
    cat("\n")
    cat("Heteroscedasticity:\n")
    print(x$hetero)
  }
  invisible(x)
}




