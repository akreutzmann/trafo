#' Print object of type trafo_mod
#' 
#' @param x an object of type trafo_mod.
#' @param ... other parameters that can be passed to the function.
#' @export

print.trafo_mod <- function(x, ...){
  # cat("Applied transformation \n")
  # cat("Transformation: ",x$trafo," \n")
  # if (x$trafo != "log") {
  #   cat("Estimation method: ", x$method, " \n")
  # }
  # cat("Optimal Parameter: ", x$lambdahat, " \n")
  # cat("\n")
  cat("Untransformed model \n")
  print(x$orig_mod)
  cat("Transformed model: ", x$trafo, "transformation \n")
  cat("\n")
  cat("Call: ", paste(deparse(x$trafo_mod$call), sep = "\n", collapse = "\n") , "\n")
  cat("formula = ",x$trafo_mod$formula, "\n")
  cat("\n")
  cat("Coefficients: \n")
  print(format(x$trafo_mod$coefficients, digits = max(3L, getOption("digits") - 3L)),
        print.gap = 2L, quote = FALSE)
  #print(x$trafo_mod)
 invisible(x)
}