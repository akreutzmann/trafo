#' Print object of type trafo_mod
#' 
#' @param x an object of type trafo_mod.
#' @param ... other parameters that can be passed to the function.
#' @export

print.compare_trafo <- function(x, ...){
  cat("Applied transformations \n")
  cat("Transformations: ",x$trafos[[1]], "and",x$trafos[[2]],"\n")
  cat("Estimation methods: ", x$method[[1]], "and", x$method[[2]], " \n")
  cat("Optimal Parameters: ", x$lambdahat[[1]], "and", x$lambdahat[[2]]," \n")
  cat("\n")
  if (inherits(x$trafoOne, "lm")) {
  cat("Model using ",x$trafos[[1]], " with ",x$method[[1]], "\n")
  cat("\n")
  cat("Call: ", paste(deparse(x$trafoOne$call), sep = "\n", collapse = "\n") , "\n")
  cat("formula = ",x$trafoOne$formula, "\n")
  cat("\n")
  cat("Coefficients: \n")
  print(format(x$trafoOne$coefficients, digits = max(3L, getOption("digits") - 3L)),
        print.gap = 2L, quote = FALSE)
  #print(x$trafo_mod)
  cat("\n")
  cat("Model using ",x$trafos[[2]], " with ",x$method[[2]], "\n")
  cat("\n")
  cat("Call: ", paste(deparse(x$trafoTwo$call), sep = "\n", collapse = "\n") , "\n")
  cat("formula = ",x$trafoTwo$formula, "\n")
  cat("\n")
  cat("Coefficients: \n")
  print(format(x$trafoTwo$coefficients, digits = max(3L, getOption("digits") - 3L)),
        print.gap = 2L, quote = FALSE)
  } else if (inherits(x$trafoOne, "lme")) {
    cat("Model using ",x$trafos[[1]], " with ",x$method[[1]], "\n")
    cat("\n")
    print(x$trafoOne)
    cat("\n")
    cat("Model using ",x$trafos[[2]], " with ",x$method[[2]], "\n")
    cat("\n")
    print(x$trafoTwo)
  }
 invisible(x)
}