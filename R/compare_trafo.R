#' Compares transformed linear models
#'
#' Function \code{compare_trafo} fits linear models with transformed dependent 
#' variable. 
#'
#' @param object an object of type lm or lme with the model to transform
#' @param trafos a list of trafo objects based on the same model as given 
#' in object.
#' @param std logical. If TRUE, the transformed model is returned based on the 
#' standardized transformation.
#' @return an object of class \code{compare_trafo}.
#' @export

compare_trafo <- function(object, trafos, std) {
  
  trafoOne <- trafos[[1]]
  trafoTwo <- trafos[[2]]
  if (inherits(trafoOne, "woparam")) {
    paramOne <- "woparam"
  } else if (inherits(trafoOne, "oneparam")) {
    paramOne <- "oneparam"
  }
  if (inherits(trafoTwo, "woparam")) {
    paramTwo <- "woparam"
  } else if (inherits(trafoTwo, "oneparam")) {
    paramTwo <- "oneparam"
  }
  
  trafoOne_mod <- get_modelt(object = object, trans_mod = trafoOne, std = std)
  trafoTwo_mod <- get_modelt(object = object, trans_mod = trafoTwo, std = std)
  
  compare_out <- list(trafoOne = trafoOne_mod,
                      trafoTwo = trafoTwo_mod,
                      trafos = c(trafoOne$family, trafoTwo$family), 
                      method = c(trafoOne$method, trafoTwo$method), 
                      lambdahat = c(trafoOne$lambdahat, 
                                    trafoTwo$lambdahat), 
                      std = std, 
                      param = c(paramOne, paramTwo))
  
  class(compare_out) <- "compare_trafo"
  
  return(compare_out)
  
}