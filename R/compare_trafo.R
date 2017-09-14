#' Compares transformed linear models
#'
#' Function \code{compare_trafo} compares linear models with transformed 
#' dependent variable. 
#'
#' @param object an object of type lm
#' @param trafos a list of \code{trafo} objects based on the same model given 
#' in object.
#' @param std logical. If TRUE, the transformed models are returned based on the 
#' standardized transformation.
#' @return an object of class \code{compare_trafo}.
#' @examples
#' # Load data
#' data("cars", package = "datasets")
#' 
#' # Fit linear model
#' lm_cars <- lm(dist ~ speed, data = cars)
#' 
#' # Transform with Bickel-Doksum transformation
#' bd_trafo <- bickeldoksum(object = lm_cars, plotit = FALSE)
#' 
#' # Transform with Box-Cox transformation
#' bc_trafo <- boxcox(object = lm_cars, method = "skew", plotit = FALSE)
#' 
#' # Compare transformed models
#' compare_trafo(object = lm_cars, trafos = list(bd_trafo, bc_trafo), 
#' std = FALSE)
#' @export

compare_trafo <- function(object, trafos, std) {
  
  check_compare_trafo(object = object, trafos = trafos, std = std)
  
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