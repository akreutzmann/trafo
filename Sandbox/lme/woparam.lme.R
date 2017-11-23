#' Transformations without parameter for linear models
#'
#' The function transforms the dependent variable of a linear model using 
#' different transformations without parameter. 
#'
#' @param object an object of type lm. 
#' @param trafo character that determines the transformation.
#' @param custom_trafo a function that specifies a transformation without 
#' paramater that needs to be estimated or given.
#' @param ... other parameters that can be passed to the function.
#' @return an object of class \code{trafo}.
#' @keywords internal


woparam.lme <- function(object, trafo, custom_trafo = NULL, ...) {
  
  check_woparam(trafo = trafo, custom_trafo = custom_trafo)
  
  # Get model variables: dependent variable y and explanatory variables x
  formula <- formula(object)
  rand_eff <- names(object$coefficients$random)
  data <- object$data
  x <- model.matrix(formula, data = object$data)
  y <- as.matrix(object$data[paste(formula[2])])
  
  # For saving returns
  ans <- list()
  
  if (trafo == "reciprocal") {
    ans$yt <- Reciprocal(y = y)
    ans$family <- "Reciprocal"
  } else if (trafo == "neglog") {
    ans$yt <- neg_log(y = y)
    ans$family <- "Neglog"
  } else if (trafo == "glog") {
    ans$yt <- g_log(y = y)
    ans$family <- "Glog"
  } else if (trafo == "custom") {
    custom_func <- custom_trafo[[1]]
    ans$yt <- custom_func(y = y)
    ans$family <- names(custom_trafo)
  }
  
  ans$lambdavector <- NULL
  ans$measvector <- NULL   
  ans$zt <- NULL         
  ans$method <- NULL      
  ans$lambdahat <- NULL 
  ans$measoptim <- NULL    
  
  # Save estimation method
  ans$method <- NULL
  
  # Save optimal transformation parameter and corresponding statistics depending
  # on the estimation method
  ans$lambdahat <- NULL
  ans$measoptim <- NULL
  
  # Get transformed model
  #ans$modelt <- get_modelt(object = object, trans_mod = ans, std = FALSE)
  ans$object <- object
  
  # New class trafo
  class(ans) <- c("trafo", "woparam")
  ans
  
}