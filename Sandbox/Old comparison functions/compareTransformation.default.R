#' Compare Transformation Default
#'
#' Compares families of transformations with respect to  sense of AIC, R.Squared, Skewness or Kurtosis; For positive y compares between Box-Cox, Dual and Manly transformations; For all values of y it compares between Yeo-Johnson, Manly, Modulus and Bickel-Doksum  
#' @param object a data.frame or a matrix, whose first column is the dependent variable and the rest of the columns are the independent variables.
#' @param \dots additional arguments to be passed to the estimation function; see docompareTransformation()
#' @return an object of class \code{transformation}; see docompareTransformation()
#' @keywords internal
#' @export
compareTransformation.default <- function(object,...){
  data <- object
  nc <- ncol(data)
  nr <- nrow(data)
  if (is.null(y <- data[,1]) || is.null(x <- data[,2:nc])) 
    stop("components Y and X must no be empty")  
  if (any(x[,1] != 1)) x <- cbind(rep(1,nr), x) 
  data <- data.frame(y = y, x = x)
  docompareTransformation(data,...)
}