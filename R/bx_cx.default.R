#' Box-Cox Default
#'
#' Box-Cox estimation
#' @param object a data.frame or a matrix, whose first column is the dependent variable and the rest of the columns are the independent variables.
#' @param \dots additional arguments to be passed to the estimation function;  see bcxEst()
#' @return an object of class \code{transformation}; see bcxEst()
#' @keywords internal
#' @export
bx_cx.default <- function(object, ...)  {
  data <- object
  nc <- ncol(data)
  nr <- nrow(data)
  if (is.null(y <- data[, 1]) || is.null(x <- data[, 2:nc])) 
    stop("components Y and X must no be empty")  
  if (any(y <= 0)) 
     stop("response variable must be positive")
  if (any(x[, 1] != 1)) x <- cbind(rep(1, nr), x) 
  bcxEst(y, x, ...)
}