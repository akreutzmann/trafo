#' #' Modulus Default
#' #'
#' #' Modulus estimation
#' #' @param object a data.frame or a matrix, whose first column is the dependent variable and the rest of the columns are the independent variables.
#' #' @param \dots additional arguments to be passed to the estimation function;  see modulusEst()
#' #' @return an object of class \code{transformation}; see modulusEst()
#' #' @keywords internal
#' #' @export
#' modulus.default <- function(object,...) {
#'   data <- object
#'   nc <- ncol(data)
#'   nr <- nrow(data)
#'   if (is.null(y <- data[, 1]) || is.null(x <- data[, 2:nc])) 
#'     stop("components Y and X must no be empty")  
#'   if (any(x[,1] != 1)) x <- cbind(rep(1,nr), x) 
#'   modulusEst(y, x, ...)
#' }