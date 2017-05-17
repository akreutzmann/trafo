#' Modulus Transformation
#'
#' Estimates the transformation parameter for the Modulus transformation employing the profile log-likehood.
#' @param object a data.frame, matrix, formula or fitted model; see below.
#' @param data an optional data frame, list or environment as in lm.
#' @param XY a data.frame or a matrix, whose first column is the dependent variable and the rest of the columns are the independent variables.
#' @param lambdarange range for the estimation parameter lambda - default c(-2, 2)
#' @return An object of class \code{transformation} with the following arguments.
#' @return llike The value of profile log-likelihood at its maximum.
#' @return logvector The profile log-likelihood evaluated at \code{lambdavector}.
#' @return lambdavector A sequence with optional values for \code{lambda}.
#' @return family Employed family of transformations.
#' @return yt Vector of the transformed response variable \code{y}.
#' @return modelt An object of type \code{lm} employing the transformed vector \code{yt} as the response variable.
#' @keywords modulus
#' @export
modulus <- function(object,...) UseMethod("modulus")