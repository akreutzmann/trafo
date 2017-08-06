#' Selects Transformation
#'
#' Chooses the 'best' transformation in the sense of AIC, R.Squared, Skewness or Kurtosis; For positive \code{y} compares between Box-Cox, Dual and Manly transformations; For all values of y it compares between Yeo-Johnson, Manly, Modulus and Bickel-Doksum.
#' @param object a data.frame, matrix, formula or fitted model; see below.
# #' @param data an optional data frame, list or environment as in lm.
# #' @param XY a data.frame or a matrix, whose first column is the dependent variable and the rest of the columns are the independent variables.
# #' @param criterion a character string to select the desired criterion to be used to select a family of transformation whose parameter is estimated using the likelihood approach. The available options ara (i) Akaike's information criterion "AIC"; (ii) R.Squared "R2"; (iii) Skewness "SK"; (iv) Kurtosis "KT". - default "AIC"
# #' @param usefamily a character string to select the transformation family. Six different transformation families for the dependent variable can be chosen. For positive response variable the available options are (i) "Box-Cox" or "BC" ; (ii) "Dual" or "D" ("log"); (iii) "Manly" or "Ma". For negative response variables (i) "Yeo-Johhson" or "YJ"; (ii) "Modulus" or "Mod"; (iii) "Bickel-Doksum" or "BD"; (iv) "Manly" or "Ma" - Default NULL.
#' @param ... other parameters that can be passed to the function.
#' @return An object of class \code{transformation} with the following arguments
#' @return llike The value of profile log-likelihood at its maximum
#' @return logvector The profile log-likelihood evaluated at \code{lambdavector}
#' @return lambdavector Employed family of transformations
#' @return A sequence with optional values for \code{lambda}
#' @return family Employed family of transformations
#' @return yt Vector of the transformed response variable \code{y}
#' @return modelt An object of type \code{lm} employing the transformed vector \code{yt} as the response variable
#' @keywords selecTransformation
#' @export
selectTransformation <- function(object,  ...) UseMethod("selectTransformation")
