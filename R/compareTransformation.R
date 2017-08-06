#' Compares Transformations
#'
#' Compares families of transformations with respect to  sense of AIC, R.Squared, Skewness or Kurtosis; For positive y compares between Box-Cox, Dual and Manly transformations; For all values of y it compares between Yeo-Johnson, Manly, Modulus and Bickel-Doksum.  
#' @param object a data.frame, matrix, formula or fitted model; see below.
# #' @param data an optional data frame, list or environment as in lm.
# #' @param plotit a logical value. If equal FALSE plots comparing transformations are not shown - default TRUE.
# #' @param XY a data.frame or a matrix, whose first column is the dependent variable and the rest of the columns are the independent variables.
#' @param ... other parameters that can be passed to the function.
#' @return A data frame comparing families of transformations. If plotit ==  TRUE, plots comparing transformations are shown as well.
#' @export
compareTransformation <- function(object,  ...) UseMethod("compareTransformation")