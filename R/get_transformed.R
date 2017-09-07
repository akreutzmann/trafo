get_transformed <- function(trafo, ans, y, lambda) {
  
  if (trafo == "boxcox") {
    # Get vector of transformed and standardized transformed variable
    ans$yt <- box_cox(y = y, lambda = lambda)$y
    ans$s <- box_cox(y = y, lambda = lambda)$shift
    ans$zt <- box_cox_std(y = y, lambda = lambda)
    
    # Save transformation family
    ans$family <- "Box-Cox"
  } else if (trafo == "bickeldoksum") {
    # Get vector of transformed and standardized transformed variable
    ans$yt <- Bick_dok(y = y, lambda = lambda)
    ans$s <- NULL
    ans$zt <- Bick_dok_std(y = y, lambda = lambda)
    
    # Save transformation family 
    ans$family <- "Bickel-Doksum"
    
  } else if (trafo == "manly") {
    # Get vector of transformed and standardized transformed variable
    ans$yt <- Manly(y = y, lambda = lambda)
    ans$s <- NULL
    ans$zt <- Manly_std(y = y, lambda = lambda)
    
    # Save transformation family and method
    ans$family <- "Manly"
  } else if (trafo == "modulus") {
    # Get vector of transformed and standardized transformed variable
    ans$yt <- modul(y = y, lambda = lambda)
    ans$s <- NULL
    ans$zt <- modul_std(y = y, lambda = lambda)
    
    # Save transformation family and method
    ans$family <- "Modulus"
  } else if (trafo == "dual") {
    # Get vector of transformed and standardized transformed variable
    ans$yt <- Dual(y = y, lambda = lambda)
    ans$s <- NULL
    ans$zt <- Dual_std(y = y, lambda = lambda)
    
    # Save transformation family and method
    ans$family <- "Dual"
  } else if (trafo == "yeojohnson") {
    # Get vector of transformed and standardized transformed variable
    ans$yt <- Yeo_john(y = y, lambda = lambda)
    ans$s <- NULL
    ans$zt <- Yeo_john_std(y = y, lambda = lambda)
    
    # Save transformation family and method
    ans$family <- "Yeo-Johnson"
  } else if (trafo == "logshiftopt") {
    # Get vector of transformed and standardized transformed variable
    ans$yt <- log_shift(y = y, lambda = lambda)
    ans$s <- NULL
    ans$zt <- log_shift_std(y = y, lambda = lambda)
    
    # Save transformation family and method
    ans$family <- "Log shift opt"
  } else if (trafo == "sqrtshift") {
    # Get vector of transformed and standardized transformed variable
    ans$yt <- sqrt_shift(y = y, lambda = lambda)
    ans$s <- NULL
    ans$zt <- sqrt_shift_std(y = y, lambda = lambda)
    
    # Save transformation family and method
    ans$family <- "Square root shift"
  } else if (trafo == "gpower") {
    # Get vector of transformed and standardized transformed variable
    ans$yt <- gPower(y = y, lambda = lambda)
    ans$s <- NULL
    ans$zt <- gPower_std(y = y, lambda = lambda)
    
    # Save transformation family and method
    ans$family <- "Gpower"
  } 
  
  return(ans)
}