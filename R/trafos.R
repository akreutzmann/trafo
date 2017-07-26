# Box Cox ----------------------------------------------------------------------


#  Transformation: Box Cox

box_cox <- function(y, lambda = lambda, shift = 0) {
  with_shift <- function(y, shift) {
    min <- min(y)
    if (min <= 0) {
      shift <- shift + abs(min(y)) +1
    } else {
      shift <- shift
    }
    return(shift)
  }
  # Shift parameter
  shift <- with_shift(y = y, shift = shift)
  
  lambda_cases <- function(y, lambda = lambda) {
    lambda_absolute <- abs(lambda)
    if (lambda_absolute <= 1e-12) {  #case lambda=0
      y <- log(y + shift)
    } else {
      y <- ((y + shift)^lambda - 1) / lambda
    }
    return(y)
  }
  y <- lambda_cases(y = y, lambda = lambda)
  
  return(list(y = y, shift = shift))
} # End box_cox



# Standardized transformation: Box Cox

geometric.mean <- function(x) { #for RMLE in the parameter estimation
  exp(mean(log(x)))
}

box_cox_std <- function(y, lambda) {
  min <- min(y)
  if (min <= 0) {
    y <- y - min + 1
  }
  
  gm <- geometric.mean(y)
  y <- if(abs(lambda) > 1e-12) {
    y <- (y^lambda - 1) / (lambda * ((gm)^(lambda - 1)))
  } else {
    y <- gm * log(y)
  }
  return(y)
}


# Back transformation: Box Cox
box_cox_back <- function(y, lambda, shift = 0) {
  
  lambda_cases_back <- function(y, lambda = lambda, shift){
    if (abs(lambda) <= 1e-12) {   #case lambda=0
      y <-  exp(y) - shift
    } else {
      y <- (lambda * y + 1)^(1 / lambda) - shift
    }
    return(y = y)
  }
  y <- lambda_cases_back(y = y, lambda = lambda, shift = shift)
  
  return(y = y)
} #  End box_cox_back

# Modulus ----------------------------------------------------------------------

#  Transformation: Modulus
modul <- function(y, lambda = lambda) {
  u <- abs(y) + 1L
  lambda_absolute <- abs(lambda)
  if (lambda_absolute <= 1e-12) {  #case lambda=0
    yt <-  sign(y)*log(u) 
  } else {
    yt <- sign(y)*(u^lambda - 1L)/lambda 
  }
  return(y = yt) 
}

# Standardized transformation: Modulus

modul_std <- function(y, lambda) {
  u <- abs(y) + 1L
  yt <- modul(y, lambda)
  zt <- yt/exp(mean(sign(y)*(lambda - 1L)*log(u)))
  return(y = zt)
}

# Back transformation: Modulus
modul_back <- function(y, lambda = lambda) {
 
}


# The Bickel-Doksum transformation ----------------------------------------------------------------------

#  Transformation: Bick-Doksum

Bick_dok <-  function(y, lambda = lambda) {
  u <- abs(y) + 1
  if (lambda > 1e-12){
    yt <- sign(y)*(u^lambda - 1)/lambda
  }
  else{
    stop("lambda must be positive for the Bick-Doksum transformation")
  }
  return(y = yt)
}

# Standardized transformation: Bick-Doksum

Bick_dok_std <- function(y, lambda) {
  u <- abs(y) + 1L
  yt <- Bick_dok(y, lambda)
  zt <- yt/exp(mean(sign(y)*(lambda-1)*log(u)))
  return(y = zt)
}


# Back transformation: Bick-Doksum
Bick_dok_back <- function(y, lambda = lambda) {
  
}

# The Manly transformation ----------------------------------------------------------------------

# Transformation: Manly
Manly <-  function(y, lambda = lambda) {
  lambda_absolute <- abs(lambda)
  if (lambda_absolute <= 1e-12) {  #case lambda=0
    yt <-  y
  } else {
    yt <- (exp(y*lambda) - 1L)/lambda
  }
  return(y = yt)
}

# Standardized transformation: Manly

Manly_std <- function(y, lambda) {
  lambda_absolute <- abs(lambda)
  yt <- Manly(y, lambda)
  if (lambda_absolute <= 1e-12) {  #case lambda=0
    zt <-  yt
  } else {
    zt <- yt/exp((mean(lambda*y)))
  }
  return(y = zt)
}

# Back transformation: Manly
Manly_back <- function(y, lambda = lambda) {
  
}

# The dual transformation ----------------------------------------------------------------------

# Transformation: dual
Dual <-  function(y, lambda = lambda) {
  lambda_absolute <- abs(lambda)
  if (lambda_absolute <= 1e-12) {  #case lambda=0
    yt <-  log(y)
  } else if (lambda > 1e-12){
    yt <- (y^(lambda) - y^(-lambda))/2*lambda
  } else { 
    stop("lambda can not be negative for the dual transformation")
  }
  return(y = yt)
}

# Standardized transformation: dual

Dual_std <- function(y, lambda) {
  yt <- Dual(y, lambda)
  zt <- yt/exp((mean(log((y^(lambda-1) + y^(-lambda-1))/2))))
  return(y = zt)
}

# Back transformation: dual
Dual_back <- function(y, lambda = lambda) {
  
}

# The Yeo-Johnson transformation ----------------------------------------------------------------------

# Transformation: Yeo-Johnson
Yeo_john <-  function(y, lambda = lambda) {
  n <- length(y)
  u <- abs(y) + 1L 
  yt <- rep(NA, n)
  negativos <- which(y < 0)
  positivos <- which(y >= 0)
  bx <- function(lambda, u, ...) {
    lambda_absolute <- abs(lambda)
    if (lambda_absolute <= 1e-12) {  #case lambda=0
      yt <- log(u)
    }
    else {
      yt <- (u^lambda - 1L)/lambda
    }
    yt
  }
  yt[positivos] <- bx(lambda, u[positivos])
  yt[negativos] <- -bx(lambda = 2L - lambda, u[negativos])
  return(y = yt)
}

# Standardized transformation: Yeo-Johnson

Yeo_john_std <- function(y, lambda) {
  u <- abs(y) + 1L
  yt <- Yeo_john(y, lambda)
  zt <- yt/exp(mean(sign(y)*(lambda-1)*log(u)))
  return(y = zt)
}


# Back transformation: Yeo-Johnson
Yeo_john_back <- function(y, lambda = lambda) {
  
}