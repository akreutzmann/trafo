# Get the models

# lm ---------------------------------------------------------------------------
data(cars, package = "datasets")
lm_cars <- lm(dist ~ speed, data = cars)


# One parameter transformations
bd_trafo <- bickeldoksum(object = lm_cars, method = "kurt", 
                         lambdarange = c(1e-11, 2), plotit = TRUE)

print(bd_trafo)
as.data.frame(bd_trafo)


# Transformation without parameter
log_trafo <- logtrafo(lm_cars)

print(log_trafo)
as.data.frame(log_trafo)


# Transformed linear model with one parameter transformation
boxcox_lm <- trafo_lm(lm_cars)

print(boxcox_lm)
summary(boxcox_lm)
diagnostics(boxcox_lm)
plot(boxcox_lm)


# Define transformation and standardized transformation in order to use
# customized transformation with one transformation parameter
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

modul_std <- function(y, lambda) {
  u <- abs(y) + 1L
  yt <- modul(y, lambda)
  zt <- yt/exp(mean(sign(y)*(lambda - 1L)*log(u)))
  
  y <- zt
  
  return(y)
}

# Use customized transformation for lm
custom1 <- trafo_lm(object = lm_cars, trafo = "custom_one", lambda = "estim", 
                    method = "ml", lambdarange = c(-2,2), 
                    custom_trafo = list(modul = modul, modul_std = modul_std))
print(custom1)
summary(custom1)
diagnostics(custom1)
plot(custom1)

# As comparison
modulus_lm <- trafo_lm(object = lm_cars, trafo = "modulus", lambda = "estim", 
                    method = "ml", lambdarange = c(-2,2), 
                    custom_trafo = NULL)
print(modulus_lm)
summary(modulus_lm)
diagnostics(modulus_lm)
plot(modulus_lm)


# Try customized transformation without parameter
neg_log <- function(y) {
  u <- abs(y) + 1L
  yt <-  sign(y)*log(u)
  
  return(y = yt)
}

# Standardized transformation: neg_log

neg_log_std <- function(y) {
  u <- abs(y) + 1L
  yt <- neg_log(y)
  zt <- yt/exp(mean(sign(y)*log(u)))
  
  y <- zt
  return(y)
}


custom2 <- trafo_lm(object = lm_cars, trafo = "custom_wo", lambda = "estim", 
                    method = "ml", lambdarange = c(-2,2), 
                    custom_trafo = list(neg_log = neg_log, 
                                        neg_log_std = neg_log_std))
print(custom2)
summary(custom2)
diagnostics(custom2)
plot(custom2)


# Try comparison

compare_lm <- trafo_compare(lm_cars, trafos = list(bd_trafo, log_trafo))

print(compare_lm)
summary(compare_lm)
diagnostics(compare_lm) # funktioniert noch nicht
plot(compare_lm)
