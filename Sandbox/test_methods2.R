# Get the models

# lm ---------------------------------------------------------------------------
data("eusilcA_Vienna")
summary(eusilcA_Vienna$eqIncome)
eusilcA_Vienna$eqIncome <- eusilcA_Vienna$eqIncome - 1000 
summary(eusilcA_Vienna$eqIncome)
which(eusilcA_Vienna$eqIncome < 0)
modelVienna <- lm(eqIncome ~ eqsize + gender + cash + unempl_ben + age_ben +
                    rent + cap_inv + tax_adj + dis_ben + sick_ben + surv_ben + 
                    fam_allow + house_allow, data = eusilcA_Vienna)


# lme with one random intercept
library(nlme)
modelVienna2 <- lme(eqIncome ~ eqsize + gender + cash + unempl_ben + age_ben +
                      rent + cap_inv + tax_adj + dis_ben + sick_ben + surv_ben + 
                      fam_allow + house_allow, random = ~ 1 | county, 
                    data = eusilcA_Vienna, na.action = na.omit)


# 1. Bickeldoksum

# lm with ml
bd_ml <- bickeldoksum(object = modelVienna, lambda = "estim", method = "ml",
                      plotit = TRUE)

print(bd_ml)


trafo_lmBD <- trafo_lm(object = modelVienna, trafo = "bickeldoksum", 
                       method = "ml", lambdarange = c(0,2), std = FALSE)

print(trafo_lmBD)
diagnostics(trafo_lmBD)
summary(trafo_lmBD)
plot(trafo_lmBD)

# lme with reml
bickeldoksum_reml <- bickeldoksum(modelVienna2, lambda = "estim", method = "reml",
                                  plotit = TRUE)

print(bickeldoksum_reml)
summary(bickeldoksum_reml)
plot(bickeldoksum_reml)


# 2. Box-Cox

# lm with skewness minimization
boxcox_skew <- boxcox(modelVienna, method = "skew", plotit = TRUE)

print(boxcox_skew)
as.data.frame(boxcox_skew, modelVienna)

trafo_lmBC <- trafo_lm(object = modelVienna, trafo = "boxcox", 
                       method = "skew", lambdarange = c(0,2), std = TRUE)

print(trafo_lmBC)
diagnostics(trafo_lmBC)
summary(trafo_lmBC)
plot(trafo_lmBC)

compare_BDBC <- compare_trafo(modelVienna, trafos = list(bd_ml, boxcox_skew), 
                              std = FALSE)

print(compare_BDBC)
diagnostics(compare_BDBC)
summary(compare_BDBC)
plot(compare_BDBC)


# lme with minimization of pooled skewness
boxcox_pskew <- boxcox(modelVienna2, lambdarange = c(-0.5,2), 
                      method = "pskew", plotit = TRUE)

print(boxcox_pskew)
summary(boxcox_pskew)
plot(boxcox_pskew)


# 3. Dual

# lm with divergence minimization following Kolmogorov-Smirnof
dual_divks <- dual(modelVienna, method = "div.ks")

print(dual_divks)
summary(dual_divks)
plot(dual_divks)

trafo_lmD <- trafo_lm(object = modelVienna, trafo = "dual", 
                       method = "div.ks", lambdarange = c(0,2), std = FALSE)

print(trafo_lmD)
summary(trafo_lmD)
plot(trafo_lmD)

# lme with divergence minimization following Cramer-von-Mises
dual_divcvm <- dual(modelVienna2, method = "div.cvm")

print(dual_divcvm)
summary(dual_divcvm)
plot(dual_divcvm)


# 4. Manly

# lm with ml
manly_ml <- dual(modelVienna, method = "ml")

print(manly_ml)
summary(manly_ml)
plot(manly_ml)

trafo_lmML <- trafo_lm(object = modelVienna, trafo = "manly", 
                      method = "ml", lambdarange = c(0.00005,0.005), std = FALSE)

print(trafo_lmML)
summary(trafo_lmML)
plot(trafo_lmML)

# lme with divergence minimization following Kullback-Leibner
manly_divkl <- manly(modelVienna2, method = "div.kl", lambdarange = c(-0.0005, 0.005))

print(manly_divkl)
summary(manly_divkl)
plot(manly_divkl)


# 5. Modulus

# lm with divergence following Cramer-von-Mises
modulus_divcvm <- modulus(modelVienna, method = "div.cvm")

print(modulus_divcvm)
summary(modulus_divcvm)
plot(modulus_divcvm)


trafo_lmMD <- trafo_lm(object = modelVienna, trafo = "modulus", 
                       method = "div.cvm", lambdarange = c(0, 2), 
                       std = TRUE)

print(trafo_lmMD)
summary(trafo_lmMD)
plot(trafo_lmMD)

# lme with skewness minimization
modulus_skew <- modulus(modelVienna2, method = "skew")

print(modulus_skew)
summary(modulus_skew)
plot(modulus_skew)


# 6. Yeo-Johnson

# lm with given lambda
yeojohnson_03 <- yeojohnson(modelVienna, lambda = 0.3)
yeojohnson_ml <- yeojohnson(modelVienna, method = "ml")

print(yeojohnson_ml)
print(yeojohnson_03)
summary(yeojohnson_03)
summary(yeojohnson_ml)
plot(yeojohnson_03)
plot(yeojohnson_ml)

trafo_lmY <- trafo_lm(object = modelVienna, trafo = "yeojohnson", 
                       method = "div.cvm", lambdarange = c(0, 2), 
                       std = FALSE)

print(trafo_lmY)
summary(trafo_lmY)
plot(trafo_lmY)

# lme with given lambda
yeojohnson_035 <- yeojohnson(modelVienna2, lambda = 0.35,  
                              lambdarange = c(0.15, 0.5), method = "reml")
yeojohnson_reml <- yeojohnson(modelVienna2, lambda = "estim",  
                             lambdarange = c(0.15, 0.7), method = "reml")

print(yeojohnson_035)
summary(yeojohnson_035)
summary(yeojohnson_reml)
plot(yeojohnson_035)


# 7. Log shift opt

# lm with divergence following Cramer-von-Mises
logshiftopt_divcvm <- logshiftopt(modelVienna, lambdarange = c(900,1000), 
                                  method = "div.cvm")

print(logshiftopt_divcvm)
summary(logshiftopt_divcvm)
plot(logshiftopt_divcvm)

trafo_lmlopt <- trafo_lm(object = modelVienna, trafo = "logshiftopt", 
                      method = "div.cvm", lambdarange = c(900, 1000), 
                      std = FALSE)

print(trafo_lmlopt)
summary(trafo_lmlopt)
plot(trafo_lmlopt)

# lme with skewness minimization
logshiftopt_skew <- logshiftopt(modelVienna2, lambdarange = c(950,1000), 
                                method = "skew", plotit = FALSE)

print(logshiftopt_skew)
summary(logshiftopt_skew)
plot(logshiftopt_skew)



# 8. Square-root shift 

# lm with divergence following Cramer-von-Mises
sqrtshift_ml <- sqrtshift(modelVienna, lambdarange = c(0,2), 
                          method = "ml")

print(sqrtshift_ml)
summary(sqrtshift_ml)
plot(sqrtshift_ml)

trafo_lmsqrt <- trafo_lm(object = modelVienna, trafo = "sqrtshift", 
                         method = "ml", lambdarange = c(0, 2), 
                         std = FALSE)

print(trafo_lmsqrt)
summary(trafo_lmsqrt)
plot(trafo_lmsqrt)

# lme with skewness minimization
sqrtshift_reml <- sqrtshift(modelVienna2, lambdarange = c(0,2), 
                                method = "reml", plotit = FALSE)

print(sqrtshift_reml)
summary(sqrtshift_reml)
plot(sqrtshift_reml)


# 9. Gpower 

# lm with ml
gpower_skew <- gpower(modelVienna, lambda = "estim", method = "skew")

print(gpower_skew)
summary(gpower_skew)
plot(gpower_skew)


trafo_lmGP <- trafo_lm(object = modelVienna, trafo = "gpower", 
                         method = "ml", lambdarange = c(0, 2), 
                         std = FALSE)

print(trafo_lmGP)
summary(trafo_lmGP)
plot(trafo_lmGP)

# lme with skewness minimization
gpower_reml <- gpower(modelVienna2, lambdarange = c(0,2), 
                            method = "reml", plotit = TRUE)

print(gpower_reml)
summary(gpower_reml)
plot(gpower_reml)


# 10. Reciprocal

reciprocal_Vienna <- woparam(modelVienna, trafo = "reciprocal")

print(reciprocal_Vienna)
summary(reciprocal_Vienna)
plot(reciprocal_Vienna)

trafo_lmRC <- trafo_lm(object = modelVienna, trafo = "reciprocal", 
                       method = "ml", lambdarange = c(0, 2), 
                       std = FALSE)

print(trafo_lmRC)
summary(trafo_lmRC)
plot(trafo_lmRC)


# 11. Neg Log

neglog_Vienna <- woparam(modelVienna, trafo = "neglog")

print(neglog_Vienna)
summary(neglog_Vienna)
plot(neglog_Vienna)


trafo_lmNL <- trafo_lm(object = modelVienna, trafo = "neglog", 
                       method = "ml", lambdarange = c(0, 2), 
                       std = FALSE)

print(trafo_lmNL)
summary(trafo_lmNL)
plot(trafo_lmNL)



neglog_custom <- woparam(modelVienna, trafo = "custom", 
                              custom_trafo = list(neglog = function(y) {
                                u <- abs(y) + 1L
                                yt <-  sign(y)*log(u)
                                
                                return(y = yt)
                              }))

print(neglog_custom)

all.equal(neglog_custom$yt, neglog_Vienna$yt)


# Test oneparam function

boxcox2_Vienna <- oneparam(modelVienna, trafo = "boxcox",  
                           method = "skew", lambdarange = c(0, 2), plotit = TRUE)


# Test oneparam function with custom transformation

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

modul2_Vienna <- oneparam(modelVienna, trafo = "custom",  
                           method = "skew", lambdarange = c(0, 2), plotit = TRUE, 
                          custom_trafo = list(modul = modul, modul_std = modul_std))


print(modul2_Vienna)
summary(modul2_Vienna)



