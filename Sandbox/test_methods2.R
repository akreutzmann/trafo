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
summary(bd_ml)
plot(bd_ml)


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
summary(boxcox_skew)
plot(boxcox_skew)

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

# lme with skewness minimization
sqrtshift_reml <- sqrtshift(modelVienna2, lambdarange = c(0,2), 
                                method = "reml", plotit = FALSE)

print(sqrtshift_reml)
summary(sqrtshift_reml)
plot(sqrtshift_reml)


# 9. Gpower 

# lm with divergence following Cramer-von-Mises
gpower_ml <- gpower(modelVienna, lambda = 100)

print(sqrtshift_ml)
summary(sqrtshift_ml)
plot(sqrtshift_ml)

# lme with skewness minimization
sqrtshift_reml <- sqrtshift(modelVienna2, lambdarange = c(0,2), 
                            method = "reml", plotit = FALSE)

print(sqrtshift_reml)
summary(sqrtshift_reml)
plot(sqrtshift_reml)
