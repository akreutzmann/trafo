# lm ---------------------------------------------------------------------------
data("eusilcA_Vienna")
modelVienna <- lm(eqIncome ~ eqsize + gender + cash + unempl_ben + age_ben +
                  rent + cap_inv + tax_adj + dis_ben + sick_ben + surv_ben + 
                  fam_allow + house_allow, data = eusilcA_Vienna)
plot(modelVienna)


# Test if all transformations work with estimation of lambda

# Box-Cox ----------------------------------------------------------------------
bc_ML <- boxcox(modelVienna, lambda = "estim", method = "ml")
  
print(bc_ML)
summary(bc_ML)
plot(bc_ML)



# library(skewness) # need to load this lilbrary before
bx_cx_skew <- boxcox(modelVienna, lambda = "estim", method = "skew")

print(bx_cx_skew)
summary(bx_cx_skew)
plot(bx_cx_skew)

bx_cx_divks <- bx_cx(modelVienna, method = "div.ks")

print(bx_cx_divks)
summary(bx_cx_divks)
plot(bx_cx_divks)


bx_cx_divcvm <- bx_cx(modelVienna, method = "div.cvm")

print(bx_cx_divcvm)
summary(bx_cx_divcvm)
plot(bx_cx_divcvm)

bx_cx_divkl <- bx_cx(modelVienna, method = "div.kl")

print(bx_cx_divkl)
summary(bx_cx_divkl)
plot(bx_cx_divkl)


# Modulus ----------------------------------------------------------------------
modulus_ml <- modulus(modelVienna, lambda = "estim",  method = "ml")

print(modulus_ml)
summary(modulus_ml)
plot(modulus_ml)

modulus_skew <- modulus(modelVienna, lambda = "estim", method = "skew")

print(modulus_skew)
summary(modulus_skew)
plot(modulus_skew)

modulus_divks <- modulus(modelVienna, method = "div.ks")

print(modulus_divks)
summary(modulus_divks)

modulus_divcvm <- modulus(modelVienna, method = "div.cvm")

print(modulus_divcvm)
summary(modulus_divcvm)

modulus_divkl <- modulus(modelVienna, method = "div.kl")

print(modulus_divkl)
summary(modulus_divkl)


# Bickel-Doksum ----------------------------------------------------------------
bd_ml <- bickeldoksum(modelVienna, lambda = "estim", method = "ml")

print(bd_ml)
summary(bd_ml)

bd_skew <- bickeldoksum(modelVienna, method = "skew")

print(bd_skew)
summary(bd_skew)

bd_divks <- bickeldoksum(modelVienna, lambda = "estim", method = "div.ks")

print(bd_divks)
summary(bd_divks)

bd_divcvm <- bickeldoksum(modelVienna, method = "div.cvm")

print(bd_divcvm)
summary(bd_divcvm)

bd_divkl <- bickeldoksum(modelVienna, method = "div.kl")

print(bd_divkl)
summary(bd_divkl)


# Manly ------------------------------------------------------------------------
manly_ml <- manly(modelVienna, lambda = "estim", method = "ml", lambdarange = c(-0.05, 0.005))

manly(modelVienna, method = "ml")

print(manly_ml)
summary(manly_ml)

manly_skew <- manly(modelVienna, lambda = "estim", method = "skew", lambdarange = c(-0.05, 0.005))

print(manly_skew)
summary(manly_skew)

manly_divks <- manly(modelVienna, method = "div.ks", lambdarange = c(-0.05, 0.005))

print(manly_divks)
summary(manly_divks)

manly_divcvm <- manly(modelVienna, method = "div.cvm", lambdarange = c(-0.05, 0.005))

print(manly_divcvm)
summary(manly_divcvm)

manly_divkl <- manly(modelVienna, method = "div.kl", lambdarange = c(-0.05, 0.005))

print(manly_divkl)
summary(manly_divkl)


# Dual -------------------------------------------------------------------------

dual_ml <- dual(modelVienna, lambda = "estim", method = "ml")

print(dual_ml)
summary(dual_ml)

dual_skew <- dual(modelVienna, method = "skew")

print(dual_skew)
summary(dual_skew)

dual_divks <- dual(modelVienna, lambda = "estim", method = "div.ks")

print(dual_divks)
summary(dual_divks)

dual_divcvm <- dual(modelVienna, method = "div.cvm")

print(dual_divcvm)
summary(dual_divcvm)

dual_divkl <- dual(modelVienna, method = "div.kl")

print(dual_divkl)
summary(dual_divkl)

# Yeo-Johnson ------------------------------------------------------------------
yeojohnson_ml <- yeojohnson(modelVienna, lambda = "estim", method = "ml")

print(yeojohnson_ml)
summary(yeojohnson_ml)

yeojohnson_skew <- yeojohnson(modelVienna, method = "skew")

print(yeojohnson_skew)
summary(yeojohnson_skew)

yeojohnson_divks <- yeojohnson(modelVienna, method = "div.ks")

print(yeojohnson_divks)
summary(yeojohnson_divks)

yeojohnson_divcvm <- yeojohnson(modelVienna, method = "div.cvm")

print(yeojohnson_divcvm)
summary(yeojohnson_divcvm)

yeojohnson_divkl <- yeojohnson(modelVienna, method = "div.kl")

print(yeojohnson_divkl)
summary(yeojohnson_divkl)


# lme --------------------------------------------------------------------------

library(laeken)
data(eusilc)
library(nlme)

which(eusilc$eqIncome == 0)
eusilc$eqIncome <- eusilc$eqIncome + 1

modelAustria <- lme(eqIncome ~ pb220a + py050n, 
                    random = ~ 1 | db040, data = eusilc, 
                    na.action = na.omit)

plot(modelAustria, db040 ~ resid(.))
plot(modelAustria, eqIncome ~ fitted(.) | db040)

# Box-Cox ----------------------------------------------------------------------
bxcx_reml <- boxcox(modelAustria, lambda = "estim", method = "reml", plotit = FALSE)

print(bxcx_reml)
summary(bxcx_reml)
plot(bxcx_reml)


bxcx_skew <- bx_cx(modelAustria, method = "skew")

print(bxcx_skew)
summary(bxcx_skew)

bxcx_pskew <- bx_cx(modelAustria, method = "pskew")

print(bxcx_pskew)
summary(bxcx_pskew)

bxcx_divks <- bx_cx(modelAustria, method = "div.ks")

print(bxcx_divks)
summary(bxcx_divks)

bxcx_divcvm <- bx_cx(modelAustria, method = "div.cvm")

print(bxcx_divcvm)
summary(bxcx_divcvm)

bxcx_divkl <- bx_cx(modelAustria, method = "div.kl")

print(bxcx_divkl)
summary(bxcx_divkl)


# Modulus ----------------------------------------------------------------------

modulus_reml <- modulus(modelAustria, lambda = "estim", method = "reml", plotit = FALSE)

print(modulus_reml)
summary(modulus_reml)

modulus_skew <- modulus(modelAustria, method = "skew")

print(modulus_skew)
summary(modulus_skew)

modulus_pskew <- modulus(modelAustria, method = "pskew")

print(modulus_pskew)
summary(modulus_pskew)

modulus_divks <- modulus(modelAustria, method = "div.ks")

print(modulus_divks)
summary(modulus_divks)

modulus_divcvm <- modulus(modelAustria, method = "div.cvm")

print(modulus_divcvm)
summary(modulus_divcvm)

modulus_divkl <- modulus(modelAustria, method = "div.kl")

print(modulus_divkl)
summary(modulus_divkl)

# Bickel-Doksum ----------------------------------------------------------------

bickeldoksum_reml <- bickeldoksum(modelAustria, lambda = 0.35, method = "reml")

print(bickeldoksum_reml)
summary(bickeldoksum_reml)

bickeldoksum_skew <- bickeldoksum(modelAustria, method = "skew")

print(bickeldoksum_skew)
summary(bickeldoksum_skew)

bickeldoksum_pskew <- bickeldoksum(modelAustria, method = "pskew")

print(bickeldoksum_pskew)
summary(bickeldoksum_pskew)

bickeldoksum_divks <- bickeldoksum(modelAustria, method = "div.ks")

print(bickeldoksum_divks)
summary(bickeldoksum_divks)

bickeldoksum_divcvm <- bickeldoksum(modelAustria, method = "div.cvm")

print(bickeldoksum_divcvm)
summary(bickeldoksum_divcvm)

bickeldoksum_divkl <- bickeldoksum(modelAustria, method = "div.kl")

print(bickeldoksum_divkl)
summary(bickeldoksum_divkl)

# Manly ------------------------------------------------------------------------

manly_reml <- manly(modelAustria, lambda = "estim", method = "reml", lambdarange = c(-0.000005, 0.00005))

print(manly_reml)
summary(manly_reml)

manly_skew <- manly(modelAustria, lambda = "estim", method = "skew", lambdarange = c(-0.05, 0.005))

print(manly_skew)
summary(manly_skew)

manly_pskew <- manly(modelAustria, method = "pskew", lambdarange = c(-0.05, 0.005))

print(manly_pskew)
summary(manly_pskew)

manly_divks <- manly(modelAustria, method = "div.ks", lambdarange = c(-0.05, 0.005))

print(manly_divks)
summary(manly_divks)

manly_divcvm <- manly(modelAustria, method = "div.cvm", lambdarange = c(-0.05, 0.005))

print(manly_divcvm)
summary(manly_divcvm)

manly_divkl <- manly(modelAustria, method = "div.kl", lambdarange = c(-0.05, 0.005))

print(manly_divkl)
summary(manly_divkl)


# Dual -------------------------------------------------------------------------

dual_reml <- dual(modelAustria, lambda = "estim",  method = "reml")

print(dual_reml)
summary(dual_reml)

dual_skew <- dual(modelAustria, lambda = "estim", method = "skew")

print(dual_skew)
summary(dual_skew)

dual_pskew <- dual(modelAustria, method = "pskew")

print(dual_pskew)
summary(dual_pskew)

dual_divks <- dual(modelAustria, method = "div.ks")

print(dual_divks)
summary(dual_divks)

dual_divcvm <- dual(modelAustria, method = "div.cvm")

print(dual_divcvm)
summary(dual_divcvm)

dual_divkl <- dual(modelAustria, method = "div.kl")

print(dual_divkl)
summary(dual_divkl)


# Yeo-Johnson ------------------------------------------------------------------

yeojohnson_reml <- yeojohnson(modelAustria, lambda = 0.35,  method = "reml")

print(yeojohnson_reml)
summary(yeojohnson_reml)

yeojohnson_skew <- yeojohnson(modelAustria, method = "skew")

print(yeojohnson_skew)
summary(yeojohnson_skew)

yeojohnson_pskew <- yeojohnson(modelAustria, method = "pskew")

print(yeojohnson_pskew)
summary(yeojohnson_pskew)

yeojohnson_divks <- yeojohnson(modelAustria, method = "div.ks")

print(yeojohnson_divks)
summary(yeojohnson_divks)

yeojohnson_divcvm <- yeojohnson(modelAustria, method = "div.cvm")

print(yeojohnson_divcvm)
summary(yeojohnson_divcvm)

yeojohnson_divkl <- yeojohnson(modelAustria, method = "div.kl")

print(yeojohnson_divkl)
summary(yeojohnson_divkl)
