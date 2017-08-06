# lm ---------------------------------------------------------------------------
data("eusilcA_Vienna")
modelVienna <- lm(eqIncome ~ eqsize + gender + cash + unempl_ben + age_ben +
                  rent + cap_inv + tax_adj + dis_ben + sick_ben + surv_ben + 
                  fam_allow + house_allow, data = eusilcA_Vienna)

library(simFrame)
data("eusilcP")


# only lilys if bcxEst(y, x, ...) in bx_cx.lm
lily_bc <- bx_cx(modelVienna, method = "ml")

source("./R/trafos.R")


boxCox <- box_cox(eusilcA_Vienna$eqIncome, lambda = 0.4690185)

boxCox <- box_cox(eusilcA_Vienna$eqIncome, lambda = -2)

boxCox <- box_cox(eusilcA_Vienna$eqIncome, lambda = -0.472136)

all.equal(box_cox$y, as.numeric(lily_bc$yt))

(box_cox$y - as.numeric(lily_bc$yt))/box_cox$y



devtools::load_all(".")
bc_ML <- bx_cx(modelVienna, method = "ml")
  
print(bc_ML)

bc_ML$model
bc_ML$modelt
  
class(bc_ML)

summary(bc_ML)
plot(bc_ML)

compareTransformation(modelVienna, lamda.dual = 0.45)


# library(skewness) # need to load this lilbrary before
bx_cx(modelVienna, method = "skew")

bx_cx(modelVienna, method = "div.ks")
bx_cx(modelVienna, method = "div.cvm")

# library(FNN) # need to load this lilbrary before
bx_cx(modelVienna, method = "div.kl")


modulus(modelVienna, method = "ml")
modulus_skew <- modulus(modelVienna, method = "skew")
modulus(modelVienna, method = "div.ks")
modulus(modelVienna, method = "div.cvm")
modulus(modelVienna, method = "div.kl")

bickeldoksum(modelVienna, method = "ml")
bickeldoksum(modelVienna, method = "skew")
bickeldoksum(modelVienna, method = "div.ks")
bickeldoksum(modelVienna, method = "div.cvm")
bickeldoksum(modelVienna, method = "div.kl")

manly(modelVienna, method = "ml")
manly(modelVienna, method = "ml", lambdarange = c(-0.05, 0.005))
manly(modelVienna, method = "skew", lambdarange = c(-0.05, 0.005))
manly(modelVienna, method = "div.ks", lambdarange = c(-0.05, 0.005))
manly(modelVienna, method = "div.cvm", lambdarange = c(-0.05, 0.005))
manly(modelVienna, method = "div.kl", lambdarange = c(-0.05, 0.005))

dual(modelVienna, method = "ml")
dual(modelVienna, method = "skew")
dual(modelVienna, method = "div.ks")
dual(modelVienna, method = "div.cvm")
dual(modelVienna, method = "div.kl")


class(modelVienna)

yeojohnson(modelVienna, method = "ml")
yeojohnson(modelVienna, method = "skew")
yeojohnson(modelVienna, method = "div.ks")
yeojohnson(modelVienna, method = "div.cvm")
yeojohnson(modelVienna, method = "div.kl")


# lme --------------------------------------------------------------------------

library(laeken)
data(eusilc)


library(nlme)
modelAustria <- lme(eqIncome ~ pb220a + py050n, random = ~ 1 | db040, data = eusilc, 
                    na.action = na.omit)

modelAustria <- lme(eusilc$eqIncome ~ eusilc$pb220a + eusilc$py050n, 
                    random = ~ 1 | eusilc$db040, 
                    na.action = na.omit)

which(eusilc$eqIncome == 0)
eusilc$eqIncome <- eusilc$eqIncome + 1

modelAustria <- lme(eqIncome ~ pb220a + py050n, 
                    random = ~ 1 | db040, data = eusilc, 
                    na.action = na.omit)

bxcx_reml <- bx_cx(modelAustria, method = "reml")
bx_cx(modelAustria, method = "skew")
bx_cx(modelAustria, method = "pskew")
bx_cx(modelAustria, method = "div.ks")
bx_cx(modelAustria, method = "div.cvm")
bx_cx(modelAustria, method = "div.kl")

modulus(modelAustria, method = "reml")
modulus(modelAustria, method = "skew")
modulus(modelAustria, method = "pskew")
modulus(modelAustria, method = "div.ks")
modulus(modelAustria, method = "div.cvm")
modulus(modelAustria, method = "div.kl")

class(modelAustria)

bickeldoksum(modelAustria, method = "reml")
bickeldoksum(modelAustria, method = "skew")
bickeldoksum(modelAustria, method = "pskew")
bickeldoksum(modelAustria, method = "div.ks")
bickeldoksum(modelAustria, method = "div.cvm")
bickeldoksum(modelAustria, method = "div.kl")


manly(modelAustria, method = "reml")
manly(modelAustria, method = "reml", lambdarange = c(-0.000005, 0.00005))
manly(modelAustria, method = "skew", lambdarange = c(-0.05, 0.005))
manly(modelAustria, method = "pskew", lambdarange = c(-0.05, 0.005))
manly(modelAustria, method = "div.ks", lambdarange = c(-0.05, 0.005))
manly(modelAustria, method = "div.cvm", lambdarange = c(-0.05, 0.005))
manly(modelAustria, method = "div.kl", lambdarange = c(-0.05, 0.005))


dual(modelAustria, method = "reml")
dual(modelAustria, method = "skew")
dual(modelAustria, method = "pskew")
dual(modelAustria, method = "div.ks")
dual(modelAustria, method = "div.cvm")
dual(modelAustria, method = "div.kl")

yeojohnson(modelAustria, method = "reml")
yeojohnson(modelAustria, method = "skew")
yeojohnson(modelAustria, method = "pskew")
yeojohnson(modelAustria, method = "div.ks")
yeojohnson(modelAustria, method = "div.cvm")
yeojohnson(modelAustria, method = "div.kl")
