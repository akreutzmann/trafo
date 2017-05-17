#' Estimates optimal parameter for Box-Cox transformations
#'
#' Function \code{optimal_parameter} estimates the optimal parameter lambda
#' that is neccessary to determine Box-Cox transformations.
#'
#' @param generic_opt a wrapper function that conducts the selected optimization
#' \code{method} in order to specify the optimal lambda for Box-Cox
#' transformations.
#' @param fixed a two-sided linear formula object describing the
#' fixed-effects part of the nested error linear regression model with the
#' dependent variable on the left of a ~ operator and the explanatory
#' variables on the right, separated by + operators. The argument corresponds
#' to the argument \code{fixed} in function \code{\link[nlme]{lme}}.
#' @param smp_data a data frame that needs to comprise all variables named in
#' \code{fixed} and \code{smp_domains}.
#' @param smp_domains a character string containing the name of a variable
#' that indicates domains in the sample data. The variable can be numeric or a
#' factor.
#' @param transformation a character string. Three different transformation
#' types for the dependent variable can be chosen (i) no transformation ("no");
#' (ii) log transformation ("log"); (iii) Box-Cox transformation ("box.cox").
#' @param method a character string. In order to determine the optimal parameter
#' for a Box-Cox transformation six different estimation methods can be chosen
#' (i) restricted Maximum-Likelihood ("reml"); (ii) pooled skewness
#' minimization ("p.skew"); (iii) skewness minimization ("skew");
#' (iv) minimization of Kolmogorov-Smirnoff divergence  ("div.ks");
#' (v) minimization of Craemer von Mises divergence ("div.cvm"); (vi)
#' minimization of Kullback Leibner divergence  ("div.kl"). In case of no and
#' log transformation "NA" can be selected since no optimization is neccessary
#' for these two transformation types.
#' @param interval a numeric vector containing a lower and upper limit
#' determining an interval for the estimation of the optimal parameter. Defaults
#' to c(-1,2).
#' @return a scalar returning the optimal lambda.
#' @examples
#' # Loading data
#' data("Xoutsamp_AuxVar")
#' data("incomedata")
#'
#' opt_par <- optimal_parameter(generic_opt, income~educ1, incomedata,
#' "provlab", "box.cox", "reml")
#' @export
#' @import nlme
#' @importFrom FNN KL.divergence
#' @importFrom moments skewness

optimal_parameter <- function(generic_opt,
                              fixed,
                              smp_data,
                              smp_domains,
                              transformation,
                              method,
                              interval=c(-1,2)) {

  if(transformation != "no" &&
     transformation != "log") {
    # no lambda -> no estimation -> no optmimization

    # Estimation of optimal lambda parameters
    optimal_parameter <- optimize(generic_opt,
                                  fixed          = fixed,
                                  smp_data       = smp_data,
                                  smp_domains    = smp_domains,
                                  transformation = transformation,
                                  method         = method,
                                  interval       = interval,
                                  maximum        = F
                                  )$minimum

  } else {
    optimal_parameter <- NULL
  }

  return(optimal_parameter)
} # End optimal parameter


# Internal documentation -------------------------------------------------------

# Function generic_opt provides different estimation methods to specifiy
# the optimal parameter lambda. Here its important that lambda is the
# first argument because generic_opt is given to optimize. Otherwise,
# lambda is missing without default.


# External documentation -------------------------------------------------------

#' Selects estimation method for optimal parameter in Box-Cox transformations
#'
#' Function \code{generic_opt} selects the estimation method for the optimal
#' lambda that is neccessary to conduct Box-Cox transformations.
#'
#' @param lambda a number that determines the Box-Cox transformation.
#' @param fixed a two-sided linear formula object describing the
#' fixed-effects part of the nested error linear regression model with the
#' dependent variable on the left of a ~ operator and the explanatory
#' variables on the right, separated by + operators. The argument corresponds
#' to the argument \code{fixed} in function \code{\link[nlme]{lme}}.
#' @param smp_data a data frame that needs to comprise all variables named in
#' \code{fixed} and \code{smp_domains}.
#' @param smp_domains a character string containing the name of a variable
#' that indicates domains in the sample data. The variable can be numeric or a
#' factor.
#' @param transformation a character string. Three different transformation
#' types for the dependent variable can be chosen (i) no transformation ("no");
#' (ii) log transformation ("log"); (iii) Box-Cox transformation ("box.cox").
#' @param method a character string. In order to determine the optimal parameter
#' for a Box-Cox transformation six different estimation methods can be chosen
#' (i) restricted Maximum-Likelihood ("reml"); (ii) pooled skewness
#' minimization ("p.skew"); (iii) skewness minimization ("skew");
#' (iv) minimization of Kolmogorov-Smirnoff divergence  ("div.ks");
#' (v) minimization of Craemer von Mises divergence ("div.cvm"); (vi)
#' minimization of Kullback Leibner divergence  ("div.kl"). In case of no and
#' log transformation "NA" can be selected since no optimization is neccessary
#' for these two transformation types.
#' @return Depending on the selected \code{method} the return is a log
#' likelihood, a skewness, a pooled skewness or a Kolmogorov-Smirnoff, Craemer
#' von Mises or Kullback Leibner divergence.
#' @examples
#' # Example for method reml
#' log_likelihood <- generic_opt(0.0589, income~educ1, incomedata, "provlab",
#' "box.cox", "reml")
#' @export
#' @import nlme
#' @importFrom FNN KL.divergence
#' @importFrom moments skewness


generic_opt <- function(lambda,
                        fixed,
                        smp_data,
                        smp_domains,
                        transformation,
                        method
                        ) {

  # Model estimation is needed for skewness and divergence minimization and
  # is different to REML
  transformed_data <- data_transformation(fixed=fixed,
                                         smp_data=smp_data,
                                         transformation=transformation,
                                         lambda=lambda
                                         )$transformed_data

  model_est <- lme(fixed     = fixed,
                   data      = transformed_data,
                   random    = as.formula(paste0("~ 1 | as.factor(", smp_domains, ")")),
                   method    = "REML",
                   keep.data = FALSE)

  res <- residuals(model_est, level=0, type = "pearson")

  #Definition of optimization function for finding the optimal lambda
  optimization <- if(method == "reml") {
        reml(fixed          = fixed,
             smp_data       = smp_data,
             smp_domains    = smp_domains,
             transformation = transformation,
             lambda         = lambda
             )
        } else if (method == "p.skew") {
          pooled_skewness_min(model_est = model_est, res = res)
        } else if (method == "skew") {
          skewness_min(res = res)
        } else if (method == "div.ks") {
          divergence_min_KS(res = res)
        } else if (method == "div.cvm") {
          divergence_min_CvM(res = res)
        } else if (method == "div.kl") {
          divergence_min_KL(res = res)
        }

      return(optimization)
}


# Estimation methods for the optimal parameter lambda (all only internal) ------


# REML method ------------------------------------------------------------------

reml <- function(fixed          = fixed,
                 smp_data       = smp_data,
                 smp_domains    = smp_domains,
                 transformation = transformation,
                 lambda         = lambda
                 ) {

  sd_transformed_data <- std_data_transformation(fixed          = fixed,
                                                 smp_data       = smp_data,
                                                 transformation = transformation,
                                                 lambda         = lambda
                                                 )
  model_REML <- lme(fixed     = fixed,
                    data      = sd_transformed_data,
                    random    = as.formula(paste0("~ 1 | as.factor(", smp_domains, ")")),
                    method    = "REML",
                    keep.data = FALSE)

  log_likelihood <- -logLik(model_REML)

  return(log_likelihood)
}


# Pooled skewness minimization by Natalia --------------------------------------

pooled_skewness_min <- function(model_est = model_est, res = res) {
  skew_resid <- skewness(res)
  random_effect <- as.matrix(random.effects(model_est))[,1]
  skew_random_effect <- skewness(random_effect)
  sigmae2est <- model_est$sigma^2
  sigmau2est <- as.numeric(VarCorr(model_est)[1,1])
  w <- sigmae2est / (sigmae2est + sigmau2est)

  pooled_skew <- w * abs(skew_resid) + (1 - w) * abs(skew_random_effect)

  return(pooled_skew)
  }

# Skewness minimization by Molina ----------------------------------------------

skewness_min <- function(res = res) {
  skew_resid <- skewness(res)

  absolute_skew <- abs(skew_resid)

  return(absolute_skew)
}


# Divergence minimization by Kolmogorov Smirnoff -------------------------------

divergence_min_KS <- function(res = res) {
  step.length <- 10^-4
  eval.probs <- seq(0, 1, by = step.length)
  eval.points <- qnorm(eval.probs, mean = mean(res), sd = sd(res))
  test.probs <- ecdf(res)(eval.points)
  difs <- eval.probs - test.probs

  supremum_diff <- max(abs(difs)) #minimization of the supremum of differences

  return(supremum_diff)
}


# Divergence minimization by Craemer von Mises ---------------------------------
divergence_min_CvM <- function(res = res) {

  step.length <- 10^-4
  eval.probs <- seq(0, 1, by = step.length)
  eval.points <- qnorm(eval.probs, mean = mean(res), sd = sd(res))
  test.probs <- ecdf(res)(eval.points)
  difs <- eval.probs - test.probs

  sum_sq_diff <- sum((difs)^2) #minimization of the sum of the squared diferences

  return(sum_sq_diff)
}

# Divergence minimization by Kullback-Leibler ----------------------------------
divergence_min_KL <- function(res = res) {

  step.length <- 10^-4
  eval.probs <- seq(0, 1, by = step.length)
  eval.probs <- eval.probs[-c(1,length(eval.probs))]
  eval.points <- qnorm(eval.probs, mean = mean(res), sd = sd(res))
  test.probs <-  quantile(res, probs = eval.probs)
  divergence_KL <- KL.divergence(eval.probs, test.probs, k = 5)

  median_divergence_KL <- median(divergence_KL)

  return(median_divergence_KL)
}
