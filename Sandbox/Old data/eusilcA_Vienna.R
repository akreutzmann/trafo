#' Simulated eusilc data - sample data for Vienna
#'
#' The data set is a selected part of the data set \code{\link[emdi]{eusilcA_smp}}. 
#' The present data set only contains information for the state Vienna.
#'
#' @format A data frame with 236 observations and 19 variables:
#' \describe{
#' \item{eqIncome}{numeric; a simplified version of the equivalized household income.}
#' \item{eqsize}{numeric; the equivalized household size according to the 
#' modified OECD scale.}
#' \item{gender}{factor; the person's gender (levels male and female).}
#' \item{cash}{numeric; employee cash or near cash income (net).}
#' \item{self_empl}{numeric; cash benefits or losses from self-employment (net).}
#' \item{unempl_ben}{numeric; unemployment benefits (net).}
#' \item{age_ben}{numeric; old-age benefits (net).}
#' \item{surv_ben}{numeric; survivor's benefits (net).}
#' \item{sick_ben}{numeric; sickness benefits (net).}
#' \item{dis_ben}{numeric; disability benefits (net).}
#' \item{rent}{numeric; income from rental of a property or land (net).}
#' \item{fam_allow}{numeric; family/children related allowances (net).}
#' \item{house_allow}{numeric; housing allowances (net).}
#' \item{cap_inv}{numeric; interest, dividends, profit from capital investments
#'  in unincorporated business (net).}
#' \item{tax_adj}{numeric; repayments/receipts for tax adjustment (net).}
#' \item{country}{factor; country (one level)}
#' \item{state}{factor; state (nine levels)}
#' \item{district}{factor; districts (96 levels)}
#' \item{county}{factor; county}
#' }
#' @docType data
"eusilcA_Vienna"