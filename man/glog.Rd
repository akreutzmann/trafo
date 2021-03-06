% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Glog.R
\name{glog}
\alias{glog}
\title{Glog transformation for linear models}
\usage{
glog(object)
}
\arguments{
\item{object}{an object of type lm.}
}
\value{
An object of class \code{trafo}. Methods such as 
\code{\link{as.data.frame.trafo}} and \code{\link{print.trafo}} can 
be used for this class.
}
\description{
The function transforms the dependent variable of a linear model using the 
Glog transformation.
}
\examples{
# Load data
data("cars", package = "datasets")

# Fit linear model
lm_cars <- lm(dist ~ speed, data = cars)

# Transform dependent variable 
glog(object = lm_cars)
}
\references{
Durbin BP, Hardin JS, Hawkins DM, Rocke DM (2002). A Variance-stabilizing 
Transformation for Gene-expression Microarray Data. Bioinformatics, 18, 
105-110.
}
