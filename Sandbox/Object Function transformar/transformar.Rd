\name{transformar}
\alias{transformar}
\title{Select a Transformation}
\usage{
transformar(object, ...)

\method{transformar}{default}(object)

\method{transformar}{formula}(object, data)

\method{transformar}{lm}(object)
}
\arguments{
\item{object}{a data.frame, matrix, formula or fitted model; see below.}

\item{data}{an optional data frame, list or environment as in lm.}

\item{XY}{a data.frame or a matrix, whose first column is the dependent variable and the rest of the columns are the independent variables.}

\item{family}{is NULL by default}
}
\value{
An object of class \code{transformation} with the following arguments

\item{lambdahat}{Optimal value for \code{lambda}}

\item{loglike}{The value of \code{profile log-likelihood}}

\item{family}{Employed family of transformations}

\item{yt}{Vector of the transformed response variable \code{y}}
}
\description{
Selects a transformation employing Akaike's Information Criterion AIC.
}
\keyword{transformations}
