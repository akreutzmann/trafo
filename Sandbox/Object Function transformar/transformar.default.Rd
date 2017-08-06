\name{transformar.default}
\alias{transformar.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Select a transformation
}
\usage{
\method{transformar}{default}(object, lambda = seq(-2, 2, 0.05), ...)
}
\arguments{

\item{a}{data.frame or a matrix, whose first column is the dependent variable and the rest of the columns are the independent variables.}
}
\value{
Optimal value for \code{lambda} and value of \code{profile log-likelihood}
}
\description{
Select a transformation
}
\keyword{internal}
