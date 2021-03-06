% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sum0.R
\name{sum0}
\alias{sum0}
\title{Sum of Vector Elements with alternative empty-set definition}
\usage{
sum0(x, na.rm = TRUE)
}
\arguments{
\item{x}{numeric, complex or logical vector}

\item{na.rm}{logical. Should missing values (including NaN) be removed?}
}
\value{
\code{scalar} sum of all values in \code{x}. Different to \code{\link{sum}}, the 
sum of an empty set is undefined (see example). This is helpful for aggregating count data 
across observations that are nested in groups and for some groups all observations are missing
}
\description{
Sum of Vector Elements with alternative empty-set definition
}
\examples{

 x <- c(NA,NA,NA)
 sum(x,na.rm=TRUE)
 sum0(x,na.rm=TRUE)
 
}
\seealso{
\code{\link{sum}}
}
