% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions-number.R
\name{zeroIsh}
\alias{zeroIsh}
\title{zeroIsh}
\usage{
zeroIsh(x, ..., digits = getOption("digits"))
}
\arguments{
\item{x}{a vector of numerics (matrix also works)}

\item{digits}{how precise to label a value zero}
}
\value{
updated vector with zeroes
}
\description{
zeroIsh
}
\examples{
options(scipen = 999);
options(digits = 22);

x = c(sin(pi), -sin(pi));
zeroIsh(x);
zeroIsh(x, 8);
}
\seealso{
Other Maths: 
\code{\link{isClose}()}
}
\concept{Maths}
