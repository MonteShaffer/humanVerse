% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions-colors.R
\name{stringToInteger}
\alias{stringToInteger}
\title{stringToInteger}
\usage{
stringToInteger(strvec, isHEX = FALSE)
}
\arguments{
\item{strvec}{Vector of strings}

\item{isHEX}{Are we looking at hexadecimal values}
}
\value{
vector of integers
}
\description{
stringToInteger
}
\examples{
stringToInteger( c("0xff", "077", "123"), FALSE );
stringToInteger( c("0xff", "077", "123"), TRUE );
}
