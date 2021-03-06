% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions-str.R
\name{strPadRight}
\alias{strPadRight}
\alias{numberPadRight}
\title{strPadRight}
\usage{
strPadRight(str, final.str.len, padding = "0", method = "stringi")
}
\arguments{
\item{str}{The 'string' (can be a number)}

\item{final.str.len}{How long the final str is to be}

\item{padding}{Fill with, default is "0" (zero)}
}
\value{
string
}
\description{
strPadRight
}
\examples{
strPadRight("33.01",5);
strPadRight("33.01",6);
strPadRight("33.01",7);
strPadRight("33.01",8);
}
