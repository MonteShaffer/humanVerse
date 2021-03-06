% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions-system.R
\name{extractList}
\alias{extractList}
\title{extractList}
\usage{
extractList(myList, envir = .GlobalEnv)
}
\arguments{
\item{myList}{the list to be extracted}

\item{envir}{the environment inwhich to extract it}
}
\value{
updates and assigns the values
}
\description{
This is not recurive, gets "names" of lists and extracts them to the environment as accessible variables using assign.
}
\examples{

mylist = list("a" = 1, "b" = 2, "c" = 3);
        extractList(mylist);
        print(a); print(b); print(c);


}
