% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions-parse.R
\name{listToString}
\alias{listToString}
\title{listToString}
\usage{
listToString(mylist, sep1 = "-", sep2 = "_")
}
\arguments{
\item{mylist}{list to stringify, NOT RECURSIVE (one level)}

\item{sep1}{"-"}

\item{sep2}{"_"}
}
\value{
string
}
\description{
Often paramaters for options can be collapsed to form a folder name or a md5 hash
}
\examples{

my.list = list("A" = 3, "L" = ".", "E" = 1, "X" = 4);
    listToString(my.list);

}
