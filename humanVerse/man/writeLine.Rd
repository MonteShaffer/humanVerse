% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions-file.R
\name{writeLine}
\alias{writeLine}
\title{writeLine}
\usage{
writeLine(str, file, append = TRUE, end = "\\n")
}
\arguments{
\item{str}{The character string to be written}

\item{file}{The file to store the (str) line}

\item{append}{If TRUE, will append to the end of the file, otherwise it will overwrite an existing file}

\item{end}{EOL character to finish the line; the line separator}
}
\value{

}
\description{
This function writes a single character string to a file.
Very useful for simulations and building data one line at a time.
[ encoding is slow ]
}
\examples{
# writeLine("hello there", file="R:/monte/says/hi/again/my.log", append=FALSE);
# writeLine("hi again", file="R:/monte/says/hi/again/my.log");
}
