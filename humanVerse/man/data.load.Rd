% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions-data.R
\name{data.load}
\alias{data.load}
\title{data.load}
\usage{
data.load(
  dfolder,
  dstem = NULL,
  mode = "rds",
  main = NULL,
  raw = NULL,
  sub = "data",
  access.remotely = FALSE,
  force.download = FALSE
)
}
\arguments{
\item{mode}{Is it ".rds" or ".json" or ".txt" file?}

\item{main}{If null, it will grab from humanVerse github}

\item{raw}{If null, it will grab from humanVerse github}

\item{sub}{Is there a subfolder to use?}

\item{access.remotely}{Should I just access remotely OR download/cache it?}

\item{force.download}{If it is cached, should I force a new download?}
}
\value{
data object (likely a dataframe or a list)
}
\description{
Generically load a data object from a resource (likely URL)
}
\examples{
## https://github.com/MonteShaffer/humanVerse/tree/main/data/personality
# prds.df = data.load("personality", "personality-raw");  # from .rds
# ptxt.df = data.load("personality", "personality-raw", mode="txt");
# p.self = data.load("personality", "personality_self-en", mode="json");
# p.other = data.load("personality", "personality_other-en", mode="json");


## OTHER github source
## https://github.com/DataWar/imdb/tree/main/2020-Sept
# main = "https://github.com/DataWar/imdb/";
# raw = "https://raw.githubusercontent.com/DataWar/imdb/";
# imdb.data = data.load("2020-Sept", "imdb", mode="rds", main=main, raw=raw, sub="");
## This imdb.data is 60MB compressed, 400+MB uncompressed ...
}
