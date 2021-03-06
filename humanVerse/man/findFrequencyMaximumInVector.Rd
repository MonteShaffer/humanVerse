% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions-vector.R
\name{findFrequencyMaximumInVector}
\alias{findFrequencyMaximumInVector}
\alias{howManyTimesDoesMaximumAppearInVector}
\alias{freqMax}
\title{findFrequencyMaximumInVector}
\usage{
findFrequencyMaximumInVector(x)
}
\arguments{
\item{x}{numeric vector}
}
\value{
count, integer of frequency
}
\description{
What frequency is maximum in x (occurs the most)?
}
\examples{
f.max = 1:9;
findFrequencyMaximumInVector( f.max ); # 1, from all of them

f.max = c( rep(1, 3), rep(2:8,5), rep(9,1) );
findFrequencyMaximumInVector( f.max );  # 5, from the 2:8 (ties)

}
\seealso{
Other Vectors: 
\code{\link{doUnique}()},
\code{\link{findAllIndexesWithValueInVector}()},
\code{\link{findFrequencyMinimumInVector}()},
\code{\link{findFrequencyValueInVector}()},
\code{\link{notDuplicated}()},
\code{\link{whichMaxFreq}()},
\code{\link{whichMax}()},
\code{\link{whichMinFreq}()},
\code{\link{whichMin}()},
\code{\link{whichValue}()}
}
\concept{Vectors}
