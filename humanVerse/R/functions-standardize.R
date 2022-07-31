

#' standardizeToFactor
#'
#' Standarize a vector by multiplying by a factor
#'
#' @family Standardize
#'
#' @param x numeric vector
#' @param factor what we will multiply by
#'
#' @return numeric vector, updated
#' @export
#'
#' @examples
#' standardizeToFactor ( c(2: 10), 2 );
#' standardizeToFactor ( c(-2, 1: 10), 1/1000 );
#'
standardizeToFactor = function(x,factor=1)
	{
	x*factor;
	}

#' standardizeToMin
#'
#' Standarize a vector to its minimum
#'
#' @family Standardize
#'
#' @param x numeric vector
#'
#' @return numeric vector, updated
#' @export
#'
#' @examples
#' standardizeToMin ( c(2: 10) );
#' standardizeToMin ( c(-2, 1: 10) );
#'
standardizeToMin = function(x, myMin=NULL)
	{
  if(is.null(myMin)) { myMin = min(x, na.rm=T); }
	x/myMin;
	}

#' standardizeToMax
#'
#' Standarize a vector to its maximum
#'
#' @family Standardize
#'
#' @param x numeric vector
#'
#' @return numeric vector, updated
#' @export
#'
#' @examples
#' standardizeToMax ( c(2: 10) );
#' standardizeToMax ( c(-2, 1: 10) );
#'
standardizeToMax = function(x, myMax=NULL)
	{
  if(is.null(myMax)) { myMax = max(x, na.rm=T); }
	x/myMax;
	}

#' standardizeToN
#'
#' Standarize a vector based on it's length
#'
#' @family Standardize
#'
#' @param x numeric vector
#' @param count.na should we use NA's in our count for normalization?
#'
#' @return numeric vector, updated
#' @export
#'
#' @examples
#' standardizeToN ( c(2: 10) );
#' standardizeToN ( c(-2, 1: 10) );
#'
standardizeToN = function(x,count.na=FALSE)  # row-normalization
	{
	n = length(stats::na.omit(x));
	if(count.na) { n = length(x); }
	x/n;
	}

#' standardizeToSum
#'
#' Standarize a vector based on it's sum
#'
#' @family Standardize
#'
#' @param x numeric vector
#'
#' @return numeric vector, updated
#' @export
#'
#' @examples
#' s.s = standardizeToSum ( c(2: 10) );    s.s;
#' sum(s.s);
#'
#' s.s = standardizeToN ( c(-2, 1: 10) );  s.s;
#' sum(s.s);
#'
standardizeToSum = function(x)  # row-normalization
	{
	x/sum(x, na.rm=T);
	}



#' convertFromOneRangeToAnother
#'
#' \url{https://stats.stackexchange.com/questions/281162/scale-a-number-between-a-range}
#'
#'
#' @param x numeric vector
#' @param onerange this is typically the current range of the vector [numeric vector of length 2]
#' @param another this is the new scaled range [numeric vector of length 2]
#'
#' @return numeric vector, updated
#' @export
#'
#' @examples
#' convertFromOneRangeToAnother( 1:5, c(0,1) );  # map to [0,1]
#' convertFromOneRangeToAnother( runif(20, min=-1, max=1), c(0,1) );  # map to [0,1]
#' convertFromOneRangeToAnother( 1:5, c(1,7) );  # e.g., Likert-5 to Likert-7
#' convertFromOneRangeToAnother( rep(1,10), c(0,1) );  #NaN ... division by zero
#'
convertFromOneRangeToAnother = function(x, another, onerange=range(x) )
	{
	if(length(another) < 2)  { return (NULL); }
	if(length(onerange) < 2) { return (NULL); }

	rmin = onerange[1]; # min
	rmax = onerange[2]; # max

	tmin = another[1]; # min
	tmax = another[2]; # max

	# if (rmax-rmin) == 0 ... will return INF or -INF (NaN)

	tmin + (tmax-tmin) * (x - rmin) / (rmax - rmin);
	}


#' convertFromOldRangeToNew
#'
#' @param x numeric vector
#' @param oldrange this is typically the current range of the vector [numeric vector of length 2]
#' @param newrange this is the new scaled range [numeric vector of length 2]
#'
#' @return numeric vector, updated
#' @export
#'
#' @examples
#' convertFromOldRangeToNew( 1:5, newrange=c(0,1) );  # map to [0,1]
#' convertFromOldRangeToNew( runif(20, min=-1, max=1), newrange=c(0,1) );  # map to [0,1]
#' convertFromOldRangeToNew( 1:5, newrange=c(1,7) );  # e.g., Likert-5 to Likert-7
#' convertFromOldRangeToNew( rep(1,10), newrange=c(0,1) );  #NaN ... division by zero
#'
convertFromOldRangeToNew = function(x, oldrange=range(x), newrange=NULL)
	{
  if(length(newrange) < 2)  { return (NULL); }
	if(length(oldrange) < 2) { return (NULL); }

	rmin = oldrange[1]; # min
	rmax = oldrange[2]; # max

	tmin = newrange[1]; # min
	tmax = newrange[2]; # max

	# if (rmax-rmin) == 0 ... will return INF or -INF (NaN)

	tmin + (tmax-tmin) * (x - rmin) / (rmax - rmin);
  }
