

#' count.sign.changes
#'
#' @param x
#' @param na.rm
#' @param part
#'
#' @return
#' @export
count.sign.changes = function(x, na.rm=TRUE, part="Re")
  {
# https://stackoverflow.com/questions/17220837/counting-the-number-of-times-a-value-change-signs-with-r
# Note that sign does not operate on complex vectors.
  if(part == "Im") { x = Im(x); } else { x = Re(x); }
  if(na.rm) { x = stats::na.omit(x); }
  sum(diff(sign(x)) != 0);
  }

#' which.sign.changes
#'
#' @param x
#' @param na.rm
#' @param part
#'
#' @return
#' @export
which.sign.changes = function(x, na.rm=TRUE, part="Re")
  {
  if(part == "Im") { x = Im(x); } else { x = Re(x); }
  if(na.rm) { x = stats::na.omit(x); }  # isn't this required to count sign changes, NA are already gone 
  which( diff(sign(x)) != 0 )
  }

#' is.wholeNumber
#'
#' `is.integer` doesn't operate as you would expect, this does
#'
#' @param x number (and vector) to evaluate
#' @param tol tolerance of "zero"
#'
#' @return TRUE OR FALSE
#' @export
#'
#' @examples
#'
#' is.wholeNumber(1);
#' is.wholeNumber(1.1);
#'
#' is.wholeNumber(0);
#' is.wholeNumber(0.1);
#'
#' is.wholeNumber(-1);
#' is.wholeNumber(-1.1);
#'
#' is.wholeNumber(rnorm(5));
#' is.wholeNumber(rpois(5,1));
#'
# is.whole.number
is.wholeNumber = function(x, ..., tol = sqrt(.Machine$double.eps), part="Re")
  {
  # See ?is.integer
  more = unlist(list(...)); x = c(x, more); 
  x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
  abs(x - round(x)) < tol;
  }




is.even = function(x, ..., part="Re")
	{
	more = unlist(list(...)); x = c(x, more); 
	x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
	x = as.integer(x);
	
	( (x %% 2) == 0 );  # this implies it is a whole number ... https://stackoverflow.com/questions/6102948/why-does-modulus-division-only-work-with-integers
	}

is.odd = function(x, ..., part="Re")
	{
	more = unlist(list(...)); x = c(x, more); 
	x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
	x = as.integer(x);
	
	( (x %% 2) == 1 );  # this implies it is a whole number
	}



#' is.positive
#'
#' @param x number (and vector) to evaluate
#' @param tol tolerance of "zero"
#'
#' @return TRUE OR FALSE
#' @export
#'
#' @examples
#'
#' is.positive(1);
#' is.positive(0);
#' is.positive(-1);
#' is.positive( c(-1*1:5,-sin(pi), 0,0, sin(pi), 1:5) );
#'
is.positive = function(x, ..., tol = sqrt(.Machine$double.eps), part="Re")
  {
  more = unlist(list(...)); x = c(x, more);
  x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
  x > tol;
  }

#' is.negative
#'
#' @param x number (and vector) to evaluate
#' @param tol tolerance of "zero"
#'
#' @return TRUE OR FALSE
#' @export
#'
#' @examples
#'
#' is.negative(1);
#' is.negative(0);
#' is.negative(-1);
#' is.negative( c(-1*1:5,-sin(pi), 0,0, sin(pi), 1:5, NA, NA) );
#'
is.negative = function(x, ..., tol = sqrt(.Machine$double.eps), part="Re")
  {
  more = unlist(list(...)); x = c(x, more);
  x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
  x < ( -1 * tol );
  }

# x = c(NULL,-1*1:5,0+2i,1+2i,-1+2i,-sin(pi), 0,0, sin(pi), 0-2i,1-2i,-1-2i, 1:5, NA, NA, NULL, NA, NULL);
# y = c( -1*1:5,0+2i,1+2i,-1+2i,-sin(pi), 0,0, sin(pi), 0-2i,1-2i,-1-2i, 1:5, NA, NA, NA);
# is.zero( c(NULL, -1*1:5,0+2i,1+2i,-1+2i,-sin(pi), 0,0, sin(pi), 0-2i,1-2i,-1-2i, 1:5, NA, NA, NULL) );
is.zero = function(x, ..., tol = sqrt(.Machine$double.eps), part="Re")
	{
	more = unlist(list(...)); x = c(x, more);
	x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
	
	#x < tol || x > -1 * tol;	
	# !is.negative(x) && !is.positive(x);	
	
	x.pos = x < tol;
	x.neg = x > -1 * tol;
	
	( (x.pos + x.neg) > 1);
	}


is.equal = `%==%` = function(x, y, tol = sqrt(.Machine$double.eps), part="Re")
	{
	if(typeof(x) == "complex" || typeof(y) == "complex") 
		{
		x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
		y = if(part == "Im") { y = Im(y); } else { y = Re(y); }
	
		res = ( ( (x + tol > y) + (y + tol > x) ) == 2);
		return(res);
		}
	isClose(x,y, comparison="==", tol=tol);
	}


# `%>=%` <- function(x, y) (x + epsilon > y)
# https://stackoverflow.com/questions/2769510/numeric-comparison-difficulty-in-r
is.ge = `%>=%` = function(x, y, tol = sqrt(.Machine$double.eps), part="Re")
	{
	if(typeof(x) == "complex" || typeof(y) == "complex") 
		{
		x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
		y = if(part == "Im") { y = Im(y); } else { y = Re(y); }
	
		res = (x + tol >= y );  # what about strictly '>'? tol is taking care of "average noise"?
		return(res);		
		}
	isClose(x,y, comparison=">=", tol=tol);
	}

is.le = `%<=%` = function(x, y, tol = sqrt(.Machine$double.eps), part="Re")
	{
	if(typeof(x) == "complex" || typeof(y) == "complex") 
		{
		# a master function to compare both Re + Im at the same time ... 
		x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
		y = if(part == "Im") { y = Im(y); } else { y = Re(y); }
		
		res = (y + tol >= x );  # what about strictly '>'? tol is taking care of "average noise"?
		return(res);
		}
	isClose(x,y, comparison=">=", tol=tol);	
	}






#' get.sign
#'
#' @param x number(s)  to evaluate
#' @param other default is NA, zero ... 0 ... may be applicable at times
#' @param tol tolerance of "zero"
#' @param return by default "number" otherwise "character"
#'
#' @return 1 (+) if positive, -1 (-) if negative, "other" otherwise
#' @export
#'
#' @examples
#'
#' get.sign(-1);
#' get.sign(-1, return="chr");
#' get.sign(1);
#' get.sign(1, return="character");
#' get.sign(0);
#' get.sign(0, return="char");
#' get.sign(0, 0);
#' get.sign(0, other=".", return="char");
#' get.sign(0, 0, other=".", return="char");
#'
#' get.sign(-1*(1:10)); # variadic now works
#' get.sign(-1*(1:10), return="char");
#'
#' get.sign(pi);
#' get.sign(-pi);
#' get.sign( sin(pi) );
#' get.sign( -sin(pi) );
#' get.sign( sin(pi), 0 );
#' get.sign( -sin(pi), 0 );
#' get.sign( sin(pi), tol = .Machine$double.eps^2 );
#' get.sign( -sin(pi), tol = .Machine$double.eps^2  );
#'
get.sign = function(xs, ..., other="NA", return="number", tol = sqrt(.Machine$double.eps))
	{
	more = unlist(list(...)); xs = c(xs, more);
	nxs = length(xs);

	myval = cval = other;
	out = c();
	for(x in xs)
		{
	  myval = other;
		if(is.negative(x, tol=tol)) { myval="-"; }
		if(is.positive(x, tol=tol)) { myval="+"; }
		# print(myval);
		if(return == "number")
			{
			if(is.na(myval))
			  {
			  if(is.numeric(other))
				{
				out = c(out, other);
				} else {
						out = c(out, NA);
						}
			  }
			if(myval == "-") { out = c(out, -1); }
			if(myval == "+") { out = c(out, 1); }
			if(is.numeric(other)) { out = c(out, other); }
			} else {
					out = c(out, as.character(myval));
					}
		}
	out;
	}




#' isClose
#'
#' Due to issues regarding float-point precision, this function
#' assesses if two numerics are equal "almost".
#'
#' See:  \url{https://stackoverflow.com/questions/63787649/}
#'
#' The structure of this function allows it to be used in traditional subsetting
#'  notation.
#'
#'
#' @family Maths
#'
#' @param a number(s) ... numeric vector of length 1+
#' @param b number(s) ... numeric vector of length 1+
#' @param tol what is the tolerance level of "equalish" ...
#'  defaults to what is used in the function \code{\link{all.equal}}
#'
#' @return vector of logical (bool) with the maximum length of (a,b)
#' @export
#'
#' @examples
#' options(digits=22);
#' n1 = 0.14999999999999999;
#' n2 = 0.15;
#' n3 = 0.15000000000000002;
#' N = c(n1,n2,n3);
#'
#' isClose(n1,n2);
#' isClose(n2,n3);
#' isClose(n1,n3);
#' isClose(N);
#'
#' myTol = 0.00000000000000002;
#' isClose(n1,n2, myTol);
#' isClose(n2,n3, myTol);
#' isClose(n1,n3, myTol);
#'
#' a = sample(N, 5, replace=TRUE);
#' isClose( a, n2, tol=myTol);
#' b = sample(N, 5, replace=TRUE);
#' isClose( n1, b, tol=myTol);
#'
#' a = sample(N, 5, replace=TRUE);
#' b = sample(N, 5, replace=TRUE);
#' isClose( a, b, tol=myTol);
#'
#' a = sample(N, 9, replace=TRUE);
#' b = sample(N, 5, replace=TRUE);
#' isClose( a, b, tol=myTol);  # prints warning, returns matrix
#'
#' sin(pi) == 0;  # Ben's example
#' as.integer( sin(pi) ) = 0;
#' isClose( sin(pi), 0 );
# is.close
# https://stackoverflow.com/questions/30773762/r-functions-aliases-documentation
isClose = function(a,b, tol=sqrt(.Machine$double.eps), comparison="==", force.pairwise=TRUE )
  {
  # comparison == works on COMPLEX numbers, others do NOT work ... 
  # we assume no issues with stats::na.omit
  if(missing(b)) { b = a; force.pairwise=FALSE; } # this will do the matrix comparison
  a.n = length(a);
  b.n = length(b);
  if(a.n == b.n && b.n == 1)  # one of each
    {	
    return ( doComparison(a,b, tol=tol, comparison=comparison ) );
    }
	
	
  if(a.n == b.n && force.pairwise==TRUE) # parallel vector comparison (pairwise)
	{
	r = logical(a.n);
	for(i in 1:a.n)
	  {
	  r[i] = doComparison(a[i],b[i], tol=tol, comparison=comparison );
	  }
	return (r);
	}
	
	if(a.n != b.n)
		{	
		# could be unequal vectors, not one
		warning("Not of Equal length ... Computing Matrix");  # cross-product [combinatorics a.n*b.n and store in matrix] of comparisons
		}

	m = matrix(NA, nrow=a.n, ncol=b.n);
	for(i in 1:a.n)
	  {
	  for(j in 1:b.n)
		{
		m[i,j] = doComparison(a[i],b[j], tol=tol, comparison=comparison );
		}
	  }
	  colnames(m) = c(paste0("b.",1:b.n));
	  rownames(m) = c(paste0("a.",1:a.n));
	return (m);	
	}



doComparison = function(a, b, comparison="==", tol=sqrt(.Machine$double.eps) )
	{
	# internal function, assumes length(a) == length(b) == 1
	res = switch(comparison,
							"=="    = isTRUE( all.equal (a,b, tol) ),
							">="    = isTRUE(a + tol >= b ),
							"<="    = isTRUE(b + tol >= a ),
				isTRUE( all.equal (a,b, tol) ) # default case of switch
				);
	res;
	}
	


#' zeroIsh
#'
#'
#' @family Maths
#'
#'
#' @param x a vector of numerics (matrix also works)
#' @param digits how precise to label a value zero
#'
#' @return updated vector with zeroes
#' @export
#'
#' @examples
#' options(scipen = 999);
#' options(digits = 22);
#'
#' x = c(sin(pi), -sin(pi));
#' zeroIsh(x);
#' zeroIsh(x, 8);
#' # NOW works for a list of matrices
zeroIsh = function(x, ..., digits=getOption("digits"), collapse=TRUE)
	{
	more = list(...); 	n.more = length(more);
	xlist = list();
	xlist[[1]] = zapZero(x, digits=digits); # univariate		
	if(n.more == 0) { return( xlist[[1]] ); } 	# no more ...
	for(i in 1:n.more)
		{
		xlist[[i+1]] = zapZero(more[[i]], digits=digits); # univariate		
		}	
	if(!collapse) { return( xlist); }	
	unlist(xlist);
	}

zapZero = function(x, digits=getOption("digits"))
	{
	# positive values
	x.pos = 1 / 10^digits
	x[((x > 0) & (x < x.pos))] = 0L;
	# negative values
	x.neg = -1 * x.pos;
	x[((x < 0) & (x > x.neg))] = 0L;  
	x;	
	}
	
	