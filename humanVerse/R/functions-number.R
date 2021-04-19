

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
  if(na.rm) { x = stats::na.omit(x); }
  which( diff(sign(x)) != 0 )
  }

#' is.whole.number
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
#' is.whole.number(1);
#' is.whole.number(1.1);
#'
#' is.whole.number(0);
#' is.whole.number(0.1);
#'
#' is.whole.number(-1);
#' is.whole.number(-1.1);
#'
#' is.whole.number(rnorm(5));
#' is.whole.number(rpois(5,1));
#'
is.whole.number = function(x, tol = sqrt(.Machine$double.eps))
  {
  # See ?is.integer
  abs(x - round(x)) < tol;
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
#' is.positive(c(-1*1:5,1:5));
#'
is.positive = function(x, ..., tol = sqrt(.Machine$double.eps))
  {
  more = unlist(list(...)); x = c(x, more);
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
#' is.negative(c(-1*1:5,1:5));
#'
is.negative = function(x, ..., tol = sqrt(.Machine$double.eps))
  {
  more = unlist(list(...)); x = c(x, more);
  x < ( -1 * tol );
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
#' get.sign(0, ".", return="char");
#'
#' # get.sign(-1*(1:10)); # doesn't work
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
get.sign = function(xs, ..., other=NA, return="number", tol = sqrt(.Machine$double.eps))
	{
	more = unlist(list(...)); xs = c(xs, more);

	myval = cval = other;
	out = c();
	for(x in xs)
		{
		if(is.negative(x,tol)) { myval="-"; }
		if(is.positive(x,tol)) { myval="+"; }
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
#'
#' myTol = 0.00000000000000002;
#' isClose(n1,n2, myTol);
#' isClose(n2,n3, myTol);
#' isClose(n1,n3, myTol);
#'
#' a = sample(N, 5, replace=TRUE);
#' isClose( a, n2, myTol);
#' b = sample(N, 5, replace=TRUE);
#' isClose( n1, b, myTol);
#'
#' a = sample(N, 5, replace=TRUE);
#' b = sample(N, 5, replace=TRUE);
#' isClose( a, b, myTol);
#'
#' a = sample(N, 9, replace=TRUE);
#' b = sample(N, 5, replace=TRUE);
#' isClose( a, b, myTol);  # prints warning, returns matrix
#'
#' sin(pi) == 0;  # Ben's example
#' isClose( sin(pi), 0 );
isClose = function(a,b, tol=sqrt(.Machine$double.eps) )
  {
  # we assume no issues with stats::na.omit
  a.n = length(a);
  b.n = length(b);

  if(a.n == b.n && b.n == 1)  # one of each
    {
    return ( isTRUE( all.equal (a,b, tol) ) );
    }
  if(a.n == b.n) # parallel vector comparison (pairwise)
    {
    r = logical(a.n);
    for(i in 1:a.n)
      {
      r[i] = isTRUE( all.equal (a[i],b[i], tol) );
      }
    return (r);
    }

  # could be unequal vectors, not one
  if(a.n != 1 && b.n !=1)
    {
    warning("Not of Equal length ... Computing Matrix");  # cross-product [combinatorics a.n*b.n and store in matrix] of comparisons

    m = matrix(NA, nrow=a.n, ncol=b.n);
    for(i in 1:a.n)
      {
      for(j in 1:b.n)
        {
        m[i,j] = isTRUE( all.equal (a[i],b[j], tol) );
        }
      }
      colnames(m) = c(paste0("b.",1:b.n));
      rownames(m) = c(paste0("a.",1:a.n));
    return (m);
    }

  one = if(a.n > b.n) { b } else { a }
  vec = if(a.n > b.n) { a } else { b }

  r = logical(length(vec));
  for(i in 1:length(vec))
    {
    r[i] = isTRUE( all.equal (one,vec[i], tol) );
    }
  r;
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
zeroIsh = function(x, ..., digits=getOption("digits"))
  {
  more = unlist(list(...)); x = c(x, more);
  # zapsmall has log10 feature

  # positive values
  x.pos = 1 / 10^digits
  x[((x > 0) & (x < x.pos))] = 0L;
  # negative values
  x.neg = -1 * x.pos;
  x[((x < 0) & (x > x.neg))] = 0L;

  # round(x, digits=digits);
  x;
  }

