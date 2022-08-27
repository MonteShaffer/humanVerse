# https://stackoverflow.com/questions/1746501/
	  # a = c(2,1,0,2,0,1,1,1)
	  # b = c(2,1,1,1,1,0,1,1)
	  # d = (a %*% b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))

	  ## OR

	  # e = crossprod(a, b) / (sqrt(crossprod(a, a)) * sqrt(crossprod(b, b)))

# RECURSIVE ... # https://www.statology.org/cosine-similarity-r/


#' .cosine.similarity
#'
#' This is a univariate calculation
#'
#' See <https://en.wikipedia.org/wiki/Cosine_similarity#Definition>
#'
#' @param a vector 'a'
#' @param b vector 'b'
#' @param method use 'crossprod' or less-efficient default option
#'
#' @return the cosine similarity
#' @export
#'
#' @examples
#' a = c(2,1,0,2,0,1,1,1); b = c(2,1,1,1,1,0,1,1);
#' .cosine.similarity( a,b );
#'
# property.get("srcref", .cosine.similarity); # lsa is NULL
.cosine.similarity = function(a, b, method="cpp", technique="crossprod")
  {
  m = functions.cleanKey(method, 1);
  tech = functions.cleanKey(technique, 4);
  # cat("\n\n ==================== COSINE SIMILARITY (a,b) ========== \n\n");
  # cat("\n", " ===  a === "); print(a); cat("\n");
  # cat("\n", " ===  b === "); print(a); cat("\n");

  # maybe perform some non-zero vector "checks"
  # sum(a); sum(b);  if "fails", return NA ... with warning()
  if(sum(a) == 0 && sum(b) == 0)
    {
    return (NA);
    }

  if(tech == "cros")
    {
    theta = crossprod(a, b) / (sqrt(crossprod(a, a)) * sqrt(crossprod(b, b)));
    } else  {
            theta = (a %*% b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)));
            }

	# flatten 
	if(is.complex(theta)) { theta = as.complex(theta); return(theta); }
  as.numeric(theta);   # as.numeric on complex ?
  }



#' .angular.distance
#'
#' This is a univariate calculation
#'
#' See <https://en.wikipedia.org/wiki/Cosine_similarity#Angular_distance_and_similarity>
#'
#' @param a vector 'a'
#' @param b vector 'b'
#'
#' @return the angular distance
#' @export
#'
#' @examples
#' a = c(2,1,0,2,0,1,1,1); b = c(2,1,1,1,1,0,1,1);
#' .angular.distance(  a, b );
#'
.angular.similarity = function(a, b, return="similarity", ...)
  {
  r = functions.cleanKey(return, 1);
  cos.sim = .cosine.similarity( a,b, ... );
  # any element in either is negative
  vector.neg = ( sum( is.negative(a,b) ) > 0 ); 
  if(vector.neg)
    {
    ad = 1 * acos(cos.sim) / pi;
    } else  {
            ad = 2 * acos(cos.sim) / pi;
            }
	as.sim = 1 - ad;
	
# angular distance
if(r == "d")
	{
	res = ad;
  res = property.set("cosine.similarity", res, cos.sim);
  res = property.set("angular.similarity", res, as.sim);
  return(res);
	}
# angular similarity 
res = as.sim;	
res = property.set("cosine.similarity", res, cos.sim);
  res = property.set("angular.distance", res, ad);
  res;
  }



angular.similarity = function(a, b=NULL, by="col", return="similarity", ...)
	{	
	# is.vector assumes there are not attributes attached ... 
	# is.atomic returns TRUE for matrix 
	adim = dim(a); bdim = dim(b);
	if(is.null(adim) && is.atomic(a) && !is.null(b) && is.null(bdim) && is.atomic(b))
		{
		return( .angular.similarity(a,b, return=return, ...) );
		}
	by = functions.cleanKey(by, 2);
	if(is.matrix(a) && is.null(b))
		{
		if(by == "ro") { a = t(a); } # just transpose 
		m.names = colnames(a);
		n = ncol(a);
		m = matrix(0, nrow=n, ncol=n, dimnames = list(m.names, m.names));		
		for(i in 2:n)
			{
			for(j in 1:(i-1))
				{
				m[i, j] = .angular.similarity(a[, i], a[, j], return=return, ...);
				}
			}
		m = m + t(m); # lower triangle
		diag(m) = 1;  # non-computed self-similarity
		return(m);
		}
		
	v = NULL;
	if(is.null(adim) && !is.null(bdim))
		{
		v = a;
		m = b;
		}
	if(!is.null(adim) && is.null(bdim))
		{
		v = b;
		m = a;
		}
		
	if(!is.null(v))
		{
		nv = length(v);	
		mdim = dim(m);
		if(by == "co" && (nv != mdim[1]))
			{
			if(nv != mdim[2]) { stop("bad dimensions, can't fix"); }
			if(nv == mdim[2]) { m = t(m); mdim = dim(m); } # just transpose 
			}
		if(by == "ro" && (nv != mdim[2]))
			{
			if(nv != mdim[1]) { stop("bad dimensions, can't fix"); }
			if(nv == mdim[1]) { m = t(m); mdim = dim(m); } # just transpose 
			}
		## good dimensions ... everything by column ...
		m.names = colnames(m);
		n = ncol(m);
		res = numeric(n);
		for(i in 1:n)
			{
			res[i] = .angular.similarity(v, m[, i], ...);
			}
		names(res) = m.names;
		return(res);		
		}
		
	stop("what are you doing here!");	
	}


# ?pmatch ?charmatch ?match.arg ... NOT argmatch ... 

cosine.similarity = function(a, b=NULL, by="col", ...)
	{	
	# is.vector assumes there are not attributes attached ... 
	# is.atomic returns TRUE for matrix 
	adim = dim(a); bdim = dim(b);
	if(is.null(adim) && is.atomic(a) && !is.null(b) && is.null(bdim) && is.atomic(b))
		{
		return( .cosine.similarity(a,b, ...) );
		}
	by = functions.cleanKey(by, 2);
	if(is.matrix(a) && is.null(b))
		{
		if(by == "ro") { a = t(a); } # just transpose 
		m.names = colnames(a);
		n = ncol(a);
		m = matrix(0, nrow=n, ncol=n, dimnames = list(m.names, m.names));		
		for(i in 2:n)
			{
			for(j in 1:(i-1))
				{
				m[i, j] = .cosine.similarity(a[, i], a[, j], ...);
				}
			}
		m = m + t(m); # lower triangle
		diag(m) = 1;  # non-computed self-similarity
		return(m);
		}
		
	# vector by-col of matrix 
	# cosine(a, data) works by-columns 
	# cosine(data, a) does NOT work ...
	# a = c(23, 34, 44, 45, 42, 27, 33, 34);
	# data = structure(c(23, 34, 44, 45, 42, 27, 33, 34, 17, 18, 22, 26, 26, 29, 31, 30, 34, 35, 35, 36, 51, 29, 30, 31), dim = c(8L, 3L), dimnames = list( NULL, c("a", "b", "c")));
	# Using control = "exact" (short for control = c("all", "hexNumeric")) comes closest to making deparse() an inverse of parse() (but we have not yet seen an example where "all", now including "digits17", would not have been as good). However, not all objects are deparse-able even with these options, and a warning will be issued if the function recognizes that it is being asked to do the impossible.
	# SET DEFAULT to dput(pi, control="all") ... in my INIT() as an exacmple of messing with the base::defaults ...
	
	
	v = NULL;
	if(is.null(adim) && !is.null(bdim))
		{
		v = a;
		m = b;
		}
	if(!is.null(adim) && is.null(bdim))
		{
		v = b;
		m = a;
		}
		
	if(!is.null(v))
		{
		nv = length(v);	
		mdim = dim(m);
		if(by == "co" && (nv != mdim[1]))
			{
			if(nv != mdim[2]) { stop("bad dimensions, can't fix"); }
			if(nv == mdim[2]) { m = t(m); mdim = dim(m); } # just transpose 
			}
		if(by == "ro" && (nv != mdim[2]))
			{
			if(nv != mdim[1]) { stop("bad dimensions, can't fix"); }
			if(nv == mdim[1]) { m = t(m); mdim = dim(m); } # just transpose 
			}
		## good dimensions ... everything by column ...
		m.names = colnames(m);
		n = ncol(m);
		res = numeric(n);
		for(i in 1:n)
			{
			res[i] = .cosine.similarity(v, m[, i], ...);
			}
		names(res) = m.names;
		return(res);		
		}
		
		
	stop("what are you doing here!");	
	}

























