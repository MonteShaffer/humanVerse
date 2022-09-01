
# toFrac ... if not very good, just return ORIG/1 as string ...
# num.toFrac( seq(0, 720, by=30) %deg%. / pi )
# (0:30)/6 * pi %rad%.


#' deg2rad 
#'
#' Convert angles from degrees to radians.
#'
#' @param degs One or more angles in degrees
#' @param ...  One or more angles in degrees
#'
#' @return One or more angles in radians.
#' @export
#'
#' @examples
#' deg2rad(c(1,3,34));
#' deg2rad(1,3,34);
#' deg2rad(1,3,"alex");
#'
deg2rad = function(degs, ...)
	{
	degs = dots.addTo(degs, ...);

	n = length(degs);
	res = numeric(n);
	for(i in 1:n)
		{
		deg = degs[i];
		ndeg = suppressWarnings(as.numeric(deg));
		rad = NaN;
		if( !is.na(ndeg) )  { rad = (pi/180) * ndeg; }
		res[i] = rad;
		}
	math.cleanup( res );
	}


#' rad2deg
#'
#' Convert angles from radians to degrees.
#' Similar to pracma::rad2deg however is vectorized (multivariate).
#'
#' @param degs One or more angles in radians.
#' @param ...  One or more angles in radians.
#'
#' @return One or more angles in degrees.
#' @export
#'
#' @examples
#' rad2deg(c(1,3,34));
#' rad2deg(1,3,34);
#' rad2deg(1,3,"alex");
#'
rad2deg = function(rads, ...)
	{
	rads = dots.addTo(rads, ...);
	n = length(rads);
	res = numeric(n);
	for(i in 1:n)
		{
		rad = rads[i];
		nrad = suppressWarnings(as.numeric(rad));
		deg = NaN;
		if( !is.na(nrad) )  { deg = (180/pi) * nrad; }
		res[i] = deg;
		}
	math.cleanup( res );
	}


"%deg%" = function(deg, r=NULL) { deg2rad(deg); }	
"%rad%" = function(rad, r=NULL) { rad2deg(rad); }	

# pi %rad%. 
# 30 %deg%. 




math.cleanup = function() {}
math.cleanup = function(x, tol = sqrt(.Machine$double.eps), ...)
	{
	# maybe sqrt(3)/2
	# zeros 
	z = is.zero(x, tol=tol, ...); # Re / Im also possible.
	x[z] = 0;
	x;
	}

math.sin = function(x, ...)
	{
	# maybe do better with fractional components
	x = dots.addTo(x, ...);
	math.cleanup( sin(x) );
	}
	
math.cos = function(x, ...)
	{
	# maybe do better with fractional components
	x = dots.addTo(x, ...);
	math.cleanup( cos(x) );
	}
	
math.tan = function(x, ...)
	{
	# maybe do better with fractional components
	x = dots.addTo(x, ...);
	math.cleanup( tan(x) );
	}

cotan 		= function(x, ...) { 1/math.tan(x,...); }
cosecant 	= function(x, ...) { 1/math.sin(x,...); } 	
secant 		= function(x, ...) { 1/math.cos(x,...); } 
	
	
math.asin = function(x, ...)
	{
	# maybe do better with fractional components
	x = dots.addTo(x, ...);
	math.cleanup( asin(x) );
	}

arcsin = function() {}
arcsin = math.asin;	

	
math.acos = function(x, ...)
	{
	# maybe do better with fractional components
	x = dots.addTo(x, ...);
	math.cleanup( acos(x) );
	}

arccos = function() {}
arccos = math.acos;

	
	
math.atan = function(x, ...)
	{
	# maybe do better with fractional components
	x = dots.addTo(x, ...);
	math.cleanup( atan(x) );
	}

arctan = function() {}
arctan = math.atan;










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
	# The cosine of two non-zero vectors (WIKI: Cosine similarity)
	if(anyNA(a) || anyNA(b))
		{
		return (NaN);
		}
	if(sum(a) == 0 && sum(b) == 0)
		{
		return (NaN);
		}

	if(tech == "cros")
		{
		theta = crossprod(a, b) / (sqrt(crossprod(a, a)) * sqrt(crossprod(b, b)));
		} else	{
				theta = (a %*% b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)));
				}

	# flatten 
	if(is.complex(theta)) { theta = as.complex(theta); return(theta); }
	as.numeric(theta);	 # as.numeric on complex ?
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
#' .angular.distance(	a, b );
#'
.angular.similarity = function(a, b, return="similarity", set.properties=FALSE, cs=NULL, ...)
	{
	r = functions.cleanKey(return, 1); # [s]imilarity or [d]istance 
	if(is.null(cs)) { cs = .cosine.similarity( a,b, ... ); }
	if(is.nan(cs)) { return(NaN); } 
	# any element in either is negative
	vector.neg = ( sum( is.negative(a,b) ) > 0 ); 
	if(vector.neg)
		{
		ad = 1 * acos(cs) / pi;
		} else	{
				ad = 2 * acos(cs) / pi;
				}
	as = 1 - ad;  # ad = 1 - as; 
	
	# angular distance
	if(r == "d")
		{
		res = ad;
		if(set.properties)
			{
			res = property.set("cosine.similarity",  res, cs);
			res = property.set("angular.similarity", res, as);
			}
		return(res);
		}
	# angular similarity 
	res = as;	
	if(set.properties)
		{
		res = property.set("cosine.similarity", res, cs);
		res = property.set("angular.distance",  res, ad);
		}
	res;
	}

 

# ?pmatch ?charmatch ?match.arg ... NOT argmatch ... 

	# vector by-col of matrix 
	# cosine(a, data) works by-columns 
	# cosine(data, a) does NOT work ...
	# a = c(23, 34, 44, 45, 42, 27, 33, 34);
	# data = structure(c(23, 34, 44, 45, 42, 27, 33, 34, 17, 18, 22, 26, 26, 29, 31, 30, 34, 35, 35, 36, 51, 29, 30, 31), dim = c(8L, 3L), dimnames = list( NULL, c("a", "b", "c")));
	# Using control = "exact" (short for control = c("all", "hexNumeric")) comes closest to making deparse() an inverse of parse() (but we have not yet seen an example where "all", now including "digits17", would not have been as good). However, not all objects are deparse-able even with these options, and a warning will be issued if the function recognizes that it is being asked to do the impossible.
	# SET DEFAULT to dput(pi, control="all") ... in my INIT() as an exacmple of messing with the base::defaults ...
	
	
cosine.similarity = function(a, b=NULL, by="col", ...)
	{	
	# is.vector assumes there are not attributes attached ... 
	# is.atomic returns TRUE for matrix 
	adim = dim(a); bdim = dim(b);
	if(is.null(adim) && is.atomic(a) && !is.null(b) && is.null(bdim) && is.atomic(b))
		{
#cat("\n MONTE \n");
		cs = .cosine.similarity(a,b, ...);
		as = .angular.similarity(a,b, cs=cs, ...);
		ad = 1-as;
		
		res = cs;	
		res = property.set("angular.similarity", res, as);
		res = property.set("angular.distance",   res, ad);
		return( res );
		}
	by = functions.cleanKey(by, 2);
	if(is.matrix(a) && is.null(b))
		{
#cat("\n ALEX \n");
		if(by == "ro") { a = t(a); } # just transpose 
		m.names = colnames(a);
		n = ncol(a);
		m = matrix(0, nrow=n, ncol=n, dimnames = list(m.names, m.names));
		d = s = m;  # angular distance, angular similarity, cosine similarity
		for(i in 1:n)
			{
			for(j in i:n)
				{
				cs = .cosine.similarity( a[, i], a[, j], ...);
				as = .angular.similarity(a[, i], a[, j], cs=cs, ...);
				
				m[i, j] = m[j, i] = cs;
				s[i, j] = s[j, i] = as;
				}
			}
		# self-similarity may be NaN ... doing "1" on diag is NOT correct
		d = 1-s;
		
		res = m;	
		res = property.set("angular.similarity", res, s);
		res = property.set("angular.distance",   res, d);
		return( res );
		}
		
	# maybe compare a vector to a matrix 
	
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
#cat("\n NAT \n");
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
		d = s = res;
		for(i in 1:n)
			{
			cs = .cosine.similarity( v, m[, i], ...);
			as = .angular.similarity(v, m[, i], cs=cs, ...);
			
			res[i] = cs;
			s[i] = as;
			}
		d = 1 - s;
		names(res) = names(d) = names(s) = m.names;
		res = property.set("angular.similarity", res, s);
		res = property.set("angular.distance",   res, d);
		return( res );		
		}
		
		
	stop("what are you doing here!");	
	}




