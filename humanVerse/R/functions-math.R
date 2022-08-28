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

cosine.similarity = function(a, b=NULL, by="col", ...)
	{	
	# is.vector assumes there are not attributes attached ... 
	# is.atomic returns TRUE for matrix 
	adim = dim(a); bdim = dim(b);
	if(is.null(adim) && is.atomic(a) && !is.null(b) && is.null(bdim) && is.atomic(b))
		{
cat("\n MONTE \n");
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
cat("\n ALEX \n");
		if(by == "ro") { a = t(a); } # just transpose 
		m.names = colnames(a);
		n = ncol(a);
		m = matrix(0, nrow=n, ncol=n, dimnames = list(m.names, m.names));
		d = s = m;  # angular distance, angular similarity, cosine similarity
		for(i in 2:n)
			{
			for(j in 1:(i-1))
				{
				cs = .cosine.similarity( a[, i], a[, j], ...);
				as = .angular.similarity(a[, i], a[, j], cs=cs, ...);
				
				m[i, j] = cs;
				s[i, j] = as;
				}
			}
		m = m + t(m); # lower triangle
		s = s + t(s);
		diag(m) = 1;	# non-computed self-similarity
		diag(s) = 1;
		d = 1-s;
		
		res = m;	
		res = property.set("angular.similarity", res, s);
		res = property.set("angular.distance",   res, d);
		return( res );
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
cat("\n NAT \n");
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








num.toCFrac = function(x, ..., 
								max.depth = 16, 
								tol = sqrt(.Machine$double.eps) , 
								part="Re"
						)
	{
	x = dots.addTo(x, ...); 
	x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
	
	n = length(x);
	e = vector("list", n);
	for(i in 1:n)
		{
		x_ = x[i];
		
		j = 1;		
		w = as.integer(x_); # whole part 
		r = x_ - w; 		# remainder 
		eps = r;			# global difference from x_  
		d = w;				# running decimal form 
		
		e[[i]] = c(w);
		while(j < max.depth)
			{
			# internally if tolerance of eps (r) is reached, we break ...
			if(abs(eps) < tol) { break; }
			w = as.integer( 1/r );
			r = (1/r) - w;			# for next iteration 
			e[[i]] = c(e[[i]], w);
			
			if(w > 1)
				{
				d = d + (1/w); 			# update the decimal form, 
										# could track num/den	
				} else {
						d = d + r;		# we are converging 
						}
			eps = x_ - d;						
			j = 1 + j;
			}
		#	NA analagous to ... (continues), marker that we stopped manually.
		if(j == max.depth) { e[[i]] = c(e[[i]], NA); } 	
		
			
		# https://math.stackexchange.com/questions/3084970/how-to-convert-continued-fractions-into-normal-fractions
		}
	e;
	}
	
num.toContinuousFraction = num.toCFrac;

# phi = (1 + sqrt(5)) / 2
# x = c(1/7, 1/123, pi, phi)
 
toFrac = function(x, ...,	max.depth=16, tol = sqrt(.Machine$double.eps) , part="Re", return="n/d")	# could return Euclidean nested
	{	
	x = dots.addTo(x, ...); 
	x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
	n = length(x);
	e = vector("list", n);
	for(i in 1:n)
		{
		x_ = x[i];
		
		j = 1;		
		w = as.integer(x_); # whole part 
		r = x_ - w; 		# remainder 
		e[[i]] = c(w);
		while(j < max.depth)
			{
			# internally if tolerance of eps (r) is reached, we break ...
			if(r < tol) { break; }
			w = as.integer( 1/r );
			r = (1/r) - w;
			e[[i]] = c(e[[i]], w);			
			j = 1 + j;
			}
		#	NA analagous to ... (continues), marker that we stopped manually.
		# if(j == max.depth) { e[[i]] = c(e[[i]], NA); } 	
		
			
		# https://math.stackexchange.com/questions/3084970/how-to-convert-continued-fractions-into-normal-fractions
		
		
		hm2 = 0; 
		hm1 = 1;
		km2 = 1;
		km1 = 0;
		
		# hn=anhn−1+hn−2
		# kn=ankn−1+kn−2
		
		num0 = den0 = 1;
		a = e[[i]]; alen = length(a);
		num = numeric(alen);	num[1] = a[2] * 1 + 0; num[2] = a[3] * num0 + 1;
		den = numeric(alen);	den[1] = a[2] * 0 + 1; den[2] = a[3] * den0 + 0;
		
		e[[i]] = property.set("info", e[[i]], list("depth" = j, "remainder" = r));
		}
	names(e) = x;
	e;
	}

# https://en.wikipedia.org/wiki/Continued_fraction#Some_useful_theorems
# error is ORIG - FRAC / ORIG 
# https://math.stackexchange.com/questions/3084970/how-to-convert-continued-fractions-into-normal-fractions















