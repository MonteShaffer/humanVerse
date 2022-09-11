
# toFrac ... if not very good, just return ORIG/1 as string ...
# num.toFrac( seq(0, 720, by=30) %deg%. / pi )
# (0:30)/6 * pi %rad%.


math.cleanup = function() {}
math.cleanup = function(x, tol = sqrt(.Machine$double.eps), ...)
	{
	# maybe sqrt(3)/2 ... or FRAC/PI ... append as attribute?, go.deep=TRUE
	# zeros 
	z = is.zero(x, tol=tol, ...); # Re / Im also possible.
	x[z] = 0;
	x;
	}
































angle.convert = function(A, ..., from="degrees", to="radians")
	{
	A = dots.addTo(A, ...);
	# convert everthing to "degrees" on first pass
	F = prep.arg(from, n=1, case="upper");
	T = prep.arg(to, n=1, case="upper");
# dput(A); dput(F); dput(T); 
	deg = switch(F,					  			
					  "D" 	= A,
					  "R"	= (180/pi) * A,	
					  "G"  	= A * 9/10,			
				A											# DEFAULT
				);
	
	# convert everything from "degrees" on second pass 
	
	res = switch(T,					  			
					  "D" 	= deg,
					  "R"	= (pi/180) * deg,	
					  "G"  	= deg * 10/9,					
				deg											# DEFAULT
				);
	math.cleanup( res );
	}





## DRG ... LOL
# x = c("D", "R", "G");
# xlon = c("deg", "rad", "gon");
# m = e1071::permutations(3)[,1:2];
# m2 =  matrix(x[m], ncol=2);
# m3 = m2[!duplicated(m2), ];
# m4 = m3[ c( which(m3[,1] == x[1]), which(m3[,1] == x[2]), which(m3[,1] == x[3]) ),  ];
# m5 = m4[ c(1,2,  4,3,  5,6), ];

# for(i in 1:6) 
	# {
	# mm = m5[i, ];
	# first = mm[1]; 
	# flon = xlon[pmatch(tolower(first), xlon)]; 
	# second = mm[2];
	# slon = xlon[pmatch(tolower(second), xlon)];
	temp.c2f = 	function(degC) { temp.convert(degC, "C", "F"); }
	# row = paste0( flon, "2", slon, " = function(A", tolower(first), ", ...) { angle.convert(A", tolower(first), ", ...,  from=\"", toupper(first), "\", to=\"", toupper(second), "\"); } ");
	# print.noquote(row);
	# cat(row, "\n\n");
	# }

deg2rad = function(Ad, ...) { angle.convert(Ad, ...,  from="D", to="R"); }  

deg2gon = function(Ad, ...) { angle.convert(Ad, ...,  from="D", to="G"); }  

rad2gon = function(Ar, ...) { angle.convert(Ar, ...,  from="R", to="G"); }  

rad2deg = function(Ar, ...) { angle.convert(Ar, ...,  from="R", to="D"); }  

gon2deg = function(Ag, ...) { angle.convert(Ag, ...,  from="G", to="D"); }  

gon2rad = function(Ag, ...) { angle.convert(Ag, ...,  from="G", to="R"); }  


	
"%deg%" = function(deg, to="R") 
	{ 
	# default is to="R"
	# traps the single dot (.) as option 
	if(!check.type(to)) { to="R"; } 
	if(is.na(to) || is.null(to)) { to = "R"; }
	angle.convert(deg, from="D", to=to); 
	}

"%rad%" = function(rad, to="D") 
	{ 
	# default is to="D"
	# traps the single dot (.) as option 
	if(!check.type(to)) { to="D"; } 
	if(is.na(to) || is.null(to)) { to = "D"; }
	angle.convert(rad, from="R", to=to); 
	}	
	

"%gon%" = function(gon, to="D") 
	{ 
	# default is to="D"
	# traps the single dot (.) as option 
	if(!check.type(to)) { to="D"; } 
	if(is.na(to) || is.null(to)) { to = "D"; }
	angle.convert(gon, from="G", to=to); 
	}


# 30  %deg%. 
# pi  %rad%.  # dot means default... or you could put pi %rad% "G"
# 100 %gon%. 

















####################### REGULAR TRIG ###########################
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


# no atan2h?
math.atan2 = function(y, x)
	{
	# x = dots.addTo(x, ...);
	math.cleanup( atan(y, x) );
	}

arctan2 = function() {}
arctan2 = math.atan2;


####################### HYPERBOLIC TRIG ###########################


math.sinh = function(x, ...)
	{
	# maybe do better with fractional components
	x = dots.addTo(x, ...);
	math.cleanup( sinh(x) );
	}
	
math.cosh = function(x, ...)
	{
	# maybe do better with fractional components
	x = dots.addTo(x, ...);
	math.cleanup( cosh(x) );
	}
	
math.tanh = function(x, ...)
	{
	# maybe do better with fractional components
	x = dots.addTo(x, ...);
	math.cleanup( tanh(x) );
	}

cotanh 		= function(x, ...) { 1/math.tanh(x,...); }
cosecanth 	= function(x, ...) { 1/math.sinh(x,...); } 	
secanth 	= function(x, ...) { 1/math.cosh(x,...); } 
	
	
math.asinh = function(x, ...)
	{
	# maybe do better with fractional components
	x = dots.addTo(x, ...);
	math.cleanup( asinh(x) );
	}

arcsinh = function() {}
arcsinh = math.asinh;	

	
math.acosh = function(x, ...)
	{
	# maybe do better with fractional components
	x = dots.addTo(x, ...);
	math.cleanup( acosh(x) );
	}

arccosh = function() {}
arccosh = math.acosh;

	
	
math.atanh = function(x, ...)
	{
	# maybe do better with fractional components
	x = dots.addTo(x, ...);
	math.cleanup( atanh(x) );
	}

arctanh = function() {}
arctanh = math.atanh;
























############## COSINE.SIMILARITY (ANGULAR DISTANCE) ###################

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
	m = prep.arg(method, 1);
	tech = prep.arg(technique, 4);
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
	r = prep.arg(return, 1); # [s]imilarity or [d]istance 
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
	by = prep.arg(by, 2);
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




