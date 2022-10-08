
# https://www.rdocumentation.org/packages/Zelig/versions/4.2-1
set.mix = function() {}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' na.remove
#'
#' Remove na's from an object (usually vector)
#'
#' @param x Data object
#' @param method [o]mit or [e]xclude
#'
#' @return
#' @export
#'
#' @examples
na.remove = function(x, method="omit", ...)
	{
	m = prep.arg(method, 1);
	if(m == "o") { return( na.omit(x, ...) )	; }
	if(m == "e") { return( na.exclude(x, ...) )	; }	
	}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' set.match
#'
#' Wrapper for match() function.  searching in set
#'
#' @param searching Elements that may/may not be in the Set
#' @param set The Entire Set
#'
#' @return
#' @export
#'
#' @examples
set.match = function(searching, set, ...)
	{
	# searching %in% set 
	# set %in% searching   # different things
	# returns NA, I don't like it ...
	# could have two NA ... but could have match and NA ?
	# 
	# res = match(searching, set, ...);
	# if(anyNA(res
	# match(x, table, nomatch = 0) > 0
	match(searching, set, ...);	
	}
	
# maybe just set.match = match 
set.diff = function() {}
# very similar to intersect ... 
set.diff = setdiff; 
set.union = function() {}
set.union = union;
set.merge = union;
set.intersect = function() {}
# would it be faster if it was union logic
# find c(u,v) ... find unique(c,v) ... and subtract ?
set.intersect = intersect;
set.equal = function() {}
# would it be faster to sort and do identical?
# than to do match(a,b) and match(b,a)
set.equal = setequal;  
set.isElement = function() {}
set.isElement = is.element;
# is .Internal(which) or .Internal(match) faster?
	
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'	
#' set.remove
#'
#' Using match() function to remove elements from the SET
#'
#' @param elements Elements to remove that may/may not be in the Set
#' @param set The Entire Set
#'
#' @return
#' @export
#'
#' @examples
set.remove = function(elements, set, ...)
	{
	if(is.null(elements)) { return(set); }
	x = na.remove( match(elements, set, ...) );
	if(length(x) == 0) { return(set); }
	set[-c(x)];
	}
  
# set.remove is set.substract(a, b) 
# below let's you get the indexes ...	


# subtract vec from parent ...
# should truncate be about a set or a length ...  
vddDFSSD.truncate = function(vec, parent, by="value", invert=FALSE)  
	{
	BY = prep.arg(by, n=1);
	# shorten parent by removing vec 
	idx = set.match(vec, parent); 
	if(invert) { idx = v.invert(vec,idx); }
	res = (1:length(parent))[-c(idx)]; 
	if(BY=="v") { res = parent[-c(idx)]; }
	# res = set.diff(parent, vec);
	v.return(res);
	}





# e = yt - y.hat
# xls.RMSE(e) ... xls.ME ... xls.MAD ... xls.MPE ... xls.MAPE 
# xls.RSQ ... xls.INTERSEPT ... xls.SLOPE ... 
# rewrite TRENDLINE function ... trendline ... use only xls.COMMANDS 
# transform the x, y to call SLOPE/INTERSEPT 
# fn = B0 + B1x1 + B2x2
# linear prob / logistic / 2-param Richards (floor=0, ceil=1) ... equivalent?
# sigma() ... fitted() ... 
# accuracy() ... forecast::forecast()
# matching coefficient ... jaccards coefficient
# misscaliffication aka error
# sensitivity ... aka recall
# 

set.info = function(A, B)
	{
	all = c(A,B);
	all.u = unique(all);
	
	A = c(1,1,3,4); # table(A);  FREQ ,,, 
	B = c(5,3,3,1); # table(B);  FREQ ,,,
	
	# match is like which.min ... just returns the first element 
	# set.info do everything ... dataframe ... duplicates yes or no version 
	# 
	
	}
	
set.union = function(A, B, allow.duplicates=FALSE)
	{
	all = c(A,B);
	
	}
	
# R set theory is wrong ... based on unique indexes?
# allow.duplicates=FALSE (replicate R behavior)
# pair theory ... not useful in data 
# https://www.youtube.com/watch?v=AAJB9l-HAZs
# complemtn of A/B from universal 
# x = 1,2,3 ... y = 3,1,3 ... 
# union would be collection with matches removed as duplicates
# unique or duplicates are not pair-matching ... bad design
# we don't assume in set theory that members of x are UNIQUE, do we?
# set.subtract (a from b)
# str.subtract (a from b)
# x = set* first 10 primes AND first 10 odd numbers ... duplicate entries
# y = set* first 3 primes, 8th prime ... AND third/fifth odd number 
# what does UNION imply?  INTERSECT, etc.  unique/duplicate seems wrong.

	
	
