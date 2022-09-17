
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
v.truncate = function(vec, parent, by="value", invert=FALSE)  
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
