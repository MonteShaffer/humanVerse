
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
set.diff = setdiff;
set.union = function() {}
set.union = union;
set.merge = union;
set.intersect = function() {}
set.intersect = intersect;
set.equal = function() {}
set.equal = setequal;
set.isElement = function() {}
set.isElement = is.element;
	
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
	
