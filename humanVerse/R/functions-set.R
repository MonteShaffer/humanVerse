##################################################
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
	m = functions.cleanKey(method, 1);
	if(m == "o") { return( na.omit(x, ...) )	; }
	if(m == "e") { return( na.exclude(x, ...) )	; }	
	}

##################################################
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
	
##################################################
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