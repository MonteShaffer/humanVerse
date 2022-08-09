



#' @rdname is.dir
#' @export
is.dir = dir.exists;

#' @rdname is.file
#' @export
is.file = file.exists;


##################################################
#'
#' is.windows
#'
#'
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
is.windows = function()
	{
	str.contains("win", str.tolower(.Platform[["OS.type"]]) );
	# .isWindows() ... from library(digest)
	# ??? ... from library(???)
	}



is.error = function(e)
	{
	condition = attributes(e)$condition;
	if(is.null(condition)) { return(FALSE); }
	# see list.fromError(e) for other ideas to improve this function
	extra = attributes(condition)$class;
	if("error" %in% extra) { return(TRUE); }
	# is this necessary
	return(FALSE);
	}


##################################################
#'
#' is.url
#'
#'
#' @param file (what is the character string to be searched)
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples

is.url = function(files)
	{
	files = str.trim(files);
	fil = functions.cleanKey(files, 3);
	x = (fil == "htt"); y = (fil == "ftp");  # multivariate, truth tables
	return ( (x+y > 0) );
	}


##################################################
#'
#' is.library
#'
#'
#' @param str (what is the character string to be searched)
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
is.library = function(str = "stringi", suggestion=TRUE)
	{
	res = isTRUE(requireNamespace( str , quietly = TRUE));
	if(!res && suggestion)
		{
		pkg = paste0( "install.packages(\"",str,"\", dependencies=TRUE ); ")
		msg = paste0("\n\n", str.commentWrapper("LIBRARY is not found!"), "\n\n",
					"You could try installing the package: ", "\n\n",
					str.commentWrapper( pkg, r.tag = "-", s.pad=15), "\n");
		warning(msg);
		}
	res;
	}




##################################################
#'
#' is.substring
#'
#'
#' @param needle (substring is UNI-VARIATE)
#' @param haystack (is MULTI-VARIATE)
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
is.substring = function(needle, haystack)
	{
	grepl(needle, haystack, fixed = TRUE);
	}



#' @rdname str_contains
#' @export
str_contains = is.substring;

#' @rdname str.contains
#' @export
str.contains = is.substring;



### THESE FUNCTIONS SEEM TO BE "mono-nuclear"
#' @rdname is.true
#' @export
is.true = isTRUE;


#' @rdname is.false
#' @export
is.false = isFALSE;



##################################################
#'
#' is.set
#'
#' isset — Determine if a variable is declared and is different than null
#' https://www.php.net/manual/en/function.isset.php
#'
#' @param object
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
is.set = function(obj, allow.NULL=FALSE)
	{
	obj.str = deparse(substitute(obj));
	my.obj = obj.fromString(obj.str);
	if(isFALSE(my.obj[1])) 
		{
		e = property.get( my.obj, "ERROR" );
		if(!is.null(e)) { return(FALSE); }
		}
	# extend functionality, we can check  is.set(obj, TRUE) ... returns true if exists REGARDLESS of NULL ... default behavior is like php::isset
	if(!allow.NULL && is.null(my.obj)) { return(FALSE); }
	return(TRUE);
	}


##################################################
#'
#' is.empty
#'
#' https://www.php.net/manual/en/function.empty.php
#' Determine whether a variable is empty
#' Determine whether a variable is considered to be empty. A variable is considered empty if it does not exist or if its value equals false. empty() does not generate a warning if the variable does not exist
#'
#' @param object
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
is.empty = function(obj)
	{
	obj.str = deparse(substitute(obj));
	my.obj = obj.fromString(obj.str);
	if(is.null(my.obj)) { return(TRUE); }  
	if(isFALSE(my.obj[1])) 
		{
		e = property.get( my.obj, "ERROR" );
		if(!is.null(e)) { return(TRUE); }
		}

	n.len = length(my.obj);
	n.type = typeof(my.obj);

	if(n.type == "list" && n.len > 0) { return(FALSE); }

	## check for all-zeros, all-str.trim "", all-na, all-nan

	if(is.numeric(my.obj) && stats.sum(my.obj) > 0) { return(FALSE); }
	if(is.logical(my.obj) && stats.sum(my.obj) > 0) { return(FALSE); }

	# all(is.na(c(NA, NaN)))

	if(is.character(my.obj))
		{
		my.obj[is.na(my.obj)] = "";
		s1 = (my.obj == ""); # empty
		if(all(s1)) { return(TRUE); }
		}

	if(!is.null( typeof(obj) ) ) { return (FALSE); }

	return (TRUE);  # unknown typeof ?
	}




##################################################
#'
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
is.wholeNumber = function(x, ..., tol = sqrt(.Machine$double.eps), part="Re")
  {
  # See ?is.integer
  more = unlist(list(...)); x = c(x, more); 
  x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
  abs(x - round(x)) < tol;
  }



##################################################
#'
#' is.even
#'
#'
#'
#' @param x number (and vector) to evaluate
#' @param part By default, the "Re"al part
#'
#' @return TRUE OR FALSE
#' @export
#'
#' @examples
#'
is.even = function(x, ..., part="Re")
	{
	more = unlist(list(...)); x = c(x, more); 
	x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
	x = as.integer(x);	
	( (x %% 2) == 0 );  
	}

##################################################
#'
#' is.odd
#'
#'
#'
#' @param x number (and vector) to evaluate
#' @param part By default, the "Re"al part
#'
#' @return TRUE OR FALSE
#' @export
#'
#' @examples
#'
is.odd = function(x, ..., part="Re")
	{
	more = unlist(list(...)); x = c(x, more); 
	x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
	x = as.integer(x);	
	( (x %% 2) == 1 );  
	}

##################################################
#'
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

##################################################
#'
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
#' is.negative( c(-1*1:5,-sin(pi), 0,0,0, sin(pi), 1:5, NA, NA) );
#'
is.negative = function(x, ..., tol = sqrt(.Machine$double.eps), part="Re")
  {
  more = unlist(list(...)); x = c(x, more);
  x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
  x < ( -1 * tol );
  }


##################################################
#'
#' is.zero
#'
#' @param x number (and vector) to evaluate
#' @param tol tolerance of "zero" [floating point]
#'
#' @return TRUE OR FALSE
#' @export
#'
#' @examples
#'
#' is.zero(1);
#' is.zero(0);
#' is.zero(-1);
#' is.zero( c(-1*1:5,-sin(pi), 0,0,0, sin(pi), 1:5, NA, NA) );
#'
is.zero = function(x, ..., tol = sqrt(.Machine$double.eps), part="Re")
	{
	more = unlist(list(...)); x = c(x, more);
	x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
	
	x.pos = x < tol;
	x.neg = x > -1 * tol;
	
	( (x.pos + x.neg) > 1);
	}




































