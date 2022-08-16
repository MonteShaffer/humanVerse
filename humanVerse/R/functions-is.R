

is.prime = function(x, ..., optimus=FALSE)
	{
	# I could do bits.prime and do a "match" ...
	# this is saying "include optimus in search"
	# NA will be a problem 
	x = dots.addTo(x, ...);
	idx = 1; if(optimus) { idx = 2; }
	factors = prime.factors(x, optimus=optimus);
	nf = length(factors);
	if(is.list(factors))
		{		
		return( list.getProperty("prime", factors) );
		}
	# if one, 
	if(x == 1) { return(optimus); }
	return( (nf == idx) ); 
	}
	
	


# is.function only works on non-string?
# exists is the reverse, only on a string, not a non-string 
is.function = function(fn)
	{
	if(is.character(fn)) { if(exists(fn)) { return(TRUE); } else { return(FALSE); } }
	base::is.function(fn);
	}
	
#' @rdname function.exists
#' @export
function.exists = is.function;




is.POSIXt = function(x) { inherits(x, "POSIXt"); }
is.POSIXlt = function(x) { inherits(x, "POSIXlt"); }
is.POSIXct = function(x) { inherits(x, "POSIXct"); }



##################################################
#'
#' 
#'
#'
#' is.substring
#'
#' @param haystack
#' @param needle
#' @param out
#'
#' @return
#' @export
#'
#' @examples
is.substring = function(haystack, needle)
  {  
  grepl(needle, haystack, fixed = TRUE);
  }




has.color <- function() 
	{
	# this is large function in crayon
	 
	# DEBIAN ... 
	# Sys.getenv("COLORTERM"); 
	# Sys.getenv("LS_COLORS"); 
	# Sys.getenv("TERM");
	#  
	# TERM                    xterm-256color
	# what if I pass a #F9A3EE into xterm-256, will it work ?
	# have a flag to store in the "humanVerse" ... disable / enable ...
	#
	
	

	}

has.emacs = function()
	{
	Sys.getenv("EMACS") != "" || Sys.getenv("INSIDE_EMACS") != ""	
	}
# property.get("EMAC*", NULL, "system")

has.rstudio = function()
	{
	!(Sys.getenv("RSTUDIO", "") == "")
	}
# property.get("RSTUDIO*", NULL, "system")

# requireNamespace("rstudioapi", quietly = TRUE) &&
#    rstudioapi::isAvailable() &&
#    rstudioapi::hasFun("getConsoleHasColor")



is.list.element = function(element, list) {}

# > is.function(md5_)
# [1] TRUE
# > is.function("md5_")
# [1] FALSE
# exists("md5_")




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





# mytype = suppressError(
# may be different for tryCatch(
is.error = function(e, where="suppressError")
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
		# cat.me(msg, "warning"); 	# does color removal if in place
									# <b><i><u><br>ight, <color fg= bg=>
									# I think I wrote a downloader once, overwrite with this?
									# RGUI windows?
		
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
# works on obj, not functions ... use is.function
# if you call it on 'serialize' I am assuming you are looking for a character vector, a boolean, and so on ...
# maybe create a list of allowable types
# maybe create an environment scope issue 
## > ls("algo")
## Error in as.environment(pos) : no item called "algo" on the search list

# ?exists(x) 
# x: a variable name (given as a character string).
# is.set (given as an object)


# sys.frame(0)
is.set = function(obj, allow.NULL=FALSE, deep.scan=TRUE, ...)
	{
	
	debug = FALSE;
	
	mytype = suppressError( typeof(obj), 
								show.notice=debug,
								msg="debugging typeof is.set" 
							);
							

if(debug) {	
	print(mytype);						
cat("\n STEP 1 \n");
		}
	if(is.error(mytype)) 	{ return(FALSE); }
	
if(debug) {	
cat("\n STEP 2 \n");
}	
	if(!deep.scan && is.character(obj)) { return( exists(obj, ...) ); }
	
	# WHAT about monte$says as input 
	
	## isset is operating on objs that are not classes/functions
	## closures
if(debug) {
cat("\n STEP 3 \n");
}
	if(mytype == "closure") { return(FALSE); }
	
if(debug) {	
cat("\n STEP 4 \n");
}
	# WE NEED TO CHECK FALSE/NULL
	if( !(mytype == "NULL" || mytype == "character") ) { return(TRUE); }
	
if(debug) {	
cat("\n STEP 5 \n");
}
	if(mytype == "character")
		{
		x = eval(parse(text = obj));
		if(is.null(x)) { return(allow.NULL); } else { return(TRUE); }
		}

if(debug) {	
cat("\n STEP 6 \n");
}
	if(is.null(obj)) { return(allow.NULL); }

if(debug) {	
cat("\n STEP X \n");
}
	# do we still have BOOLEAN
	return(TRUE);		
	}



#' @rdname isset
#' @export
isset = is.set;


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
	
	# show.notice = TRUE ... debugging
	mytype = suppressError( typeof(my.obj), show.notice=FALSE,
							msg="debugging typeof is.empty" );	
	if(is.error(mytype)) { return(TRUE); }
	if(mytype == "closure") { return(FALSE); }
	
	
	if(isFALSE(my.obj[1])) 
		{
		e = property.get( "ERROR", my.obj );
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


#' @rdname empty
#' @export
empty = is.empty;


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
	
	( (x.pos + x.neg) > 1);  # TWO 
	}


# x `~=` y 
is.equal = function(x, y, tol = sqrt(.Machine$double.eps), part="Re")
	{
	check.isCompatibleLength(x, y);
	x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
	y = if(part == "Im") { y = Im(y); } else { y = Re(y); }
	d = x - y; 
	is.zero(d, tol=tol, part=part);	
	}
 
# https://www.r-bloggers.com/2016/11/how-to-write-and-document-special-functions-in-r/
# "%notin%" <- function(x, table) !(match(x, table, nomatch = 0) > 0)

#' @rdname %`~=`%
#' @export
"%~=%" = "%eq%" = is.equal;

# x `~>=` y 
is.ge = function(x, y, tol = sqrt(.Machine$double.eps), part="Re")
	{
	check.isCompatibleLength(x, y);
	x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
	y = if(part == "Im") { y = Im(y); } else { y = Re(y); }
	d = x - y; 
	is.positive(d, tol=tol, part=part);
	}

"%~>%" = "%ge%" = is.ge;

# x `~<=` y 
is.le = function(x, y, tol = sqrt(.Machine$double.eps), part="Re")
	{
	check.isCompatibleLength(x, y);
	x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
	y = if(part == "Im") { y = Im(y); } else { y = Re(y); }
	d = x - y; 
	is.negative(d, tol=tol, part=part);
	}
	
"%~<%" = "%le%" = is.le;



check.ifConformable = function(x, y) {} # matrix?

check.isCompatibleLength = function(x, y, 
									method="equal",  # "1-1-equal"
									action="warning", 
									msg = " obj1 [x] and obj2 [y] are incompatible lengths, you may get spurious results."
								)
	{
	met = functions.cleanKey(method, 3, keep="-");
	acti = functions.cleanKey(action, 4);
	xlen = length(x);
	ylen = length(y);
	b = (ylen == xlen);  
		if(met == "equ") { return(TRUE); }
		
	xone = (xlen == 1);
	yone = (ylen == 1);	
		if( (met == "11e" || met == "1,1") && (xone || yone) )
			{
			return(TRUE);
			}
			
	if(acti == "warn") { warning(msg); }
	if(acti == "stop") { stop(msg); }
	}
						
























