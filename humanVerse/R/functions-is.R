

# https://www.rdocumentation.org/packages/Zelig/versions/4.2-1
is.formula = function() {}
# is.model?

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
	if(x == 1) { return(optimus); } ## this is not multivariate, list above 
	return( (nf == idx) ); 
	}
	
	



are.functions = function(fnV = "sum", ..., suggestion=TRUE)
	{
	# if I make multivariate, must be string input ... 
	fnV = dots.addTo(fnV, ...);
	n = length(fnV);
	res = logical(n);
	for(i in 1:n)
		{
		fn = fnV[i];
		res[i] = function.exists(fn);  # not to confuse with base::is.function
		}
	names(res) = fnV;
	res;
	}


# base::is.function only works on non-string?
# exists is the reverse, only on a string, not a non-string 
# overwriting/extending base::
is.function = function(fn)
	{
debug = FALSE;
	# is.function(match.fun("outer"))
	if(is.character(fn)) 
		{ 
		x = suppressError( match.fun(fn), show.notice=debug, msg="debug is.function");
		if(is.error(x)) { return(FALSE); }  # should be TRUE otherwise 
		base::is.function(x);
		} else { base::is.function(fn); }
	}
	
#' @rdname function.exists
#' @export
function.exists = is.function;



is.POSIXt  = function(x)	{ inherits(x, "POSIXt");  }
is.POSIXlt = function(x)	{ inherits(x, "POSIXlt"); }
is.POSIXct = function(x)	{ inherits(x, "POSIXct"); }

is.Date = function(x)		{ inherits(x, "Date"); }

is.boolean = is.bool = is.logical;
# is.name EQUIVALENT to is.symbol

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' is.substring
#'
#'
#' @param needle (substring is UNI-VARIATE)
#' @param haystack (is MULTI-VARIATE)
#'
#' @return TRUE or FALSE (Logical VECTOR of length(haystack)
#' @export
#'
#' @examples
is.substring = function(needle, haystack)
	{
	grepl(needle, haystack, fixed = TRUE);
	}







#' @rdname is.dir
#' @export
is.dir = function() {}
is.dir = dir.exists;

#' @rdname is.file
#' @export
is.file = function() {}
is.file = file.exists;


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
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
	str.contains("win", tolower(.Platform[["OS.type"]]) );
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



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
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
	checktype = check.type(str);
	if(!checktype) { str = as.character(substitute(str)); }
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
	names(res) = str; 
	res;
	}


are.libraries = function(strV = "stringi", ..., suggestion=TRUE)
	{
	# if I make multivariate, must be string input ... 
	strV = dots.addTo(strV, ...);
	# make multivariate?
	n = length(strV);
	res = logical(n);
	for(i in 1:n)
		{
		str = strV[i];
		# if(!is.character(str)) { str = as.character(substitute(str)); }
		res[i] = isTRUE(requireNamespace( str , quietly = TRUE));
		if(!res[i] && suggestion)
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
		}
	names(res) = strV;
	res;
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



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
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
debug = TRUE;
	
	checktype = check.type(obj);
							

if(debug) {	
	print(checktype);						
cat("\n STEP 1 \n");
		}
	if(!checktype) 	{ return(FALSE); }
	
	mytype = property.get("typeof", checktype);
	
if(debug) {	
cat("\n STEP 2 \n");
}	
	if(!deep.scan && is.character(obj)) { return( exists(obj, ...) ); }
	
	# WHAT about monte$says as input 
	# monte$says@attribute*class NOTATION 
	
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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
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
is.wholeNumber = function() {}
is.wholeNumber = function(x, ..., tol = sqrt(.Machine$double.eps), part="Re")
  {
  # See ?is.integer
  x = dots.addTo(x, ...);
  x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
  abs(x - round(x)) < tol;
  }



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
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
	x = dots.addTo(x, ...);
	x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
	x = as.integer(x);  # this rounds numerics down ... complex internal is NOT integer
	( (x %% 2) == 0 );  
	}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
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
	x = dots.addTo(x, ...);
	x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
	x = as.integer(x);  # this rounds numerics down ... complex internal is NOT integer
	( (x %% 2) == 1 );  
	}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
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
is.positive = function() {}
is.positive = function(x, ..., tol = sqrt(.Machine$double.eps), part="Re")
  {
  x = dots.addTo(x, ...);
x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
  x > tol;
  }

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
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
is.negative = function() {}
is.negative = function(x, ..., tol = sqrt(.Machine$double.eps), part="Re")
	{
	x = dots.addTo(x, ...); 
	x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
	x < ( -1 * tol );
	}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
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
is.zero = function(x) {}
is.zero = function(x, ..., tol = sqrt(.Machine$double.eps), part="Re")
	{
	x = dots.addTo(x, ...);
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
is.ge = function() {}
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
is.le = function(x) {}
is.le = function(x, y, tol = sqrt(.Machine$double.eps), part="Re")
	{
	check.isCompatibleLength(x, y);
	x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
	y = if(part == "Im") { y = Im(y); } else { y = Re(y); }
	d = x - y; 
	is.negative(d, tol=tol, part=part);
	}
	
"%~<%" = "%le%" = is.le;






















math.countSignChanges = function() {}
math.countSignChanges = function(x, ..., tol = sqrt(.Machine$double.eps), part="Re")
	{
	x = dots.addTo(x, ...);
	x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
	x = math.cleanup( x ); # if close to zero, do this before checking sign 
	x.sign = sign(x);
	# https://stackoverflow.com/questions/17220837/
	sum(diff( x.sign ) != 0);	
	}


math.sign = function() {}
# lol in ?sign ... what is sign(sin(pi)) vs math.sign(sin(pi))
math.sign = function(x, ..., tol = sqrt(.Machine$double.eps), part="Re", return="integer")
	{
	r = functions.cleanupKey(return, 1);	
	x = dots.addTo(x, ...);
	x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
	x = math.cleanup( x ); # if close to zero, do this before checking sign 
	x.sign = sign(x);
	if(r == "i") { return(x.sign); }
	res = x; # NA's are preserved 
	res[x.sign== 0] = ""; 
	res[x.sign== 1] = "+";
	res[x.sign==-1] = "-";
	res;
	}
	
	
is.wholeNumber = function(x, ..., tol = sqrt(.Machine$double.eps), part="Re")
  {
  # See ?is.integer
  more = unlist(list(...)); x = c(x, more); 
  x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
  abs(x - round(x)) < tol;
  }
  
