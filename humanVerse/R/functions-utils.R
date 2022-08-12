



dots.addToKey = function(key, ...)
	{
	more = unlist(list(...));
	c(key, more);
	}



# testit = function() { if(rand(0,1) == 1) {stop("ERROR");} else {return(1); }}
# x = suppressError(testit);
# x = suppressError(testit(), show.notice=FALSE); str(x);
# x = suppressError(testit(), msg="-HI-"); str(x); if(is.error(x)) { list.fromError(x); }
suppressError = function(expression, show.notice = TRUE, msg = "")
	{
	if(show.notice)
		{
		if(msg == "") 
			{
			msg = "\n\n tldr; \n\n\n\t R-dev believes this is poor programming practice to allow you to \n\t\t suppressError( so they have not included it in base R.  \n\t\t It is probably true, but 'git-r-done' first, and then \n\t\t figure out the minutia such as why this function is \n\t\t throwing an error.  That is why I have past such a \n\t\t VERBOSE message to you, dear reader. \n\n\t By altering this function [set msg to something else, not empty ''], \n\t\t you can reduce the length of this message.  \n\n\t Or you can set the flag show.notice=FALSE to prevent it from printing. \n\t\t  THIS my friends is how choice architecture works!  Cheers and Aloha! \n\n\n";
			}
		# cat(msg);
		warning(msg, call. = FALSE, immediate. = TRUE);
		}
	try( expression , silent = TRUE);
	}


# general trap function
# maybe in functions ... 
# on.error 
# digest ... errormode=c("stop","warn","silent"),
# .errorhandler <- function(txt, obj="", mode="stop") 


					# Error in as.POSIXlt.numeric(vals) : 'origin' must be supplied
as.type = function(vals, types="character", ...) # could I pass ... dots
	{
	n = length(vals);
	nt = length(types); if( (n != nt) && (nt != 1) ) { stop("lenghts must match or types must be of length 1;"); }
	
	# NULL => null
		w = is.null(types);
	# "POSIXct.POSIXlt" ... upper case 
	types[w] = str.tolower(types[w]);

	# as.complex, as.double, as.null, as.single, as.integer 
	# seems like it is of the form as.{typeof(vals))
	if(nt == 1)
		{
		# one type on a vector
		cmd = paste0("vals = as.",types,"(vals);"); # could I eval with ... (dots)
		} else {
				str = character(n);
				for(i in 1:n)
					{
					# pairwise
					str[i] = paste0("vals[" , i , "] = as.",
										types[i],"(vals[" , i , "]);");
					}
				# we could create a vector or one long string, parse(text is multivariate
				cmd = paste0(str, collapse=" ");
				}
	# maybe trap this?
	# maybe a function?  
	eval(parse(text = cmd));
	vals;
	}


##################################################
#'
#' bindec
#'
#'
#' Converts a binary (string) to decimal (number)
#'
#' @param binstr (vector)
#'
#' @return vector 
#' @export
#' @alias bin2dec
bindec = function(binstr)
	{
	N = length(binstr);
	res = integer(n);
	for(i in 1:N)
		{
		ibinstr = binstr[i];
		n = strlen(ibinstr);
		re = 0; power = 0;
		for(i in n:1)
			{
			bit = as.integer(charAt(ibinstr,i));
			add = 0;
			if(bit == 1) { add = 2^power; }
			
			re = re + add;
			power = 1 + power;
			}
		res[i] = re;
		}
	
	}

#' @rdname bin2dec
#' @export
bin2dec = bindec;




decbin = function(decnum) 
	{
	bvect = rep(0, 1 + floor(log(decnum, 2))); # pre-populate with zeroes
	while (decnum >= 2) 
		{
		power = floor(log(decnum, 2));
		bin_vect[1 + power] = 1;
		decnum = decnum - 2^power;
		} 
	bvect[1] = decnum %% 2;
	paste(rev(bvect), collapse = ""); # convert to a string
	} 



# DDEECC -> rounds to dcedcb
# hexadecimal to decimal
# hexdec("FF");
# alias hex2dec


#' hexdec
#'
#' Converts a hexadecimal to decimal
#'
#' @param hexstr vector of one or more hex values as a string
#' @param ... vector of one or more hex values as a string
#'
#' @return vector of one or more integer values
#' @export
#' @alias hex2dec
#'
#' @examples
#' hexdec("FF");
#' hexdec("0xFFFF");
#' hexdec("0xFFFF", "#FF");
hexdec = function(hexstr, ...)
	{
  # http://php.net/manual/en/function.hexdec.php
  # http://php.net/manual/en/function.dechex.php
  # java conversions:: http://www.cs.rit.edu/~ncs/color/t_convert.html
  #	http://www.easyrgb.com/math.php?MATH=M19#text19

	more = unlist(list(...));
	hexstr = c(hexstr, more);

	# if it has "color" pre-pend, remove it ...
	hexstr = str_replace("#", "", hexstr);
	# rather than checking, let's remove and add leading "0x"
	hexstr = paste0("0x", str_replace("0x", "", trimMe(tolower(hexstr))) );
	stringToInteger(hexstr, TRUE);
	}




#' dechex
#'
#' Converts a decimal to hexadecimal
#'
#' @param intdec vector of one or more integer values
#' @param ... vector of one or more integer values
#' @param n Should we prepend some zeroes, if so how many?
#' @param hash Should we pre..pre-pend the "#" for colors?
#'
#' @return vector of one or more hex values as a string
#' @export
#' @alias dec2hex
#'
#' @examples
#' dechex(123,255,50, n=2, hash=FALSE);
#' dechex(16581375,12581375,50, n=6, hash=TRUE);
#' dechex(16581375,12581375,50, hash=FALSE);
#' dechex(255,133,50, hash=FALSE);
dechex = function(intdec, ..., n=NULL, hash=FALSE)
	{
	more = unlist(list(...));
	intdec = c(intdec, more);

	res = toupper( as.character( as.hexmode( as.integer( round(intdec) ) ) ) );
	# if the vector already has two-character mode ... dechex( 0:255);  ... n is not necessary
	if(!is.null(n)) { res = strPadLeft( res, n, "0"); 	}
	if(hash) { res = paste0("#",res); }
	res;
	}



#' deg2rad
#'
#' Convert angles from degrees to radians.
#' Similar to pracma::deg2rad however is vectorized (multivariate).
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
	more = unlist(list(...));
	degs = c(degs, more);

	res = list();
	i = 0;
	for(deg in degs)
		{
		i = 1 + i;
		ndeg = suppressWarnings(as.numeric(deg));
		rad = NaN;
		if( !is.na(ndeg) )  { rad = (pi/180) * ndeg; }
		res[[i]] = rad;
		}
	returnList(res);
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
	more = unlist(list(...));
	rads = c(rads, more);

	res = list();
	i = 0;
	for(rad in rads)
		{
		nrad = suppressWarnings(as.numeric(rad));
		i = 1 + i;
		deg = NaN;
		if( !is.na(nrad) )  { deg = (180/pi) * nrad; }
		res[[i]] = deg;
		}
	returnList(res);
	}
	
	
	
	
	
	
	

#' charAt
#'
#' Get the character of a string at position [idx]
#'
#' @param str String
#' @param idx position to get character
#'
#' @return single character
#' @export
#'
#' @examples
#'
#' charAt("Alex", 2);
#' charAt(c("Hello","there","Alex"), 2);
#' charAt("Alex", 8);
#' charAt("Alexander", 8);
#'
charAt = function(str,idx)
  {
  substr(str,idx,idx);
  }

#' lastChar
#'
#' Get the last character of a string
#'
#' @param str String
#' @param trim should the string be trimmed first
#'
#' @return single character
#' @export
#'
#' @examples
#'
#' lastChar("Alex");
#' lastChar(c("Hello","there","Alex"));
#' lastChar("Sasha");
#' lastChar("Alexander");
#'
lastChar = function(str, trim=TRUE)
	{
	# this also works:: ... # .substr(str, -1)
	if(trim){ str = trimMe(str); }
	s.len = strlen(str);
	charAt(str, s.len);
	}


#' charCodeAt
#'
#' Get the ASCII character code of a string at position [idx]
#'
#' @param str String
#' @param idx position to get character
#'
#' @return
#' @export
#'
#' @examples
#'
#' charCodeAt("Alex", 2);
#' charCodeAt(c("Hello","there","Alex"), 2);
#' charCodeAt("Alex", 8);
#' charCodeAt("Alexander", 8);
#'
charCodeAt = function(str,idx)
  {
  charCode ( charAt(str,idx) ); #  as.numeric( iconv( charAt(str,idx), from="ASCII", to="unicodeFFFE", toRaw=TRUE)[[1]][2] );
  }


#' charCode
#'
#' @param svec A vector of characters
#'
#' @return ASCII character code for each character
#' @export
#'
#' @examples
#'
#' s = "Alexander"; svec = strsplit(s,"",fixed=TRUE)[[1]];
#' charCode(svec);
#'
charCode = function(svec)
  {
  #v1 = iconv( svec, from="ASCII", to="unicodeFFFE", toRaw=TRUE);
	v1 = iconv( svec, from="UTF-8", to="unicodeFFFE", toRaw=TRUE);
	v2 = as.integer( unlist(v1) )
	v2[v2 > 0];

	# https://coolbutuseless.github.io/2021/12/04/base64-encoding/decoding-in-plain-r/

	#unname(vapply(as.character(svec), utf8ToInt, integer(1)))
  }
	
	

