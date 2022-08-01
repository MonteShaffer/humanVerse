

#' catMe
#'
#' @param lines
#' @param pre
#' @param post
#' @param file
#' @param append
#'
#' @return
#' @export
#'
#' @examples
catMe = function(lines, pre="\n", post="", file="", append=TRUE)
	{
	lines = getElementsInList(lines, 1);
	n = length(lines);
	for(line in lines)
		{
		cat(pre, line, post, file=file, append=append);
		}
	}


#' charVector
#'
#' @param strvec
#' @param sep
#'
#' @return
#' @export
#'
#' @examples
charVector = function(strvec, sep="")
	{
  # s = "Alexander"; svec = strsplit(s,"",fixed=TRUE)[[1]];
	n = length(strvec);
	res = list();
	for(i in 1:n)
		{
		res[[i]] = strsplit(strvec[i], sep ,fixed=TRUE)[[1]];
		}
	if(n == 1)
		{
		res[[i]];
		} else 	{
				res;
				}
	}

#' strtolower
#'
#' @param str
#'
#' @return
#' @export
#'
#' @examples
strtolower = function(str)
	{
	tolower(str);
	}

#' strtoupper
#'
#' @param str
#'
#' @return
#' @export
#'
#' @examples
strtoupper = function(str)
	{
	toupper(str);
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
  # s = "monte";
  # svec = strsplit(s,"",fixed=TRUE)[[1]];
  r = c();
  for(s in svec)
    {
    r = c(r, as.numeric( iconv( s, from="ASCII", to="unicodeFFFE", toRaw=TRUE)[[1]][2] ) );
    }
  r;
  }

#
#' trimMe
#'
#'
#' library(stringi);
#'
#' @param str character string to be "trimmed"
#'
#' @return updated trimmed string
#' @export
#'
#' @examples
#'
#' trimMe( c(" Monte", " is ", "Alexander's ", "  daddy!") );
#'
#' trimMe("    four   scores    and  seven      years     ");
#' trimMe("    four   scores    and  seven      years     ", "left");
#' trimMe("    four   scores    and  seven      years     ", "riGht");
#' trimMe("    four   scores    and  seven      years     ", "both");
#' trimMe("    four   scores    and  seven      years     ", "albjdskj")
#'
#' trimMe("\r\n    four   scores    and  seven      years   \t\t  ");
#'
trimMe = function(str, side="both", method="stringi")
  {
  # sides = c("both", "left", "right")
  # methods = c("cpp", "stringi", "base")
  side = tolower(side);
  ###m = substr(tolower(method),1,1);  # is this slowing me down?
  # stringr::str_trim(str);
  # if(!is.element(side,sides)) { stop("option for 'side' must be one of:  both, left, right"); }
  # set default to both

  # dump stringr and go to stringi
  # dump tidyverse altogether
  # review httr and build appropriate functions
  # use base whenever possible, then the best packages whenever possible
  # define best by "least associated with new grammar tactics"
  # new grammar is arbitrary and not c-based
  # human-readable functions with variadic inputs is not new grammar
# lsf.str("package:stringi")
# ls("package:stringi")

	if(method == "cpp" && exists("cpp_trim"))
			{
			res = switch(side,
							  "left"  = cpp_ltrim(str),
							  "right" = cpp_rtrim(str),
							  "both"  = cpp_trim(str),
					cpp_trim(str)
					);
			return (res);
			}
		
		
  if( method == "stringi" && isTRUE(requireNamespace("stringi", quietly = TRUE)) )
    {
    res = switch(side,
						  "left"  = stringi::stri_trim_left(str),
						  "right" = stringi::stri_trim_right(str),
						  "both"  = stringi::stri_trim_both(str),
				stringi::stri_trim_both(str)
				);
	return (res);
	}
	
	
		
    res = switch(side,
						  "left"  = gsub("^\\s+", "", str),
						  "right" = gsub("\\s+$", "", str),
						  "both"  = gsub("^\\s+|\\s+$", "", str),
                  gsub("^\\s+|\\s+$", "", str)
                  );
    return (res);
  }


#' explodeMe
#'
#' Similar to javascript.split and php.explode
#'
#' @param delimiter character(s) to delimit the split
#' @param str a character string to be split
#'
#' @return a character vector
#' @export
#'
#' @examples
#' str = removeWhiteSpace("    four   scores    and  seven      years     ", "[s]");
#' strvec = explodeMe("[s]", str);
#' strvec[3];
explodeMe = function(delimiter = " ", str = "hello friend", n = 1)
  {
  res = strsplit(str, delimiter, fixed=TRUE);
  if(is.null(n)) { return (res); } # ALL OF THEM
  res[[n]]; # single element
  }



#' implodeMe
#'
#' Similar to javascript.join and php.implode
#'
#' @param delimiter character(s) to unsplit with
#' @param strvec a character string to be unsplit
#'
#' @return a character string
#' @export
#'
#' @examples
#' implodeMe();
#'
#'
#' str = removeWhiteSpace("    four   scores    and  seven      years     ", "[s]");
#' strvec = explodeMe("[s]", str);
#' implodeMe(",", strvec);
#'
implodeMe = function(delimiter=" ", strvec = c("hello","friend") )
  {
  paste0(strvec, collapse = delimiter);
  }



#' removeWhiteSpace
#'
#' @param str character string to be adjusted
#' @param replace what will we replace the white space with
#' @param n number of spaces to find and replace
#' @param pre.trim if TRUE, trims the string before removing white space within
#' @param post.trim if TRUE, trims the string after removing white space within
#'
#' @return updated adjusted string
#' @export
#'
#' @examples
#' removeWhiteSpace("    four   scores    and  seven      years     ");
#' removeWhiteSpace("\r\n    four   scores    and  seven      years   \t\t  ");
#'
#' removeWhiteSpace("    four   scores    and  seven      years     ", "");
#' removeWhiteSpace("    four   scores    and  seven      years     ", "[s]");
#'
#' removeWhiteSpace("The quick brown fox jumps over the lazy dog", ""); # default is 2
#' removeWhiteSpace("The quick brown fox jumps over the lazy dog", "", n=1);
removeWhiteSpace = function( str, replace=" ", n = 2,
                              pre.trim = TRUE, post.trim = TRUE, trim.method = "stringi" )
  {
  # ?regex
  # $string = preg_replace('/\s+/', '', $string);
  if(pre.trim) { str = trimMe(str, method = trim.method); }
    regex.s = paste0("[[:space:]]{",n,",}");
  str = gsub( regex.s, replace, str );
  # str = gsub("[[:space:]]", remain, str); # ... call it twice ?
  if(post.trim) { str = trimMe(str, method = trim.method); }
  str;
  }


#' removeFunnyCharacters
#'
#' @param str
#' @param which
#' @param ASCII
#'
#' @return
#' @export
#'
#' @examples
removeFunnyCharacters = function(str, which="alpha-numeric", ASCII=TRUE, trim=TRUE)
	{
  method = tolower(which);  # str.toLowerCase()
	# https://stackoverflow.com/questions/10294284/remove-all-special-characters-from-a-string-in-r
	# str = "Ábcdêãçoàúü"; str = iconv(str, from = 'UTF-8', to = 'ASCII//TRANSLIT'); str;
	# str = "Ábcdêãçoàúü"; str = iconv(str, from = '', to = 'ASCII//TRANSLIT'); str;
	if(ASCII) { str = iconv(str, from = '', to = 'ASCII//TRANSLIT'); }

	# ?regex
  # switch ...
  str = switch(method,
            "alpha-numeric"    = gsub("[^[:alnum:]]", " ", str),
            "alphanumeric"    = gsub("[^[:alnum:]]", " ", str),

            "alpha"    = gsub("[^[:alpha:]]", " ", str),

            "numeric"    = gsub("[^[:digit:]]", " ", str),
            "num"    = gsub("[^[:digit:]]", " ", str),
            "number"    = gsub("[^[:digit:]]", " ", str),

           str # default case of switch
          );

  if(trim) { str = trimMe(str); }

	str; # maybe just ascii move
	}

#' strPadLeft
#'
#' When caching pages of content, useful for organization.
#'  (e.g., page1.html becomes page_001.html)
#'
#' @param str The 'string' (can be a number)
#' @param final.str.len How long the final str is to be
#' @param padding Fill with, default is "0" (zero)
#'
#' @return string
#' @export
#'
#' @aliases numberPadLeft str_pad_left
#'
#' @examples
#' strPadLeft(33,1);
#' strPadLeft(33,2);
#' strPadLeft(33,3);
#' strPadLeft(33,4);
strPadLeft = function(str, final.str.len, padding="0", method="stringi")
  {
  if( isTRUE(requireNamespace("stringi", quietly = TRUE)) && method=="stringi" )
    {
    stringi::stri_pad_left(str, final.str.len, pad = padding);
    } else {
            n = strlen(str);
            r = final.str.len - n;
            if(r < 0) { stop("strPadLeft is too short!"); }
            paste0(paste(rep(padding,r),collapse=""),str);

            }
  }

#' strPadRight
#'
#'
#'
#' @param str The 'string' (can be a number)
#' @param final.str.len How long the final str is to be
#' @param padding Fill with, default is "0" (zero)
#'
#' @return string
#' @export
#'
#' @aliases numberPadRight str_pad_right
#'
#' @examples
#' strPadRight("33.01",5);
#' strPadRight("33.01",6);
#' strPadRight("33.01",7);
#' strPadRight("33.01",8);
strPadRight = function(str, final.str.len, padding="0", method="stringi")
  {
  if( isTRUE(requireNamespace("stringi", quietly = TRUE)) && method=="stringi" )
    {
    stringi::stri_pad_right(str, final.str.len, pad = padding);
    } else {
            n = strlen(str);
            r = final.str.len - n;
            if(r < 0) { stop("strPadRight is too short!"); }
            paste0(str, paste(rep(padding,r),collapse=""));
            }
  }


#' strlen
#'
#' @param str the character string
#'
#' @return the numeric length of said string
#' @export
#'
#' @examples
#' strlen("3.1415926535897932384626");
#' strlen( pi );
#' strvec = c("hi","how","are","you"); strlen(strvec);
strlen = function(str)
  {
  # history :: # https://en.cppreference.com/w/c/string/byte/strlen
  # http://www.cplusplus.com/reference/cstring/
  # https://en.wikipedia.org/wiki/C99
  # https://www.programiz.com/c-programming/library-function/string.h/strlen
  # vectorized ... already
  nchar( as.character(str), type="chars");
  }


#' .substr
#'
#' @param str
#' @param n
#' @param length
#' @param PHP.offset
#'
#' @return
#' @export
#'
#' @examples
.substr = substr.neg = function(str, n = -1, length=NULL, PHP.offset=TRUE)
	{
  # https://stackoverflow.com/questions/2681786/how-to-get-the-last-char-of-a-string-in-php
  # .substr = function(str,  # maybe write a PHP wrapper
  # by default, this will return the last character of a string ...
  # .substr("abcdef", 4, -4);  // returns false ... returns EMPTY ""
  ## earlier called 'substr.neg'
  # PHP wrapper ... https://www.php.net/manual/en/function.substr.php

	n = as.integer(n);
		if(!PHP.offset) { n = n - 1; } # PHP indexes at "0"

	if(!is.null(length))
		{
		length = as.integer(length);
		if(!PHP.offset) { length = length - 1; } # PHP indexes at "0"
		}

	str.len = strlen(str);
		if(is.negative(n))
			{
			str.tmp = substr(str, start=1+(str.len + n), stop=str.len );
				if(is.null(length)) { return (str.tmp); }
				if(length == 0) 	{ return (str.tmp); }
			if(is.positive(length))
				{
				str.final = substr(str.tmp, start=1, stop = length);
				} else {
						str.len.tmp = strlen(str.tmp);
						str.final = substr(str.tmp, start=1, stop = str.len.tmp + length);
						}
			return ( str.final );
			} else {
					# PHP allows n = 0 ... first element ...
					str.tmp = substr(str, start=1+n, stop=str.len );
						if(is.null(length)) { return (str.tmp); }
						if(length == 0) 	{ return (str.tmp); }


					if(is.positive(length))
						{
						str.final = substr(str.tmp, start=1, stop = length);
						} else {
								str.len.tmp = strlen(str.tmp);
								str.final = substr(str.tmp, start=1, stop = str.len.tmp + length);
								}
					return ( str.final );
					}
	stop("humanVerse::.substr ... how did you get here?!?");
}


#' .substring
#'
#' @param strvec
#' @param n
#' @param length
#' @param PHP.offset
#'
#' @return
#' @export
#'
#' @examples
.substring = function(strvec, n = -1, length=NULL, PHP.offset=TRUE)
	{
  # this is a vectorized form of .substr ... not analagous to base::substring at all?
  # x <- c("asfef", "qwerty", "yuiop[", "b", "stuff.blah.yech")
  # ? substr
  # .substring(x, 2, 5, FALSE) ... is equivalent to ... substr(x, 2, 5) ... but why?  If you want the PHP negative offsetting, just use its indexing.

	m = length(strvec);
	res = character(m);
	for(i in 1:m)
		{
		str = strvec[i];
		res[i] = .substr(strvec[i], n=n, length=length, PHP.offset=PHP.offset);
		}
	res;
	}




#' char.vec
#'
#' @param str
#' @param sep
#'
#' @return
#' @export
#'
#' @examples
char.vec = function(str, sep="")
	{
  # splits a string into a vector ...
	strsplit(str, sep, fixed=TRUE)[[1]];
	}



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
is.substring = function(haystack, needle, out="BOOLEAN")
  {
  # e.g., strpos in PHP
  out = substr(trimMe(toupper(out)),1,1);
  if(out == "B")
	{
	grepl(needle, haystack, fixed = TRUE);
	} else 	{
			# TODO, add strpos as list of places found ... multiple ?
	    nlen = strlen(needle);
	    arr = explodeMe(needle, haystack );
	    # strpos starts at 1 ...
			}
  }




#' str_repeat
#'
#' @param str
#' @param times
#'
#' @return
#' @export
#'
#' @examples
str_repeat = function(str, times=1)
	{
  # https://www.php.net/manual/en/function.str-repeat.php
	paste( rep(str, times), collapse="");
	}


#' str_replace
#'
#' @param find
#' @param replace
#' @param str
#' @param method
#'
#' @return
#' @export
#'
#' @examples
str_replace = function(find, replace, str, method="base")
  {
  # this is "fixed" find and replace # str = gsub(find[i], replace[i], str, fixed=TRUE);
  # method = base, method = stringi
  # stringi is not performing well on this:  "{wsu.crimson}" with fixed


  # if find/replace are longer ... if one is length one, repeat the other one
  n.find = length(find);
  n.replace = length(replace);
  n.max = max(n.find, n.replace);

  if(n.find == 1 && n.replace == 1)
    {
    if(n.find == 1)
      {
      find = rep(find, n.replace);
      } else {
              if(n.replace == 1)
                {
                find = rep(replace, n.find);
                } else {
                        stop("find and replace mismatch");
                        }
              }
    }
  ### let's loop and replace ...
  for(i in 1:n.max)
    {
    if( isTRUE(requireNamespace("stringi", quietly = TRUE)) && method=="stringi" )
      {
      # I need to verify this is the correct function ...
      str = stringi::stri_replace_first_fixed(str, find[i], replace[i]);
      } else {
              str = gsub(find[i], replace[i], str, fixed=TRUE);
              }
    }
  str;
  }








#' ascii.line
#'
#' @param strs
#' @param out.length
#' @param left
#' @param right
#' @param sep
#' @param justification
#'
#' @return
#' @export
#'
#' @examples
ascii.line = function(strs, out.length=66, left = "## ", right = " ##", sep=" ", justification="center")
	{
	res = list();
	n = length(strs);
	for(i in 1:n)
		{
		str = strs[i];

		sep = charAt(sep,1); # we only allow a 1-element separator

		len.s = strlen(str);
		len.l = strlen(left);
		len.r = strlen(right);

		if(justification == "center")
			{
			out.left  = out.right = floor( (out.length - len.l - len.s - len.r )/2 );
				# offset = out.length - len.l - out.left - len.s - out.right - len.r;
			line = paste0(left, str_repeat(sep,out.left), str, str_repeat(sep, out.right));

			remaining = out.length - strlen(line) - len.r;
			if(remaining > 0)
				{
				line = paste0(line, str_repeat(sep, remaining));
				}
			line = paste0(line, right);
			} else {
					# left
					line = paste0(left, str);
					remaining = out.length - strlen(line) - len.r;
					if(remaining > 0)
						{
						line = paste0(line, str_repeat(sep, remaining), right);
						}
					}

		res[[i]] = line;
		}

	if(n > 1) { res; } else { res[[1]]; }
	}



