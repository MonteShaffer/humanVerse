


str.getPositions = function(str, search, index=NULL)
	{
	n.list = length(str);
	slen = strlen(search);
	info = str.explode(search, str);

	ss = as.relistable( str.len(info) ); # a list of lengths
		u.info = unlist(ss);	# unlist
		u.info = u.info + slen;  	# add	
	ee = relist(u.info);			# relist
	list("start" = ss, "end" = ee);
	}


str.between = function(str, keys=c("__B64_", "_B64__"))
	{
	info = str.explode(keys[1], str);
	info2 = str.explode(keys[2], list.getElements(info, 2) );
	list.getElements(info2, 1);
	}


str.len = function(str, method="stringi", locale="")
  {
	# if list 
	if(is.list(str)) 
		{ 
		n.times = length(str);
		res = list();
		for(i in 1:n.times)
			{
			res[[i]] = str.len(str[[i]], method=method, locale=locale);
			}
		return( list.return(res) );
		}

# LOCALE is a TODO
	# necessary overhead
	m = functions.cleanKey(method, 1);

	if(m == "s" && is.library("stringi") )
		{		
		return ( stringi::stri_length(str) );
		}

	if(m == "b")
		{
		return( nchar( as.character(str), type="chars") );
		}

	if(m == "c" && exists("cpp_strlen"))
		{
		return( cpp_strlen(str) );
		} 


	nchar( as.character(str), type="chars");
	}

#' @rdname str.length
#' @export
str.length = str.len;

#' @rdname strlen
#' @export
strlen = str.len;



##################################################
#'
#' str.tolower
#'
#' @param str VECTOR of strings to be case managed
#' @param method Use C++ is library(HVcpp) or Rcpp::sourceCpp(str.cpp)
#'
#' @return
#' @export
#'
#' @examples
str.tolower = function(str, method="cpp", locale="en_US.UTF-8")
	{
# l10n_info();      # NON-TRIVIAL
# Sys.getlocale();
# stri_trans_tolower(string, locale = locale)

	# necessary overhead
	m = functions.cleanKey(method, 1);

	if(m == "s" && is.library("stringi") )
		{		
		return ( stringi::stri_trans_tolower(str, locale) );
		}

	if(m == "b")
		{
		return( tolower(str) );
		}

	if(m == "c" && exists("cpp_strtolower"))
		{
		return( cpp_strtolower(str) );
		} 


	tolower(str);
	}

#' @rdname strtolower
#' @export
strtolower = str.tolower;


##################################################
#'
#' str.toupper
#'
#' @param str VECTOR of strings to be case managed
#' @param method Use C++ is library(HVcpp) or Rcpp::sourceCpp(str.cpp)
#'
#' @return
#' @export
#'
#' @examples
str.toupper = function(str, method="cpp", locale="en_US.UTF-8")
	{
	# necessary overhead
	m = functions.cleanKey(method, 1);

	if(m == "s" && is.library("stringi") )
		{		
		return ( stringi::stri_trans_toupper(str, locale) );
		}

	if(m == "b")
		{
		return( toupper(str) );
		}

	if(m == "c" && exists("cpp_strtoupper"))
		{
		return( cpp_strtoupper(str) );
		} 

	toupper(str);
	}

#' @rdname strtoupper
#' @export
strtoupper = str.toupper;


##################################################
#'
#' str.trim
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
#' str.trim( c(" Monte", " is ", "Alexander's ", "  daddy!") );
#'
#' str.trim("    four   scores    and  seven      years     ");
#' str.trim("    four   scores    and  seven      years     ", "left");
#' str.trim("    four   scores    and  seven      years     ", "riGht");
#' str.trim("    four   scores    and  seven      years     ", "both");
#' str.trim("    four   scores    and  seven      years     ", "albjdskj")
#'
#' str.trim("\r\n    four   scores    and  seven      years   \t\t  ");
#'
str.trim = function(str, side="both", method="stringi", pattern="", fixed=TRUE, ...)
  {
	# necessary overhead
	s = functions.cleanKey(side, 1);
	m = functions.cleanKey(method, 1);
	

	if(m == "s" && is.library("stringi") )
		{
		p = "\\P{Wspace}";
		if(pattern != "") { p = pattern; }
		res = switch(s,
						  "l"	= stringi::stri_trim_left (str, p, ...),
						  "r" 	= stringi::stri_trim_right(str, p, ...),
						  "b"  	= stringi::stri_trim_both (str, p, ...),
					stringi::stri_trim_both(str, p, ...)
					);
		return (res);
		}

	if(m == "c" && exists("cpp_trim"))
		{
		# this is FIXED == TRUE ... I don't have REGEX built in
		t = " \t\n\r\f\v";
		if(pattern != "") { t = pattern; }
		res = switch(s,
						  "l"  	= cpp_ltrim(str, t),
						  "r" 	= cpp_rtrim(str, t),
						  "b"  	= cpp_trim (str, t),
					cpp_trim(str, t)
					);
		return (res);
		}

	
	
	g = "\\s+";
	if(pattern != "") { g = pattern; }
	res = switch(s,
						  "l" 	= gsub( paste0("^",g), "", str),
						  "r" 	= gsub( paste0(g,"$"), "", str),
						  "b"  	= gsub( paste0("^",g,"|",g,"$"), "", str),
                  gsub( paste0("^",g,"|",g,"$"), "", str)
                  );
    return (res);
	}

#' @rdname trimMe
#' @export
trimMe = str.trim;



##################################################
#'
#' str.explode
#'
#' Similar to javascript.split and php.explode
#'
#' @param sep [separator] character(s) to delimit the split
#' @param str a character string to be split [NOT a vector]
#' @param n if NULL, return ALL of THEM ... otherwise just single element
#'
#' @return a character vector
#' @export
#'
#' @examples
str.explode = function(sep = " ", str = "hello friend", method="base",  ...)
	{
	# necessary overhead
	m = functions.cleanKey(method, 1);

	hasResult = FALSE;

	if(m == "b")
		{
		res = strsplit(str, sep, fixed=TRUE);
		hasResult = TRUE;
		}

	if(!hasResult && m == "s" && is.library("stringi") )
		{
		res = (stringi::stri_split_fixed(str, sep, ...));
		hasResult = TRUE;
		}

	if(!hasResult && m == "c" && exists("cpp_explode"))
		{
		res = ( cpp_explode(sep, str) );
		hasResult = TRUE;
		}
	
	if(!hasResult)
		{
		res = strsplit(str, sep, fixed=TRUE);
		}

	# will be collapsed into CharacterVector if len == 1
	list.return(res, unlist=FALSE);
	}


#' @rdname explodeMe
#' @export
explodeMe = str.explode;

#' @rdname str.split
#' @export
str.split = str.explode;


















##################################################
#'
#' str.repeat
#'
#' @param str
#' @param times
#'
#' @return
#' @export
#'
#' @examples
str.repeat = function(str, times=1, method="cpp")
	{
	m = functions.cleanKey(method, 1);

	if(m == "c" && exists("cpp_trim"))
		{
		res = cpp_str_repeat(str, times);
		return (res);
		}


	n = length(str);
	res = character(n);
	for(i in 1:n)
		{
		res[i] = paste( rep(str, times), collapse="");
		}
	res;
	}

#' @rdname str_repeat
#' @export
str_repeat = str.repeat;





##################################################
#'
#' str.replace
#'
#' @param search
#' @param replace
#' @param subject
#' @param method
#'
#' @return
#' @export
#'
#' @examples
#' subject = c("Four score and seven years ago", "Abraham Lincoln buoying vessel"); 
#' search = c("a", "b", "c"); replace = str.toupper(search);

# str.replace("{PROTOCOL}", list.getElements(info, 1), sep.http);
# 1, 1, 1
# 1, 2, 1  # this will replace {} on two things ... rep(subject)
# 2, 1, 1
# 1, 2, 2
# 2, 2, 1 (same as 1,1,1)
# 1, 1, N
# 1, 2, N
# 2, 1, N
# 2, 3, 1
# 3, 2, 1
# 2, 3, N
# 3, 2, N  # I don't think we worry about these 

str.replace = function(search, replace, subject, method="base")
	{
	m = functions.cleanKey(method, 1);

	if(m == "c" && exists("cpp_trim"))
		{
		# need to update the code to MATCH the base code LOGIC below
		res = cpp_str_replace(search, replace, subject);
		return (res);
		}

	# stringi::stri_replace_all_fixed
	# doesn't seem to work correctly  ... 

 

	slen = length(search);
	rlen = length(replace);
	nlen = length(subject);


	if(slen == rlen)  ## pairwise over EACH subject
		{
		res = character(nlen);
		for(j = 1:nlen)
			{
			str = subject[j];
			for(i in 1:slen)
				{
				str = gsub(search[i], replace[i], str, fixed=TRUE);
				}	
			res[j] = str;
			}
		return (res);
		}

	# str.replace(c("{monte}", "{for}"), "MONTE", c("Here is {monte} template", "Here is another {for} sure template {monte}!") );
	if(rlen == 1)
		{
		res = character(nlen);
		for(j = 1:nlen)
			{
			str = subject[j];
			for(i in 1:slen)
				{
				str = gsub(search[i], replace[1], str, fixed=TRUE);
				}	
			res[j] = str;
			}
		return (res);
		}

	# str.replace(c("{monte}"), c("MONTE","FOR"), c("Here is {monte} template", "Here is another {for} sure template {monte}!") );
	if(slen == 1 && rlen > nlen)
		{
		res = character(rlen);
		si = 1;
		for(j = 1:rlen)
			{
			str = subject[si]; 
			str = gsub(search[1], replace[j], str, fixed=TRUE);
			res[j] = str;
			si = 1 + si;  if(si > nlen) { si = 1; }  # loop over s, end, back to beginning
			}
		return (res);
		}

	# DEFAULT ... all replaces over all subjects
	res = character(nlen);
	for(j in 1:nlen)
		{
		str = subject[j];
		mlen = max(rlen, slen);
		si = ri = 1;
		for(i in 1:mlen)
			{
			mysearch = search[si];
			myreplace = replace[ri];
			str = gsub(mysearch, myreplace, str, fixed=TRUE);
			si = 1 + si;  if(si > slen) { si = 1; }  # loop over s, end, back to beginning
			ri = 1 + ri;  if(ri > rlen) { ri = 1; }  # loop over s, end, back to beginning
			}
		res[j] = str;			
		}
	return(res);
	}








for(long long unsigned int i = 0; i < mlen; i++)
		{
		std::string mysearch = (slen == 1) ? search[0] : ( (i < slen) ? search[i] : "" );
		std::string myreplace = (rlen == 1) ? replace[0] : ( (i < rlen) ? replace[i] : "" );
		
		std::cout << mysearch;
		std::cout << "\n";
		std::cout << myreplace;
		std::cout << "\n";
		
		std::vector<std::string> tmp = s_explode(mysearch, res);
		res = s_implode(myreplace, tmp);
		}
		///*
		std::cout << res;
		std::cout << "\n";
		//*/
		
	return res;
	}
	










#' @rdname str_replace
#' @export
str_replace = str.replace;




##################################################
#'
#' str.replaceFromList
#'
#' @param mylist
#' @param mysubject
#' @param method
#'
#' @return
#' @export
#'
#' @examples
#' mysubject = c("Four score and seven years ago", "Abraham Lincoln buoying vessel"); 
#' mylist = c("a" = "A", "b" = "B", "c" = "C");
str.replaceFromList = function(mylist, mysubject, ...)
	{
	str.replace( names(mylist), mylist, mysubject);
	}


##################################################
#'
#' str.pad
#'
#' When caching pages of content, useful for organization.
#'  (e.g., page1.html becomes page_001.html)
#'
#' @param str The 'string' (can be a number)
#' @param final.length How long the final str is to be
#' @param padding Fill with, default is "0" (zero)
#'
#' @return string
#' @export
#'
#'
#' @examples
#'
#' str = c("1", "12", "123"); padding = "0";
str.pad = function(str, final.length, padding="0", side="RIGHT", method="stringi")
	{
	str = as.character(str);
	# necessary overhead
	s = functions.cleanKey(side, 1);
	m = functions.cleanKey(method, 1);

	if(m == "s" && is.library("stringi") )
		{
		res = switch(s,
						  "l"	= stringi::stri_pad_left (str, width=final.length, pad=padding),
						  "r" 	= stringi::stri_pad_right (str, width=final.length, pad=padding),
						  "b"  	= stringi::stri_pad_both (str, width=final.length, pad=padding),
					stringi::stri_pad_both (str, width=final.length, pad=padding)
					);
		return (res);
		}

	ns = strlen(str);
	rs = final.length - n;
	n = length(str); # how many strings
	res = character(n);
	
	for(i in 1:n)
		{
		myr = rs[i];
		pads = str.repeat(padding, myr); 
		if(s == "b")
			{
			myr_right	= ceiling(myr / 2);
			pad_right	= str.repeat(padding, ( myr_right )	);
			pad_left	= str.repeat(padding, ( myr - myr_right )	);
			}
		
		# if padding is multiple length, may be too long
		res[i] = switch(s,
						  "l"	= paste0(paste( pads , collapse=""), str[i]),
						  "r" 	= paste0(str[i], paste( pads , collapse="")),
						  "b"  	= paste0(paste( pad_left , collapse=""), str[i], 
											paste( pad_right , collapse="")),
					paste0(paste( pad_left , collapse=""), str[i], 
											paste( pad_right , collapse=""))
					);
		}

	res;
	}


#' @rdname str_trim
#' @export
str_trim = str.trim;


/*
p = "\\P{Wspace}";
		if(pattern != "") { p = pattern; }
		res = switch(s,
						  "l"	= stringi::stri_trim_left (str, p, ...),
						  "r" 	= stringi::stri_trim_right(str, p, ...),
						  "b"  	= stringi::stri_trim_both (str, p, ...),
*/

str.removeWhiteSpace = function( str, replace=" ", n = 2,
                              pre.trim = TRUE, post.trim = TRUE, ...)
  {
	if(pre.trim) { str = trimMe(str, ...); }
	# REQUIRES string?
	if(is.library("stringi"))
		{
		# p = "\\P{Wspace}";
		# p <- c("\\w", "\\d", "\\s")
		# structure(stri_extract_all_regex(x, p), names = p)
		regex.s = paste0("\\s{",n,",}");
		stringi::stri_replace_all_regex(str, regex.s, replace); 
		} else {
				regex.s = paste0("[[:space:]]{",n,",}");
				str = gsub( regex.s, replace, str );  # multivariate works
				}
	# likely not necessary, but may be an edge case out there
	if(post.trim) { str = str.trim(str, ...); }
  str;
  }



#' @rdname removeWhiteSpace
#' @export
removeWhiteSpace = str.removeWhiteSpace;

# stringi::stri_trans_general(c("groß© żółć La Niña köszönöm", "Ábcdêãçoàúü", "Record high °C"), "latin-ascii")
# get rid of temperature ??? caused a BUG before?

str.translate = function(str, to="latin-ascii")
	{
	# to = "upper; latin-ascii"; # ALSO works, in the DOCS
	"upper; latin-ascii"
	stringi::stri_trans_general(str, to=to);
	}




## is this stringr::str_c ??
str.push.back = function(str, sub, collapse=NULL)
	{
	paste0(str, sub, collapse=collapse);
	}

#' @rdname str.push.last
#' @export
str.push.last = str.push.back;


str.push.front = function(str, sub, collapse=NULL)
	{
	paste0(sub, str, collapse=collapse);
	}


#' @rdname str.push.first
#' @export
str.push.first = str.push.front;



str.wrap = function() {}

##################################################
#'
#' str.commentWrapper
#'
#' @param str This should be less than one-line long
#' @param nchars How many characters in a line (max about 80)
#' @param c.tag What is the single-character comment tag "#"
#' @param r.tag What is the single-character repeat tag "-"
#' @param s.tag What is the single-character space tag " "
#' @param s.pad What is the padding (both left and right) for spaces
#'
#' @return Updated str
#' @export
#'
#' @examples
#' str.commentWrapper("LIBRARY is not found!");
#' pname = "stringi"; pkg = paste0( "install.packages(\"",pname,"\", dependencies=TRUE ); ");
#' str.commentWrapper( pkg, r.tag = "-", s.pad=15);
#' 
str.commentWrapper = function(str, nchars=0, c.tag="#", r.tag=c.tag, s.tag=" ", s.pad=5)
	{
	# punchcards had 80 characters, a traditional typewriter US had 72 characters per line (CPL)
	# http://mikeyanderson.com/optimal_characters_per_line
	# Quotes "Jakob Nielson" + 5 ... states 66 is optimal
	# 6.5 inches (1 inch margin) x 10 per inch ... about 65 ... we would do +/- 3 in typing class ... override end
	
	n = length(str);
	res = character(n);
	mylengths = integer(n);
	for(i in 1:n)
		{
		s = paste0(c.tag, r.tag, str.repeat(s.tag, s.pad), str[i], str.repeat(s.tag, s.pad), r.tag, c.tag);
		slen = strlen(s);
		if(slen < nchars)
			{
			n.tag = 1 + ceiling( (nchars - slen)/2 );
			s = paste0(	c.tag, r.tag, str.repeat(s.tag, n.tag), 
									str[i], 
						str.repeat(s.tag, n.tag), r.tag, c.tag
						);
			slen = strlen(s);
			}
		res[i] = paste0(c.tag, str.repeat(r.tag, slen - 2), c.tag, "\n", 
									s, "\n", 
						c.tag, str.repeat(r.tag, slen - 2), c.tag, "\n"
						);
		mylengths[i] = slen;
		}
	
	res = property.set(res, "lengths", mylengths);
	res;
	}

















































## strpos ... strrpos ... reverse


















# Rcpp::sourceCpp("C:\\_git_\\github\\MonteShaffer\\humanVerse\\HVcpp\\src\\str.cpp");
# rm(list=ls())
# rm(list=ls(all.names=TRUE)); gc();














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
### Error in str.len(str) : could not find function "str.len" ... strlen, str_len, str.length, str_length
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

# str_replace_all(






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

# str_replace_all(











##################################################
#'
#' str.contains
#'
#' See PHP str_contains
#'
#' @param haystack Can be MULTI-VARIATE
#' @param needle is UNI-VARIATE
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
str.contains = function(haystack = "hello friend", needle = " ", ...)
	{
	grepl(needle, haystack, fixed = TRUE);
	# /*
	# tmp = str.split(needle, haystack);
	# n = length(tmp);
	# res = logical(n);
	# for(i in 1:n)
		# {
		# res[i] = if( length(tmp[[i]]) > 1 ) { TRUE } else { FALSE }
		# }
	# res;
	# */
	}

