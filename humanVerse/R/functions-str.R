

##################################################
#'
#' str.count
#'
#'
str.count = function(what="|", str)
	{
	info = str.explode(what, str);
	list.getLengths(info);
	}
	

##################################################
#'
#' str.toInteger
#'
#'
str.toInteger = function(str, isHEX=FALSE, base=0L)
	{
	# FALSE means it is just normal numbers we want to convert 
	if(!isHEX) { return (as.integer(str)); }  # e.g, strtoi(str, base=10)?
	return( strtoi(str, base=base) );
	}

##################################################
#'
#' str.fromInteger
#'
#'
str.fromInteger = function(intvec)
	{
	as.character(as.integer(intvec));
	}

##################################################
#'
#' str.toHex
#'
#'
# technically "text.toHex"
str.toHex = function(str)
	{

	}

##################################################
#'
#' str.fromHex
#'
#'
# technically "text.fromHex"
str.fromHex = function(hexstr)
	{

	}

##################################################
#'
#' str.fromCharacters
#'
#'
str.toCharacters = function(str, sep="")
	{
	# strsplit(str, sep, fixed=TRUE)[[1]];
	res = str.explode(sep, str);
	list.return(res);
	}

##################################################
#'
#' str.fromCharacters
#'
#'
str.fromCharacters = function(charslist, sep="")
	{
	# res = chars;
	# if(!is.list(chars)) { res = list(); res[[1]] = chars; }
	res = list.prep(charslist);
	str.implode(sep, res);
	}

##################################################
#'
#' str.toMD5
#'
#'
str.toMD5 = function(str, times=1, method="digest", ...)
	{
	# necessary overhead
	m = functions.cleanKey(method, 1);
	times = as.integer(times);  # make certain it's an integer
	str = as.character(str);	# make certain it's a string (character)
	if(times < 1) 
		{ 
		warning("what are you doing setting the times < 1, RETURNING NULL "); 
		return(NULL); 
		}

	if(m == "c" && exists("cpp_md5"))
		{
		# cpp_md5( (str = c("monte","shaffer") ), 9 );
		res = ( cpp_md5(str, times) );
		return(res);
		}

	is.digest = (m == "d" && is.library("digest") );
	is.openssl = (m == "o" && is.library("openssl") );
	# we have to use the SLOW base function
	is.hack = (!is.digest && !is.openssl); 

	if(is.hack)
		{
		## first look in system /inst/R/functions-md5_.R 
		if(!is.function(md5_))
			{
			source("https://raw.githubusercontent.com/MonteShaffer/humanVerse/main/humanVerse/inst/R/functions-md5_.R");
			Sys.sleep(1); # let it settle?
			}
		
		if(!is.function(md5_))
			{
			stop("UNABLE to load backup MD5 function");
			}
		}

	
	
	n = length(str); # how many
	res = character(n);
	for(i in 1:n)
		{
		res[i] = str[i];
		for(j in 1:times)
			{
			if(is.digest)
				{
				# vdigest ... # Dirk, I don't see it!
				# digest::digest(str, algo="md5", serialize=FALSE);
				### THIS is locally scoped?
				if( !exists("algo", inherits = FALSE ) )			
						{ 
						algo 		= "md5"; 
						}
				if( !exists("serialize", inherits = FALSE ) )	
						{ 
						serialize 	= FALSE; 
						}
				
				res[i] = digest::digest(res[i], 
											algo=algo, 
											serialize=serialize, ...);
				}
			if(is.openssl)
				{
				# openssl::md5(strvec);
				# could speed this up in vector form
				res[i] = openssl::md5(res[i]);
				}
			if(is.hack)
				{
				## I could vector this if digest also is, 
				## then I could remove one 'FOR' loop
				res[i] = md5_(res[i]);
				}
			}
		}
	return(res);
	}


##################################################
#'
#' str.fromMD5
#'
#'
str.fromMD5 = function(str, times=1, method="digest", ...)
	{
	#  body(str.fromMD5);
	msg = "\n\n tldr; \n\n\t MD5 is a hashing algorithm that digests an input into a 32-bit hexstr. \n\t\t The [times=1] feature will allow iterative MD5 hashing:  e.g., n=2 is MD5(MD5(str)) \n\t\t It would be cool if it had an inverse, but alas, it does not!  There are \n\t\t HACKERS that have rainbow tables of results, a dictionary-based INVERSION.  \n\t\t Who knows, maybe I will implement that lookup in R 'one day' \n\n\t -->  MD5 is **SAFE** to hash most things (except passwords, encryption keys, etc.)  But don't worry, in the early days FACEBOOK stored your password in the database in PLAIN TEXT form.  At least an MD5 storage requires some work. [This about how much LIFE ENERGY we waste dealing with password-type events.  Is that MKULTRA programming?!?]  The optimal storage, IMHO, is using the SJCL algorithm as it has easy-to-use client side (Javascript, n1=12346 times) and server side (PHP [or Javascript], n2=12345678 times) interfacing.  Like MD5, you don't ever know what the password is, but you can perform a handshake. \n\t\t Good coding STANDARDS requires me to include the inverse function here to send an important \n\t\t VERBOSE message to you, dear reader.  \n\t\t  THIS my friends is how choice architecture works!  Cheers and Aloha! \n\n\n\t\t\t MD5: https://en.wikipedia.org/wiki/MD5 \n\n\n\t\t\t HASHING:  https://en.wikipedia.org/wiki/Hash_function \n\n\n\t\t\t RAINBOWS: https://crackstation.net/ [Great, except NEW captch] \n\n\t\t\t\t https://md5decrypt.net/en/ \n\n\t\t\t http://md5.mshaffer.com  \n\n\n\t\t\t SJCL: https://bitwiseshiftleft.github.io/sjcl/ ";
	
	warning(msg, call. = TRUE, immediate. = TRUE);
	
	}


##################################################
#'
#' str.toRaw
#'
#'
# technically "text.toRaw"
str.toRaw = function(str)
	{
	n = length(str);
	res = list();
	for(i in 1:n)
		{
		res[[i]] = charToRaw(str[i]);
		}
	list.return(res);
	}

##################################################
#'
#' str.fromRaw
#'
#'
str.fromRaw = function(raw)
	{
	if(is.list(raw))
		{
		n = length(raw);
		res = character(n);
		for(i in 1:n)
			{
			res[i] = rawToChar(raw[[i]]);
			}
		res;
		} else { rawToChar(raw); }
	}


##################################################
#'
#' str.trimFromAny
#'
#'
# monte was here 
# str = c("\n monte \n", "# says ", "hi#", "## to Alex#");
# str.trimFromAny(str, search="#tx")
# str.trimFromAny(str, search="#tx\n", side="left")
str.trimFromAny = function(str, search="#me", side="both", ...)
	{
	search = as.character(search);
	if(search == "") { stop("you need to enter at least one character"); }
		s = functions.cleanKey(side, 1);

	# let's explode on "" to get chars as list
	search = str.explode("", search); # turn into a set
	chars = str.explode("", str);
	n = length(str);
	res = character(n);
	for(i in 1:n)
		{
		char = chars[[i]];
		nc = length(char);
		from.left = NULL;
		from.right = NULL;
		if(s == "b" || s == "l")
			{
			for(j in 1:nc)
				{
				if(char[j] %in% search) { from.left = c(from.left, j); } else { break; }
				}
			}
			
		if(s == "b" || s == "r")
			{
			# technically the search could reduce from PREVIOUS SEARCH
			for(j in nc:1)
				{
				if(char[j] %in% search) { from.right = c(from.right, j); } else { break; }
				}
			}

		# stop("here");
		set = switch(s,
						  "l"	= from.left,
						  "r" 	= from.right,
						  "b"  	= c(from.left, from.right),
					c(from.left, from.right)
					);
		nchar = char;
		if(!is.null(set)) { nchar = char[-c(set)]; }

		res[i] = str.implode("", nchar);
		}
	res;
	}


##################################################
#'
#' str.trimFromFixed
#'
#'
str.trimFromFixed = function(str, trim="#", side="both", ...)
	{
	s = functions.cleanKey(side, 1);
	str.len = strlen(str);
	n.str = length(str);
	slen = strlen(trim);
	# if x is character vector, x[1][2] should return the charAt(x[1], 2)
	# likely the OLD SCHOOL LEGACY of multidimensional arrays?
	
	first = substring(str, 1, slen);
	last = substring(str, str.len-slen+1, str.len);
	
	right = (last == trim);
	left = (first == trim);

	start = rep(1, n.str); # don't do anything
	if( (s=="l" || s=="b") )
		{
		# since multivariate, throws error
		# if(left) { start = 1 + slen; }
		start[left] = 1 + slen;
		}
	
	# both = (right & left);
	stop = str.len;
	if( (s=="r" || s=="b") )
		{
		stop[right] = stop[right] - slen;
		}

	substring(str, start, stop);	
	}


##################################################
#'
#' str.between
#'
#'
str.between = function(str, keys=c("__B64_", "_B64__"))
	{
	info = str.explode(keys[1], str);
	info2 = str.explode(keys[2], list.getElements(info, 2) );
	list.getElements(info2, 1);
	}


##################################################
#'
#' str.len
#'
#'
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


#' @rdname strlen
#' @export
strlen = str.len;

#' @rdname str.length
#' @export
str.length = str.len;


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
str.trim = function(str, side="both", method="stringi", pattern="", ...)
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
#' str.implode
#'
#'
str.implode = function(sep, str, method="base", ...)
	{
	# necessary overhead
	m = functions.cleanKey(method, 1);
	# if(!is.list(str)) { tmp = str; str = list(); str[[1]] = tmp; }
	str = list.prep(str);  # maybe redundant of a check from another function

	if(m == "c" && exists("cpp_implode"))
		{
		res = ( cpp_implode(sep, str) );
		return(res);
		}

	n = length(str);
	res = character(n);
	for(i in 1:n)
		{
		res[i] = paste0(str[[i]], collapse = sep);
		}
	res;
	}


#' @rdname implodeMe
#' @export
implodeMe = str.implode;

#' @rdname str.unsplit
#' @export
str.unsplit = str.implode;


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
str.repeat = function(str, times=1, method="base")
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

	### CASE 1
	if(slen == rlen)  ## pairwise over EACH subject
		{
		cat("\n", "CASE 1", "\n");
		res = character(nlen);
		for(j in 1:nlen)
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

	### CASE 2
	# str.replace(c("{monte}", "{for}"), "MONTE", c("Here is {monte} template", "Here is another {for} sure template {monte}!") );
	if(rlen == 1)
		{
		cat("\n", "CASE 2", "\n");
		res = character(nlen);
		for(j in 1:nlen)
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

	### CASE 3
	# str.replace(c("{monte}"), c("MONTE","FOR"), c("Here is {monte} template", "Here is another {for} sure template {monte}!") );
	if(slen == 1 && rlen > nlen)
		{
		cat("\n", "CASE 3", "\n");
		res = character(rlen);
		si = 1;
		for(j in 1:rlen)
			{
			str = subject[si]; 
			str = gsub(search[1], replace[j], str, fixed=TRUE);
			res[j] = str;
			si = 1 + si;  if(si > nlen) { si = 1; }  # loop over s, end, back to beginning
			}
		return (res);
		}

	cat("\n", "CASE 4", "\n");
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


#' @rdname str_replace
#' @export
str_replace = str.replace;


##################################################
#'
#' str.toObject
#'
#'
# eval ... parse 
str.toObject = function(obj.str) 
	{ 
	eval(parse(text = obj.str));  # as-is, no checks?
	}  
	
##################################################
#'
#' str.fromObject
#'
#'
# as.character substitute ... doesn't have to exist ...
# this does require the object to exist ...  
str.fromObject = function(obj) 
	{ 
	if( is.set(obj, TRUE) ) { return(as.character(substitute(obj)));	}
	return("");
	}  


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


##################################################
#'
#' str.removeWhiteSpace
#'
#'
str.removeWhiteSpace = function( str, replace=" ", n = 2,
                              pre.trim = TRUE, post.trim = TRUE, ...)
  {
	if(pre.trim) { str = str.trim(str, ...); }
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



str.stripTags = function(str)
	{
	return(gsub("<.*?>", "", str));
	}
	
strip.tags = str.stripTags;
strip_tags = str.stripTags;

##################################################
#'
#' str.translate
#'
#'
# stringi::stri_trans_general(c("groß© żółć La Niña köszönöm", "Ábcdêãçoàúü", "Record high °C"), "latin-ascii")
# get rid of temperature ??? caused a BUG before?
str.translate = function(str, to="latin-ascii")
	{
	# to = "upper; latin-ascii"; # ALSO works, in the DOCS
	"upper; latin-ascii"
	stringi::stri_trans_general(str, to=to);
	}


##################################################
#'
#' str.push_back
#'
#'
## is this stringr::str_c ??
## C++ ... obj.push_back(element) ... element, obj
str.push_back = function(sub, str, sub, collapse="")
	{
	paste0(str, sub, collapse=collapse);
	}

#' @rdname str.push_last
#' @export
str.push_last = str.push_back;


##################################################
#'
#' str.push_front
#'
#'
str.push_front = function(sub, str, collapse="")
	{
	paste0(sub, str, collapse=collapse);
	}


#' @rdname str.push_first
#' @export
str.push_first = str.push_front;


# ucfirst ...
str.capitalize = function(str) {} 


##################################################
#'
#' str.grammaticalNumber
#'
#'
str.grammaticalNumber = function(str, n=1, type="noun")
	{
	# 1 timer, 0 timers, 3 timers 
	# we just return the word (input is singular) as a plural if necessary
	
	if(n == 1) {} else { return( paste0(str,"s", collapse="") ); }
	
	}


##################################################
#'
#' str.wordWrap
#'
#'
# https://www.php.net/manual/en/function.wordwrap.php
str.wordWrap = function(str, width=66, 
								line.break = "\n", 
								cut_long_words = FALSE, 
								wrap_long_words_if_possible = TRUE,
								indent = "\t",
								hanging.indent = "\t\t",
								use.hanging = TRUE,
								paragraph = "_{P}_", 
								templates = c("{2n}", "{2n2t}"),
								replaces  = c("\n\n", "\n\n\t\t"),
								l.tag = "", r.tag = "",
								breaks.first = 0, breaks.last = 0,
								tabn = 4, use.tabs = TRUE
						) 
	{
	# if(cut_long_words == FALSE) ... and wrap_long==TRUE ... I will split and " " and move to next line (if the line width is enough
	# we will use tabn to compute current width
	# we will print RAW result ... cat(result) if you want ...
	# paragraph will create extra space, do first-indent, then other indent
	# r.tag may be jagged if tabn not used?
	# if(use.tabs == FALSE) ... we replace with tabn
	

msg = "tldr; \n\t R-dev believes this is poor programming practice to allow you to \n\t
suppressError( so they have not included it in base R.  It is probably true, but 'git-r-done' first, and then figure out the minutia such as why this function is throwing an error.  That is why I have past such a VERBOSE message to you, dear reader.";


	}


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
#' ^ i.pad # s.pad CONTENT s.pad # $
str.commentWrapper = function() {}
str.commentWrapper = function(str="Welcome to the {humanVerse}", 
									nchars=0, 
									c.tag="#", 
									r.tag=c.tag, 
									s.tag=" ", s.pad=5, 
									i.tag=" ", i.pad=15
							)
	{
	# punchcards had 80 characters, a traditional typewriter US had 72 characters per line (CPL)
	# http://mikeyanderson.com/optimal_characters_per_line
	# Quotes "Jakob Nielson" + 5 ... states 66 is optimal
	# 6.5 inches (1 inch margin) x 10 per inch ... about 65 ... we would do +/- 3 in typing class ... override end
	
	n = length(str);
	res = character(n);
	mylengths = integer(n);
		i.str = str.repeat(i.tag, i.pad); i.len = strlen(i.str);
		s.str = str.repeat(s.tag, s.pad); s.len = strlen(s.str); 
	for(i in 1:n)
		{
		s = paste0(		i.str, 
						c.tag, 
						r.tag, 
						s.str, 
					str[i], 
						s.str, 
						r.tag, 
						c.tag
					);
		slen = strlen(s);
		if(slen < nchars)
			{
			n.tag = 1 + ceiling( (nchars - slen)/2 );
				n.str = str.repeat(s.tag, n.tag);
			s = paste0(		i.str,
							c.tag, 
							r.tag, 
							n.str, 
						str[i], 
							n.str,
							r.tag, 
							c.tag
						);
			slen = strlen(s);
			}
			f.n = slen - 2*strlen(s.pad) - i.len - strlen(i.pad);
			f.str = str.repeat(r.tag, f.n);
			c.line = paste0(i.str, c.tag, r.tag, f.str, r.tag, c.tag, "\n");
		res[i] = paste0(c.line, s, "\n", c.line);
		# mylengths[i] = slen;
		mylengths[i] = strlen(res[i]);
		}
	
	res = property.set(res, "lengths", mylengths);
	res = property.set(res, "indent", i.len);
	res;
	}


##################################################
#'
#' charVector
#'
#' @param strvec
#' @param sep
#'
#' @return
#' @export
#'
#' @examples
str.toCharVector = function(strvec, sep="")
	{
	n = length(strvec);
	res = list();
	for(i in 1:n)
		{
		res[[i]] = strsplit(strvec[i], sep ,fixed=TRUE)[[1]];
		}
	list.return(res);
	}


#' @rdname charVector
#' @export
charVector = str.toCharVector;

