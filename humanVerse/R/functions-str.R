


str.width = function() {}
str.height = function() {} 



str.compare = function(a, b=NULL, methods="all")
	{
	# similar to cosine similarity, do matrix(a) ... or vec to matr 
	# jaro-winkler
	# leven ... dist/sim 
	# modified leven 
	# should we explode into charVectors and do counts / cosine.similarity?
	# soundex(ASCII)
	
	
	}


# conflict with "str" method?
# http://adv-r.had.co.nz/S3.html
# .S3PrimitiveGenerics
# methods("str");
# strwidth(s, units = "user", cex = NULL, font = NULL, vfont = NULL, ...)
# strheight(s, units = "user", cex = NULL, font = NULL, vfont = NULL, ...)
# maybe gr.


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' str.count
#'
#'
#------------------------------------------------#
str.count = function(str, what="|") 
	{ 
	# count occurrence of "what" in a string 
	# n.pipes = str.count(lines, what="|");
	info = str.explode(what, str);
# dput(info);
	res = list.getLengths(info);
	if(is.null(res)) { return(0*length(str)); }	
	res-1;  # 2 splits = 1 occurrence; ... 3 splits = 2 occurrences ... 
	}
	

	


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' str.between
#'
#'
#------------------------------------------------#
str.between = function() {}
str.between = function(str, keys=c("__B64_", "_B64__"))
	{
	info = str.explode(keys[1], str);
	if(keys[2] == "") 
		{
		# we are at the END of the string ...
		return( list.getElements(info, 2) );
		}
	info2 = str.explode(keys[2], list.getElements(info, 2) );
	list.getElements(info2, 1);
	}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' str.len
#'
#'
#------------------------------------------------#
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
	METHOD = prep.arg(method, 1);

	if(METHOD == "s" && is.library_("stringi") )
		{		
		return ( stringi::stri_length(str) );
		}

	if(METHOD == "b")
		{
		return( nchar( as.character(str), type="chars") );
		}

	if(METHOD == "c" && exists("cpp_strlen"))
		{
		return( cpp_strlen(str) );
		} 


	nchar( as.character(str), type="chars");
	}


#++++++++++++++++++++++++#
#'
#' @rdname strlen
#' @export
strlen = str.len;

#++++++++++++++++++++++++#
#'
#' @rdname str.length
#' @export
str.length = str.len;


 
str.case = function(str, case="lower")
	{
#cat("\n str.toCase: ", str, "\n");  # was there a weird str() collision?
	cas = substring(tolower(case), 1, 3);
#dput(cas);
			# has to be base-R (not str.tolower, recursion)
	res = switch(cas,					
					  "low"	= tolower(str),		# lowercase
					  "upp" = toupper(str),		# uppercase 
 					  "ucf" = ucfirst(str),		# ucfirst
					  "ucw" = ucwords(str),		# ucwords
				str								# DEFAULT [as-is]
				);	
#dput(str);
#dput(res); 	
 	
	res;	
	}


str.toCase = str.case;


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
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
#------------------------------------------------#
str.trim = function(str, side="both", method="first", pattern=NULL, ...)
	{  
	METHOD = prep.strMethod(method, n=1);
	SIDE = prep.strSide(side, n=1);
	
	PATTERN = list(
				"cpp" = " \t\n\r\f\v",
				"stringi" = "\\P{Wspace}",
				"base" = "\\s+",  # is this faster than base::trimws?
				"trimws" = "[ \t\r\n]"
				);
				
	p = pattern; if(is.null(p)) { p = PATTERN[[METHOD]]; }
	
	FNS = list(
			"cpp" 		= function() { cpp_trim(str, SIDE, p); } , 
			"stringi" 	= function() { stringi::stri_trim(str, SIDE, p); } ,
			"base" 		= function() { strtrim_(str, SIDE, p); },
			"trimws" 	= function() { trimws(str, SIDE, p); }
			);
	
	if(METHOD != "first")
		{
		# cat("\n as =- is \n");
		# AS-IS, no checks 
		return(  FNS[[METHOD]]() );
		}

	# CASCADING, first-one to meet criteria 
	hasResult = FALSE;
	if(!hasResult && exists("cpp_trim"))
		{
		# cat("\n monte \n");
		hasResult = TRUE;
		if(is.null(p)) { p = PATTERN[["cpp"]]; }
		res = FNS[["cpp"]]();
		}
		
	if(!hasResult && is.library_("stringi"))
		{
		hasResult = TRUE;
		if(is.null(p)) { p = PATTERN[["stringi"]]; }
		res = FNS[["stringi"]]();		
		}
	if(!hasResult)
		{
		if(is.null(p)) { p = PATTERN[["base"]]; }
		res = FNS[["base"]]();
		}

	res;
	}

#++++++++++++++++++++++++#
#'
#' @rdname trimMe
#' @export
trimMe = str.trim;



#++++++++++++++++++++++++#
#'
#' @rdname str_trim
#' @export
str_trim = str.trim;



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
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
#------------------------------------------------# 
# BASE is broken ... 
str.explode = function(sep = " ", str = "hello friend", method="first")
	{
	METHOD = prep.strMethod(method, n=1);

	FNS = list(
			"cpp" 		= function() { cpp_explode(sep, str); } , 
			"stringi" 	= function() { stringi::stri_split_fixed(str, sep); } ,
			"base" 		= function() { strsplit_(str, sep, fixed=TRUE); }
			);
			
	if(METHOD != "first")
		{
		# AS-IS, no checks 
		return(  list.return( FNS[[METHOD]]() ) );
		}

	# CASCADING, first-one to meet criteria 
	hasResult = FALSE;
	if(!hasResult && exists("cpp_explode"))
		{
		# must not have exported it ... 
		hasResult = TRUE;
		res = FNS[["cpp"]]();
		}
		
	if(!hasResult && is.library_("stringi") && sep != "" )
		{
		hasResult = TRUE;
		res = FNS[["stringi"]]();		
		}
		
	if(!hasResult)
		{
		res = FNS[["base"]]();
		}

	# will be collapsed into CharacterVector if len == 1
	list.return(res);
	}

#++++++++++++++++++++++++#
#'
#' @rdname explodeMe
#' @export
explodeMe = str.explode;

#++++++++++++++++++++++++#
#'
#' @rdname str.split
#' @export
str.split = str.explode;


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' str.implode
#'
#'
#------------------------------------------------#
str.implode = function(sep=" ", str, method="base")
	{
	str = check.list(str);  # maybe redundant of a check from another function

	METHOD = prep.strMethod(method, n=1);

	FNS = list(
			"cpp" 		= function() { cpp_implode(sep, str); } , 
			"stringi" 	= function() { stop("stringi implemented?"); } ,
			"base" 		= function() { strunsplit_(str, sep); }
			);
			
	if(METHOD != "first")
		{
		# AS-IS, no checks 
		return(  FNS[[METHOD]]() );
		}

	# CASCADING, first-one to meet criteria 
	hasResult = FALSE;
	if(!hasResult && exists("cpp_implode"))
		{
		# must not have exported it ... 
		hasResult = TRUE;
		res = FNS[["cpp"]]();
		}
		
	# if(!hasResult && is.library_("stringi"))
		# {
		# hasResult = TRUE;
		# res = FNS[["stringi"]]();		
		# }
		
	if(!hasResult)
		{
		res = FNS[["base"]]();
		}

	# will be collapsed into CharacterVector if len == 1
	# unnecessary (takes a list, returns a charVec)
	res;
	}


#++++++++++++++++++++++++#
#'
#' @rdname implodeMe
#' @export
implodeMe = str.implode;

#++++++++++++++++++++++++#
#'
#' @rdname str.unsplit
#' @export
str.unsplit = str.implode;


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
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
#------------------------------------------------#
str.repeat = function(str, times=1, method="base")
	{
	METHOD = prep.strMethod(method, n=1);

	FNS = list(
			"cpp" 		= function() { cpp_str_repeat(str, times); } , 
			"stringi" 	= function() { stop("stringi implemented?"); } ,
			"base" 		= function() { strrep_(str, times); }
			);
			
	if(METHOD != "first")
		{
		# AS-IS, no checks 
		return(  list.return( FNS[[METHOD]]() ) );
		}

	# CASCADING, first-one to meet criteria 
	hasResult = FALSE;
	if(!hasResult && exists("cpp_str_repeat"))
		{
		# must not have exported it ... 
		hasResult = TRUE;
		res = FNS[["cpp"]]();
		}
		
	# if(!hasResult && is.library_("stringi"))
		# {
		# hasResult = TRUE;
		# res = FNS[["stringi"]]();		
		# }

	if(!hasResult)
		{
		res = FNS[["base"]]();
		}

	res;
	}

#++++++++++++++++++++++++#
#'
#' @rdname str_repeat
#' @export
str_repeat = str.repeat;

#++++++++++++++++++++++++#
#'
#' @rdname str.rep
#' @export
str.rep = str.repeat;

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
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
#------------------------------------------------#
	# maybe attribute force.simple = TRUE ... 1-1 over subjects if 1-n
	# maybe just make replace a list(replace) if not, then loop over it's length ... this is for EVAL to work ... 
	# three sets of dots, how to know when next ... maybe a semicolon;
	# search,search,search; replace,replace; subject,subject 
str.replace = function(search, replace, subject, method="first", force.case=0)
	{
	search = as.character(search);
	replace = as.character(replace);
	subject = as.character(subject);
	# TODO ... add to CPP logic force.case = 0
	# zero is auto, case 1, 2, 3, 4 are the choices below ...
	# 1 is pairwise 	n-n over all N
	# 2 is 				n-1 over all N
	# 3 is 				1-n paired over each N 
	# 4 is 				m=n over each N ... recycling (nonsensical)
debug = FALSE;

	METHOD = prep.strMethod(method, n=1);

			# stringi::stri_replace_all_fixed
			# doesn't seem to work correctly  ... 
	FNS = list(
			"cpp" 		= function() { cpp_str_replace(search, replace, subject); } , 
			"stringi" 	= function() { stop("stringi implemented?"); } ,
			"base" 		= function() { strreplace_(search, replace, subject); }
			);
			
	if(METHOD != "first")
		{
		# AS-IS, no checks 
		return(  list.return( FNS[[METHOD]]() ) );
		}

	# CASCADING, first-one to meet criteria 
	hasResult = FALSE;
	if(!hasResult && exists("cpp_str_replace"))
		{
		# must not have exported it ... 
		hasResult = TRUE;
		res = FNS[["cpp"]]();
		}
		
	# if(!hasResult && is.library_("stringi"))
		# {
		# hasResult = TRUE;
		# res = FNS[["stringi"]]();		
		# }

	if(!hasResult)
		{
		res = FNS[["base"]]();
		}

	res;
	}


#++++++++++++++++++++++++#
#'
#' @rdname str_replace
#' @export
str_replace = str.replace;



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
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
#------------------------------------------------#
str.pad = function() {}
str.pad = function(str,
					to.length	= max(str.len(str)),
					padding		= "0", 
					side		= "RIGHT",  # default is for NNN.dd00 decimal
					  
					method		= "first"
					)
	{
	str = as.character(str);	
	
	METHOD = prep.strMethod(method, n=1);
	SIDE = prep.strSide(side, n=1);

	FNS = list(
			"cpp" 		= function() { stop("not [cpp_str_pad] implementedn (yet?)") } , 
			"stringi" 	= function() { stringi::stri_pad(str, to.length, SIDE, padding); } ,
			"base" 		= function() { strpad_(str, to.length, padding, SIDE); }
			);
			
	if(METHOD != "first")
		{
		# AS-IS, no checks 
		return(  list.return( FNS[[METHOD]]() ) );
		}

	# CASCADING, first-one to meet criteria 
	hasResult = FALSE;
	if(!hasResult && exists("cpp_str_pad"))
		{
		# must not have exported it ... 
		hasResult = TRUE;
		res = FNS[["cpp"]]();
		}
		
	if(!hasResult && is.library_("stringi"))
		{
		hasResult = TRUE;
		res = FNS[["stringi"]]();		
		}
	if(!hasResult)
		{
		res = FNS[["base"]]();
		}

	res;
	}


# paste0( substring(x[!idx], 1, (cwidth-1) ), trunc.sym);
str.truncate = function(str, to.length=5, keep="right")
	{
	# str.pad in stringi FAVORS the right ... 
	#   "1"   "12"  "123" ==> "010" "120" "123"
	KEEP = prep.arg(keep, n=1);
	if(KEEP == "l")
		{
		res = substring(str, 1, to.length);
		}
	if(KEEP == "r")
		{
		slen = str.len(str);
		from = slen-to.length+1; 
		ifelse( {from < 1} , { from = 1; }, { from = from; });
		res = substring(str, from, slen);
		}
	if(KEEP == "o")  # outer ... opposite of inner 
		{
		# str = c("123000456", "ABC000DEF")

		slen = str.len(str);
				
		info = (to.length)/2;
		wleft = ceiling(info);
		wright = floor(info);
		
		from = slen-wright+1; 
		ifelse( {from < 1} , { from = 1; }, { from = from; });
		resR = substring(str, from, slen);
		
		to = wleft;
		# maximum is to return original string ...
		ifelse( {from <= to} , { to = from-1 }, { to = to; });
		ifelse( {to < 1} , { to = 0; s=0; }, { to = to; s=1; });
		resL = substring(str, s, to);
		res = paste0(resL,resR);
		res;
		}
	if(KEEP == "i")  # "i"nner 
		{
		# str=c("000123000", "00123000", "00012300");

		slen = str.len(str);
		info = (slen - to.length)/2;
		wleft = ceiling(info);
		
		# wright = floor(info); # one off on either side ...
		
		res = substring(str, wleft+1, wleft+to.length);
		}
	res;
	}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' str.translate
#'
#'
# stringi::stri_trans_general(c("groß© żółć La Niña köszönöm", "Ábcdêãçoàúü", "Record high °C"), "latin-ascii")
# get rid of temperature ??? caused a BUG before?
#------------------------------------------------#
str.translate = function(str, to="latin-ascii")
	{  
	# to = "upper; latin-ascii"; # ALSO works, in the DOCS
	"upper; latin-ascii"
	stringi::stri_trans_general(str, to=to);
	}
 



str.end = function(search="</i>", str="<i>hello friend</i>", trim = FALSE )
	{
	strlen = str.len(str);
	slen = str.len(search);
		start = strlen - slen + 1;	idx = v.return(which(start < 1));
		if(!is.null(idx)) { start[idx] = 1; }
	sub = substring(str, start, strlen);	
	res = (sub == search);
	
	if(!trim) { return(res); }	
	if(allFALSE(res)) { return(str); }
	
	rem = substring(str, 1, (start-1));  # TEST  ... str == paste0(rem,sub)
		
	nstr = str;
	nstr[res] = rem[res];
	nstr;  
	}

# STRPOS() returns the index of the first occurence of its second argument (“needle”) in its first argument (“haystack”), or -1 if there are no occurrences.

# n=1 returns first position of occurrence 
# skip != 0 ... skip this far into the search (truncate but add result pos back)
# if n=1 we return a vector ... of pos with NA for not found 
# x = "monte says hi monte loves alex and mama and all";
# str.pos(c(x, str.wordShuffle(x), str.wordShuffle(x)), "and");
str.pos = function(str, search, n=Inf, skip=0)
	{
	slen = str.len(str);	
# dput(str);	
	len.search = str.len(search);
	info = check.list(str.explode(search, str));
	ni = length(info);
	res = vector("list", ni);
	for(i in 1:ni)
		{
		vec = info[[i]];
		vlen = str.len(vec);
		vn = length(vlen);
		vlen = vlen[-c(vn)]; 
		pos = vlen + 1;
		pn = length(pos);
		if(pn > 1)
			{
			for(j in 2:pn)
				{
				pos[j] = pos[j-1] + len.search + vlen[j];
				}
			}
		idx = v.return(which(pos < skip));
		if(!is.null(idx)) { pos = pos[-c(idx)]; }
		if(n < Inf)
			{
			pos = v.fill(pos, n, NA);
			}		
		pos = v.return(pos);
		res[[i]] = pos;
		}
	list.return(res);
	}
	
strpos = str.pos;