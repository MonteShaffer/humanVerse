


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
#' str.between
#'
#'
#------------------------------------------------#
str.between = function() {}
# str.between = function(str, keys=c("__B64_", "_B64__"))
str.between = function(L = "__B64_", str=str, R = "_B64__")
	{  
	if(L != "")
		{
		info = str.explode(L, str);
		if(R == "") 
			{
			# we are at the END of the string ...
			return( list.getElements(info, 2) );
			}
		info2 = str.explode(R, list.getElements(info, 2) );
		return( list.getElements(info2, 1) );
		}
	info = str.explode(R, str);
	list.getElements(info, 1);		
	}

str.slice = function(L = '<section id="', str, R = '</section>', keep = TRUE )
	{
	str = str[1];
		slen = strlen(L);
		rlen = strlen(R);
	Lpos = str.pos(L, str);
	Rpos = str.pos(R, str);  
	n = length(Lpos);
	m = length(Rpos);
		if(m > n)
			{
			# update Rpos, nearest AFTER Lpos 
			Npos = integer(n);
			for(i in 1:n)
				{
				idx =  which(Rpos > (slen + Lpos[i]) );
				Npos[i] = Rpos[ idx[1] ];
				}
			Rpos = Npos;
			}
	res = character(n);
	for(i in 1:n)
		{ 
		### Rpos may be longer, find nearest > Lpos 
		s = Lpos[i];		if(!keep) { s %+=% slen; }
		e = Rpos[i]-1;		if( keep) { e %+=% rlen; }
		
		res[i] = substring(str, s, e);
		}
	res;
	} 
	
	


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' str.len
#'
#'
#------------------------------------------------#
str.len = function(str, method="first", locale="")
  {
	# LOCALE is a TODO

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
		
	str = as.character(str);	
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.METHOD = check.type(method);
	if(!ct.METHOD || !is.character(method))	
		{ method = deparse(substitute(method)); } 
##########################################################
	METHOD = prep.strMethod(method, n=1);

	FNS = list(
			"cpp" 		= function() { cpp_strlen(str); } , 
			"stringi" 	= function() { stringi::stri_length(str); } ,
			"base" 		= function() { nchar( str, type="chars"); }
			);
			
	if(METHOD != "first")
		{
		# AS-IS, no checks 
		return(  list.return( FNS[[METHOD]]() ) );
		}

	# CASCADING, first-one to meet criteria 
	hasResult = FALSE;
	if(!hasResult && exists("cpp_strlen"))
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
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.CASE = check.type(case);
	if(!ct.CASE || !is.character(case))	
		{ case = deparse(substitute(case)); } 
##########################################################
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
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.METHOD = check.type(method);
	if(!ct.METHOD || !is.character(method))	
		{ method = deparse(substitute(method)); } 
##########################################################
	METHOD = prep.strMethod(method, n=1);
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.SIDE = check.type(side);
	if(!ct.SIDE || !is.character(side))	
		{ side = deparse(substitute(side)); } 
##########################################################
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
#' str.repeat
#'
#' @param str
#' @param times
#'
#' @return
#' @export
#'
#' @examples
# repeats a str in a str, n times (not a vector like `rep`)
#------------------------------------------------#
str.repeat = function(str, times=1, method="base")
	{
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.METHOD = check.type(method);
	if(!ct.METHOD || !is.character(method))	
		{ method = deparse(substitute(method)); } 
##########################################################
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
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.METHOD = check.type(method);
	if(!ct.METHOD || !is.character(method))	
		{ method = deparse(substitute(method)); } 
##########################################################
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
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.METHOD = check.type(method);
	if(!ct.METHOD || !is.character(method))	
		{ method = deparse(substitute(method)); } 
##########################################################
	METHOD = prep.strMethod(method, n=1);
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.SIDE = check.type(side);
	if(!ct.SIDE || !is.character(side))	
		{ side = deparse(substitute(side)); } 
##########################################################
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
	KEEP = prep.switch(	prep.arg(keep, n=1), 
						c("l","r","i","o"), 
						c("left", "right", "inner", "outer"), 
						"right"
						);

	if(KEEP == "left")
		{
		res = substring(str, 1, to.length);
		}
	if(KEEP == "right")
		{
		slen = str.len(str);
		from = slen-to.length+1; 
		ifelse( {from < 1} , { from = 1; }, { from = from; });
		res = substring(str, from, slen);
		}
	if(KEEP == "outer")  # outer ... opposite of inner 
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
	if(KEEP == "inner")  # "i"nner 
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
 




str.subtract = function(a, b, from="left")
	{	
	n = length(a);
	if(length(b) != n) { b = rep(b, out.length = n); }
	res = character(n);
	a_ = a; b_ = b;
	
# dput.one(a);
# dput.one(b);
# .cat( "a = ", a, "\n", "b = ", b, "\n\t", from );
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.FROM = check.type(from);
	if(!ct.FROM || !is.character(from))	
		{ from = deparse(substitute(from)); } 
##########################################################
	FROM = prep.switch(	prep.arg(from, n=1), 
						c("l","r"), 
						c("left", "right"), 
						"left"
						);
	
	if(FROM == "left")
		{
		for(i in 1:n)
			{
			a = a_[i];
			b = b_[i];
			if(b == "") { res[i] = a; next; }
			# contiguous elements that are the same from the left ...
			avec = str.explode("", a);
				alen = length(avec);
			bvec = str.explode("", b);
			
			# idx = set.match(avec, bvec);
			# left = left.contiguous(idx, alen);
			
			logic = suppressWarnings((avec) == (bvec));
			idx   = v.which(logic, TRUE);
			left  = left.contiguous(idx, alen);
					
					
			mtrim = 0; 
			if(!is.null(left)) { mtrim = v.last(left); }
			res[i] = substring(a, 1+mtrim, alen) ;
			}
		return(res);
		} else {  
				for(i in 1:n)
					{
					a = a_[i];
					b = b_[i];
					if(b == "") { res[i] = a; next; }
					avec = str.explode("", a);
						alen = length(avec);
					bvec = str.explode("", b);
					# reverse the array
					# idx = set.match(rev(avec), rev(bvec));
					# left = left.contiguous(idx, alen);
					
					logic = suppressWarnings(rev(avec) == rev(bvec));
					idx   = v.which(logic, TRUE);
					left = left.contiguous(idx, alen);
					
					mtrim = 0; 
					if(!is.null(left)) { mtrim = v.last(left); }
					res[i] = substring(a, 1, alen-mtrim) ;
					}
				return(res);
				}
	}
	

#############################################
#  THESE are 'action' functions on strings  #
#   action element is first, str is second  #
#############################################

str.contains = function(needle, haystack)
	{
	grepl(needle, haystack, fixed = TRUE);
	}


	
# str.startsWith = str.starts 
str.starts = function() {}
str.starts = function(search="<i>", str=c("<i>hello friend</i>", "<i>how are you doing today?</i>", "I am fine <i>[well]</i>, thank you for asking. [fine/well are ambiguous ... --> Estoy bien, gracias a Dios ... <i>TRIOS?</i>]"), trim = FALSE )
	{
	info = check.list(str.explode(search, str));
	len = list.getLengths(info); 
	first = list.getElements(info, 1);
	
	logic = v.test(first, EMPTY);
	if(!trim) { return(logic); }

	len[logic] = len[logic] - 1;
	
	b = rep(0, length(len));
	b[logic] = b[logic] + 1;
	
	new = list.truncate(info, b, "beginning");  # begin, start , anything but [e]nd ... 
		  
	str.implode(search, new);  
	}
	
	
# str.endsWith = str.ends
str.ends = function() {}
str.ends = function(search="</i>", str=c("<i>hello friend</i>", "<i>how are you doing today?</i>", "I am fine <i>[well]</i>, thank you for asking. [fine/well are ambiguous ... --> Estoy bien, gracias a Dios ... <i>TRIOS?</i>]"), trim = FALSE )
	{ 
	info = check.list(str.explode(search, str));
	# last = list.getLastElements(info);
	len = list.getLengths(info); 
	last = list.getElements(info, len);
	
	logic = v.test(last, EMPTY);
	if(!trim) { return(logic); }

	len[logic] = len[logic] - 1;
	
	new = list.truncate(info, len);
		  
	str.implode(search, new);
	}
	
	
	
str.before = function(search, str, posIDX=1)
	{
	n = length(str); 
	if(length(posIDX) != n) 
		{ posIDX = rep(posIDX, out.length = n); }
	 
	selen = str.len(search);
	#slen = str.len(str);
	
	pos = check.list(str.pos(search, str));
		
	# return elements ... or just IDX?
	posIDX = list.checkIDX(pos, posIDX); 
	   
	idx = list.getElements(pos, posIDX);
	  
	# substring(str, idx+selen, slen);
	substring(str, 1, idx-selen);
	}
	
str.after = function(search, str, posIDX=1)
	{	  
	n = length(str);
	if(length(posIDX) != n) 
		{ posIDX = rep(posIDX, out.length = n); }
	
	selen = str.len(search);
	slen = str.len(str);
	
	pos  = check.list(str.pos(search, str));
	
		PIDX = prep.arg(posIDX, n=2);	# allow non-numeric
		logicL = v.test(PIDX, "la");	# last 
		logicF = v.test(PIDX, "fi");	# first 
		
	posIDX[logicF] = 1;  
	posIDX[logicL] = list.getLengths(pos[logicL]);
		posIDX = as.integer(posIDX);
	
	sidx = list.getElements(pos, posIDX);
	
	substring(str, sidx+selen, slen);
	}
	
	


# STRPOS() returns the index of the first occurence of its second argument (“needle”) in its first argument (“haystack”), or -1 if there are no occurrences.

# n=1 returns first position of occurrence 
# skip != 0 ... skip this far into the search (truncate but add result pos back)
# if n=1 we return a vector ... of pos with NA for not found 
# x = "monte says hi monte loves alex and mama and all";
# str.pos(c(x, str.wordShuffle(x), str.wordShuffle(x)), "and");
# with str.explode(search, str) ... PHP is SCHIZO like R.
str.pos = function(search, str, n=Inf, skip=0)
	{
	# what if I have lots of searches, one str 
	# currently lots of strings, one search 
	## ns = length(search);
	## if(ns > 1) { }
	
	slen = str.len(str);	

	len.search = str.len(search);
	info = check.list(str.explode(search, str));
	ni = length(info);
	if(ni == 0) { return( -1 ); }
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
		pos = v.TO(v.return(pos), NULL, -1);
		res[[i]] = pos;
		}
	list.return(res);
	}
	 
	 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' str.count
#'
#'
#------------------------------------------------#
str.count = function(what="|", str) 
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
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.METHOD = check.type(method);
	if(!ct.METHOD || !is.character(method))	
		{ method = deparse(substitute(method)); } 
##########################################################
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



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' str.implode
#'
#'
#------------------------------------------------#
str.implode = function(sep=" ", str, method="base")
	{
	str = check.list(str);  # maybe redundant of a check from another function
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.METHOD = check.type(method);
	if(!ct.METHOD || !is.character(method))	
		{ method = deparse(substitute(method)); } 
##########################################################
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






