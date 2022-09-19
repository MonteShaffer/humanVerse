


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
	m = prep.arg(method, 1);

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
str.trim = function(str, side="both", method="stringi", pattern="", ...)
  {
  # do something smart with dots.addTo ... 
  # based on CALLER formals ...
  # assign back "side", method, pattern in order ... 
  # allow for dots to be ignored if looking for "both" ??? !!! ???
  # str = dots.addTo(str, ...);
	# necessary overhead
	s = prep.arg(side, 1);
	m = prep.arg(method, 1);
	

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

	
	
	# is this faster than base::trimws?
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
str.explode = function(sep = " ", str = "hello friend", method="stringi")
	{
	# necessary overhead
	METHOD = prep.arg(method, 1);

	hasResult = FALSE;

	if(!hasResult && METHOD == "s" && is.library("stringi") )
		{
		res = (stringi::stri_split_fixed(str, sep));
		hasResult = TRUE;
		}

	if(!hasResult && METHOD == "c" && exists("cpp_explode"))
		{
		res = ( cpp_explode(sep, str) );
		hasResult = TRUE;
		}
	
	if(!hasResult)  
		{
		# stringi works as expected, what about cpp?
		# if "<i>humanVerse</i>" ... 
			# "<i>" returns "" "humanVerse</i>"
			# "</i>" returns "<i>humanVerse" without trailing "" 
			# SO ... it's a feature ... 
			fill = "~"; if(sep == "~") { fill = "^"; }
			flen = str.len(fill);
			tmp = paste0(str,fill);
		res = tres = strsplit(tmp, sep, fixed=TRUE);
			nt = length(tres);
			tlen = list.getLengths(tres);
		for(i in 1:nt)
			{
			sidx = tlen[i];
			element = tres[[i]][ sidx ];
			slen = str.len(element);
			
			if(slen == flen) { element = ""; }
			if(slen > flen)  { element = substring(element, 1, slen-flen); }
			res[[i]][ sidx ] = element;
			}
			
		# res = strsplit(str, sep, fixed=TRUE);
		# res;
		}

	# will be collapsed into CharacterVector if len == 1
	list.return(res, unlist=FALSE);
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
	# necessary overhead
	m = prep.arg(method, 1);
	# if(!is.list(str)) { tmp = str; str = list(); str[[1]] = tmp; }
	str = check.list(str);  # maybe redundant of a check from another function

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
	m = prep.arg(method, 1);

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
str.replace = function(search, replace, subject, method="base", force.case=0)
	{
	# TODO ... add to CPP logic force.case = 0
	# zero is auto, case 1, 2, 3, 4 are the choices below ...
	# 1 is pairwise 	n-n over all N
	# 2 is 				n-1 over all N
	# 3 is 				1-n paired over each N 
	# 4 is 				m=n over each N ... recycling (nonsensical)
debug = FALSE;
	m = prep.arg(method, 1);

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
if(debug)
	{
cat("\n", "CASE 1", "\n");
	}
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
if(debug)
	{
cat("\n", "CASE 2", "\n");
	}
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
if(debug)
	{
cat("\n", "CASE 3", "\n");
	}
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

if(debug)
	{
cat("\n", "CASE 4", "\n");
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
					  
					method		= "stringi"
					)
	{
	# str = dots.addTo(str, ...);
	str = as.character(str);
	# necessary overhead
	SIDE = prep.arg(side, n=1);
	METHOD = prep.arg(method, n=1);

	if(METHOD == "s" && is.library("stringi") )
		{
		res = switch(SIDE,
						  "l"	= stringi::stri_pad_left (str, width=to.length, pad=padding),
						  "r" 	= stringi::stri_pad_right (str, width=to.length, pad=padding),
						  "b"  	= stringi::stri_pad_both (str, width=to.length, pad=padding),
					stringi::stri_pad_both (str, width=to.length, pad=padding)
					);
		return (res);
		}

	# METHOD == "base";  	# FALLBACK DEFAULT 
	ns = str.len(str);
	rs = to.length - ns;  	# how many pads per element 
	n = length(str); 		# how many strings
	res = character(n);
	
	for(i in 1:n)
		{
		myr = rs[i];
		pads = str.repeat(padding, myr); 
		if(SIDE == "b")
			{
			myr_right	= ceiling(myr / 2);
			pad_right	= str.repeat(padding, ( myr_right )	);
			pad_left	= str.repeat(padding, ( myr - myr_right )	);
			}
		
		# if padding is multiple length, may be too long
		res[i] = switch(SIDE,
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