














#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' str.grammaticalNumber
#'
#'
#------------------------------------------------#
str.grammaticalNumber = function(str, n=1, type="noun") 
	{
	# 1 timer, 0 timers, 3 timers 
	# we just return the word (input is singular) as a plural if necessary
	
	if(n == 1) {} else { return( paste0(str,"s", collapse="") ); }
	
	}



#++++++++++++++++++++++++#
#'
#' @rdname str.gn
#' @export
str.gn = str.grammaticalNumber;




 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' str.characterFrequency
#'
#'
# [u]nsorted,   [u]nsorted-[r]everse;  # this is how `table` naturally gives	
# [c]ount, 		[c]ount-[r]everse;  
# [a]lpha, 		[a]lpha-[r]everse, 
# alpha puts special chars before the letter [a] 
#------------------------------------------------#
str.characterFrequency = function(str,  
										case="lower", 
										sort.by="count",
										horizontal=TRUE,
										space.char = "[sp]"
									)
	{
	ca  = prep.arg(case, 2);
	# we collapse all to get FREQ of what was entered
	all = paste0(str, collapse="");  # we added a character here ... "\n"
	if(ca == "lo") { all = tolower(all); }
	if(ca == "up") { all = toupper(all); }
	tmp = str.explode(" ", all);
	sp = length(tmp);
	all = paste0(tmp, collapse="");
	tmp = str.explode("", all);
	
	mytable = as.data.frame( table(tmp) );  ## currently factors
		colnames(mytable) = c("char", "count");
	nt = nrow(mytable);
	
	# add space back as row, this recast the data types
	row = c(space.char, sp); 
	mytable = df.addRow(mytable, row, "start");
	nt = nrow(mytable);
	
	# sort 
	sb  = prep.arg(sort.by, 1, keep = "-");
	te = str.explode("-", sb);
	so = te[1]; by = te[2]; if(is.na(by)) { by = ""; }

			
	if(so == "u" && by == "r")
		{
		mytable = mytable[ rev(1:nt), ]
		}
		
	if(so == "c")
		{
		dir = "DESC"; if(by == "r") { dir = "ASC"; }
		mytable = df.sortBy(mytable, "count", dir)
		}
		
	if(so == "a")
		{
		dir = "DESC"; if(by == "r") { dir = "ASC"; }
		mytable = df.sortBy(mytable, "char", dir)
		}
	
	if(horizontal)
		{
		# not a dataframe ... vector with names ...
		mykeys = as.character(mytable$char);
		myvals = as.integer(mytable$count);
		names(myvals) = mykeys;
		return(myvals);
		}
		
	mytable;
	}
	
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' str.splitN
#'
#'	
#------------------------------------------------# 
str.splitN = function(..., n=2, insert.a.sep="`c80^08c`", from="end")
	{
	str = prep.dots(...);
	FROM = prep.arg(from, n=3);
	# we will INTERNALLY use insert.a.sep to SPLIT the string 
	# THEREFORE, the string cannot contain it... maybe ^ or `^`
	s.test = str.contains(insert.a.sep, str);
	if( sum( s.test ) > 0) 
		{ 
		# all = paste0(str, collapse="\n"); 
		# all.f = str.characterFrequency(all);
		# print(all.f);
		stop("Your str has the [insert.a.sep] in it, maybe try a different one");
		}
	
	# str = c("monte says hi", "alex you're awesome", "mama is amazing")
	
	slen = str.len(str);
	if(FROM == "end")
		{
		# make certain it has even groups ...
		nlen = num.round(slen, by=n, how="ceiling");
		str = str.pad(str, nlen, side="LEFT");
		}
	
	pattern = paste0("(.{", n, "})");
	
	s.str = gsub(pattern, paste0("\\1", insert.a.sep), str);
	s.str = str.end(insert.a.sep, s.str, trim=TRUE);
	
	# we have to go from right/left (hex/binary padding) ... 
	# rev(str) ... do the above, rev(str) again ...
	
	res = check.list(str.explode(insert.a.sep, s.str));
	if(FROM == "end")
		{
		# truncate the padding placed at beginning ...
		rem = 1 + nlen - slen; 
		first = list.getElements(res, 1);
		first = substring(first, rem, n);
		res = list.setElements(res, 1, first);
		}
	
	# https://stackoverflow.com/a/26497699/184614
	# https://stackoverflow.com/a/24900744/184614
	
	list.return(res);
	}













#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
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
#------------------------------------------------#
str.tolower = function(str, method="cpp", locale="en_US.UTF-8")
	{
# l10n_info();      # NON-TRIVIAL
# Sys.getlocale();
# stri_trans_tolower(string, locale = locale)

	# necessary overhead
	m = prep.arg(method, 1);

	if(m == "s" && is.library_("stringi") )
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

#++++++++++++++++++++++++#
#'
#' @rdname strtolower
#' @export
strtolower = str.tolower;


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
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
#------------------------------------------------#
str.toupper = function(str, method="cpp", locale="en_US.UTF-8")
	{
	# necessary overhead
	m = prep.arg(method, 1);

	if(m == "s" && is.library_("stringi") )
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

#++++++++++++++++++++++++#
#'
#' @rdname strtoupper
#' @export
strtoupper = str.toupper;















#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' str.capitalizeFirst
#'
#'
# ucfirst ...
#------------------------------------------------#
str.capitalizeFirst = function(...) 
	{
	str = prep.dots(...);
	len.str = str.len(str);
	first = charAt(str, 1);
	first.uc = toupper(first);
	paste0( first.uc, substring(str, 2, len.str) );
	} 

#++++++++++++++++++++++++#
#'
#' @rdname ucfirst
#' @export
ucfirst = str.capitalizeFirst;


# ucwords(string $string, string $separators = " \t\r\n\f\v"): string
# ucwords — Uppercase the first character of each word in a string
# Returns a string with the first character of each word in string capitalized, if that character is alphabetic.
# str = c("monte says hi", " \t Alex \r \n says hello|world", " \t \f \v \r \r\n  \n alex | says \t\t hello|world\tnow");
#------------------------------------------------#
str.capitalizeWords = function(..., sep.any=" \t\r\n\f\v") 
	{
	str = prep.dots(...);
	ostr = str;  # original, copy ... help with matching `sep.any` on reversal
	seps = str.explode("", sep.any);
	# " " must be first in seps ...
	idx = which(seps == " ");
	# regarless of current pos, remove all, and put at front ...
	if(length(idx) > 0) { seps = seps[-c(idx)]; seps = c(" ", seps); }
	info = str.replace(seps, " ", ostr); # cast EVERYTHING as simple space 
	
	
	
	tmp = str.explode(" ", info);  # list.return on str.explode ... only 1 str 
	tmp = check.list(tmp);  # need to coerce to list 
	
	n = length(tmp);
	new = character(n);
	for(i in 1:n)
		{
		k = 0;
		res = tmp[[i]];		
		first = charAt(res, 1);
		first.uc = toupper(first);
		len.res = str.len(res);
		
		res = paste0( first.uc, substring(res, 2, len.res) );
		
		# I need to get strpos of all "weird" CHARS, map back from " "
		n.len = length(len.res); # how many keys 
		o = ostr[i];

		
		nstr = "";
		pos = 1;
		for(j in 1:n.len)
			{	
			nch = charAt(o, pos);
			mylen = len.res[j];
			if(mylen == 0) { nstr = paste0(nstr, nch); pos = 1 + pos; next; }
			# we have a strlen, which means a sep after ...  
			nstr = paste0(nstr, res[j]); pos = mylen + pos;
			if(j < n.len)
				{
				nch = charAt(o, pos);
				nstr = paste0(nstr, nch); pos = 1 + pos;
				}
# cat("\n j = ", j, " ====> "); print(nstr); cat("\n");
# stop("monte");
			}
# stop("monte");
		new[i] = nstr; 
		}
	new;
	} 


#++++++++++++++++++++++++#
#'
#' @rdname ucwords
#' @export
ucwords = str.capitalizeWords;





str.replaceN = function(search, replace, subject, method="base", times=1)
	{
	res = subject;
	for(i in 1:times)
		{
		res = str.replace(search, replace, res, method=method)
		}
	res;
	}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
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
#------------------------------------------------#
str.replaceFromList = function(mylist, mysubject, ...)
	{
	str.replace( names(mylist), mylist, mysubject);
	}

 
 

# str = c("monte says hi", "alex you're awesome", "mama is amazing")
str.letterReverse = function(strs)
	{
	strs = check.list(strs);
	ni = length(strs);
	res = vector("list", ni);
	for(j in 1:ni)
		{
		str = strs[j];
		n = length(str);
		out = character(n);
		for(i in 1:n)
			{
			out[i] = int.u8(rev(u8.int(str[i])));
			}
		res[[j]] = out;
		}
	list.return(res);
	}

# maybe speed up  
# https://www.r-bloggers.com/2019/05/four-ways-to-reverse-a-string-in-r/
# str = c("monte says hi", "alex you're awesome", "mama is amazing")
str.wordReverse = function(str, sep=" ")
	{  
	strs = check.list(str);  # multivariate 
	n = length(strs);
	out = vector("list", n);
	for(i in 1:n)
		{
		str = strs[[i]];
		info = check.list(str.explode(sep, str));
		nj = length(info);
		res = character(nj);
		for(j in 1:nj)
			{
			vr = rev(info[[j]]);	
			res[j] = paste0(vr, collapse=sep);
			}
		out[[i]] = res;
		}
	list.return(out);
	}
	
	


str.letterShuffle = function(str, sep="")
	{
	strs = check.list(str);  # multivariate 
	n = length(strs);
	out = vector("list", n);
	for(i in 1:n)
		{
		str = strs[[i]];
		info = check.list(str.explode(sep, str));
		nj = length(info);
		res = character(nj);
		seeds = seed.create(nj);
		for(j in 1:nj)
			{
			vs = v.shuffle(info[[j]], seed=seeds[j], append=FALSE);	
			res[j] = paste0(vs, collapse=sep);
			}
		res = property.set("seeds", res, seeds);
		out[[i]] = res;
		}
	list.return(out);
	}
	
str.wordShuffle = function(str, sep=" ")
	{
	strs = check.list(str);  # multivariate 
	n = length(strs);
	out = vector("list", n);
	for(i in 1:n)
		{ 
		str = strs[[i]];
		info = check.list(str.explode(sep, str));
		nj = length(info);
		res = character(nj);
		seeds = seed.create(nj);
		for(j in 1:nj)
			{
			vs = v.shuffle(info[[j]], seed=seeds[j], append=FALSE);	
			res[j] = paste0(vs, collapse=sep);
			}
		res = property.set("seeds", res, seeds);
		out[[i]] = res;
		}
	list.return(out);
	}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' str.removeWhiteSpace
#'
#'
#------------------------------------------------#
str.removeWhiteSpace = function() {}
str.removeWhiteSpace = function( str, replace=" ", n = 2,
								method = "base", 
								pattern = paste0("[[:space:]]{",n,",}"),
								pre.trim = TRUE, post.trim = TRUE, 
								...
								)  
  {
  METHOD = functions.cleanupKey(method, 1);
	if(pre.trim) { str = str.trim(str, ...); }
	# REQUIRES string?
	if(METHOD == "s" && is.library_("stringi"))
		{
		# p = "\\P{Wspace}";
		# p <- c("\\w", "\\d", "\\s")
		# structure(stri_extract_all_regex(x, p), names = p)
		regex.s = paste0("\\s{",n,",}");
		stringi::stri_replace_all_regex(str, regex.s, replace); 
		} else {
				# regex.s = paste0("[[:space:]]{",n,",}");
				regex.s = pattern;
				str = gsub( regex.s, replace, str );  # multivariate works
				}
	# likely not necessary, but may be an edge case out there
	if(post.trim) { str = str.trim(str, ...); }
  str;
  }


#++++++++++++++++++++++++#
#'
#' @rdname removeWhiteSpace
#' @export
removeWhiteSpace = str.removeWhiteSpace;



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' str.trimFromAny
#'
#'
# monte was here 
# str = c("\n monte \n", "# says ", "hi#", "## to Alex#");
# str.trimFromAny(str, search="#tx")
# str.trimFromAny(str, search="#tx\n", side="left")
#------------------------------------------------#
str.trimFromAny = function() {}
str.trimFromAny = function(str, search="#me", side="both")
	{
	search = as.character(search);
	if(search == "") { stop("you need to enter at least one character"); }
		SIDE = prep.strSide(side, 1);

	# let's explode on "" to get chars as list
	search = str.explode("", search); # turn into a set
	chars = check.list(str.explode("", str));
	n = length(chars);
	res = character(n);
	for(j in 1:n)
		{
		char = chars[[j]];
		from.left = NULL;
		from.right = NULL;
		
		nc = length(char);
		# maybe set.intersect? ... gives vals not idx 
		IDX = sort(unique(set.match(search,char)));
		# nothing to do 
		if(allNA(IDX) || length(IDX) == 0) { res[j] = str.implode("", char); next; }
		# will be at least length of search ...		
		IDX = IDX[ !is.na(IDX) ];  
		ilen = length(IDX);  
		if((SIDE=="left" || SIDE=="both") && IDX[1] == 1)
			{
			# walk until we are not contiguous ...
			for(i in 1:ilen)
				{
				if(IDX[i] == i) { from.left = c(from.left, i); } else { break; } 
				}			
			}
		if((SIDE=="right" || SIDE=="both") && IDX[ilen] == nc)
			{
			# walk until we are not contiguous ...
			for(i in ilen:1)
				{
				if(IDX[i] == (nc)) { from.right = c(nc, from.right); nc %--%.; } else { break; }
				}			
			}
		
		
		set = switch(SIDE,
						  "left"	= from.left,
						  "right" 	= from.right,
						  "both"  	= c(from.left, from.right),
					c(from.left, from.right)
					);
	  
		nchar = char;
		if(!is.null(set)) { nchar = char[-c(set)]; }

		res[j] = str.implode("", nchar);
		}
	res;
	}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' str.trimFromFixed
#'
#'
#------------------------------------------------#
str.trimFromFixed = function() {}
str.trimFromFixed = function(str, trim="#", side="both" )
	{
	# multiple see trimFromAny 
	# if there are a contiguous group "00001000" ... "0"
	s = prep.arg(side, 1);
	len.str = str.len(str);
	n.str = length(str);
	slen = str.len(trim);
	# if x is character vector, x[1][2] should return the charAt(x[1], 2)
	# likely the OLD SCHOOL LEGACY of multidimensional arrays?
	
	first = substring(str, 1, slen);
	last = substring(str, len.str-slen+1, len.str);
	
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
	stop = len.str;
	if( (s=="r" || s=="b") )
		{
		stop[right] = stop[right] - slen;
		}

	substring(str, start, stop);	
	}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' str.wordWrap
#'
#'
# https://www.php.net/manual/en/function.wordwrap.php
#------------------------------------------------#
str.wordWrap = function() {}
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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
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


str.pipeHeader = function() {}
str.pipeHeader = function(str="Welcome to the {humanVerse}", 
							width=72, 
							ctag="#",
							stag=ctag,
							above = 3,
							below = 3
							)
	{
	ctag = substring(ctag, 1, 1); # forced to length one ...
	str = as.character(str[1]);
		lines = str.explode("\n", str);
		n = length(lines);
	slen = str.len(lines);  
		# for centering 
		half = as.integer( (width-max(slen))/2);
		
		brand = "{humanVerse}"; blen = str.len(brand);					
	bline = paste0( str.rep(ctag, (width-blen-5)),
					brand,
					str.rep(ctag, 5) );
					
		rand = "{R}"; rlen = str.len(rand);	
	rline = paste0( str.rep(ctag, 3),
					rand,
					str.rep(ctag, (width-rlen-3)) );
	
	cline = str.rep(ctag, width);
		
	sline = stag;  # line with just a comment or whatever stag is ...
	
	out = character();
	idx = 1;
		out[idx] = bline; 	idx %++%.;
	for(i in idx:(idx+above)) { out[i] = sline; }
							idx %+=% above;
	for(i in 1:n)
		{		
		out[idx] = paste0(stag, str.rep(" ",half), lines[i]);
							idx %++%.;
		}
	for(i in idx:(idx+(below-1))) { out[i] = sline; }
							idx %+=% (below-1);
		out[idx] = ctag;	# below - 1... we hardcode one here ... 
							idx %++%.;
		out[idx] = rline;
	res = paste0(out, collapse="\n");
	res = property.set("more", res, list("rline" = rline, "cline" = cline, "bline" = bline));
	res;
	}
 
    
str.commentOneLine = function(str = "Start LOG file",
								width = 72,
								ctag = "#",
								htag = ".",
								ptag = " ",
								npad = 3,
								brand = "{humanVerse}",
								brand.dir = "right"
								)
	{
	str = str[1];
	slen = strlen(str);
	
	clen = strlen(ctag);
	pad = str.rep(ptag, npad);
	plen = strlen(pad);
	
	blen = strlen(brand);  # brand = "{R}" ... {humanVerse} ... left/right 
	
	bpad = str.rep(ctag, 3);
	
	tlen = clen + plen + slen + plen + clen +  ( 3*clen + blen + 3*clen );
	
	half = floor( (width-tlen)/2 );  if(half < 0) { half = 0; }
	# hpad = strrep_(ctag, to.length=half);
	hpad = strrep_(htag, half, TRUE);

	
	if(brand.dir == "right")
		{
		res = paste0( ctag, hpad, pad, str, pad, hpad, bpad, brand, bpad, ctag );
		res = substring(res, 1, width);
		res = str.pad(res, width, ctag, "RIGHT");
		}
	if(brand.dir == "left")
		{
		res = paste0( ctag, bpad, brand, bpad, hpad, pad, str, pad, hpad, ctag );
		res = substring(res, 1, width);
		res = str.pad(res, width, ctag, "LEFT");
		}	
	
	res;
	}
	
	 

#------------------------------------------------#
str.commentWrapper = function() {}   # what about \n in str for "blank vertical space"?
str.commentWrapper = function(str="Welcome to the {humanVerse}", 
									nchars=0, 
									ctag="#", 
									rtag=ctag, 
									stag=" ", spad=5, 
									itag=" ", ipad=15
							)
	{
	# punchcards had 80 characters, a traditional typewriter US had 72 characters per line (CPL)
	# http://mikeyanderson.com/optimal_characters_per_line
	# Quotes "Jakob Nielson" + 5 ... states 66 is optimal
	# 6.5 inches (1 inch margin) x 10 per inch ... about 65 ... we would do +/- 3 in typing class ... override end
	
	# once max is set, build a basic "empty" for "\n" ... copy it ...
	# if str.trim(str[i]) == "" ... do empty row ... 
	n = length(str);  lenstr = str.len(str);  maxlen = max(lenstr);
		if(nchars > 0 && maxlen > nchars) { nchars = maxlen; }
		if(nchars == 0) { nchars = maxlen; } 
	res = character(n);
	mylengths = integer(n);
		istr = str.repeat(itag, ipad); ilen = strlen(istr);
		sstr = str.repeat(stag, spad); slen = strlen(sstr); 
											clen = strlen(ctag);
											rlen = strlen(rtag);
	
	
	
	
	for(i in 1:n)
		{
		tlen = (ilen + slen + clen + rlen);
		tmp.len = tlen + lenstr[i];
		d.len = nchars - tlen;
		if(nchars > 0) { if( tmp.len < nchars) { str[i] = str.pad(str[i], d.len, " ", "BOTH"); } }
		s = paste0(		istr, 
						ctag, 
						rtag, 
						sstr, 
					str[i], 
						sstr, 
						rtag, 
						ctag
					);
		slen = strlen(s);
		if(slen < nchars)
			{
			ntag = 1 + ceiling( (nchars - slen)/2 );
				nstr = str.repeat(stag, ntag);
			s = paste0(		istr,
							ctag, 
							rtag, 
							nstr, 
						str[i], 
							nstr,
							rtag, 
							ctag
						);
			slen = strlen(s);
			}
		nchars = slen; # first line will dictacte the others ...
			fn = slen - 2*strlen(spad) - ilen - strlen(ipad);
			fstr = str.repeat(rtag, fn);
			
		if(i == 1) 
			{ 
			cline = paste0(istr, ctag, rtag, fstr, rtag, ctag, "\n");
			res[i] = paste0(cline, s, "\n");
			} else if (i == n)
							{
							res[i] = paste0(s, "\n", cline);
							} else {
									res[i] = paste0(s, "\n");
									}
		mylengths[i] = slen;
		#mylengths[i] = strlen(res[i]);
		}
	
	res = property.set("lengths", res,  mylengths);
	res = property.set("indent", res, ilen);
	res;
	}












#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' str.push_back
#'
#'
## is this stringr::str_c ??
## C++ ... obj.push_back(element) ... element, obj
#------------------------------------------------#
str.push_back = function(sub, str, collapse="")
	{
	paste0(str, sub, collapse=collapse);
	}

#++++++++++++++++++++++++#
#'
#' @rdname str.push_last
#' @export
str.push_last = str.push_back;

#++++++++++++++++++++++++#
#'
#' @rdname str.push_end
#' @export
str.push_end = str.push_back;

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' str.push_front
#'
#'
#------------------------------------------------#
str.push_front = function(sub, str, collapse="")
	{
	paste0(sub, str, collapse=collapse);
	}

#++++++++++++++++++++++++#
#'
#' @rdname str.push_first
#' @export
str.push_first = str.push_front;


#++++++++++++++++++++++++#
#'
#' @rdname str.push_begin
#' @export
str.push_begin = str.push_front;















if(TRUE)  # set to TRUE or FALSE 
{xela = ' % we are inside a <s>ingle <q>uote ... don<sq>t use them!
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/*
*|  ASCII ART HERE WOULD BE FUN.
*|			You CAN use ..., try <dots> or <dots /> or `...` to specify
*|				===> that you mean the functional dots <dots> in `R`
*|			If you meant ellipses, try latex form <ldots> or <ldots />
*|			BASE LATEX will be able to parse, so think like latex
*|
*|			<a href="https://www.atqed.com/latex-dot" target="_blank">MORE DOTS</a>
*|			Two tagged Latex commands would need more data passed
*|
*|			<sq> as [s]ingle [q]uote ... when in doubt, use <sq>
*|			<dq> as [d]ouble [q]uote ... when in doubt, use <dq>
*|			<b>[b]old text</b> ... <i>[i]talicized (emph) text</i>
*|			<u>[u]nderlined text</u> ... 
*|			<element fgcolor="#FFF099" bgcolor="red">WHATEVER</element>
*|			<element></element> means a relevant <sq>variable</sq> and 
*|				will work inside <b> for example
*|			<b fgcolor="green" bgcolor="pink">my text </b>
*|			COLORS allowed are ANY HEX and NAMES from `grDevices::colors()`
^|	
!
% still works.  Don<sq> USE <tick> or <tick3> maybe <3tick>, they will confuse MY parser
&
()		
*|			Think HTML v. 1995-ish, FRAMES just comming online
*|			<a href="https://en.wikipedia.org/wiki/Standard_Generalized_Markup_Language">SGML</a>
*|			This is loosely typed <SGML />
*|			<br> or <br /> or <BR> or <BR /> will all work [force new line, ala `\n`]
*|			You can also use <n> or <n /> for a new line, <t> or <t /> for tabs 
*|			
*/

I didn<sq>t need to do those funny *| characters on the left ... just to demo
how annoying they are.  I can write a function that will scan the file before
R tries and parse it and cleanup <sq> <dq> <n> <t>

BIBTEX::: 
Place them anywhere ... you will need a key defined for it to work
Parser would loop through everything to get all the needed keys, 
replacing <citep>Shaffer:1999</citep> with <bs>citep{Shaffer:1999}

NOTICE the \n is an allowed escape character but <bs>citep{Shaffer:1999}
as "c" is not allowed to be escaped by R CLI parser ...

[b]ack [s]lash, not [B]ul [S]hita {HEBREW REFERENCE}

So <bs> means [b]ack [s]lash
So <fs> means [f]orward [s]lash

<ss> as [s]ingle [s]pace 
<ds> as [d]ouble [s]pace 

I can autogenreate at the <family> level and at an overall <index> level


<bibtex key="Shaffer:1999">
COPY a bibtex entry here, no single quotes, replace with <sq> or <sq/>
</bitext>

<citet>Shaffer:1999</citet>
<citep>Shaffer:1999</citep>


```
# CSS3 named colors, BREWER colors, all part of RGB(a) specification 
# end.the = "#ffeedd"; 
# See: https://www.unm.edu/~tbeach/IT145/color.html
html.colors = c("black", "white", "gray", "silver", 
				"maroon", "red", "purple", "fushsia", 
				"green", "lime", "olive", "yellow", 
				"navy", "blue", "teal", "aqua"
				);
```

Besides the single tick `operator` <tick>operator<tick> 
and three-tick version <tick3>code, multilined if you want</tick3> ... maybe <tickn>?


Maybe allow doxygen type syntax intermixed, as desired ... just put without
#<sq> on LHS ... 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		WELCOME to the humanVerse 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%% below is an example ... I looked at Rprofmem.Rd to "invent" these tags.

<name>Rprofmem</name>
	<alias>Rprofmem</alias>
	<alias>RprofmemALIAS2</alias>
	<alias>RprofmemALIAS3</alias>

% The stuff can go in any order, I will be looking.  I will only parse stuff
# inside a <element>WHATEVER</element> tag, so all this extra commentary just 
# disappears, a lot like my `=` and `:` 
# good thing I have ```property.get("srcref", ping.domain); ``` 

% TBD if I can pass colors into the FINAL FILE ...
<title bgcolor="#D5D5D5">Enable Profiling of R<sq>s Memory Use</title>

% You can intentionally go multiline to make it more readible for you.
<description>
	Enable or disable reporting of memory allocation in R.
</description

% UNBELIEVABLE, WTF!  Only Micro$oft is using XML-like documntation tags.

<usage>
	Rprofmem( filename = "Rprofmem.out", 
						append = FALSE, 
							threshold = 0
			)
</usage>


<return>[empty]</return> ... maps to "value"


% Yes, indeed, where is the arguments function in R?  If only, if only.
% notice I am recycling <name> and <description> ... nested, == OKEE - DOKEE
<arguments>
	<argument>
		<name>filename</name>
		
		<type>character (string)</type>
		
		<description>
				The file to be used for recording the memory allocations.
		</description>
		
		<default>[missing]</default>  # or I could just not include this tag ... 
		
		% you don<sq>t nest <code></code> inside of <option = ></option>, so `single tick`
		% you could put singletons inside like <sq>
		<option = `NULL`, `""`> To disable reporting [to turn off].  That is,
			the filename turns reporting [on]			
		</option>

		
	</argument>
	
	<argument>
		<name>append</name>
		<type>logical (boolean)</type>
		
		<description>
				Should the file be over-written or appended to?
		</description>
		
		<option = `TRUE`, `FALSE`> 
			[T]RUE to append; 
			[F]ALSE to overwrite		
		</option>
		
		% instead of above, I could do multiple options ...
		%% do the ABOVE or BELOW, as you wish ... 
		%% although inside <lt><bs>argument<gt> ... notice codes
		%% comments can be scattered and WON<sq> render ... 
		%% you can be as VERSOBE as you WANT ... 
		%% escape chars ... https://mateam.net/html-escape-characters/
		%% <lt> as &lt; ... <gt> as &gt;
		%% maybe allow all the codes, UTF that is ...
		
		<option = `TRUE`> 
			[T]RUE to append;		
		</option>
		
		<option = `FALSE`> 
			[F]ALSE to overwrite;		
		</option>
		
	</argument>
	
	
	% VERBOSITY is HIGH, but we can HIDE these lines easily in NOTEPAD++
	% YOU could easily select everything herein, do FIND/REPLACE on <sq>
	<argument>
		<name>threshold</name>
		
		<type>double (numeric)</type>  % maybe try and be precise ... 
		
		<description>
				Allocations on R<sq>s "large vector" heap larger than this number of bytes will be reported.
		</description>
		
		
	</argument>
	
</arguments>


%% will treat `verbatim` but tags are allowed, HTML tables are allowed ...
%% IMAGES are allowed, maybe <figure> to capture <html> and <latex> variants
%% input TABLE as HTML or as TABULAR ... convert back and forth ...
%% both <code>malloc</code> and `malloc` will be rendered the same.

%% notice, no comments inside of details ... it is the outer layer of SGML
<details>
  Enabling profiling automatically disables any existing profiling to
  another or the same file. 

  Profiling writes the call stack to the specified file every time
  <code>malloc</code> is called to allocate a large vector object or to
  allocate a page of memory for small objects. The size of a page of
  memory and the size above which `malloc` is used for vectors are
  compile-time constants, by default 2000 and 128 bytes respectively.

  The profiler tracks allocations, some of which will be to previously
  used memory and will not increase the total memory use of R.
</details>

%% [b]ack [s]lash R was breaking, so <R> ... 
<note>
  The memory profiler slows down R even when not in use, and so is a
  compile-time option.
  (It is enabled in a standard Windows build of <R>.)

  The memory profiler can be used at the same time as other <R> and C profilers.
</note>




%% <iref><code> ... <code><iref> would parse equivalent, no?
%% internal reference <iref>
<see.also>
  The R sampling profiler, <iref><code>Rprof</code></iref> also collects
  memory information.

  <iref><code>tracemem</code></iref> 

  The "Writing R Extensions" manual section on "Tidying and profiling R code"
</see.also>

%% ESCAPING <bs>frac ... joke !
<equation label="named:3">
	p(x) = <bs>frac{<bs>lambda^x e^{-<bs>lambda}}{x!} 	
	<text this="is.optional"> TEXT FORM of EQUATION ...  <bs>lambda^x exp(-<bs>lambda)/x! </text>
</equation>

Here is some text with inline <equation>x = 0, 1, 2, <bs>ldots</equation> or <equation>x = 0, 1, 2, <ldots></equation>


%% monte is here ... 
<example>
	print("hello world, run this part of this example.  multiple <example>");
	<do.not.run>
		## not supported unless R is compiled to support it.
		Rprofmem("Rprofmem.out", threshold = 1000)
		example(glm)
		Rprofmem(NULL)
		noquote(readLines("Rprofmem.out", n = 5))
	</do.not.run>
</example>


<keywords sep=";">
	utilities; two words; three words, with a comma; and so on ... 
<keywords>


% maybe write a reverse-parser 	... take current #<sq> @param <bs> and reverse
								... take current .Rd file and do the same


parse.fromRd
parse.toRd
parse.fromTidy
parse.toTidy

% auto-generate like CNTRL-SHIFT-L ... maybe source from .sgml external (if desired)...


# I could use a scan file and verify , get start/end lines ... str.contains(<sq>)
whatever I want except for single  .. # lksdjf lkj

------------------------------------------------*/
'}


		
strtrim_ = function(str, side="both", pattern=NULL)
	{
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.SIDE = check.type(side);
	if(!ct.SIDE || !is.character(side))	
		{ side = deparse(substitute(side)); } 
##########################################################

	SIDE = prep.strSide(side, n=1);	
	# is this faster than base::trimws? ... no collapsing?
	g = "\\s+";
	if(!is.null(pattern)) { g = pattern; }
	res = switch(SIDE,
						  "left" 	= gsub( paste0("^",g), "", str),
						  "right" 	= gsub( paste0(g,"$"), "", str),
						  "both"  	= gsub( paste0("^",g,"|",g,"$"), "", str),
					gsub( paste0("^",g,"|",g,"$"), "", str)
				);
    res;
	}
	
		
strpad_ = function() {}
strpad_ = function(str, 
					to.length	= max(str.len(str)),
					padding		= "0", 
					side		= "right"  # default is for NNN.dd00 decimal
					)
	{
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.SIDE = check.type(side);
	if(!ct.SIDE || !is.character(side))	
		{ side = deparse(substitute(side)); } 
##########################################################
	str = as.character(str);
	SIDE = prep.strSide(side, n=1);
	ns = str.len(str);
	rs = to.length - ns;  	# how many pads per element 
	n = length(str); 		# how many strings
	res = character(n);
	
	for(i in 1:n)
		{
		myr = rs[i];
		pads = str.repeat(padding, myr); 
		if(SIDE == "both")
			{
			myr_right	= ceiling(myr / 2);
			pad_right	= str.repeat(padding, ( myr_right )	);
			pad_left	= str.repeat(padding, ( myr - myr_right )	);
			}
		
		# if padding is multiple length, may be too long
		res[i] = switch(SIDE,
						  "left"	= paste0(paste( pads , collapse=""), str[i]),
						  "right" 	= paste0(str[i], paste( pads , collapse="")),
						  "both"  	= paste0(paste( pad_left , collapse=""), str[i], 
											paste( pad_right , collapse="")),
					paste0(paste( pad_left , collapse=""), str[i], 
											paste( pad_right , collapse=""))
					);
					
		}
	res;
	}



strreplace_ = function(search, replace, subject)
	{
debug = FALSE;
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

strrep_ = function(str, times=1, as.lengthout=FALSE)
	{
	n = length(str);
	res = character(n);
	for(i in 1:n)
		{
		if(as.lengthout)
			{
			res[i] = paste( rep(str, length.out = times), collapse="");
			} else {
					res[i] = paste( rep(str, times=times), collapse="");
					}
		}
	res;	
	}

strunsplit_ = function(str, sep)
	{
	str = check.list(str);
	n = length(str); # is this a vector or list ? 
	res = character(n);
	for(i in 1:n)
		{
		res[i] = paste0(str[[i]], collapse = sep);
		}
	res;
	}


# hack base to deal with EOS issue 
strsplit_ = function(str, sep, fixed=TRUE, ...)
	{
	hasResult = FALSE;

	if(!hasResult && sep == "")  
		{
		hasResult = TRUE;
		res = strsplit(str, sep, fixed=fixed, ...);
		}

	if(!hasResult)
		{
		end = str.end_(sep, str);
		if(allFALSE(end)) 
			{ 
			hasResult = TRUE;
			res = strsplit(str, sep, fixed=fixed, ...);
			}		
		}
	
	if(!hasResult)	
		{
		hasResult = TRUE;
		# stringi works as expected, what about cpp?
		# if "<i>humanVerse</i>" ... 
			# "<i>" returns "" "humanVerse</i>"
			# "</i>" returns "<i>humanVerse" without trailing "" 
			# SO ... it's a feature ... 
		# is separator at END? 
		# good = !end; bad = end;
			fill = "~"; if(sep == "~") { fill = "^"; }
			tmp = paste0(str,fill);
		tres = strsplit(tmp, sep, fixed=TRUE);
		tres = check.list(tres);		
		res = list.removeFillFromEnd(tres, fill=fill);
		}

	res;
	}


# recursion-proof for strsplit_
str.end_ = function(search="</i>", str="<i>hello friend</i>", trim = FALSE )
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
	
