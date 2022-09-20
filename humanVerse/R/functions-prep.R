
 

parse.syscall = function(syscall)
	{
	# none of these functions can have dots (...)  ::: prep.dots(...)
	str = lang2str(syscall);
dput(str);  stop("monte");
	info = strsplit(str, "(", fixed=TRUE)[[1]];
	fn = trimws(info[1], which="both");			
			
	# put everything back but the function call 
	nstr = str.implode("(", info[-c(1)] );
	nstr = str.replace(")", "", nstr);
	
	ninfo = strsplit(nstr, ",", fixed=TRUE);
	minfo = strsplit(ninfo[[1]], "=", fixed=TRUE);
	
	pkeys = str.replace('"', "", list.getElements(minfo, 1) );
	pkeys = trimws(pkeys, which="both");
	pvals = list.getElements(minfo, 2);
			
			
			
			
			
	fkeys = NULL;  # final keys are not in parameters ... just dots 
	n = length(pkeys);
cat("\n", "pkeys: ", pkeys, "\t keys:", keys, "\n\n");
	params = list();
	map = list();
	if(n > 0)
		{
		has.dots = FALSE;
		key = nkey = "-EMPDKJTY-";
		kstart = knext = NULL;
		for(i in 1:n)
			{
			pkey = pkeys[i];
			pval = pvals[i];
print(map);
cat("\n", "i ==> ", i, "\t\t", "pkey: ", pkey, "\t pval:", pval, "\t kstart:", as.character(kstart), "\t knext:", as.character(knext), "\t nkey:", nkey, "\t key:", key, "\n\n");			
			if(is.null(knext)) { key = keys[i]; } else { key = keys[knext]; knext = 1 + knext; }
cat("\n", "i ==> ", i, "\t\t", "pkey: ", pkey, "\t pval:", pval, "\t kstart:", as.character(kstart), "\t knext:", as.character(knext), "\t nkey:", nkey, "\t key:", key, "\n\n");
			if(key == "...") { has.dots = TRUE; kstart = i;}
			# if(key == nkey)  { cat("\n\n MONTE \n\n"); has.dots = FALSE; knext = i;}
			
			if(!is.null(knext)) { key = keys[kstart + 1]; }
			# mapping keys to pkey/fkeys ...
cat("\n", "i ==> ", i, "\t\t", "pkey: ", pkey, "\t pval:", pval, "\t kstart:", as.character(kstart), "\t knext:", as.character(knext), "\t nkey:", nkey, "\t key:", key, "\n\n");
		

			if(!has.dots) 
				{ 
				map[[key]] = pkey; 
				}
			
			if(!is.na(pval))
				{
				params[[ pkey ]] = eval(parse(text=pval));
				} else { 
						if(has.dots)
							{
							fkeys = c(fkeys, pkey);
							nkey = keys[kstart + 1]; 
							if(is.na(nkey)) { nkey = "-EMPDKJTY-"; } else { knext = 1 + kstart; }
							map[[key]] = fkeys;
							}
						
						}
			}
		}		
	



	
	f = as.list(formals(fn));
	keys = names(f);
	params = list();
	map = list();		# f$param => user.entered
	form = list();		# formals, nicely
	nf = length(f);
		dlen = length(fkeys);
		dskip = 0;
	for(i in 1:nf)
		{
		# defaults ... 
		key = keys[i];
		val = f[[key]];		ct.VAL = check.type(val);
		if(!ct.VAL) { val = ""; }
		form[[key]] = val;
		
		j = dskip + i;
		pkey = pkeys[j];
		pval = pvals[j];
		map[[key]] = pkey;
		if(!is.na(pval))
				{
				params[[ pkey ]] = eval(parse(text=pval));
				} else { fkeys = c(fkeys, pkey); }
		
		
		}
	
	# trimws collapses list, strsplit DOESN'T collapse list 
	# ARGH!?DSM
	# trimws(strsplit(ninfo, "=", fixed=TRUE), which="both");

		
	missing = length(keys) - length(pkeys); 
	
	list(
		"fn" = fn, 
		"dot.keys"  = fkeys,
		"params" = params, 
		"missing" = missing, 
		"formals" = form
		);
	}
  
  

prep.dots = function(..., 
						collapse 	= TRUE, 
						default 	= NULL, 
						append.info = TRUE, 
						has.objects = FALSE,
						by="column"
					)
	{
	dots = NULL;
	if(is.null(dots) && collapse) 
		{ 		
		dots = unlist(list(...));
		if(length(dots) == 0 && !is.null(default)) { dots = default; }
		return( dots ); 
		}
 
	BY = prep.arg(by, n=3);  # don't know if we still need this 
		
	## EQUIV:: # # parent.call = sys.call(sys.nframe() - 1L);
	fn = sys.calls()[[sys.nframe()-1]]; 
	finfo = parse.syscall(fn);
	
# dput(finfo);
# stop("monte");	
	if(is.null(dots) && has.objects) 
		{
		dots = finfo$dot.keys; # just strings 
		}
		
# dput(dots);
# stop("monte");

	if(is.null(dots))
		{
		dots = list(...);
		names(dots) = finfo$dot.keys;

		# let's flatten to one set of lists 
		res = list();
		n = length(dots);
		if(n > 0)
			{
			for(i in 1:n)
				{
				dot = dots[[i]];
				dname = paste0(finfo$dot.keys[i],".",i); # keep unique ...
				
				# we are treating multi-dimension (matrix/df) as by.column 
				if(is.dataframe(dot) || is.matrix(dot))
					{
					nd = dim(dot)[2];
					for(j in 1:nd)
						{
						dcol = dot[, j];
						dcoln = colnames(dot)[j];
						
						ddname = paste0(dname,".",dcoln,".",j); # keep unique
						
						res[[ddname]] = dcol;
						}
					} else { res[[dname]] = dot; }
				}
			}
# dput(dots);
# dput(res);
		dots = res;
		}


	if(length(dots) == 0 && !is.null(default)) { dots = default; }
	
	if(append.info) { dots = property.set("fn.info", dots, finfo); } 	
	dots;
	}





params = function(...)
	{
	x = prep.dots(..., as="character");
	
	}


# if NEW exists, it overrides the DEFAULT (cascade)
map.args = function(DEFAULT, NEW)
	{
	keys = names(DEFAULT);
	n = length(keys);
	for(i in 1:n)
		{
		key = keys[i];
		# exists or is.null(NEW[[key]]) ???
		# if( exists(key, NEW) ) { DEFAULT[[key]] = NEW[[key]]; }
		if( !is.null(NEW[[key]]) ) { DEFAULT[[key]] = NEW[[key]]; }
		}
	DEFAULT;  # updated 
	}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' prep.arg
#'
#' This INTENTIONALLY uses internal base R to prevent 'recursion'
#'
#' @param key (what is the character string to be keyed)
#' @param n=1 (how many characters of key to return)
#'
#' @return Updated key
#' @export
#' 
#' @examples

# extra = "! #"



prep.case = function(key, case="lower")
	{
	CASE = substring(tolower(case), 1, 3);
	str  = switch(CASE,					
					  "low"	= tolower(key),		# lowercase
					  "upp" = toupper(key),		# uppercase 
				key								# DEFAULT [as-is]
				);	
	str;	
	}
	
prep.arg = function(key, n=1, keep="", case = "lower", extra = "")
	{  
	# str = str.toCase(key, case=case); # recursion issue here ... HARD CODE for this function only 
	 
	str = prep.case(key,case=case);
	

#dput(key); 
#dput(str);   
#dput(extra); 
	if(extra != "")
		{
		n = nchar(extra);  # nchars 
		extra_ = strsplit(extra, "", fixed=TRUE)[[1]];
		res = str;
		for(i in 1:n)
			{			
			res = gsub(extra_[i], "", res, fixed=TRUE);
			}
		str = res;
		}
	if(keep != "")
		{
#dput(keep);
#dput(str);  
		tmp = strsplit(str, keep, fixed=TRUE)[[1]];
		res = paste0( substring(tmp, 1, n), collapse=keep);
		return(v.return(res));
		}
	
	v.return(substring(str, 1, n));
	} 


arg.prep = prep.arg; 

#' @rdname functions.cleanupKey
#' @export
functions.cleanupKey = prep.arg;
functions.cleanKey = prep.arg;

#' @rdname functions.cleanUpKey
#' @export
functions.cleanUpKey = prep.arg;

