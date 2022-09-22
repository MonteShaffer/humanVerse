
 

parse.syscall = function(syscall)
	{
	# none of these functions can have dots (...)  ::: prep.dots(...)
	str = lang2str(syscall);
 dput(str);  # stop("monte");
	info = strsplit(str, "(", fixed=TRUE)[[1]];
	fn = trimws(info[1], which="both");			
			
	# put everything back but the function call 
	nstr = str.implode("(", info[-c(1)] );
	#nstr = str.replace(")", "", nstr);
	nstr = str.end(")", nstr, trim=TRUE);
	
	ninfo = strsplit(nstr, ",", fixed=TRUE);
	minfo = strsplit(ninfo[[1]], "=", fixed=TRUE);
	
	pkeys = str.replace('"', "", list.getElements(minfo, 1) );
	pkeys = trimws(pkeys, which="both");
	
	pvals = list.getElements(minfo, 2);
	
	

	
	f = as.list(formals(fn));
	keys = names(f);
	form = list();		# formals, nicely
	params = list();	# parameters with values
	map = list();		# map from paramName to symbol user inputed 
	fkeys = NULL;		# above may be out of order ... 
	n = length(keys);
	pskip = 0;
	for(i in 1:n)
		{		
		key = keys[i];
		val = f[[key]];		ct.VAL = check.type(val);
		if(!ct.VAL) { val = "--EMPTY--"; }
		form[[key]] = val;
		}
		

	
	withV = v.which(pvals, NA, invert=TRUE);

# if(fn == "v.chain") { print(str(form)); print(withV); }
	
	for(i in 1:n)
		{		
		key = keys[i];
		pidx = i + pskip;
		pkey = fkeys = pkeys[pidx]; 
		if(key != "...")
			{
			pval = pvals[pidx]; 
			
# if(fn == "v.chain") { cat("\n\n key: ", key, " \t pidx :", pidx, " \t pkey :", pkey, " \t pval :", pval, "\n\n"); 	}
	
			if(!is.na(pval))
				{
				params[[ pkey ]] = eval(parse(text=pval));
				}
			}
		if(key == "...")
			{			
			nkey = keys[i+1]; if(is.na(nkey)) { nkey = "kdsfjklsdj093-"; }
			if(nkey != pkey) { fkeys = pkey; } else { stop("bad keys"); }
			np = length(pkeys); # maximum search 
			j = pidx;
			while(j <= np)
				{
				pskip %++%.
				j = pidx = i + pskip;
				pkey = pkeys[pidx];
				if(!is.null(withV) && withV[1] == pidx) { pskip %--%. ; break; }
				if(is.na(pkey)) { pskip %--%. ; break; }  # out pf pkeys ... 
				if(nkey != pkey) { fkeys = c(fkeys, pkey); } else { pskip %--%. ; break; } 
				}
			}
		map[[key]] = fkeys; 
		}

#dput(map);

	fkeys = NULL;
	if(!is.null(map[["..."]])) { fkeys = map[["..."]]; }
	# trimws collapses list, strsplit DOESN'T collapse list 
	# ARGH!?DSM
	# trimws(strsplit(ninfo, "=", fixed=TRUE), which="both");
	
	res = list(
				"fn" = fn, 
				"dot.keys"  = fkeys,
				"params" = params, 
				"map" = map, 
				"formals" = form
				);

# if(fn == "v.chain") { print(str(res)); }
	res;
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

#dput(fn);	
#dput(finfo);
#stop("monte");	
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






prep.switch = function(THING="both", keys=c("l","r","b"), vals=c("left", "right", "both"), default = "both")
	{
	# THING has been cleansed as string already
	newthing = NULL;
	n = length(keys);  # these need to be equal length .... 
	for(i in 1:n)
		{
		if(keys[i] == THING ) { newthing = vals[i]; break; }		
		}
	if(is.null(newthing)) { newthing = default; }
	newthing;	
	}
	
	
prep.strSide = function(side="both", n=1, ... , default="both", keys=NULL, vals=NULL)
	{
	side = check.string(side);		
	SIDE = prep.arg(side, n=n, ...);
		
	if(is.null(keys)) { keys = c("l","r","b"); }
	if(is.null(vals)) { vals = c("left", "right", "both"); }
	
	prep.switch(SIDE, keys, vals, default);
	}


	
prep.strMethod = function(method="first", n=1, ... , default="base", keys=NULL, vals=NULL)
	{
	method = check.string(method);		
	METHOD = prep.arg(method, n=n, ...);

	if(is.null(keys)) { keys = c("f","c","s","b", "t"); }
	if(is.null(vals)) {	vals = c("first", "cpp", "stringi", "base", "trimws"); }

	
	prep.switch(METHOD, keys, vals, default);
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

