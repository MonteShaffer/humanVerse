

# was wrap.lang 
prep.msg = function(...,  type="msg", out="paste0", sep=" ")
	{	
	str = prep.dots(...);
	str = check.ansi(str);
	str = check.lang(str);
		# append res = property.set("msg.type", res, "message or notice or warning 3 or error");

	if(is.null(out)) { return(str); } # do nothing ...
	# out is a string ... one level deep
	fn.str = as.character(substitute(out));
	fn.obj = function.find(fn.str, character.only=TRUE);
	if(is.null(fn.obj)) { return(str); }
# dput( as.character(substitute(out)) );
	# find.function(out) ... 
	# res = paste0(str, collapse=sep);
	# do.call("paste0", list(str, collapse=""))
	# other functions may have different parameters ... 
	do.call(fn.str, list(str, collapse=sep));
	}
	 

parse.syscall = function(syscall, pf=NULL)
	{
debug = TRUE;
 
	str 	= paste0(lang2str(syscall), collapse=""); 
	
	info	= str.explode("(", str);
	fn		= str.trim(info[1]);

debug = (fn == "ini.parseFiles");
.cat("debug ", debug, " for fn : ", fn); 

	has.dots = FALSE;
	if(!is.null(pf))
		{
		nms = ls(envir = parent.frame(2L), all.names = TRUE, sorted = FALSE);
		
		# nms = ls(envir = pf, all.names = TRUE, sorted = FALSE);
		not = setdiff(nms, "...");   # not dots 			
		if("..." %in% nms) { has.dots = TRUE; }
			# eval(parse(text=not[i]), envir = pf);
			# eval(quote(list(...)), envir = pf);
if(debug)
	{
	# fno = match.fun(fn); 
dput(fn);
	# mc = match.call(fno, call = sys.call(sys.parent(2L)), envir=pf);
# dput(mc);
.cat(" ... NAMES ... "); 
dput(nms);
dput(has.dots);
dput(not);
	} 
		}

	nstr 	= str.implode("(", info[-c(1)] );
	nstr 	= str.ends(")", nstr, trim=TRUE);

	# this could be a problem on a nested list with commas ... 
	ninfo = check.list(str.explode("," , nstr));
	minfo = str.explode("=", ninfo[[1]]);
	
	pkeys = str.trim( 
				str.replace('"', "", 
					list.getElements(minfo, 1) ) );

	# pvals = list.getElements(minfo, 2); # maybe not strings?
	
	# NA if not a match to the pkeys before ... no value 
	pvals = str.trim( 
				str.replace('"', "", 
					list.getElements(minfo, 2) ) );

	
	dot.vals = NULL;
	
	
if(debug)
	{
.cat("pkeys: ", pkeys, "\t pvals: ", pvals);
	}
	
	f = as.list(formals(fn));
	keys = names(f);
	form = list();		# formals, nicely
	params = list();	# parameters with values
	map = list();		# map from paramName to symbol user inputed 
	fkeys = NULL;		# above may be out of order ... 
	n = length(keys);
	pskip = 0;
	Types = character(n);
	for(i in 1:n)
		{		
		key = keys[i];
		val = f[[key]];		
			ct.VAL = check.type(val);
		if(!ct.VAL) { val = "--EMPTY--"; Types[i] = "";} 
				else {Types[i] = property.get("typeof", ct.VAL);}
			
		form[[key]] = val;
		}
		
if(debug)
	{
.cat("form: ", form, "\n\n\t Types: ", Types);
	}


	
	withV = v.which(pvals, NA, invert=TRUE);

	# loop over "formals" keys ... expand dots 
	dot.data = list();
	for(i in 1:n)
		{		
		Type 	= Types[i];
		key 	= keys[i];
		pidx 	= i + pskip;
		pkey 	= fkeys = pkeys[pidx]; 
		if(is.na(fkeys)) { fkeys = NULL; }
		if(key != "...")
			{
			pval = pvals[pidx]; 
			  
if(debug) 
	{ 
.cat(" ===> ???", "\t key: ", key, "\t pkey: ", pkey, "\t pval: ", pval, "\t form[[key]]:", form[[key]]);	
	}
	  
			
			logic = (key == pkey);
			idx = v.which(not, pkey);	
			# use.cache = use.cache 
			hasValue = FALSE;
			if(!hasValue && logic && !is.null(idx))
				{
				test = eval(parse(text=not[idx]), envir=pf);
				
if(debug)
	{
.cat(" ===> 1", "\t key: ", key, "\t pkey: ", pkey, "\t pval: ", pval, "\t form[[key]]:", form[[key]]);
.cat(test);
	}
				params[[ key ]] = test;
				hasValue = TRUE;
				}
				
			if(!hasValue && logic && !is.na(pval))
				{
				test = eval(parse(text=pval));
				if(Type != "") { test = as.type(test, type=Type);}

if(debug)
	{
.cat(" ===> 2", "\t key: ", key, "\t pkey: ", pkey, "\t pval: ", pval, "\t form[[key]]:", form[[key]]);
.cat(test); 
	}
				if(!.anyNA(test)) { params[[ key ]] = test; }
				hasValue = TRUE;
				}
				
			if(!hasValue && (logic || is.na(pval)) )
				{
				test = eval(parse(text=pkey));
				if(Type != "") { test = as.type(test, type=Type);}
if(debug)
	{ 
.cat(" ===> 3", "\t key: ", key, "\t pkey: ", pkey, "\t pval: ", pval, "\t form[[key]]:", form[[key]]);
.cat(test);
	}
				if(!.anyNA(test)) { params[[ key ]] = test; }
				hasValue = TRUE;
				}
		


				# it is one of the ... dots 
				if(!hasValue) { pskip %--%. ; next; }
				
			}
		if(key == "...")
			{			
			nkey = keys[i+1]; 
if(debug)
	{ 
.cat("key: ", key, " \t pidx :", pidx, " \t pkey :", pkey, " \t pval :", pval, " \t nkey :", nkey);
 	}
				if(is.na(nkey)) { nkey = "kdsfjklsdj093-"; }
			if(!is.na(pkey))  # na if nothing inside 
				{
				if(nkey != pkey) 
					{ 
					fkeys = pkey; 
					dot.vals = c(dot.vals, eval(pkey));
					} else { stop("bad keys"); }
				np = length(pkeys); # maximum search 
				j = pidx;
				while(j <= np)
					{
					pskip %++%.
					j = pidx = i + pskip;
					pkey = pkeys[pidx];
					if(!is.null(withV) && withV[1] == pidx) { pskip %--%. ; break; }
					if(is.na(pkey)) { pskip %--%. ; break; }  # out pf pkeys ... 
					if(nkey != pkey) 
						{ 
 

if(debug)
	{ 
.cat("key: ", key, " \t pidx :", pidx, " \t pkey :", pkey, " \t pval :", pval, " \t nkey :", nkey, " \t pskip :", pskip); 	
	}

						fkeys = c(fkeys, pkey); 
						dot.vals = c(dot.vals, eval(pkey));
						} else { pskip %--%. ; break; } 
					}
				}
			}
			
		map[[key]] = fkeys; 
		}

 
	fkeys = NULL;
	if(!is.null(map[["..."]])) { fkeys = map[["..."]]; }
	# trimws collapses list, strsplit DOESN'T collapse list 
	
	res = list(
				"fn" 		= fn, 
				"dot.keys"  = fkeys,
				"dot.vals"  = dot.vals,
				"params" 	= params, 
				"map" 		= map, 
				"formals" 	= form
				);


if(debug)
	{ 
print(str(res)); 
	}
	
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
debug = FALSE;
	dots = NULL;
	
	if(is.null(dots) && collapse) 
		{
		# parenth ( is a function operator ... 
		r = suppressError( (...), 
									show.notice=debug, 
									msg="debug prep.dots "
							);
		if(is.error(r)) { r = list(...); }
# dput.one(r);
		# if it has a structure ... return it ... 
		# dots = unlist(o);
#dput.one(dots);
		dots = r; 
		if(length(dots) == 0 && !is.null(default)) 
			{ dots = default; }
		return( dots );  
	 	}
 
 
	  
	
	BY = prep.arg(by, n=3);  # don't know if we still need this 
		
	## EQUIV:: # # parent.call = sys.call(sys.nframe() - 1L);
	fn 		= sys.calls()[[ sys.nframe()-1 ]];

	finfo 	= parse.syscall(fn, parent.frame(1));

	# what is has.objects?
	if(is.null(dots) && has.objects) 
		{
		o = NULL;
		dots = finfo$dot.keys; # just strings 
		} else { o = list(...);	}
		
	if(is.null(dots))
		{ 
		dots = o;
		names(dots) = finfo$dot.keys;

		# let's flatten to one set of lists 
		res = list();
		n = length(dots); 
		if(n > 0)
			{
			for(i in 1:n)
				{
				dot = dots[[i]];
				# keep unique ...
				dname = paste0(finfo$dot.keys[i],".",i); 
				
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
	
	if(append.info) 
		{ 
		dots = property.set("fn.info", dots, finfo); 
		dots = property.set("original", dots, o); 
		} 	
	dots;
	}

 



prep.switch = function() {}
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
	SIDE = prep.arg(side, n=n, ...);
		
	if(is.null(keys)) { keys = c("l","r","b"); }
	if(is.null(vals)) { vals = c("left", "right", "both"); }
	
	prep.switch(SIDE, keys, vals, default);
	}
	
prep.rand = function(method="high-low", n=2, ... , default="high-low", keys=NULL, vals=NULL)
	{	
	METHOD = prep.arg(method, n=n, ...);
		
	if(is.null(keys)) { keys = c("hi","fl","sa"); }
	if(is.null(vals)) { vals = c("high-low", "floor", "sample"); }
	
	prep.switch(METHOD, keys, vals, default);
	}


	
prep.strMethod = function(method="cpp", n=1, ... , default="first", keys=NULL, vals=NULL)
	{	
	METHOD = prep.arg(method, n=n, ...);

	if(is.null(keys)) { keys = c("c","s","b", "t"); }
	if(is.null(vals)) {	vals = c("cpp", "stringi", "base", "trimws"); }

	
	prep.switch(METHOD, keys, vals, default);
	}
	
	
	

prep.primeMethod = function(method="bit", n=1, ... , default="first", keys=NULL, vals=NULL)
	{		
	METHOD = prep.arg(method, n=n, ...);

	if(is.null(keys)) { keys = c("c","p","b", "s", "h"); }
	if(is.null(vals)) {	vals = c("cpp", "pracma", "bit", "sfsmisc", "hack"); }

	
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






prep.evalKey = function(key)
	{
	key = str.replace('"', "", key);
	key;	
	}
	
	

prep.evalValue = function(value)
	{
	nv = length(value);
	if(is.character(value) && nv==1) 
		{ 
		value = paste0('"',value,'"'); 
		} else { 
				value = deparse(value);
				}
	value;
	}


	
# library(help = "datasets")
# x = c(getwd(), "C:/dsjkfklj/klsdjf/", "C:\\rtools42\\x86_64-w64-mingw32.static.posix\\bin\\c++.exe");

prep.path = function(x, trailing = TRUE, force.trailing=FALSE)
	{
	z = check.ext(x);
	y = str.replace(DIR_WINDOZE, DIR_LINUX, x);
	# you may want to create a directory with stem
	# force.trailing ... DATA PROVENANCE ... 
	# "C:/.../Temp/Rtmp2XXr6l/iris.txt" => "C:/.../Temp/Rtmp2XXr6l/iris.txt/"
	# if((trailing && z==EMPTY) || force.trailing) 
	logic = v.test(z, EMPTY);
	if(trailing)
		{
		y[logic] = paste0(y[logic], DIR_LINUX); 
		y[logic] = str.replace(DOUBLE_SLASH, DIR_LINUX, y[logic]);
		}
	if(force.trailing) 
		{  
		y = paste0(y, DIR_LINUX); 
		y = str.replace(DOUBLE_SLASH, DIR_LINUX, y);
		}
	y = str.replace(DOUBLE_SLASH, DIR_LINUX, y);  # ONE more, just in CASE 
	# minvisible(y, display=print, key="DIR");	
	# Error in eval(parse(text = objstr)) :  trying to get slot "original" from an object of a basic class ("list") with no slots
 
	y;
	}
