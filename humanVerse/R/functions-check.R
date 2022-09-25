
check.ifConformable = function(x, y) {} # matrix?

check.square = function(x)
	{
	( nrow(x) == ncol(x) );
	}
	
check.rank = function(x)
	{
	# get dim 
	# get rank 
	# check "full rank"
	
	}


check.list = function(input)
	{
	# we may have only a vector, not a list 
	if(is.list(input)) { return(input); }
		res = list(); 
		res[[1]] = input;
	res;
	}
	
check.base = function(base = 10)
	{
	base = as.integer(base);
	if(base == 64) { return(base); }  # base64 will work as string/int
	if(base > 36 || base < 2) { stop("'base' must be between 2 and 36."); }
	base; 
	}



check.ns = function(..., character.only=FALSE)
	{
	pkgs = prep.dots(..., collapse=character.only, has.objects=!character.only, default="stringi");
	if(!character.only) { pkgs = as.character(pkgs); }
debug = FALSE;
	n = length(pkgs);
	idx = logical(n); # FALSE by default 
	for(i in 1:n)
		{
		pkg = pkgs[i];
		pkg.ns = suppressError( getNamespace(pkg), 
									show.notice=debug, 
									msg="debug check.ns "
							);
		if(!is.error(pkg.ns)) { idx[i] = TRUE; }	 
		}
	bad = v.return(pkgs[!idx]);
		if(!is.null(bad)) { cat.warning("\n\n", "bad package names spaces: maybe not installed", bad, "\n\n"); }
	idx;	
	}


# this works 
# environmentName(getNamespace("stringi"))



check.pkg = function(..., character.only=FALSE, name.space=TRUE)
	{
	pkgs = prep.dots(..., collapse=character.only, has.objects=!character.only, default="stringi");
	if(!character.only) { pkgs = as.character(pkgs); }
	
	# if not pkg true name, character.only
	idx = fidx = is.library(pkgs, character.only = TRUE);
		# is.library alreayd has a warning ... 
	bad = v.return(pkgs[!idx]);
		if(!is.null(bad)) { cat.warning("\n\n", "bad package names: maybe not installed", bad, "\n\n"); }
		
	if(name.space)
		{
		idx2 = check.ns(pkgs, character.only = TRUE);
		fidx = (idx & idx2);
		}	
		
	v.return(fidx);
	}




check.fn = function(..., character.only = FALSE, check.match=TRUE)
	{
debug = FALSE;
	fns = prep.dots(..., collapse=character.only, has.objects=!character.only, default="base::sin");
	if(!character.only) { fns = as.character(fns); }
	
	idx1 = check.obj(fns, character.only = TRUE);
	if(!check.match) { return(idx1); }
	
	n = length(fns);
	idx = idx1;    # if object is false, skip ... 
	for(i in 1:n)
		{
		id = idx[i]; if(!id) { next; }		
		fn = fns[i];
		fn.obj = eval(parse(text = fn));
		# # https://stackoverflow.com/a/73690894/184614
		fn.match = suppressError( match.fun(fn.obj), 
									show.notice=debug, 
									msg="debug check.fn ... match.fun"
								);
		if(is.error(fn.match)) { idx[i] = FALSE; }
		}	
	idx;
	}


check.obj = function(..., character.only = FALSE)
	{
debug = FALSE;
	objs = prep.dots(..., collapse=character.only, has.objects=!character.only, default="base::sin");
	if(!character.only) { objs = as.character(objs); }
	
	n = length(objs);
	idx = logical(n); # FALSE by default 
	for(i in 1:n)
		{
		obj = objs[i];
		o.obj = suppressError( eval(parse(text = obj)), 
										show.notice=debug, 
										msg="debug check.obj ... eval "
								);
		if(is.error(o.obj)) { next; }
		idx[i] = TRUE;
		}
	idx;
	}
	


## REWRITE 
## is.set and is.empty 
## with new updates, multivariate ...
## MONTE ::: TODO 	
	

check.type = function(...)
	{
debug = FALSE;
	checktype = suppressError( typeof(...), 
								show.notice=debug,
								msg="debugging typeof check.type REGULAR" 
							);
	res = TRUE;
	if(is.error(checktype)) { res = FALSE; }
	res = property.set("typeof", res, checktype);
	res; 
	}



check.string = function(thing)
	{
	ct.THING = check.type(thing);
	if(!ct.THING || !is.character(thing) ) 
		{ thing = deparse(substitute(thing)); }
	thing;
	}





check.isCompatibleLength = function() {}
check.isCompatibleLength = function(x, y, 
									method="equal",  # "1-1-equal"
									action="warning", 
									msg = " obj1 [x] and obj2 [y] are incompatible lengths, you may get spurious results."
								)
	{
	METHOD = prep.arg(method, 3, keep="-");
	acti = prep.arg(action, 4);
	xlen = length(x);
	ylen = length(y);
	b = (ylen == xlen);  
		if(METHOD == "equ") { return(b); }
		
	xone = (xlen == 1);
	yone = (ylen == 1);	
		if( (METHOD == "11e" || METHOD == "1,1") && (xone || yone) )
			{
			return(TRUE);
			}
			
	if(acti == "warn") { warning(msg); }
	if(acti == "stop") { stop(msg); }
	}

























