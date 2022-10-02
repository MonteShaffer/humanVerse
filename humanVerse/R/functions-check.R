

check.sample = function(replace, min, max, n)
	{
	if(!replace)
		{
		len = (max - min) + 1;
		if(len < n)
			{
			msg = prep.msg( "sample.replace forced to TRUE" );
			cat.warning(msg);
			replace = TRUE;
			}
		}
	replace;	
	}



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
	if(base > 36 || base < 2) 
		{ 
		
		cat.warning("'base' must be between 2 and 36."); 
		return(NULL);
		}
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


check.number = function(x)
	{
	# TRUE/FALSE result 
	# https://stackoverflow.com/a/13638403/184614
	# idxI = suppressWarning( !is.na(as.integer(x)) );
	suppressWarning( !is.na(as.numeric(x)) );	
	}

check.boolean = function(x)
	{
	suppressWarning( !is.na(as.logical(x)) );	
	}


# memory or file caching ...
cache.get = function(cobj, cache="memory", unused=NULL)
	{
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.CACHE = check.type(cache);
	if(!ct.CACHE || !is.character(cache))	
		{ cache = deparse(substitute(cache)); } 
##########################################################

	CACHE = prep.switch( prep.arg(cache, n=1), c("m","f"), 
						c("memory", "filesystem"), "memory");
		
	md5 = str.toMD5( JSON.stringify( cobj ) );				
		
	if(cache == "memory")
		{
		obj = memory.get(md5, "-CACHE-");
		}  
	if(cache == "filesystem")
		{ 
		d = "C:/_R_/-humanVerse-/SYSTEM/cache/runtime/YYYY-MM-DD/"
		f = paste0(d, md5, ".rds");
		obj = NULL;
		if( file.exists_(f) ) { obj = readRDS(f); }
		}

	list("md5" = md5,
		"obj"	= obj
		);
	}

# memory or file caching ...
cache.set = function(nobj, cache="memory", md5="abcdef")
	{ 
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.CACHE = check.type(cache);
	if(!ct.CACHE || !is.character(cache))	
		{ cache = deparse(substitute(cache)); } 
##########################################################

	CACHE = prep.switch( prep.arg(cache, n=1), c("m","f"), 
						c("memory", "filesystem"), "memory");
		
		 
	if(cache == "memory")
		{ 
		memory.set(md5, "-CACHE-", nobj);
		}
	if(cache == "filesystem")
		{
		d = "C:/_R_/-humanVerse-/SYSTEM/cache/runtime/YYYY-MM-DD/"
		f = paste0(d, md5, ".rds");
		writeRDS(nobj, f);
		}
	}



.NULL = function(x, type=typeof(x))
	{
	# TRAPPING NULLS in VECTORS ... 
	if(!is.defined(NULL_INT)) { constants.default(); }
	.NULL_ = NULL_CHAR; # default 
	if(type == "integer") { .NULL_ = NULL_INT; }
	if(type == "double")  { .NULL_ = NULL_NUM; }
	.NULL_;	
	}

.isNULL = function(x, type=typeof(x), invert=FALSE)
	{
	# TRAPPING NULLS in VECTORS ... 
	.NULL_ = .NULL(x, type=type);	
	v.test(x, .NULL_, invert=invert);
	} 

# x = c("C:/_R_/humanVerse/SANDBOX/data/iris", "C:/_R_/humanVerse/SANDBOX/data/iris.txt", "C:/_R_/humanVerse/SANDBOX/data/.iris");
# all are files, hidden 
check.ext = function(x, dotless=TRUE)
	{
	# if I use basename_ does that create a recursion LOOP?
	# path.summary is the only one that would create recursion?
	stem = basename(x);  # not filename()
	# stem_ = basename_(x, trailing=TRUE);  # doesn't give me what I want ...  
	
	# EXT = "." in CONSTANTS 
	s = str.pos(EXT, stem);  # hard to use ... but returns NULL
	# s = str.explode(EXT, stem); # easier to use
	if(is.null(s)) { return(EMPTY); }  # univariate ...
	
	n = length(x);
	s = check.list(s);  # multivariate 
	lenstem = str.len(stem);
	
		# I have NULL's trapped inside, what will get ELEMENTS DO 
		# maybe create a bogus NUMBER 
		# NULL_NUM = 59912001001550.4068925815,
		# NULL_INT = 59912001001550,
		# NULL_CHAR = "U+EA08",
		# TRAP in VECTORS ... 
		# FREAKING AWESOME
		# maybe implement into list.getElements ... 
		# ultimately THROUGHOUT ... TRAP NULLS in VECTORS... AWESOME
	
	vals = list.getElements(s, 1);  # NA's on NULLs ... 
	# vlen = str.len(vals);
	
	
	logic = ( v.test(vals, NA, invert=TRUE) & !str.begin(EXT, stem) )
	
	dot = 1;
	if(!dotless) { dot = 0; }  # could allow for DOT of any length ...
	
	res = rep(EMPTY, n);  	
	res[logic] = substring(stem[logic], 
							dot + vals[logic], 
							lenstem[logic]
							);
	res;
	}	



	
# tempdir()
# create as in create.DIRECTORY, trailing SLASH 
# path can be file or [dir]ectory 

	
check.dir = function(path, trailing = TRUE, create=TRUE)
	{
	# if NOT LOCAL, download to TMP location
	# update filename using %TO% ?  parent.frame(1)
	# d = dirname(path);  # we lose the /second 
	p = prep.path(path, trailing=trailing);
	
	stem = basename_(path, trailing=trailing);
	
	d = p %-.% stem;  # "right" side str.subtract 
	
	if(create)
		{
		n = length(d);
		for(i in 1:n)
			{
			dir.create( d[i], 
						showWarnings = FALSE, 
						recursive = TRUE
					);
			}
		}
	d; 
	}



# create as in create.DIRECTORY
check.file = function(path, trailing = TRUE, create=TRUE)  
	{
	# this would work, but not CREATE
	#p = prep.dir(path, trailing=trailing);  
	d = check.dir(path, create=create, trailing=trailing);
	
	# stem = basename(path);  # not filename()	
	stem = basename_(path, trailing=trailing);
	f = paste0(d, stem);
	# if they don't exist, touch them ...
	if(create)
		{
		n = length(f);
		for(i in 1:n)
			{
			touch(f[i]);
			}
		}
	f;
	}



check.path = function() {}
check.path = function(path=getwd(), trailing=TRUE, create=TRUE, open=FALSE) 
	{
	s = path.summary(path, trailing=trailing);
	type = s$type;
	if(type == "dir")
		{
		d = check.dir(path, trailing=trailing, create=create);
		if(open) { openSesame(d); }
		return(invisible(d));
		}
	if(type == "file")
		{
		f = check.file(path, trailing=trailing, create=create);
		if(open) { openSesame(f); }
		return(invisible(f));
		}
	return(NULL);
	}



	
# check.path("C:/garba/dkfj/")
# check.path(tmp.file("sldsfeep.txt"))
# exists from writeToPipe(sleep, tmp.file("sleep.txt"));
# check.path(tmp.file("sleep.txt"));  
# check.path(getwd())
# a "path" is a file or a dir ... this clears that up ...
# check.path()$exists
# check.path()$type == "dir"	# TRUE is.dir 
# check.path()$type == "file"	# TRUE is.file 

# I have a lingering NULL 
# these were aliases ...
	



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



# https://github.com/douglascrockford/JSON-js/blob/master/json2.js
check.json = function(str)
	{
	# returns NULL if NOT, otherwise JSON.parse ...
	
	}






















