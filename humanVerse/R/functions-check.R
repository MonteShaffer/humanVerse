

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


check.number = function(x)
	{
	# TRUE/FALSE result 
	# https://stackoverflow.com/a/13638403/184614
	# idxI = suppressWarning( !is.na(as.integer(x)) );
	suppressWarning( !is.na(as.numeric(x)) );	
	}


# create as in create.DIRECTORY
check.file = function(path, trailing = TRUE, create=TRUE)  
	{
	d = check.dir(path, create=create, trailing=trailing);
	stem = basename(path);  # not filename()	
	f = paste0(d, stem);
	f;
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

check.ext = function(x, dotless=TRUE)
	{
	stem = basename(x);  # not filename()
	lenstem = str.len(stem);
	
	s = str.pos(EXT, stem);  # hard to use ... but returns NULL
	# s = str.explode(EXT, stem); # easier to use
	if(is.null(s)) { return(EMPTY); }  # univariate ...
	s = check.list(s);  # multivariate 
	
	
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
	
	logic = v.test(vals, NA, invert=TRUE);
	
	dot = 1;
	if(!dotless) { dot = 0; }  # could allow for DOT of any length ...
	
	res = EMPTY;  	
	res[logic] = substring(stem[logic], 
							dot + vals[logic], 
							lenstem[logic]
							);
	res;
	}	
	
# tempdir()
check.dir = function(path, trailing = TRUE, create=TRUE)
	{
	# if NOT LOCAL, download to TMP location
	# update filename using %TO% ?  parent.frame(1)
	d = dirname(path);
	d = prep.dir(d, trailing=trailing);
	
	if(create)
		{
		dir.create( d, 
					showWarnings = FALSE, 
					recursive = TRUE
				);
		}
	d;
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
is.file = function(path=getwd(), ...) 
	{ r = check.path(path, ...); (r$type == "file"); }
is.dir = function(path=getwd(), ...) 
	{ r = check.path(path, ...); (r$type == "dir"); }

dir.exists_ = function(path=getwd(), ...) 
	{ r = check.path(path, ...); (r$type == "dir" && r$exists); }
file.exists_ = function(path=getwd(), ...) 
	{ r = check.path(path, ...); (r$type == "file" && r$exists); }
	

check.path = function() {}
check.path = function(path=getwd(), trailing = TRUE, create=FALSE)
	{
	# currently UNIVARIATE ...
	
	
	# is.file and is.dir fails on path=getwd() ... not a file 
	# fopen(path)  cannot open file 'C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R': Permission denied
	# ergo, its a path ?
	
	# logic BROKE with move over and removing STUFF ...
	# str.diff
	# TODO fix ... 
	# I HAVE LOST a / somewhere.
	# is.null(ext) vs is.empty(ext) ... ext == EMPTY ...
	# b/c I updated check.ext and made it multivariate 
	# quick.dir() broke?  is.dir ??? NODE overflow ... 
	# memory.logging keeps jamming with prep.dots(...) on df.row 
	# maybe need to go OLD-SCHOOL with df.row on MANUAL dots ...
	# how to GET NAMES ... ETC?
	## maybe it is the is.defined() function?
	## what changed ... DID I add a lazy-loading SOMEWHERE BAD?
	# regardless ... prep.dots needs EASY, MEDIUM, DIFFICULT ... 
	# a rewrite ... 
	
	b 	= basename(path);
	e 	= check.ext(path);
	d 	= dirname(path);
				if(trailing) { d = paste0(d, DIR_LINUX); }
	pd 	= prep.dir(path, trailing=trailing); 
	pf 	= check.file(path, trailing=trailing, create=create);
				if_ = file.exists(pf);  # reserved word 
				id_ = dir.exists(pf);
			# I believe these are file.stat[us] and dir.stat[us] functions 
			# May include R/W forbidden 0777 info ?
	is_ = c("-UNKNOWN-", "!exists");
	is__ = FALSE;
			if(if_ && !id_) { is_ = c("file", "exists");  is__ = TRUE; }
			if(if_ && id_ ) { is_ = c("dir",  "exists");  is__ = TRUE;}
	
	# subtract pd - d ... if it contains a SLASH
	di = pd %.-% d;  # str.subract(a,b)
			status = "file";
			if(str.contains(SLASH, di)) { status = "dir"; }
	
			
	# not the best logic, but all I got, I think ...
	######################  OTHER TESTS ######  OS differences ?
	ext.test 		= "file"; 
					if(e == EMPTY) { ext.test 		= "dir"; }
	trailing.test 	= "file"; 
					if(pd != pf)   { trailing.test 	= "dir"; } 

	info = list("type" 				= status,
				"exists" 			= is__,
				"exists.info" 		= is_,
				"stem" 				= b,
				"ext" 				= e,
				"path.as.dirname"	= d,
				"path.as.dir" 		= pd,
				"path.dir.diff"		= di, # this is ultimately "mytest"
				"path.as.file" 		= pf,
				"ext.test" 			= ext.test,
				"dir.test" 			= trailing.test,
				
				"stat.file" 		= if_,  # file.stat means has r/w perms?
				"stat.dir" 			= id_		# chmod ... run as ADMIN
				);
	# print(str(info));
	minvisible(info, key="PATH_INFO", display=str);
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

























