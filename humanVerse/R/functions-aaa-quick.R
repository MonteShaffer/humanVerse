



check.type_ = function(...)
	{
	checktype = suppressError_( typeof(...) );
	res = TRUE;
	if(is.error_(checktype)) { res = FALSE; }
	attributes(res)[[ "typeof" ]] = checktype;
	res; 
	}


is.error_ = function(e, where="suppressError")
	{
	condition 	= attributes(e)$condition;
	if(is.null(condition)) { return(FALSE); }
	
	extra 		= attributes(condition)$class;
	if("error" %in% extra) { return(TRUE);  }
	
	### is this necessary
	return(FALSE);
	}

suppressError_ = function(expression)
	{
	try( expression , silent = TRUE);
	}


include.dir_ = function() {}
include.dir_ = function(path = getwd(), verbose = TRUE, pattern = "[.][RrSsQq]$")
	{
debug = FALSE;
	op = options(); on.exit(options(op)); 
	
	path = normalizePath(path);
	myfiles = list.files(path, pattern = pattern);
	n 		= length(myfiles);

if(verbose) 
	{ 
.cat_("ATTEMPTING TO INCLUDE ", n, " files ... "); 
	}
	
	j = NULL;
	myerrors 	= character(n);
	efiles 		= character(n);
	myfullpaths = file.path(path, myfiles);
	for(i in 1:n)
		{
if(verbose) 
	{ 
.cat_(myfiles[i],": "); 
	}
		
		w = suppressError_( source( myfullpaths[i] ));
					
		if( is.error_(w) ) 
			{ 
			efiles[i] = myfiles[i];
			w = as.character(w); j = c(j, i); myerrors[i] = w;
if(verbose)
	{
.cat_(w);
	}
			}
		
		# SOURCE may have changed options, back to DEFAULT 
		options(op);
		}
		
	nj = length(j);
	df = data.frame( cbind(myfiles, efiles, 
						myerrors, myfullpaths) );
	
if(verbose) 
	{ 
.cat_("Reporting ", nj, " errors on ", n, " includes ... ");
	}
	
	invisible(df);
	}




quick = function(one, res=NULL, verbose=FALSE)
	{	
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.ONE = check.type_(one);
	if(!ct.ONE || !is.character(one))	
		{ one = deparse(substitute(one)); }
##########################################################
# if I have declared str = "hello" ... creates a problem ... 
#dput(one); stop("monte");

	## shortcut .... quick(dir) like quick.dir()
	if(one == "dir") 	{ return( quick.dir() ); }
	if(one == "quick") 	{ one = "aaa-quick"; }

	if(is.null(res)) { res = alex; }  # GLOBAL [at the moment]
	
	sfile 	= paste0("functions-",one,".R");
	idx		= which(res$myfiles == sfile);
	
	
.cat_("QUICK: ", sfile, " with idx: ", idx );
	if(length(idx) == 0) { stop("bad idx"); }
	
	o = source(res$myfullpaths[ idx[1] ], verbose=verbose);
	
	._____init.settings();
	} 




quick.dir = function(f="C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/") 
	{ 
	setwd(f);

	alex 	= include.dir_(f);  	
	fn 		= ls(all.names = TRUE, pos=1);	
				attributes(alex)[[ "fn" ]] = fn;
	
	assign("alex", alex, envir = .GlobalEnv);

	print( alex$myerrors[ alex$myerrors != ""]);
	print( alex$efiles[ alex$efiles != ""]);

	quick(quick);  
	}

.cat_ = function(..., sep="\n\n")
	{
	cat(sep);	cat(...);	cat(sep);
	}

