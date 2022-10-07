



check.type = function(...)
	{
debug = FALSE;
	checktype = suppressError( typeof(...), 
								show.notice = debug,
								msg = "debugging typeof check.type QUICK");
	res = TRUE;
	if(is.error(checktype)) { res = FALSE; }
		attributes(res)[[ "typeof" ]] = checktype;
	res; 
	}
	





is.error = function(e, where="suppressError")
	{
	### see list.fromError(e) for ideas to improve this function
	condition 	= attributes(e)$condition;
	if(is.null(condition)) { return(FALSE); }
	
	extra 		= attributes(condition)$class;
	if("error" %in% extra) { return(TRUE);  }
	
	### is this necessary
	return(FALSE);
	}











suppressError = function(expression, show.notice = TRUE, msg = "")
	{
	if(show.notice && msg == "")
		{
		# example when I want to control the exact display with \n and \t ... not just str.wrap ... 
			
		msg = wrap.lang("\n\n", "tldr;", "\n\n\n\t", "R-DEV believes this is poor programming practice to allow you to", "\n\t\t", "`suppressError()` so they have not included it in base R.", "\n\t\t", "It is probably true, but 'git-r-done' first, and then", "\n\t\t", "figure out the minutia such as why this function is", "\n\t\t", "throwing an error.  That is why I have past such a ", "\n\t\t",  "VERBOSE message to you, dear reader.", "\n\n\t", "By altering this function [set msg to something else, not empty ''],", "\n\t\t",  "you can reduce the length of this message.", "\n\n\t", "Or you can set the flag show.notice=FALSE to prevent it from printing.", "\n\t\t", "THIS my friends is how choice architecture works!  Cheers and Aloha!", "\n\n\n");
		}				
	if(show.notice)
		{
		cat.warning(msg, call. = FALSE, immediate. = TRUE);
		}
	try( expression , silent = TRUE);
	}














include.dir = function() {}
include.dir = function(path = getwd(), verbose = TRUE, pattern = "[.][RrSsQq]$")
	{
debug = FALSE;
	op = options(); on.exit(options(op)); 
	
	myfiles = list.files(path, pattern = pattern);
	n = length(myfiles);

if(verbose) 
	{ 
.cat("ATTEMPTING TO INCLUDE ", n, " files ... "); 
	}
	
	j = NULL;
	myerrors 	= character(n);
	myfullpaths = character(n);
	for(i in 1:n)
		{
if(verbose) 
	{ 
.cat(myfiles[i],": "); 
	}
		myfullpaths[i] = file.path(path, myfiles[i]);
		w = suppressError( source( myfullpaths[i] ), 
								show.notice=debug, 
								msg="debugging include.dir" );
					
		if( is.error(w) ) 
			{ 
			w = as.character(w); j = c(j, i); myerrors[i] = w;
if(verbose)
	{
.cat(w);
	}
			}
		
		# SOURCE may have changed options, back to DEFAULT 
		options(op);
		}
		
	nj = length(j);
	df = data.frame( cbind(myfiles, myerrors, myfullpaths) );
	
if(verbose) 
	{ 
.cat("Reporting ", nj, " errors on ", n, " includes ... ");
	}
	
	invisible(df);
	}


quick = function(one, res=NULL, verbose=FALSE)
	{	
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.ONE = check.type(one);
	if(!ct.ONE || !is.character(one))	
		{ one = deparse(substitute(one)); } 
##########################################################

	## shortcut .... quick(dir) like quick.dir()
	if(one == "dir") { return( quick.dir() ); }
	if(one == "quick") { one = "aaa-quick"; }

	if(is.null(res)) { res = alex; }  # GLOBAL [at the moment]
	
	sfile 	= paste0("functions-",one,".R");
	idx 	= v.which(res$myfiles, sfile);
	
.cat("QUICK: ", sfile, " with idx: ", idx, "\n");
	if(is.null(idx)) { stop("bad idx"); }
	
	o = source(res$myfullpaths[idx], verbose=verbose);
	
	._____init.settings();
	} 


quick.dir = function(f="C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/") 
	{ 
	setwd(f);

	alex 	= include.dir(f);  	
	fn 		= ls(all.names = TRUE, pos=1);	
				attributes(alex)[[ "fn" ]] = fn;
	
	assign("alex", alex, envir = .GlobalEnv);

	print( alex$myerrors[ alex$myerrors != ""]);

	quick(quick);  
	}

.cat = function(..., sep="\n\n")
	{
	cat(sep);	cat(...);	cat(sep);
	}
	
# aaa comes first, so NEWER versions of functions *MAY* follow.

