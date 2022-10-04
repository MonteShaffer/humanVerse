
"%GLOBAL%" 	= .GLOBAL.;
B64			= "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";
B64v		= str.explode("", B64);
BXX			= "0123456789ABCDEFGHIJKLMNOPQRSTUV";
BXXv		= str.explode("", BXX);
Bits64		= int2base(0:63, base=2);

ATTRIBUTE_KEY = "@";
DEFAULT_TOLERANCE = sqrt(.Machine$double.eps);
DEFAULT_TIMEZONE = "UTC";
EXT = ".";
DIR_WINDOZE = "\\";
DOUBLE_SLASH = "//";
SLASH = "/";
DIR_LINUX = "/";

INTEGER_MAXIMUM = 2147483647;

BUFFER 				= 1024
SEEK_START 			= "start"
SINGLE_QUOTE 		= "'"
DOUBLE_QUOTE 		= '"'



EMPTY 	= "";
"%++%" 	= .PLUS_PLUS.;
"%--%" 	= .MINUS_MINUS.;

# mytype = suppressError( 
# may be different for tryCatch(
is.error = function(e, where="suppressError")
	{
	condition = attributes(e)$condition;
	if(is.null(condition)) { return(FALSE); }
	# see list.fromError(e) for other ideas to improve this function
	extra = attributes(condition)$class;
	if("error" %in% extra) { return(TRUE); }
	# is this necessary
	return(FALSE);
	}




# testit = function() { if(rand(0,1) == 1) {stop("ERROR");} else {return(1); }}
# x = suppressError(testit);
# x = suppressError(testit(), show.notice=FALSE); str(x);
# x = suppressError(testit(), msg="-HI-"); str(x); if(is.error(x)) { list.fromError(x); }


suppressError = function(expression, show.notice = TRUE, msg = "")
	{
	if(show.notice)
		{
		if(msg == "") 
			{
			# example when I want to control the exact display with \n and \t ... not just str.wrap ... 
			msg = wrap.lang("\n\n", "tldr;", "\n\n\n\t", "R-DEV believes this is poor programming practice to allow you to", "\n\t\t", "`suppressError()` so they have not included it in base R.", "\n\t\t", "It is probably true, but 'git-r-done' first, and then", "\n\t\t", "figure out the minutia such as why this function is", "\n\t\t", "throwing an error.  That is why I have past such a ", "\n\t\t",  "VERBOSE message to you, dear reader.", "\n\n\t", "By altering this function [set msg to something else, not empty ''],", "\n\t\t",  "you can reduce the length of this message.", "\n\n\t", "Or you can set the flag show.notice=FALSE to prevent it from printing.", "\n\t\t", "THIS my friends is how choice architecture works!  Cheers and Aloha!", "\n\n\n");
			}
		# cat(msg);
		warning(msg, call. = FALSE, immediate. = TRUE);
		}
	try( expression , silent = TRUE);
	}







	
	

# setwd("C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R")

include.dir = function() {}
include.dir = function(path = getwd(), verbose = TRUE, pattern = "[.][RrSsQq]$")
	{
debug = FALSE;
	op = options(); on.exit(options(op)); # to reset after each 
	myfiles = list.files(path, pattern = pattern);
	n = length(myfiles);
	if(verbose) { cat("\n ATTEMPTING TO INCLUDE ", n, " files ... ", "\n"); }
	j = NULL;
	# myerrors = list();
	myerrors = character(n);
	myfullpaths = character(n);
	for(i in 1:n)
		{
		myfile = myfiles[i];
		if(verbose) { cat(myfile,": "); }
		# this should go in `include.local` ... 
		# you can't USE `dots` inside suppressError ... maybe wrap in TRY/CaTCH
		myfullpaths[i] = file.path(path, myfile);
		# w = suppressError( source( myfullpaths[i], ...), 
		#			show.notice=debug, msg="debugging source.dir" );
					
					
		w = suppressError( source( myfullpaths[i] ), 
					show.notice=debug, msg="debugging include.dir" );
					
		if( is.error(w) ) 
			{ 
			w = as.character(w);			
			j = c(j, i); 			
			myerrors[i] = w;
			if(verbose)
				{
				cat("\n\n", w, "\n\n"); 
				}
			} else { if(verbose) { cat("\n"); } }
		
		# so maybe the SOURCE is not just functions, but CODE
		# changing the options 
		options(op);
		}
		
	df = data.frame( cbind(myfiles, myerrors, myfullpaths) );
	if(verbose) 
		{ 
		cat("\n Reporting ", length(j), " errors on ", n, " includes ... ", "\n"); 
		print(df);
		cat("\n Reporting ", length(j), " errors on ", n, " includes ... ", "\n");
		print(df$myerrors[ df$myerrors != ""]);
		}	
	invisible(df);
	}






# include.dir = source.dir ... if is local ? otherwise, I need a parser ... github
# USE dir.exists ........... is.dir ... ?local in help on SOURCE is different thing
# include.url = source.url
# include = source.local ... this indexes 

# # ?source ... sourceDir
## cat("\n", res$myerrors[ res$myerrors != ""], sep="\n" );
 
## setwd("C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R")
## alex = include.dir(getwd()); View(alex); alex$myerrors[ alex$myerrors != ""]; fn = ls(); str(fn); View(fn);





# source( res$
## assuming res is alive from include.dir
# get bytecode ... of fun... => name ?
quick = function(..., character.only = FALSE, res=NULL, verbose=FALSE)
	{  
	
	fns = prep.dots(..., collapse=character.only, has.objects=!character.only);
# dput(fns);
	#####if(!is.defined(EMPTY)) { constants.default(); }
		################ minvisible(fns, display=none);
	if(!character.only) { fns = as.character(fns); }
dput(fns);

	## shortcut .... 
	if(fns == "dir") { return( quick.dir() ); }
	
	# we moved our aliases, so quick(utils) doesn't update %TO% 
#	fns = unique( c(fns, "zza-special", "zzz-alias"));

###	if(is.null(res)) { res = memory.get("alex", "SYSTEM"); };
	if(is.null(res)) { res = alex; }  # GLOBAL at the moment ...
	
	for(key in fns)
		{
		sfile = paste0("functions-",key,".R");
		idx = v.which(res$myfiles, sfile);
	cat("\n QUICK: ", sfile, " with idx: ", idx, "\n");
		if(!is.null(idx))
			{
			source(res$myfullpaths[idx], verbose=verbose);
			} else { stop("bad idx"); }
		}
	
	init.settings();
	} 

cpp = "C:/_git_/github/MonteShaffer/humanVerse/HVcpp/src/";
# Rcpp::sourceCpp("matrix.cpp");
# C:\_git_\github\MonteShaffer\humanVerse\HVcpp\src

quick.dir = function() 
	{ 
	setwd("C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R");
	alex = include.dir(getwd()); # View(alex); 
	fn = ls(all.names = TRUE, pos=1); # str(fn); View(fn);
	alex = property.set("fn", alex, fn); 
############ 	memory.init(); memory.set("alex", "SYSTEM", alex);
		"alex" %GLOBAL% alex;
	
	print( alex$myerrors[ alex$myerrors != ""]);
###############	# minvisible(alex);


	quick(quick);  # load "defaults" AFTER ... which will load init(); 
	}

 


