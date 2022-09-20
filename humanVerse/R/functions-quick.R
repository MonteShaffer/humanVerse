



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



suppressErrors = suppressError;
suppressWarning = suppressWarnings;




# source( res$
## assuming res is alive from include.dir
quick.source = function(key="pipple", res=NULL, verbose=FALSE)
	{
	if(is.null(res)) { memory.init(); res = memory.get("alex", "SYSTEM"); }
	sfile = paste0("functions-",key,".R");
	idx = v.which(res$myfiles, sfile);
cat("\n QUICK: ", sfile, " with idx: ", idx, "\n");
	if(!is.null(idx))
		{
		source(res$myfullpaths[idx], verbose=verbose);
		} else { stop("bad idx"); }
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


quick = quick.source;




# include.dir = source.dir ... if is local ? otherwise, I need a parser ... github
# USE dir.exists ........... is.dir ... ?local in help on SOURCE is different thing
# include.url = source.url
# include = source.local ... this indexes 

# # ?source ... sourceDir
## cat("\n", res$myerrors[ res$myerrors != ""], sep="\n" );
 
## setwd("C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R")
## alex = include.dir(getwd()); View(alex); alex$myerrors[ alex$myerrors != ""]; fn = ls(); str(fn); View(fn);

quick.dir = function() 
	{ 
	setwd("C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R");
	alex = include.dir(getwd()); # View(alex); 
	fn = ls(all.names = TRUE, pos=1); # str(fn); View(fn);
	alex = property.set("fn", alex, fn); 
	memory.init(); memory.set("alex", "SYSTEM", alex);
		"alex" %GLOBAL% alex;
	
	print( alex$myerrors[ alex$myerrors != ""]);
	minvisible(alex); 
	}

 


