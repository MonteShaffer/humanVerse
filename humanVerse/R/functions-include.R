

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
		}
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

quick.dir = function() 
	{ 
	setwd("C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R");
	alex = include.dir(getwd()); View(alex); 
	fn = ls(all.names = TRUE, pos=1); str(fn); View(fn);
	alex = property.set("fn", alex, fn);
	memory.init(); memory.set("alex", "SYSTEM", alex);
		"alex" %GLOBAL% alex;
	
	print( alex$myerrors[ alex$myerrors != ""]);
	minvisible(alex); 
	}

 





















ifsdfsnclude.dir = function() 
	{
	# if remote, we make it local, and include there ... force.override?
	
	
	}
	
	 
include.package = include.pkg = i.pkg = i.p = function() {}
# include ccp via Rcpp
include.cpp = include.Rcpp = i.Rcpp = i.ccp = function() {}
# include.file
include.file = i.file = i.f = function() {}	

f.download = filedlkfj.download() {}
	
include = function(..., character.only=FALSE)
	{
	if(character.only) 
		{ 
		what = unlist(list(...)); 
		} else {
				what = str.fromObjectName(...);
				}
				
	# is it a library or a file ?
	# is it LOCAL/REMOTE file?
				
	# include(library) ... we maybe do sourceME on it 
	# include(file) ... if not local, we download, and store to a CACHE [SANDBOX]/R/include/web/FULL-PATH/ or /R/include/github/SHORT-PATH/
	# [SANDBOX]/R/packages/CRAN/ or /packages/github/ or /packages/web/ or ???
	
	# RhV ... Lotus-V ... RVStudio ... RhVStudio
	# XAMPP ... from PHP send commands via APACHE call to SHELL R-VANILLA
	# https://www.apachefriends.org/
	# XAMPP on WINDOZE, Linux, MacOS
	# https://vitux.com/how-to-install-apache-mariadb-and-php-lamp-on-debian-11/
	# https://gist.github.com/Eyal-Shalev/1b747a0ce47ae3123fe30e0635567803
	# https://techsparx.com/software-development/docker/damp/
	# https://www.mamp.info/en/mac/
	# https://www.how2shout.com/how-to/install-wamp-server-windows-10-step-by-step-tutorial-guide.html
	## HOST FILE (WINDOZE) and APACHE CONF ... https://humanVerse 
	# localhost ssl snakeoil
	# https://sphinxsearch.com/
		## SELECT ... FROM test1 WHERE (lat BETWEEN 53.23 AND 53.42) AND (lon BETWEEN -6.45 AND -6.05)
		## lots of new JSON stuff
		##  Problem is, Porter stemmer does not normalize goose to itself, it will emit goos stem instead. So in order to fixup Porter stemmer specifically you would have to map to that stem, ie. use a geese => goos wordform. Extreme care.
		## UDFs in C ... user defined functions [RUSSIANS!]
		## terms that have special characters in them, like @Rihanna, or Procter&Gamble or U.S.A
		## Starting with v.3.3 we removed several legacy IDF calculation methods, and now Sphinx always uses the following formula to compute IDF from n (the document frequency) and N (the corpus size).
		## idf = log(N/n) / (2*log(N+1)) * term_idf_boost
		## So we start with de-facto standard raw_idf = log(N/n); then we normalize that for the corpus size; and further compress the idf into [0.0, 0.5) range.
	 
	 ## https://cran.r-project.org/doc/manuals/R-admin.html#Installing-R-under-Unix_002dalikes
	 ## Further options, e.g for hyperref, can be included in a file Rd.cfg somewhere on your LaTeX search path. 
	 ## R --arch=name CMD INSTALL --libs-only pkg1 pkg2 
	 ## https://rdrr.io/r/base/search.html
	 ## searchpaths() ... search() 
	 ## Alternatively, the installed R can be run, preferably with --vanilla. Then pdf("tests.pdf") ## optional, but prevents flashing gr
	 ## getOption("defaultPackages")
	 ## R_SCRIPT_DEFAULT_PACKAGES; if set, this takes precedence over R_DEFAULT_PACKAGES.
	 ## chooseCRANmirror()
	 ## install.packages(c("pkg1", "pkg2"))
	 ## install.packages can install a source package from a local .tar.gz file (or a URL to such a file) by setting argument repos to NULL: this will be selected automatically if the name given is a single .tar.gz file.
	 ## install.packages can look in several repositories, specified as a character vector by the argument repos: these can include a CRAN mirror, Bioconductor, R-forge, rforge.net, local archives, local files, …). Function setRepositories() can select amongst those repositories that the R installation is aware of.
	 
	 ## On Windows install.packages can also install a binary package from a local zip file (or the URL of such a file) by setting argument repos to NULL.
	 ## R CMD INSTALL works in Windows to install source packages.
	 ## update.packages() 
	 ## 18,000  ... license, no author?
	 ## inst <- packageStatus()$inst; inst[inst$Status != "ok", c("Package", "Version", "Status")]
	## mostly httr ... world, constantly needs updates ... 
	## Packages can be removed in a number of ways. From a command prompt they can be removed by 
		## R CMD REMOVE -l /path/to/library pkg1 pkg2 …
		## remove.packages(c("pkg1", "pkg2"), lib = file.path("path", "to", "library"))
		## Finally, one can just remove the package directory from the library.
		## R CMD INSTALL -l libdir pkg > pkg.log 2>&1 
		## R CMD check -l libdir --install=check:pkg.log pkg
		### Only 64-bit builds support ‘long vectors’, those with 2^{31} or more elements (which needs at least 16GB of storage for each numeric vector).
		## A little care is needed to use the random-number routines. You will need to supply the uniform random number generator
		##     double unif_rand(void)
		## or use the one supplied (and with a shared library or DLL you may have to use the one supplied, which is the Marsaglia-multicarry with an entry point
		##    set_seed(unsigned int, unsigned int)
		## build large MAN pages with SRC CODE ... 
		## GNU gettext: ...  Cairo and Pango libraries. Cairo version 1.2.0 or later and Pango version 1.10
		## For the best font experience with these devices you need suitable fonts installed: Linux users will want the urw-fonts package. 
		## bzip2 cairo fontconfig freetype fribidi glib2 harfbuzz libX11 libXext libXt libcurl libicu libjpeg libpng libtiff libtirpc libxcrypt ncurses pango pkgconf-pkg-config pcre2 readline tcl tk xz zlib
		## these require a full JDK
		## BLAS (Basic Linear Algebra Subprograms ... vs LAPACK
		## The instructions here are for Intel 64-bit (‘x86_64’) builds on macOS 10.13–10.15 (High Sierra, Mojave and Catalina), 11 (Big Sur), 12 (Monterey) and 13 (Ventura), or ‘Apple Silicon’ (‘arm64’) builds on macOS 11 (Big Sur), 12 (Monterey) and 13 (Ventura).
		
		## .Last <- function() system("R --vanilla")
		## > q("no")
		## https://stackoverflow.com/a/12540166/184614
		## https://stackoverflow.com/a/70841811/184614
		## https://stat.ethz.ch/R-manual/R-devel/library/base/html/Startup.html
		## FIRST ... 
		## https://stackoverflow.com/questions/30893829/running-rscript-in-command-line-and-loading-packages
		## https://www.rdocumentation.org/packages/utils/versions/3.6.2/topics/Rscript
		## http://127.0.0.1:12293/library/utils/html/Rscript.html
		## https://cran.r-project.org/doc/manuals/R-intro.html
		## factor(cut(incomes, breaks = 35+10*(0:7))) -> incomef
		## list.ABC <- c(list.A, list.B, list.C)
		## Recall that with vector objects as arguments the concatenation function similarly joined together all arguments into a single vector structure. In this case all other attributes, such as dim attributes, are discarded.
		## extract ...
		## The attach() function takes a ‘database’ such as a list or data frame as its argument. Thus suppose lentils is a data frame with three variables lentils$u, lentils$v, lentils$w. The attach
		## not available until detached/attached again???!!!
		## When invoked on a data frame or matrix, edit brings up a separate spreadsheet-like environment for editing. This is useful for making small changes once a data set has been read. The command
		## 8.1 R as a set of statistical tables
		## are variances EQUAL
		## var.test(A, B) ...          F test to compare two variances
		## %anything% ... # Defining new binary operators
		## what about unary?
		## Less frequently, a function will need to refer to components of ‘…’. The expression list(...) evaluates all such arguments and returns them in a named list, while ..1, ..2, etc. evaluate them one at a time, with ‘..n’ returning the n’th unmatched argument.
		## how about ...a, ...z for multiple dots ... 
		## If global and permanent assignments are intended within a function, then either the “superassignment” operator, <<- or the function assign() can be used. See the help document for details.
		## Lexical scope ... allows the list to contain functions ... pseudo-class ... 
		
		# > .First <- function() {
  # options(prompt="$ ", continue="+\t")  # $ is the prompt
  # options(digits=5, length=999)         # custom numbers and printout
  # x11()                                 # for graphics
  # par(pch = "+")                        # plotting character
  # source(file.path(Sys.getenv("HOME"), "R", "mystuff.R"))
       #                                 my personal functions
  # library(MASS)                         # attach a package
# }



		# methods(coef);; # notice the *
		# getAnywhere("coef.aov")
		# getS3method("coef", "aov")
		
		# X is the model matrix or design matrix 
		# formulae on the left side below s
		# 'formula' vs 'model'  
		## y ~ x  ... y ~ 1 + x
		## Both imply the same simple linear regression model of y on x. The first has an implicit intercept term, and the second an explicit one.
		# y ~ 0 + x ... 
		# Simple linear regression of y on x through the origin (that is, without an intercept term).
		## LOTS of examples for xls.FIT ... 
		## fitted.model <- lm(formula, data = data.frame)
		## demo(Hershey)  \\AR ... ARIES symbol 
		## text(locator(1), "Outlier", adj=0)
		
		# ## > oldpar <- par(no.readonly=TRUE)
  # … plotting commands …
# > par(oldpar)
# ### save/restoreState ... with key=DEFAULT ...
	 
# col.axis, lab, sub, main from PALETTE

## 12.7 Dynamic graphics ... GGobi by Swayne, Cook and Buja available from
## http://ggobi.org/	
## http://127.0.0.1:12293/doc/html/Search?pattern=is.function&fields.alias=1&fields.title=1&fields.concept=1&ignore.case=1&types.help=1&types.vignette=1&types.demo=1
## help.start()
## http://127.0.0.1:12293/library/base/html/is.function.html
## C:\Program Files\R\R-4.0.2\library\base\html
## I am *NOT* seeing v 4.2 RGU help html files ... maybe no changes?
## *** WHERE is the WEBSERVER running ***
## ... it does 404, SEARCH, CGI scripts are running ...
## http://127.0.0.1:12293/doc/html/index.html
## C:\_git_\-downloads-\-11-Aug-2022-\R-latest.tar\R-4.2.1\doc\html
## C:\_git_\-downloads-\-11-Aug-2022-\R-latest.tar\R-4.2.1\doc
## CRAN_mirrors.csv 
## C:\_git_\-downloads-\-11-Aug-2022-\R-latest.tar\R-4.2.1\src\modules\internet
## Rhttpd.c ... 
## C_startHTTPD ... where is ... 
## tools::startDynamicHelp 
## tools:::httpdPort
## https://stackoverflow.com/questions/51133736/deploy-rook-apps-in-r-internal-web-server
## svn revision 
## https://jeffreyhorner.tumblr.com/post/33814488298/deploy-rook-apps-part-ii
## https://github.com/jeffreyhorner/Rook/issues/7 


## PORT 80 -> 8008
## PORT 443 => 88 ... the perfect form 8, ancient egypt/hebrew ... SHA ...
 
 
 
	 
	# geany on WINDOZE, Linux, MacOS 
	# https://cran.r-project.org/web/packages/gert/index.html
	# Sphinx search ... Packages / R_DEV-forums, etc.
	# for files, I preprocess and remove MULTI-LINE comments 
	# https://wiki.geany.org/howtos/using_geany_with_r
	
	
	}
	
	
# would be nice if __FILE__ and __DIR__ worked with R ?!!!?	
# include_path=".:/php/includes"
# include_path=".;c:\php\includes"	

include_path.show = function()
	{
	 
	
	}
	
	
include_path.add = function(path, where="last")
	{
	# must be a string
	}
	
include_path.remove = function(path)
	{
	# string / idx 
	}
	
include_path.order = function(new.order)
	{
	# can be string or idx 
	# v.arrange with ALL elements ... 
	}



	






















































































#' includeLocalDirectory
#'
#' @param directory The local directory you will scan
#' @param verbose Display details regarding includes
#' @param pattern This is the extension of files to be sourced ( .R )
#' @param ... Pass extra parameters to the source function
#'
#' @return
#' @export
# include.localDirectory
includeLocalDirectory = function(path, verbose=TRUE, pattern = "[.][RrSsQq]$", ...)
  {
  # ?source
  for (nm in list.files(path, pattern = pattern))
    {
    if(verbose) { cat(nm,":"); }
    # source(file.path(path, nm), ...);
	  source(file.path(path, nm));
    if(verbose) { cat("\n"); }
    }
  }



#' includeLocalFiles
#'
#' This loops through an "array" of files and sources them.
#'
#' @param files Vector of full file paths
#' @param ...  Pass extra parameters to the source function
#'
#' @return
#' @export
#'
#' @examples
# include.localFiles
includeLocalFiles = function(files, ...)
  {
  for(file in files)
    {
    # source(file, ...);
	  source(file);
    }
  }

#' includeRemoteFiles
#'
#' @param urls
#' @param verbose
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
# include.remoteFiles
includeRemoteFiles = function(urls, pattern = "[.][RrSsQq]$", verbose=FALSE, ...)
  {
  # # github.includeFolder ...
# includeRemoteDirectoryGithub
# includeRemoteFiles("https://raw.githubusercontent.com/MonteShaffer/humanVerse/main/misc/functions-md5.R");

  idx = grep(pattern, urls);
  if(length(idx) > 0)
	{
	  goodurls = urls[idx];

	  cat (" INCLUDING:","\n","=========","\n");
	  for(url in goodurls)
		{
		myfile = getRemoteAndCache(url, ...);
		if(verbose) { cat("\t", url, " ===> \n"); }
		# source(myfile, ...);
		  source(myfile);
		# sourceMe(myfile);
		if(verbose) { cat("\t ... ",myfile); } else { cat("\t ... ",basename(myfile),"\n"); }
		}
	}
  }









