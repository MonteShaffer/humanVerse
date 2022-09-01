


# ?Syntax ... not ?Symbols or ?Operators
# .end question ... THIS ... based on idx in array (matrix, dataframe, vector)
# abort()
# http://127.0.0.1:14469/library/base/html/Reserved.html
# > ?"for"
# > ?Control
# > ?Reserved
# help...
# is.name((y = substitute(stats))); as.character(y);
# is.symbol is more precise ... 
# casting topic to a string as well ... 
# ischar <- tryCatch(is.character(topic) && length(topic) ==     1L, error = function(e) FALSE)
#  deparse1(substitute(topic))
## utils:::index.search ???
## ?find.package
## packages <- .packages(TRUE, lib)
## .libPaths()  

## launch browser, custom template ... reorganize the HELP data ... into tabs?
## TEXT only ... ( x = library(help = package, lib.loc = NULL, character.only = TRUE) )
## x.help = x$info[[2]];
## NOT even odd, "/t" separated ?
## x.more = str.removeWhiteSpace(x.help, "|", pre.trim=FALSE);
## lines = str.removeWhiteSpace(x.help, "|"); # if not a key|value, overflow from previous line ... 
	
# df = help.parseFromLibrary("stats"); head(df); str(df)
# mask.ok ... they can make mistakes on NAMING, we can't 
# fn and line number ?
help.parseFromLibrary = function(pkg = "base", lib.loc = NULL, ...)
	{
	x = library(help = pkg, lib.loc = lib.loc, character.only = TRUE, ...);
	x.help = x$info[[2]];
	
	### if not a key|value, overflow from previous line ... 
	lines = str.removeWhiteSpace(x.help, "|"); 
	n = length(lines);
	res = list();
	plin = NULL;
	for(i in 1:n)
		{
		line = lines[i];
		lin = str.explode("|",line);
		if(!is.na(lin[2]))
			{
			res[[ lin[1] ]] = lin[2];		
			} else {
					res[[ plin[1] ]] = paste0( res[[ plin[1] ]] , lin[1] );
					}
		plin = lin;  # first one can't be EMPTY
		}

	df = as.data.frame( cbind( names(res), unname(unlist(res)) ) );
		colnames(df) = c("search", "short.description");
		
	# is.function
	# cat( normalizePath("C:/PROGRA~1/R/R-42~1.1/library/base") );
	# find.package(pkg); # [1] "C:/PROGRA~1/R/R-42~1.1/library/base"
	# utils:::index.search("sum", find.package("base") ); # [1] "C:/PROGRA~1/R/R-42~1.1/library/base/help/sum"
	## doesn't always work, is the location of the help file, not src ...
	## whereis 
	# df$whereis = utils:::index.search(df$search, find.package(pkg) );
	# C:\Program Files\R\R-4.2.1\library\base\R
	# lazyLoad(   file.path(system.file("help", package=pkg), pkg),  envir = e )
	# base.rdx and base.rdb
	
unser <- function(s){
  i <- as.numeric(s)
  return(rdx$variables[[i]])
}

readRDB <- function(filename, offset, size, type = 'gzip') {
       f <- file(filename, 'rb')
       on.exit(close(f))
       seek(f, offset + 4)
       unserialize(memDecompress(readBin(f, 'raw', size - 4), type), refhook=unser)
}

	# filename: the .rdb file
# offset, size: the pair of values from the .rdx
# type: 'gzip' if $compressed is TRUE, 'bzip2' for 2, 'xz' for 3
# https://stackoverflow.com/a/62394942/184614
readRDB <- function(filename, offset, size, type = 'gzip') {
        f <- file(filename, 'rb')
        on.exit(close(f))
        seek(f, offset + 4)
        unserialize(memDecompress(readBin(f, 'raw', size - 4), type))
}
		
	df = property.set("path", df, x$path);
	df = property.set("dcf", df, x$info[[1]]);
	
	df;
	}
	
	
help.index = function(deep = TRUE) {}

## str(x.help);
# ? help.start
# prompt() builds default HELP page ...
## key values as odd even 
## all = 1:length(x.help);  keys = all[is.odd(all)]; vals = all[is.even(all)];
## x.info = list.create(x.help[keys], x.help[vals]);
### x.info = x.help[vals];
### names(x.info) = x.help[keys];
## This lists all topics ... 

## port <- tools::startDynamicHelp(NA)
## package = "base";
## browser = getOption("browser")
## browseURL(paste0("http://127.0.0.1:", port, "/library/", package, "/html/00Index.html"), browser)




# https://www.alphr.com/how-to-get-transcript-youtube-video/#:~:text=Open%20YouTube%20and%20the%20video%20you%20want.&text=Tap%20on%20the%20three%20dots%20below%20the%20video%20to%20select%20Open%20transcript.&text=Click%20on%20the%20three%20dots%20on%20the%20transcription.&text=Select%20the%20transcription%20using%20the%20mouse.

# https://github.com/eddelbuettel/gsir-te/blob/master/docs/Getting-Started-in-R.pdf
# https://github.com/eddelbuettel/gsir-te/blob/master/docs/Getting-Started-in-R.p1p2.png
# ?X is help(X)
# examples(X)
# apropos("rand")

# > ?%in%
# Error: unexpected SPECIAL in "?%in%"
# > help("%in%")
# > ?`%in%`

# break out "+" ... ESCAPE key

# sign(-2.9)


# https://www.tinyverse.org/
# tools::package_dependencies(package="liteq", recursive=FALSE, db=AP)$liteq
# tools::package_dependencies(package="liteq", recursive=TRUE, db=AP)$liteq

# AP = readRDS("https://cran.r-project.org/web/packages/packages.rds");

# tools::package_dependencies(package="liteq", recursive=TRUE)[["liteq"]]
# installed.packages()
# tools::installed_packages();

# https://www.analyticsvidhya.com/blog/2016/05/data-table-data-frame-work-large-data-sets/
# multi-line comments ... https://www.programiz.com/r/comments

help.packageInfo = function(package, key="dependencies", ...)
	{
	# number of authors
	# function to PARSE DESCRIPTION FILE
	
	tools::package_dependencies(package=package, recursive=TRUE)[[package]];
	}


# pkg <- "stats"
# desc_path <- system.file("DESCRIPTION", package = pkg)
# (z = read.dcf(desc_path) )

#  packagePath <- find.package(package, lib.loc, quiet = TRUE)

# https://www.crockford.com/javascript/
# https://www.crockford.com/javascript/javascript.html
# https://en.wikipedia.org/wiki/JavaScript
# https://en.wikipedia.org/wiki/Java_(programming_language)



