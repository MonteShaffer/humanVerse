

# .libPaths( c( "~/userLibrary" , .libPaths() ) )


pkg.help = function()
	{
	
	
	}
	
	
pkg.version = function(..., character.only=FALSE)
	{
	pkgs = prep.dots(..., collapse=character.only, has.objects=!character.only, default="Rcpp");
	if(!character.only) { pkgs = as.character(pkgs); }
	packageVersion(pkgs);
	}


pkg.install = function(..., character.only=FALSE)
	{
	pkgs = prep.dots(..., collapse=character.only, has.objects=!character.only, default="Rcpp");
	if(!character.only) { pkgs = as.character(pkgs); }

	# I lost the DOTS, this is default 
	install.packages(pkgs);
	}
	
	
	

	
pkg.installed = function(..., character.only=FALSE)
	{
	pkgs = prep.dots(..., collapse=character.only, has.objects=!character.only);
	if(!character.only) { pkgs = as.character(pkgs); }
	
	if(length(pkgs) == 0)
		{
		# all = (.packages(all.available=TRUE));  # sessioninfo vs sessionInfo
		all = as.data.frame( library()$results ); 
		libs = unique(all$LibPath);
		all$LibPath = NULL;
		all = property.set("LibPath", all, libs);
		return( minvisible(all, display=head)); 
		}
	
	# just check.pkg ?  # TRUE / FALSE
	check.pkgs(pkgs, as.character=TRUE);	
	}


	
pkg.include = function() {}
pkg.attach = function()
	{
	# wraps the library function, allow backup method ?
	
	}
	
	
pkg.detach = function()
	{
	# wraps the library function, allow backup method ?
	# detach(package:ggplot2,unload=TRUE)
	# package which was attached by library.  # or apparently require 
	# The object to detach. Defaults to search()[pos]. This can be an unquoted name or a character string but not a character vector. If a number is supplied this is taken as pos.
	
	# detach(package:splines)
	# pkg <- "package:splines"
	# detach(pkg, character.only = TRUE)

	
	}
	
	

pkg.attached = function()
	{
	# TRUE / FALSE 
	
	}









## do map of functions within a library
## map functions to base::, utils:: and other libraries
## append full function name,  base::fn or base:::fn (if hidden)
## build fn-fn adjacency matrix, compute eigenRank of that libraries functions 
## assume function is frome library unless otherwise noted ...

# lsf.str("package:stats")
# ls("package:stats", all=TRUE);  # include ".fn" # has to be loaded 
# https://stackoverflow.com/questions/20535247/how-to-find-all-functions-in-an-r-package
# EXPORTED or NOT
## HIDDEN
# x1 = length(setdiff(ls(getNamespace("stringr")), getNamespaceExports("stringr")))
## 
# x2 = length( ls(getNamespace("stringr"),all=TRUE) );
## includes EXPORTS magittre
# x3 = length( getNamespaceExports("stringr") );
# count  ls(getNamespace("stringr"),all=TRUE)
# functions-packages ... list packages (loaded vs installed)
# https://rpubs.com/Mentors_Ubiqum/list_packages
# my_packages <- library()$results[,1]  # lists all installed
# (.packages());  # lists all loaded 
# tolower on both ... "stringr" %in% my_packages;
# packageVersion()
#
# df.panelize ... key|val|comment
#  CL( = C( ... count left parenthes, CL{, CL[, CPIPE (saving as piped)
#  C= ... C<- ... C% ... C'#, C#, C++, C+, C-, C*, C/, C\\, C%%, C%*%, C%in%
#  count string length of comments at header, inline, and code strlen 
#  count number of parameters, C... , C. , C; , C_, Clower, Cupper
#  C!, C^, C~, C`, C&, C&&, C|, C|| [how to remove pipe] ... C!^, C!^^
#  C[dsp], C[sp], C[\t], C[\n], C", C', C@, C$, 
#  C:::, C::, C: [do cascading, so : isn't inflated] , same with ...
#  count aliases, number of parameters in a function, strlen of non comment
#  count number of examples, has vignette?
######## do hclust on these features ... store features in key|val|comment
######## by function (filename) ... scan from source files as processing -ONE-
######## pkg|function|key|val|comment ... 
######## separately,  store   (comment line #, formal line #, body line number
########       pkg|filename|function|cline.s/e|fline.s/e|bline.s/e
# count strlen after @
# count lines in comment body (how many consecutive lines with #'
# count lines after examples
# HCLUST will be very interesting ...
# count function calls ... store as base::fn or base:::fn ... search / lookup
#  TOKYO DB would be good to speed this up ... 
## dcf parse ... # of chars in file, lengths of all keys, # authors, # depends, # imports, etc.
#' @return numeric vector that contains the indexes of *all* min FREQ elements, not just the *first*
#' @export
#'
#' @examples


# https://statsandr.com/blog/an-efficient-way-to-install-and-load-r-packages/

package.install = function(pkg, ...)
	{
	pkg = str.fromObjectName(pkg, parent="pkg");	
	if(!is.character(pkg)) { pkg = as.character(substitute(pkg)); }
	#package.installed ... 
	# library woroks on pkg/pkg.str 
	# install.packages() only on pkg.str not pkg ... 
	#github wrapper here ???
	
	}
	
	
package.version = function(pkg)
	{
	# allow str or obj for pkg
	# packageVersion() 
	# Citation
	}
	

	



# works like library, but scans all functions/files
# /R ... MAYBE future ... and /inst /src ???
# builds and caches a -ONE.R- file stored in CACHE /R-libraries/
# advanced dynamics can grab info from CRAN
# if CRAN links to GITHUB, grab info from GITHUB
# cache with date YYYYMMDDHHMMSS ... and most recent (two copies)
# by default, just grab most recent 
# but can rebuild ... 
HV.library = function() {}

# package.detach = function(pkg = "stringi", method = "lib
package.attach = function(pkg = "stringi", method="library")
	{
	# require(pkg);
	# library(pkg);
	
	
	
	}

# You cannot detach either the workspace (position 1) nor the base package (the last item in the search list), and attempting to do so will throw an error.

package.info = function(pkg = "stringi")
	{
	x = library(help = pkg);
	x;

	
	}


packages.version = function(pkgs)
	{
	# pkgs are strings 
	
	
	}

	
# plural intentional, but make aliases 
								# [b]y-[l]ibrary  ... [a]s-[l]ist 
								# [r]aw ... [a]s-[d]ataframe
packages.installed = function(return="by-library", add.version=TRUE)
	{
	r = prep.arg(return, n=1, keep="-");
	# functions.inPackage(sessioninfo) vs functions.inPackage(sessionInfo)
	# all shows lowercase, folder shows upper case .. need to map to all 
	# all = (.packages(all.available=TRUE));
	all = as.data.frame( library()$results ); 
	if(r == "r" || r == "a-d") { return(all); }
	mypaths = .libPaths();
	n = length(mypaths);
	res = vector("list", n);
	for(i in 1:n)
		{
		# res[[i]] = list.dirs(mypaths[i], full.names = FALSE, recursive = FALSE);
		s = subset(all, LibPath == mypaths[i]);
		res[[i]] = s$Package;
		res[[i]] = property.set("titles", res[[i]], s$Title);
		}
	names(res) = mypaths;
	res;
	}
	 
	
# .libPaths() # get library location
# library()   # see all packages installed
# search()    # see packages currently loaded	
# https://www.r-bloggers.com/2016/11/a-simple-guide-to-s3-methods/	
	
packages.attached = function()
	{
	# grep("^package:", search(), value = TRUE)
	# loadedOnly <- loadedNamespaces()
	# could have referenced a package in search() ls() with stringi::XXX without loading it ... 
	## lapply("stats", packageDescription, encoding = NA)
	## PRIORITY = "base" 
	## lapply("MASS", packageDescription, encoding = NA)
	## PRIORITY = recommended
	##  packageDate("stats")
	## file.path(.Library, "base")
	## > .Library ##  "C:/PROGRA~1/R/R-42~1.1/library"
	## notice it is WIN-ENCODE vs "C:/Program Files/R/R-4.2.1/library "




	(.packages());
	}
	
## eigenRank at package, attached, installed levels ...

# f <- read.fortunes(system.file("fortunes", "fortunes.csv", package = "fortunes"))
## HOW often do they call parse() in base, utils, etc.
## THEY are allowed, we are not ... GOOSE and GANDER
# R> library(fortunes)
# R> fortune("parse")
# 
# If the answer is parse() you should usually rethink the question.
#    -- Thomas Lumley
#       R-help (February 2005)
# 
# R>




	
	
	