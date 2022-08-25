
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

package.detach = function(pkg = "stringi", method = "lib
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

	
# plural intentional, but make aliases 	
packages.installed = function()
	{
	library()$results[,1];
	}
	
	
# .libPaths() # get library location
# library()   # see all packages installed
# search()    # see packages currently loaded	
# https://www.r-bloggers.com/2016/11/a-simple-guide-to-s3-methods/	
	
packages.attached = function()
	{
	(.packages());
	}
	
## eigenRank at package, attached, installed levels ...


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




	
	
	