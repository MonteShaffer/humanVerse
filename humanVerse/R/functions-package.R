
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

# https://stackoverflow.com/questions/1567718/getting-a-function-name-as-a-string
# as.character(substitute(package))
# assign(function.names[2], get(function.names[2]))
# deparse(quote(foo.bar))
#  as.character(substitute(f))
# deparse(substitute(fn));
#function.getName = function(fn)
#	{
#	fn.str = as.character(substitute(fn));
#	fn.str;	
#	}
	
###' @rdname obj.getName
###' @export
##obj.getName = function.getName;

## ====> moved to str 

# https://stackoverflow.com/questions/1567718/getting-a-function-name-as-a-string

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



package.listALL = function()
	{
	
	}
	
	
	