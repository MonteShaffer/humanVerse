
## have local / remote for these ?
path.setBase = function() {}
path.getBase = function() {}   
path.source = function() {}   ## try to build filepath with BASE or WD
## path.source("./../../myfolder/ksdajf.html");
path.translate = function(pathstr, basestr=NULL) {}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' path.init
#'
#'
#'
#' @export
#'
#' @examples
path.init = function(verbose = TRUE)
	{
	key = "wd";
	memory.init();

	wd = getwd();
	if(verbose)
		{
		## NOTICE, MESSAGE, WARNING, ERROR [ECMA-like]
		msg = paste0("\n\n", str.commentWrapper(" WORKING DIRECTORY getwd() RETURNS ", r.tag = "-", s.pad=15), "\n\n",
					"ACTUAL:  ", "\t", wd, "\n\n",
					"WINDOZE: ", "\t", (normalizePath(wd)), "\n",
					"\n\n");
		warning(msg);
		}	
	
	(.GlobalEnv$.humanVerse[["path"]][[key]] = wd );
	}



#' @rdname path.getWorkingDirectory
#' @export
path.getWorkingDirectory = path.init;



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' path.get
#'
#'
#' @param key (what is the unique storage 'key')
#'
#' @export
#'
#' @examples	
path.get = function(key="wd")
	{
	memory.init();
	if(!exists(key, .GlobalEnv$.humanVerse[["path"]]))
		{
		if(key == "wd") { path.init(); } else {	stop(paste0("Nothing to get as key: ", key, " not 'set' yet!")); }
		}
	.GlobalEnv$.humanVerse[["path"]][[key]];
	}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' path.set
#'
#' This is a function to get/set a PATH (local or http?)
#'
#' @param key NAME of the attribute
#' @param value VALUE to set NAME attribute
#'
#' @return 
#' @export
#'
#' @examples
path.set = function(key="wd", value, checkPath=FALSE)
	{
	memory.init();
	# they can pass in a file
	.GlobalEnv$.humanVerse[["path"]][[key]] = dirname(value);  
	}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' path.setAsWorkingDirectory
#'
#' This is a function to get/set a PATH (local or http?)
#'
#' @param key NAME of the attribute
#'
#' @return 
#' @export
#'
#' @examples
path.setAsWorkingDirectory = function(key="wd")
	{
	path = path.get(key);
	setwd( path );
	path.init();
	}


# C:\_git_\github\MonteShaffer\humanVerse\HVcpp
path.copyToClipboard = function(key="wd")
	{
	path = path.get(key);	
	if(is.windows())
		{
		writeClipboard ( normalizePath(path) );
		} else {
				writeClipboard ( (path) );
				}
	}



path.showAll = function()
	{
	.GlobalEnv$.humanVerse[["path"]];
	} 

path.openInExplorer = function(key = "wd")
	{
	path = path.get(key);
	utils::browseURL(path);
	}
	


# cat(normalizePath(c(R.home(), tempdir())), sep = "\n")
# list.files(R.home("bin"))
# https://stackoverflow.com/questions/64476043/
## cmd = paste("explorer",  gsub('/', '\\\\', myDir, fixed=TRUE ) );
##  suppressWarnings( shell( cmd ) );        # R:\data\state-capitals\final\

## path.user ... base path ... based on user ... from R-sys.env
## path.device
## this can be RELATIVE or ABSOLUTE
## get/set is a bit of a collision because of getwd / setwd
## https://stackoverflow.com/questions/1815606/determine-path-of-the-executing-script

# https://stat.ethz.ch/R-manual/R-devel/library/utils/html/sourceutils.html
# https://github.com/gagolews/teaching_data
