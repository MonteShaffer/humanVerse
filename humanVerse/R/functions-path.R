
# x = c(getwd(), "C:/dsjkfklj/klsdjf/", "C:\\rtools42\\x86_64-w64-mingw32.static.posix\\bin\\c++.exe");
# scary ...  openSesame(x[3])
path.summary = function() {}
path.summary = function(path=getwd(), trailing = TRUE)
	{
	n = length(path); # multivariate 
	
	# is.file and is.dir fails on path=getwd() ... not a file 
	# fopen(path)  cannot open file 'C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R': Permission denied
	# ergo, its a path ?
	
	# logic BROKE with move over and removing STUFF ...
	# str.diff
	# TODO fix ... 
	# I HAVE LOST a / somewhere.
	# is.null(ext) vs is.empty(ext) ... ext == EMPTY ...
	# b/c I updated check.ext and made it multivariate 
	# quick.dir() broke?  is.dir ??? NODE overflow ... 
	# memory.logging keeps jamming with prep.dots(...) on df.row 
	# maybe need to go OLD-SCHOOL with df.row on MANUAL dots ...
	# how to GET NAMES ... ETC?
	## maybe it is the is.defined() function?
	## what changed ... DID I add a lazy-loading SOMEWHERE BAD?
	# regardless ... prep.dots needs EASY, MEDIUM, DIFFICULT ... 
	# a rewrite ... 
	b_	= basename_(path);
	#b 	= basename(path);
	e 	= check.ext(path);
	d 	= dirname(path);
	d_ 	= d;		# so subtraction works 
		if(trailing) { d_ = paste0(d_, SLASH); }		
	pp 	= check.dir(path, trailing=trailing, create=FALSE); # dir 
	pf 	= prep.path(path, trailing=trailing); 				# file 
				if_ = file.exists(pf);  # reserved word 
				id_ = dir.exists(pf);
			# I believe these are file.stat[us] and dir.stat[us] functions 
			# May include R/W forbidden 0777 info ?
			# Actually ... if_ may be `stat` on the INODE 
			#              id_ is that INODE a directory?
	 
	# subtract pd - d ... if it contains a SLASH
	di = pp %.-% d_;  # str.subract(a,b)
		
		status = rep("file", n);
		logic = str.contains(SLASH, di);
		status[logic] = "dir";
		
					# C:\_R_\humanVerse\SANDBOX\data\
	
	# do I want to REFORMAT to dataframE?
	info = list("type" 				= status,
				"exists" 			= if_,
				"stem" 				= b_,
				"ext" 				= e,
				"dir.R"				= d,
				"dir.humanVerse" 	= pp,
				# this is ultimately "my type test"	
				"diff.dir"			= di, 	
				"inode.file" 		= if_,  
				"inode.dir" 		= id_,
				"path.humanVerse"	= pf   # cleansed ... 
				);
	# print(str(info));
	info = property.set("more", info, file.info(pf));
	
	# minvisible(info, key="PATH_INFO", display=str);
	info;
	}



# maybe have openSesame, but this is generally me copying PATH from WINDOWZ into R ...
path.fromClipboard = function(trailing = TRUE)
	{
	x = readClipboard();
	prep.path(x);	# prep.path cleanses .. check.path verifies it 
	}

# > y = path.fromClipboard()
# > y
# [1] "C:/_git_/github/MonteShaffer/humanVerse/humanVerse/inst/R/"
# > openSesame(y)














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
	

	
# rework this with INCLUDE in mind and new memory KEYS 




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
