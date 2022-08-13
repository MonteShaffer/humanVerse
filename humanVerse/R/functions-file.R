


#' @rdname is.dir
#' @export
is.dir = dir.exists;





dir.getIncludes
dir.addToIncludes
dir.rankIncludes



dir.smartPath = function(relative, base.path=NULL)
	{
	
	}


dir.getSeparator = function(file.sep = "")
	{
	sep = .Platform[["file.sep"]];					# this is WRONG on WINDOZE?
	if(is.windows()) { sep = "\\"; }				# this is the WINDOZE form
	if(file.sep != "") { sep = file.sep; }		# manual OVERRIDE
	sep;
	}

dir.cleanupPath = function(path, file.sep="")
	{
	sep = dir.getSeparator(file.sep);
	str.replace(c("/", "\\"), sep, path);
	}


dir.normalizePath = function(path, ..., suppressWarnings=TRUE)
	{
	path = path[1];  # # normalizePath is multivariate, key this univariate
	
	
	path.info = tryCatch	(

							{
							info = normalizePath(path, ...);
							},

							warning = function(w)
								{
								warning(paste0("### WARNING ###  throws a warning","\n\n",w));
								# set KEY on INFO to w
								info = property.set("WARNING", info, w);
								if(!suppressWarnings) { warning(w); }
								# info; # let's still return the value 	
								return(info);
								},
		
							error = function(e)
								{
								# warning(paste0("### ERROR ###  throws an error","\n\n",e));
								info = property.set("ERROR", info, e);
								if(!suppressWarnings) { warning(e); }
								# info; # let's still return the value 
								return(info);
								# res = FALSE;
								# res = property.set("ERROR", res, e);
								# return (res);
								},

							finally =
								{
					
								}
							);
	return(path.info);
	}


## https://www.urbandictionary.com/define.php?term=Git-R-Done

## fp = file.open = open
## file.close = close

##  cat(stri_info(short = TRUE))
## file:///C:/Users/Monte%20J.%20Shaffer/Desktop/v103i02.pdf
## https://stackoverflow.com/questions/7779032/validate-a-character-as-a-file-path

testme = "C:\\_git_\\github\\MonteShaffer\\humanVerse\\humanVerse\\R\\functions-HC.R";

# file.exists(testme);
# dir.exists( dirname(testme) );


##################################################
#'
#' file.init;
#'
#' @param my.path
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
#' WARNING:  OneDrive, DropBox may have file-lock ... CACHE, DATA, CODE are separate

file.init = function(
	CONFIG = paste0(Sys.getenv("R_USER"),"/R/humanVerse/"),
	CACHE = paste0(Sys.getenv("TEMP"),"/R/humanVerse/"),
	DATA = "C:/_R-DATA_/",
	CODE = "C:/_git_/github/MonteShaffer/humanVerse/notebooks/-functions-/",
	base.path = getwd(),
	verbose = TRUE)
	{


	}


file.init = function(path.data, path.code, path.humanVerse, base.path="", verbose = TRUE)
file.init = function(my.path = "C:/_R-TEMP_/", verbose = TRUE,
	CONFIG = paste0(Sys.getenv("R_USER"),"/R/humanVerse/"),
	CACHE = paste0(Sys.getenv("TEMP"),"/R/humanVerse/"),
	DATA = "C:/_R-DATA_/",
	CODE = "C:/_git_/github/MonteShaffer/humanVerse/notebooks/-functions-/",
	base.path = getwd()
;
						CACHE="/humanVerse/CACHE/", append.cache = TRUE,
						DATA="C:/_R-DATA_/", 			append.data  = TRUE,
						CODE="/-CODE-/",			append.code  = TRUE )
  {
	my.path = normalizePath(my.path);

	msg = list();
	msg[["INIT"]] = paste0(" INITIALIZING FILESYSTEM WITH _PATH_ ", "\n\t\t\t", my.path, "\n\n");
	msg[["STORING"]] = paste0(" STORING _PATH ", "\n\t\t\t", my.path, "\n\n");

	if(verbose) { cat(msg$INIT); }

	

	dir.createDirectoryRecursive(my.path);

	if(verbose) { cat(msg$STORING); }

	path.init(); # sets wd
	path.set("_PATH_", my.path);

	# file.path("E:", "DATA", "example.csv")

	my.pathCACHE = if(append.cache) { normalizePath( paste0(my.path, CACHE), mustWork=FALSE ); } else { normalizePath( CACHE, mustWork=TRUE ); }

	dir.createDirectoryRecursive(my.pathCACHE);
	path.set("_CACHE_", my.pathCACHE);

	my.pathDATA = if(append.data) { normalizePath( paste0(my.path, DATA), mustWork=FALSE ); } else { normalizePath( DATA, mustWork=TRUE ); }

	dir.createDirectoryRecursive(my.pathDATA);
	path.set("_DATA_", my.pathDATA);

	my.pathCODE = normalizePath( paste0(my.path, CODE), mustWork=FALSE );
	my.pathCODE = if(append.code) { normalizePath( paste0(my.path, CODE), mustWork=FALSE ); } else { normalizePath( CODE, mustWork=TRUE ); }

	dir.createDirectoryRecursive(my.pathCODE);
	path.set("_CODE_", my.pathCODE);
  }



##################################################
#'
#' dir.createDirectoryRecursive
#'
#' @param folder the folder to be created
#'
#' @return
#' @export
#'
#' @examples
#' # dir.createDirectoryRecursive("R:/monte/says/hi/");
#' dir.createDirectoryRecursive("aldkj"); # ... will create in getwd()
dir.createDirectoryRecursive = function(folder, verbose=TRUE)
  {
	folder = dir.smartInclude(folder);
	msg = list();
	msg[["EXISTS"]] = paste0(" DIRECTORY ", "\n\t\t\t", folder, "\n\n\t", "already exists", "\n\n");
	msg[["ATTEMPT"]] = paste0(" ATTEMPTING TO CREATE DIRECTORY ", "\n\t\t\t", folder, "\n\n");
	msg[["SUCCESS"]] = paste0(" SUCCESS ", "\n\t\t\t", folder, "\n\n");
	msg[["FAILURE"]] = paste0(" FAILURE ", "\n\t\t\t", folder, "\n\n");

			
  if(dir.exists(folder))
	{
	if(verbose) { cat(msg$EXISTS); }
    }
	else
		{
		if(verbose) { cat(msg$ATTEMPT); }

		dir.create(folder, recursive=TRUE);

		if(dir.exists(folder))
			{
			if(verbose) { cat(msg$SUCCESS); }
			} else {
					if(verbose) { cat(msg$FAILURE); return(FALSE);}
					}
		}
	return(TRUE);
    }

#' @rdname createDirectoryRecursive
#' @export
createDirectoryRecursive = dir.createDirectoryRecursive;




##################################################
#'
#' file.readFrom;
#'
#' @param file
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
#' WARNING:  OneDrive, DropBox may have file-lock ... CACHE, DATA, CODE are separate
file.readFrom = function(file, ..., method="stringi")
	{
	mmm = functions.cleanKey(method, 3);

	if(mmm == "csv" || mmm == "pip")
		{
		# PIPE / CSV with allowed comments
		if(missing(header) ) 		{ header = TRUE;}
		if(missing(quote) )  		{ quote = ""; 	}
		if(missing(sep) )			{ sep = "|";	}
		if(missing(comment.char) )	{ comment.char = "#"; }
		return( utils::read.csv(file, header=header, sep=sep, quote=quote, 
									comment.char=comment.char, ...) );
		}

	if(mmm == "tab")  # table
		{
		return( utils::read.table(file, ...) );
		}

	if(mmm == "rds")
		{
		return( readRDS(file) );
		}

	if(mmm == "jso")  # JSON
		{
		# maybe call this function again with "str" to get the stringi form.
		# json 	= rjson::fromJSON(json_str = readChar(file, file.info(file)$size), ...);
		# switch to jsonlite ???
		return( jsonlite::read_json(file=file, ...) );
		}

	# readChar is one long string; readLines is a vector broken on "\n"

	if(mmm == "str")  # stringi
		{
		# file:///C:/Users/Monte%20J.%20Shaffer/Desktop/v103i02.pdf
		x = stringi::stri_read_raw(file);
		if(!is.set(from))
			{
			y = stringi::stri_enc_detect(x);
			from = y[[1]][1,]$Encoding;  # most probable
			}
		if(!is.set(to)) { to = "UTF-8"; }
		z = stringi::stri_encode(x, from = from, to = to);
		return (z);

		## also has a readLines ...  stri_read_lines("ES_latin1.txt", encoding = "ISO-8859-1")
		}


	if(mmm == "cha")  # readChar
		{
		return( readChar(file, file.info(file)$size) );
		}

	if(mmm == "lin")  # readLines
		{
		if(missing(n) ) 		{ n = 10^5;} # guessing [pass in the value]
		return( readLines(myFile, n) );
		}

	if(mmm == "bin")  # binary
		{
		if(missing(what) ) 		{ what = "raw";}
		if(missing(n) ) 		{ n = 10^5;} # guessing [pass in the value]
		return( readBin(file, what, n=n, ...) );
		}

	if(mmm == "dcf" || mmm =="deb")  # debian
		{
		return( read.dcf(file, ...) );
		}

	stop(paste0("Appropriate Method [",method,"] was not found!"));

	}




















dir.createDirectoryRecursive
dir.getDirectoryPath
dir.deleteLocalCacheFolder
dir.getSourceLocation


file.readFrom  RDS, PIPE, CSV, BIN, STR (lines), RDS(remote)
file.writeTo
file.getDirectoryName # dirname
file.move  # file.rename(from, to)
file.writeLine

# is this url.download() not file?
file.download = function() {} 
	# Note that you cannot use devtools::install_github() because it uses curl ;)
	# install.packages("https://github.com/jeroen/curl/archive/master.tar.gz", repos = NULL)
	# https://jeroen.cran.dev/curl/

# curl::curl_version()
# libcurlVersion()
## https://github.com/jeroen/curl/issues/276


/*
library(curl)

repro <- function(n) {
  urls <- paste0("https://httpbingo.org/get?q=", 1:n)

  make_handle <- function(url) new_handle(url=url)

  pool <- new_pool()

  fail <- function(msg) cat("failed connection:", msg, "\n")

  done <- function(data) cat("status:", data$status_code, "\n")

  for(u in urls)
    multi_add(make_handle(u), done=done, fail=fail, pool=pool)

  stat <- multi_run(timeout=10, pool=pool)

  cat("remaining:",  stat$pending, "\n")
}
*/




























































##################################################
#'
#' createDirectoryRecursive
#'
#' @param folder the folder to be created
#'
#' @return
#' @export
#'
#' @examples
#' # createDirectoryRecursive("R:/monte/says/hi/");
createDirectoryRecursive = function(folder, verbose=TRUE)
  {
	msg = list();
	msg[["EXISTS"]] = paste0(" DIRECTORY ", "\n\t\t\t", folder, "\n\n\t", "already exists", "\n\n");
	msg[["ATTEMPT"]] = paste0(" ATTEMPTING TO CREATE DIRECTORY ", "\n\t\t\t", folder, "\n\n");
	msg[["SUCCESS"]] = paste0(" SUCCESS ", "\n\t\t\t", folder, "\n\n");
	msg[["FAILURE"]] = paste0(" FAILURE ", "\n\t\t\t", folder, "\n\n");

			
  if(dir.exists(folder))
	{
	if(verbose) { print(msg$EXISTS); }
    }
	else
		{
		if(verbose) { print(msg$ATTEMPT); }

		dir.create(folder, recursive=TRUE);

		if(dir.exists(folder))
			{
			if(verbose) { print(msg$SUCCESS); }
			} else {
					if(verbose) { print(msg$FAILURE); }
					}
		}
	
    }



#' @rdname createDirectoryRecursive
#' @export
createDirectoryRecursive = dir.createDirectoryRecursive;



#' writeLine
#'
#' This function writes a single character string to a file.
#' Very useful for simulations and building data one line at a time.
#' [ encoding is slow ]
#'
#' @param str The character string to be written
#' @param append If TRUE, will append to the end of the file, otherwise it will overwrite an existing file
#' @param end EOL character to finish the line; the line separator
#' @param file The file to store the (str) line
#'
#' @return
#' @export
#'
#' @examples
#' # writeLine("hello there", file="R:/monte/says/hi/again/my.log", append=FALSE);
#' # writeLine("hi again", file="R:/monte/says/hi/again/my.log");
writeLine = function(str, file, append=TRUE, end="\n")
  {
  cat( paste(str,end,sep=""),
      file=file,
        sep="", append=append );
  }

#' storeToFile
#'
#' Store a string to a file (e.g., an HTML page downloaded).
#'
#' @param str The string to store
#' @param file The file to store the string (it will override).
#'
#' @return
#' @export
storeToFile = function (str, file)
	{
	cat(str, file=file, append=FALSE);
	}



#' readRDS.url
#'
#' This wraps 'url' and 'readRDS' so they are webfriendly ...
#'
#' @param file The file is likely a URL in this function
#'
#' @return a data object, likely a dataframe or a list
#' @export
#'
readRDS.url = function(file)
	{

	file = cleanup.url(file);
	# kudos to antonio
	readRDS( url(file) );
	# https://stackoverflow.com/questions/19890633/
	# readRDS( RCurl::getURL(file, ssl.verifypeer=0L, followlocation=1L));
	}


#' writeRDS
#'
#' The opposite of readRDS is writeRDS, make it so.
#'
#' @param obj The object to be stored
#' @param myfile The file to store the object
#'
#' @return
#' @export
#'
writeRDS = function(obj, myfile)
	{
	saveRDS(obj, file=myfile);
	}


#' writeToPipe
#'
#' This is the inverse of 'readFromPipe'
#'
#' @param df dataframe to be stored
#' @param file filename and path
#' @param header whether or not to add a header
#' @param quote whether or not to add quotes
#' @param sep "pipe" means sep="|" but you could change
#' @param row.names whether or not to include row names
#'
#' @return NOTHING, it writes
#' @export
#'
#' @aliases storeToPipe
writeToPipe = function(df, file, header=TRUE, quote="", sep="|", row.names=FALSE)
  {
  if(quote == "") { quote = FALSE; }
  utils::write.table(df, file=file, quote=quote, col.names=header, row.names=row.names, sep=sep);
  }


#' readFromPipe
#'
#' This is the inverse of 'writeToPipe'
#'
#' @param file filename and path
#' @param header whether or not to add a header
#' @param quote whether or not to add quotes
#' @param sep "pipe" means sep="|" but you could change
#'
#' @return a dataframe
#' @export
readFromPipe = function(file, header=TRUE, quote="", sep="|", comment.char="#")
  {
  utils::read.csv(file, header=header, quote=quote, sep=sep, comment.char = comment.char);
  }

#' file.readLines
#'
#' @param file
#' @param n
#' @param skip
#'
#' @return
#' @export
#'
#' @examples
file.readLines = function(file, n=-1, skip=NULL)
	{
	# why base::readLines doesn't have skip ?!?
	# where did the fopen/fread stuff go ... that would enable skip
	content = readLines(file, n=n);
	nlen = length(content); # how many lines ?
	if(!is.null(skip))
		{
		if(skip < nlen)
			{
			content = content[skip:nlen];
			}
		}
	paste(content, collapse="\n");
	}








isForceDownload = function(args)
	{
	force.download = FALSE;
	if(exists("args"))
			{
			if(exists(".dots.keys.", where=args))
				{
				if(is.element("force.download", args$.dots.keys.))
					{
					# idx = which(args$.dots.keys. == "force.download");
					force.download = args$.dots.vals.$force.download;
					}
				}
			}
	force.download;
	}






#' readStringFromFile
#'
#' @param myFile
#' @param n
#' @param method
#' @param source
#'
#' @return
#' @export
#'
#' @examples
readStringFromFile = function(myFile, n = NULL, method ="readChar", source = "local")
	{
	# methods are "readChar" or "readLines"
	# readChar is one long string; readLines is a vector broken on "\n"
	if(source == "remote")
		{
		myFile = cleanup.url(myFile);
		if(is.null(n)) { n = if(method == "readLines") { n = -1; } else { n = (2^31 - 1); } }
		} else {
				if(is.null(n)) { n = if(method == "readLines") { n = -1; } else { n = file.info(myFile)$size; } }
				}

	if(method == "readLines")
		{
		readLines(myFile, n);
		} else 	{
				readChar(myFile, n);
				}
	}








#' getSourceLocation
#'
#' @param tmp.folder
#' @param create
#'
#' @return
#' @export
#'
#' @examples
getSourceLocation = function(tmp.subfolder = "/humanVerse/cache/", create=FALSE)
  {
  my.tmp = Sys.getenv("HUMANVERSE_CACHE");
	if(trimMe(my.tmp) == "") { my.tmp = Sys.getenv("TMP"); }
	if(trimMe(my.tmp) == "") { my.tmp = Sys.getenv("TEMP"); }
	if(trimMe(my.tmp) == "")
		{
		message.stop ("Function: *getSourceLocation* requires \n\t a HUMANVERSE_CACHE or TMP or TEMP folder \n\t in your 'Sys.getenv()' \n   Maybe run 'Sys.setenv(\"HUMANVERSE_CACHE\" = \"/path/to/CACHE\")' \n\t and make certain the directory is made and writeable \n\t as in 'mkdir /path/to/CACHE' ");
		}


  tmp = gsub("\\", "/", paste0(my.tmp,"/") , fixed=TRUE); # windoze?
  mypath = paste0(tmp, tmp.subfolder);
  mypath = cleanup.local(mypath);
	if(create) { createDirectoryRecursive(mypath); }

  mypath;
  }


#' getDirectoryPath
#'
#' @param file
#' @param trailing
#'
#' @return
#' @export
#'
#' @examples
getDirectoryPath = function(file, trailing=TRUE)
	{
	dn = dirname(file);
	dn = str_replace("\\", "/", dn); # windoze issues
	if(trailing)
		{
		paste0(dirname(file), "/");
		} else 	{
				dirname(file);
				}
	}





#' getRemoteAndCache
#'
#' @param remote
#' @param local.file
#' @param tmp.folder
#' @param force.download
#' @param verbose
#' @param md5.hash
#' @param append
#'
#' @return
#' @export
#'
#' @examples
getRemoteAndCache = function(remote, local.file = NULL, local.pre = "TMP"
    tmp.folder = "/humanVerse/cache/", force.download = FALSE,
    verbose = FALSE, md5.hash = FALSE, append = "")
  {
  remote = cleanup.url(remote);
  useTEMP = FALSE;
  trailingSlash = ( lastChar(remote) == "/");
  if(verbose)
    {
    cat("\n", "remote ... ", remote, "\n\n");
    cat("\n", "force.download ... ", force.download, "\n\n");
    }
  if(!is.null(local.file))
    {
    localpath = paste0( dirname(local.file), "/", local.pre, "/" );  # project subfolder
		createDirectoryRecursive(localpath);
    if(!dir.exists(localpath)) { useTEMP = TRUE; }
    } else { useTEMP = TRUE; }

if(verbose)
    {
    cat("\n", "useTEMP ... ", useTEMP, "\n\n");
    }

  if(useTEMP)
    {
	subfolder = if(trailingSlash) {  folderizeURL(remote); } else { folderizeURL(dirname(remote)); }
	filestem  = if(trailingSlash) {  "index.html" } else { basename(remote); }

	# if(!isFALSE(append.me)) { filestem = cleanup.local(filestem); } # this will append ".html" if necessary ...

	filestem = cleanup.local(filestem, append=append);

    if(md5.hash) { filestem = md5(filestem); }

	mypath = getSourceLocation(subfolder);
		createDirectoryRecursive(mypath);
    myfile = paste0(mypath,"/",filestem);
    } else {
			mypath 		= dirname(local.file);
				createDirectoryRecursive(mypath);
			filestem 	= basename(local.file);
			myfile 		= local.file;
            }


	myfile 		= cleanup.local(myfile);
	mypath 		= cleanup.local(mypath);
	filestem 	= cleanup.local(filestem);

    myfile = setAttribute("path", 		mypath, 	myfile);
    myfile = setAttribute("filestem", 	filestem, 	myfile);

  if(verbose)
    {
    cat("\n", "myfile ... ", myfile, "\n\n");
    }

  # cat("\n", "mypath ... ", mypath, "\n\n");

  if(force.download)
    {
    if(file.exists(myfile))
      {
	    mypath.b = paste0(mypath, "/.backup/");  createDirectoryRecursive(mypath.b);
	    myfile.b = paste0(mypath.b, "/", filestem, "-", as.integer(Sys.time()) );

      # file.copy(myfile, myfile.b);  # this is not file.move, doesn't exist
	  # unlink(myfile);

	    moveFile(myfile, myfile.b);
      }
    }
  if(!file.exists(myfile))
    {
    downloadFile(remote, myfile, cacheOK = !force.download);
    }
  myfile;
  }


#' moveFile
#'
#' @param src
#' @param dest
#' @param delete.src
#'
#' @return
#' @export
#'
#' @examples
moveFile = function(src, dest, delete.src=TRUE)
	{
		src 	= as.character(src);
		dest 	= as.character(dest);
	file.copy(src, dest);  # there is no file.move ???
	if(delete.src) { unlink(src); }
	}


#' deleteLocalCacheFolder
#'
#' @param folder
#'
#' @return
#' @export
#'
#' @examples
deleteLocalCacheFolder = function(folder)
  {
# TODO # unlink("tmp", recursive = TRUE)
  }


#' downloadFile
#'
#' @param remote
#' @param myfile
#' @param n
#' @param quiet
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
downloadFile = function(remote, myfile, n=(2^31 - 1), quiet = TRUE, mode="wb", ...)  # n could be 2^31 - 1
  {
  if(isTRUE(capabilities("libcurl")))
    {
    utils::download.file(remote, myfile, quiet = quiet, mode=mode, ...);
    } else {
			# this approach is not working ... maybe readChar
            raw.binary = readBin(remote, "raw", n);
            # what if I don't have stringi ???   ... encoding = "UTF-8"
            url.encoding = "UTF-8";
			if( isTRUE(requireNamespace("stringi", quietly = TRUE)) )
				{
				url.encoding = stringi::stri_enc_detect(raw.binary)[[1]]$Encoding[1];
				}
            raw.out = iconv( readBin(raw.binary, character()), from = url.encoding, to = "UTF-8");
            writeChar(raw.out, myfile);
            }
  }







