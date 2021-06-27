#' createDirectoryRecursive
#'
#' @param folder the folder to be created
#'
#' @return
#' @export
#'
#' @examples
#' # createDirectoryRecursive("R:/monte/says/hi/");
createDirectoryRecursive = function(folder)
  {
  if(!dir.exists(folder))
    {
    dir.create(folder, recursive=TRUE);
    }
  }


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
readFromPipe = function(file, header=TRUE, quote="", sep="|")
  {
  utils::read.csv(file, header=header, quote=quote, sep=sep);
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
getRemoteAndCache = function(remote, local.file = NULL,
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
    localpath = dirname(local.file);
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

	filestem = cleanup.local(filestem, append=append)

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
#' @param unlink
#'
#' @return
#' @export
#'
#' @examples
moveFile = function(src, dest, unlink=TRUE)
	{
		src 	= as.character(src);
		dest 	= as.character(dest);
	file.copy(src, dest);  # there is no file.move ???
	unlink(src);
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
# TODO
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







