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


#' cleanup.local
#'
#' @param myfile The original filename, may be a URL with ? = & symbols
#'
#' @return A windoze-friendly filename
#' @export
#'
#' @examples
#' cleanup.local("index.html?endYear=1920&amount=1000000");
#'
cleanup.local = function(myfile)
	{
	myfile = str_replace("//", "/",   myfile);
	myfile = str_replace("?", "^-QUESTION-^",   myfile);
	myfile = str_replace("&", "^-AND-^",   myfile);
	myfile = str_replace("=", "^-EQUAL-^",   myfile);
	myfile;
	}


#' cleanup.url
#'
#' @param url URL to be cleansed
#'
#' @return cleansed URL
#' @export
#'
#' @examples
#' cleanup.url("https://www.myprofiletraits.com//");
#' cleanup.url("https:/www.mshaffer.com//arizona//");
#'
cleanup.url = function(url)
	{

	url = str_replace("//", "/",   url);
	url = str_replace(":/", "://", url); # https://
	url;
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


#' includeLocalDirectory
#'
#' @param directory The local directory you will scan
#' @param verbose Display details regarding includes
#' @param pattern This is the extension of files to be sourced ( .R )
#' @param ... Pass extra parameters to the source function
#'
#' @return
#' @export
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
includeRemoteFiles = function(urls, pattern = "[.][RrSsQq]$", verbose=FALSE, ...)
  {
  # # includeGithubFolder ...
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
		if(verbose) { cat("\t ... ",myfile); } else { cat("\t ... ",basename(myfile),"\n"); }
		}
	}
  }





#' sourceMe
#'
#' @param myfile
#' @param key
#' @param indexFunctions
#'
#' @return
#' @export
#'
#' @examples
sourceMe = function(myfile, key = "local", indexFunctions = TRUE)
	{
  # source('C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-get-set.R')
# mySource('C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-get-set.R')

	if(!indexFunctions)
		{
		source(myfile);
		} else  {
				indexFunctionsInFile(myfile, key=key); # this will store to cache
				source(myfile);
				}
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




listGithubFiles = function(github.user="MonteShaffer", github.repo="humanVerse", github.path="", ...)
	{
	args = getFunctionParameters();
	force.download = isForceDownload(args);
	cat("\n", "force.download ... ", force.download, "\n\n");
	
	url = buildGithubPath(github.user, github.repo);
	url = paste0(url, github.path);
	
	res = parseGithubList(url, force.download = force.download);
		res = setAttribute("url", url, res);
		res = setAttribute("force.download", force.download, res);
	res;
	}



buildTarFromGithubRepo = function(github.user="MonteShaffer", github.repo="humanVerse", force.download=FALSE)
	{
	url = buildGithubPath(github.user, github.repo);	
	res = parseGithubList(url, force.download = force.download);
	
	zip.url = getAttribute("zipclone", res);
	if(is.null(zip.url))
		{
		stop("no zip.url attached to url");
		}
	
	## This is a redirect ... 
	## <html><body>You are being <a href="https://codeload.github.com/MonteShaffer/humanVerse/zip/refs/heads/main">redirected</a>.</body></html>
	## https://stackoverflow.com/questions/25474682/rcurl-geturlcontent-detect-content-type-through-final-redirect
	## h <- basicTextGatherer()
	## x = getBinaryURL('http://timesofindia.indiatimes.com//articleshow/2933019.cms',                  headerfunction = h$update, curl = curl)
	
	# require(RCurl)
	# agent="Firefox/23.0" 
	# curl.fun = basicTextGatherer();
	# curl.ch = getCurlHandle();
	
#	x = getBinaryURL(zip.url, curl = curl.ch )
	## maybe add this to below function
	
#	myzip = getRemoteAndCache(zip.url, force.download=force.download);
	
	
	
	}

#' installGithubLibrary
#'
#' @return
#' @export
#'
#' @examples
installGithubLibrary = function(github.user="MonteShaffer", github.repo="humanVerse", github.path="", pattern = "\\.zip$", github.version="latest", ...)
	{
	## This doesn't grab the clone element, assumes you have a .zip uploaded ...
	## https://stackoverflow.com/questions/67144476/
	## you could download "clone", look for .Rproj, BUILD that folder to ZIP, then INSTALL 
	
	## https://github.com/MonteShaffer/humanVerse/tree/main/humanVerse
	
	## https://github.com/r-lib/devtools/archive/refs/heads/master.zip
	## for me, this is the outer layer ...
	## https://github.com/MonteShaffer/humanVerseWSU/archive/refs/heads/master.zip
	## unzip ... look for github.repo ... humanVerse.Rproj ... or /R /man DESCRIPTION NAMESPACE in folder to figure out which folder ... maybe '.Rbuildignore'
	## build ...  ?build vs ?devtools::build
	## just build a .tar.gz from github ...
	
	
	# .git.ignore is not allowing the other ... 
	# build-source ==> .tar.gz
	# build-binary ==> .zip  [WINDOZE]
	# maybe ==> .tgz 
	# ?install.packages
	github.df = listGithubFiles(github.user=github.user, github.repo=github.repo, github.path=github.path, ...);
	
	idx.zips = grep("\\.zip$", github.df$links);
	idx.tars = NULL;
	# idx.tars = grep("\\.tar.giz$", github.df$links);
	
	
	github.idx = c(idx.zips, idx.tars);
	if(length(github.idx) == 0)
		{
		stop("no candidates");
		}	
	
	github.candidates = github.df[github.idx,] ;
	## 
	
	cat("\n\n =-=-=-=-=-=-=-=-=-=- CANDIDATES =-=-=-=-=-=-=-=-=-=- \n\n");
	cat("\n\n", "  [ latest ---> ]", paste(github.candidates$name, collapse="\n\t"), "\n\n\n");
	
	github.zip = github.candidates[1,];  # these are sorted by latest ... 
	
	if(github.version != "latest")
		{
		# github.version is wildcard, grab first ...
		grx = utils::glob2rx(github.version);
		grx.grep = grep(grx, github.candidates$links);
			if(length(github.grep) > 0)
				{
				my.idx = grx.grep[1]; # first match 
				github.zip = github.candidates[my.idx,];
				}
		}
	
	cat("\n\n", paste( github.zip[c(1,2)], collapse = "\t\t"), "\n\n\n");
	zip.url = as.character(github.zip[8]);	
	
	cat("\n\n", "\t\t", "      FROM: ", as.character(github.zip[8]), "\n\n");
	myzip = getRemoteAndCache(zip.url, ...);
	
	cat("\n\n", "\t\t", "TO INSTALL: ", as.character(github.zip[1]), "\n\n");
	install.packages(myzip, repos=NULL, type="source");
		# unzips into folder 'C:/Users/Alexander Nevsky/Documents/R/win-library/4.0'
	}


buildGithubPath = function(github.user="MonteShaffer", github.repo="humanVerse", which="http")
	{
	if(which == "raw")
		{
		paste0("https://raw.githubusercontent.com/", github.user, "/", github.repo, "/");
		} else 	{				
				paste0("https://github.com/", github.user, "/", github.repo, "/");
				}	
	}


	

parseGithubList = function(url, force.download = FALSE)
	{	
	html.local = getRemoteAndCache(url, force.download = force.download);
	html.cache = str_replace(".html", ".cache", html.local);
	
	github.base = "https://github.com/";
	github.raw 	= "https://raw.githubusercontent.com/";
	
	### Could we do API/JSON instead of HTML CACHING?
		### github.api = "https://api.github.com/";
		## https://api.github.com/repos/MonteShaffer/humanVerse/git/trees/main
		##  ==> https://api.github.com/repos/MonteShaffer/humanVerse/git/trees/75741912434181b468b761303eaa3ec312998e1d
		### if(type == "blob") AND extension = ".R" ... include ...
		### less to parse with HTML

	if(file.exists(html.cache) && !force.download)
		{
		cat("\n", "============", "GRABBING FROM CACHE", "============", "\n");
		# results = as.character( unlist( readFromPipe(html.cache) ) );
		# results = readFromPipe(html.cache);
		results = readRDS(html.cache);
		} else {
		    cat("\n", "============", "DOWNLOADING DIRECTORY PAGE", "============", "\n");
				html.str = readStringFromFile(html.local);
								
				page.info = sliceDiceContent(html.str, start='<div class="js-details-container Details">', end='<div class="Details-content--shown', strip=FALSE, direction="start");
				
				results = NULL;
				page.rows = explodeMe('<div role="row"', page.info);
				nr = length(page.rows);
				for(i in 2:nr)
					{
					row = explodeMe('<span', page.rows[i]);
						row.dt = explodeMe("T", sliceDiceContent(row[3], start='datetime="', end='"', strip=TRUE, direction="start") );
					
						
						
						row.link = sliceDiceContent(row[2], start='href="', end='"', strip=TRUE, direction="start");
							tmp = explodeMe("/", row.link); ntmp = length(tmp);
						row.name = tmp[ntmp];
						row.commit = sliceDiceContent(row[3], start='href="', end='"', strip=TRUE, direction="start");
						row.commit.info = sliceDiceContent(row[3], start="\n", end='<a>', strip=TRUE, direction="start");
						
						row.time = paste0(row.dt[1], " ", str_replace("Z", "", row.dt[2]) );
						
						# install_github("Displayr/flipTime")
						# https://github.com/Displayr/flipTime/blob/master/R/asdatetime.R
						
											
					rinfo = c(row.name, row.time, row.link, row.commit, row.commit.info);
					
					results = rbind(results, rinfo);
					}
				
				results = as.data.frame(results);
					colnames(results) = c("name", "when", "url", "commit", "commit.info");
					# rownames(results) = results$name; # should be unique
					rownames(results) = NULL;
				results$when.time = asDateTime(results$when);
				results = sortDataFrameByNumericColumns(results,"when.time","DESC"); # newest first
				results$folder = !is.substring(results$url, "/blob/"); # blobs are files ...
				
					links = cleanup.url( paste0(github.raw, results$url) );		
					links = str_replace("/blob/", "/", links);
				results$links = "";
				results$links[ which(!results$folder) ] = links [ which(!results$folder) ];
				
				# pipe loses "attributes" ... 
				zipclone = sliceDiceContent(html.str, start='data-open-app="link" href="', end='">', strip=FALSE, direction="start");
				zipclone = cleanup.url( paste0(github.base, zipclone) );
				
				results = setAttribute("zipclone", zipclone, results);
				
				# writeToPipe(results, html.cache);
				writeRDS(results, html.cache);
				}
	results;
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
					force.cache = args$.dots.vals.$force.download;
					}
				}
			}
	force.download;
	}
	
	
#' includeGithubFolder
#'
#' @param url
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
includeGithubFolder = function(url, ...)  # pattern = "[.][RrSsQq]$",
	{
	# args = grabFunctionParameters();
	args = getFunctionParameters();
        # TRUE would store "last" in some memory (GLOBAL SCOPE)
	# args = .GlobalEnv$.args = grabFunctionParameters();

	##cat("\n\n === MY-ARGS === \n\n");

	##print(args);

	# stop("monte");

	force.download = isForceDownload(args);
	cat("\n", "force.download ... ", force.download, "\n\n");

	github.df = parseGithubList(url, force.download = force.download);	
	github.df = subsetDataFrame(github.df, "folder", "==", FALSE);
		
	links = na.omit(github.df$links);
	
	includeRemoteFiles(links, ...);
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





#' getSourceLocation
#'
#' @param tmp.folder
#'
#' @return
#' @export
#'
#' @examples
getSourceLocation = function(tmp.subfolder = "/humanVerse/cache/")
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
	createDirectoryRecursive(mypath);
  
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
#'
#' @return
#' @export
#'
#' @examples
getRemoteAndCache = function(remote, local.file = NULL,
    tmp.folder = "/humanVerse/cache/", force.download = FALSE,
    verbose = FALSE, md5.hash = FALSE)
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
	file.copy(src, dest);  # there is no file.move ???
	unlink(src);
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
downloadFile = function(remote, myfile, n=(2^31 - 1), quiet = TRUE, ...)  # n could be 2^31 - 1
  {
  if(isTRUE(capabilities("libcurl")))
    {
    utils::download.file(remote, myfile, quiet = quiet, ...);
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







