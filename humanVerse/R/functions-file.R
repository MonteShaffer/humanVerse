#' createDirectoryRecursive
#'
#' @param folder the folder to be created
#'
#' @return
#' @export
#'
#' @examples
#' createDirectoryRecursive("R:/monte/says/hi/");
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
#' writeLine("hello there", file="R:/monte/says/hi/again/my.log", append=FALSE);
#' writeLine("hi again", file="R:/monte/says/hi/again/my.log");
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



readRDS.url = function(file)
	{
	readRDS(url(file));
	}


#' writeToPipe
#'
#' @param df dataframe to be stored
#' @param file filename and path
#' @param header whether or not to add a header
#' @param quote whether or not to add quotes
#' @param sep "pipe" means sep="|" but you could change
#' @param row.names whether or not to include row names
#'
#' @return
#' @export
#'
#' @examples
#'
#' # alias storeToPipe
writeToPipe = function(df, file, header=TRUE, quote="", sep="|", row.names=FALSE)
  {
  if(quote == "") { quote = FALSE; }
  utils::write.table(df, file=file, quote=quote, col.names=header, row.names=row.names, sep=sep);
  }

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
#'
#' @examples
includeLocalDirectory = function(directory, verbose=TRUE, pattern = "[.][RrSsQq]$", ...)
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

# # includeGithubFolder ...
# includeRemoteDirectoryGithub
# includeRemoteFiles("https://raw.githubusercontent.com/MonteShaffer/humanVerse/main/misc/functions-md5.R");
includeRemoteFiles = function(urls, verbose=FALSE, ...)
  {
  cat (" INCLUDING:","\n","=========","\n");
  for(url in urls)
    {
    myfile = getRemoteAndCache(url, ...);
    if(verbose) { cat("\t", url, " ===> \n"); }
    # source(myfile, ...);
	  source(myfile);
    if(verbose) { cat("\t ... ",myfile); } else { cat("\t ... ",basename(myfile),"\n"); }
    }
  }



	
	
# source('C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-get-set.R')
# mySource('C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-get-set.R')
sourceMe = function(myfile, key = "local", indexFunctions = TRUE)
	{
	if(!indexFunctions)
		{
		source(myfile);
		} else  {
				indexFunctionsInFile(myfile, key=key); # this will store to cache	
				source(myfile);
				}	
	}


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


installGithubLibrary = function()
	{

	#
	# {
  # "id": 294247360,
  # "node_id": "MDEwOlJlcG9zaXRvcnkyOTQyNDczNjA=",
  # "name": "humanVerseWSU",
  # "full_name": "MonteShaffer/humanVerseWSU",
  # "private": false,


	# https://api.github.com/
	# "repository_url": "https://api.github.com/repos/{owner}/{repo}",
	# "repository_url": "https://api.github.com/repos/MonteShaffer/humanVerseWSU",

	# check out remotes ...

	# remote_download.github_remote <- function(x, quiet = FALSE) {
  # if (!quiet) {
    # message("Downloading GitHub repo ", x$username, "/", x$repo, "@", x$ref)
  # }

  # dest <- tempfile(fileext = paste0(".tar.gz"))
  # src_root <- build_url(x$host, "repos", x$username, x$repo)
  # src <- paste0(src_root, "/tarball/", utils::URLencode(x$ref, reserved = TRUE))

  # download(dest, src, auth_token = x$auth_token)





  # github_remote <- function(repo, ref = "HEAD", subdir = NULL,
                       # auth_token = github_pat(), sha = NULL,
                       # host = "api.github.com", ...) {

  # meta <- parse_git_repo(repo)
  # meta <- github_resolve_ref(meta$ref %||% ref, meta, host = host, auth_token = auth_token)

	}

includeGithubFolder = function(url, ...)  # pattern = "[.][RrSsQq]$",
	{
	args = grabFunctionParameters();
        # TRUE would store "last" in some memory (GLOBAL SCOPE)
	# args = .GlobalEnv$.args = grabFunctionParameters();
	# print(args);

	# stop("monte");

	force.cache = FALSE;  # maybe move as a parameter ???
	if(!force.cache)
		{
		# may live in ... as force.download ...
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
		}

	cat("\n", "force.cache ... ", force.cache, "\n\n");


	### Could we do API/JSON instead of HTML CACHING?

	### github.api = "https://api.github.com/";
	## https://api.github.com/repos/MonteShaffer/humanVerse/git/trees/main
	##  ==> https://api.github.com/repos/MonteShaffer/humanVerse/git/trees/75741912434181b468b761303eaa3ec312998e1d
	### if(type == "blob") AND extension = ".R" ... include ...
	html.local = getRemoteAndCache(url, ...);
	html.cache = gsub(".html", ".cache", html.local);

	if(file.exists(html.cache) && !force.cache)
		{
		cat("\n", "============", "GRABBING FROM CACHE", "============", "\n");
		links = as.character( unlist( readFromPipe(html.cache, header=FALSE) ) );
		} else {
		    cat("\n", "============", "DOWNLOADING DIRECTORY PAGE", "============", "\n");
				html.str = readStringFromFile(html.local);

				github.base = "https://github.com/";
				github.raw = "https://raw.githubusercontent.com/";

				# MonteShaffer/humanVerse/tree/main/humanVerse/R/
				# MonteShaffer/humanVerse/blob/main/humanVerse/R/functions-colors.R


				# https://raw.githubusercontent.com/MonteShaffer/humanVerse/main/humanVerse/R/globals.R


				html.search = gsub(github.base, "", url, fixed = TRUE);
				html.search = gsub("/tree/", "/blob/", html.search, fixed = TRUE);

				html.keys = explodeMe(html.search, html.str);
				n = length(html.keys)

				html.raw = paste0(github.raw, gsub("/blob/", "/", html.search, fixed = TRUE) );

				links = c();
				for(i in 2:n)
					{
					str = html.keys[i];
						link = paste0(html.raw, explodeMe("\"",str)[1]);
						# do a check that it has the write extension ... # pattern = "[.][RrSsQq]$",
					links = c(links, link);
					}
				storeToPipe(as.data.frame(links), html.cache, header=FALSE);
				}

	includeRemoteFiles(links, ...);
	}



readStringFromFile = function(myFile, n = NULL, method ="readChar", source = "local")
	{
	# methods are "readChar" or "readLines"
	# readChar is one long string; readLines is a vector broken on "\n"
	if(source == "remote")
		{
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


deleteLocalCacheFolder = function(folder)
  {

  }




# https://github.com/MonteShaffer/humanVerse
# https://github.com/MonteShaffer/humanVerse/blob/main/humanVerse/R/functions-algebra.R
# https://raw.githubusercontent.com/MonteShaffer/humanVerse/main/humanVerse/R/functions-algebra.R

getSourceLocation = function(tmp.folder = "/humanVerse/cache/")
  {
  tmp = gsub("\\","/",Sys.getenv("TMP"), fixed=TRUE); # windoze?
  mypath = paste0(tmp, tmp.folder);
  mypath;
  }


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

getRemoteAndCache = function(remote, local.file = NULL,
    tmp.folder = "/humanVerse/cache/", force.download = FALSE,
    verbose = FALSE, md5.hash = FALSE)
  {
  useTEMP = FALSE;
  trailingSlash = (.substr(remote, -1) == "/");
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

  if(useTEMP)
    {
	subfolder = if(trailingSlash) {  folderizeURL(remote); } else { folderizeURL(dirname(remote)); }
	filestem  = if(trailingSlash) {  "index.html" } else { basename(remote); }

    if(md5.hash) { filestem = md5(filestem); }

    tmp = gsub("\\","/",Sys.getenv("TMP"), fixed=TRUE); # windoze?
    mypath = paste0(tmp, tmp.folder, subfolder);
    createDirectoryRecursive(mypath);
    myfile = paste0(mypath,"/",filestem);
    } else {
            mypath = dirname(local);
            filestem = basename(local);
            if(md5.hash) { filestem = md5(local); }
            createDirectoryRecursive(mypath);
            myfile = paste0(mypath,"/",filestem);
            }
    myfile = setAttribute("path", mypath, myfile);
    myfile = setAttribute("filestem", filestem, myfile);

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


moveFile = function(src, dest, unlink=TRUE)
	{
	file.copy(src, dest);  # there is no file.move ???
	unlink(src);
	}


downloadFile = function(remote, myfile, n=(2^31 - 1), quiet = TRUE, ...)  # n could be 2^31 - 1
  {
  if(isTRUE(capabilities("libcurl")))
    {
    download.file(remote, myfile, quiet = quiet, ...);
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







