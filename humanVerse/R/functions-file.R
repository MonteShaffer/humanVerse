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
#' @param encoding by default UTF-8 ... Windows is ANSI_X3.4-1986 or RStudio default is ISO-8859-1
#'
#' @return
#' @export
storeToFile = function (str, file)
	{
  cat(str, file=file, append=FALSE);
	}


storeToPipe = function(df, file, header=TRUE, quote="", sep="|", row.names=FALSE)
  {
  if(quote == "") { quote = FALSE; }
  utils::write.table(df, file=file, quote=quote, col.names=header, row.names=row.names, sep=sep);
  }

readFromPipe = function(file, header=TRUE, quote="", sep="|")
  {
  utils::read.csv(file, header=header, quote=quote, sep=sep);
  }


includeLocalDirectory = function(directory, verbose=TRUE, ...)
  {
  # ?source
  for (nm in list.files(path, pattern = "[.][RrSsQq]$"))
    {
    if(verbose) { cat(nm,":"); }
    source(file.path(path, nm), ...);
    if(verbose) {cat("\n"); }
    }
  }

includeLocalFiles = function(files, ...)
  {
  for(file in files)
    {
    source(file, ...);
    }
  }

# includeRemoteDirectoryGithub
# includeRemoteFiles("https://raw.githubusercontent.com/MonteShaffer/humanVerse/main/misc/functions-md5.R");
includeRemoteFiles = function(urls, verbose=FALSE, ...)
  {
  cat (" INCLUDING:","\n","=========","\n");
  for(url in urls)
    {
    myfile = getRemoteAndCache(url, ...);
    if(verbose) { cat("\t", url, " ===> \n"); }
    source(myfile, ...);
    if(verbose) { cat("\t ... ",myfile); } else { cat("\t ... ",basename(myfile),"\n"); }
    }
  }


getRemoteAndCache = function(remote, local.file = NULL,
    tmp.folder = "/humanVerse/cache/", force.download = FALSE,
    md5.hash = FALSE)
  {
  useTEMP = FALSE;
  if(!is.null(local.file))
    {
    localpath = dirname(local.file);
    createDirectoryRecursive(localpath);
    if(!dir.exists(localpath)) { useTEMP = TRUE; }
    } else { useTEMP = TRUE; }

  if(useTEMP)
    {
    subfolder = folderizeURL( dirname(remote));
    filestem = basename(remote);
    if(md5.hash) { filestem = md5(filestem); }

    tmp = gsub("\\","/",Sys.getenv("TMP"), fixed=TRUE);
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

  if(force.download)
    {
    if(file.exists(myfile))
      {
      file.copy(myfile, paste0(myfile,"-",as.numeric(Sys.time())));
      }
    }
  if(!file.exists(myfile))
    {
    downloadFile(remote, myfile);
    }
  # if(return) { return (content); }
  myfile;
  }


downloadFile = function(remote, myfile, n=999999, quiet = TRUE, ...)
  {
  if(capabilities("libcurl"))
    {
    download.file(remote, myfile, quiet = quiet, ...);
    } else {
            raw.binary = readBin(remote, "raw", n);
            # what if I don't have stringi ???   ... encoding = "UTF-8"
            url.encoding = stringi::stri_enc_detect(raw.binary)[[1]]$Encoding[1];
            raw.out = iconv(readBin(content, character()), from = encoding, to = "UTF-8");
            writeChar(raw.out, myfile);
            }
  }







http.headers = function(remote)
  {
  # requires libcurl
  curlGetHeaders(remote);
  }


http.status = function(headers)
  {
  # requires libcurl
  attributes(headers)$status;
  }


http.headerValue = function(headers, which="Content-Length:")
  {
  # requires libcurl
  # "Content-Length: 30528\r\n"
  for(header in rev(headers)) # reverse so we get past redirects
    {
    if(is.substring(header, which))
      {
      size = trimMe( gsub(which, "", header, fixed=TRUE) );
      return( as.integer( size ) );
      }
    }
  FALSE;
  }

http.size = function(headers)
  {
  # requires libcurl
  # "Content-Length: 30528\r\n"
  http.headerValue(headers, "Content-Length:");
  }



