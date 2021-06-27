
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
  # # github.includeFolder ...
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
		# sourceMe(myfile);
		if(verbose) { cat("\t ... ",myfile); } else { cat("\t ... ",basename(myfile),"\n"); }
		}
	}
  }









