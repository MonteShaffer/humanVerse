
# TODO ???
# dir.getIncludes
# dir.addToIncludes
# dir.rankIncludes



#' @rdname is.dir
#' @export













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

							warning = function(w) #
								{
								warning(paste0("### WARNING ###  throws a warning","\n\n",w));
								# set KEY on INFO to w
								info = property.set("WARNING", info, w);
								if(!suppressWarnings) { warning(w); }
								# info; # let's still return the value 	
								return(info);
								},
		
							error = function(e) #
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




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
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
	# folder = dir.smartInclude(folder);
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




