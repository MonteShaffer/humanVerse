




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


