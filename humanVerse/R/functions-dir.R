

dir.setSeparator = function(force=NULL)
	{
	if(!is.defined(DIR_SEPARATOR)) { constants.default(); }
	if(is.null(force))
		{	
		DIR_SEPARATOR = DIR_LINUX;
		if(is.windows()) { DIR_SEPARATOR = DIR_WINDOZE; }
		} else { DIR_SEPARATOR = force; } # manually force ... 
	
	# this needs to be modified to NAMESPACE of library
	# keep ls() clean ... 
	# %NAMESPACE% could also be functions ...
	# DIR_SEPARATOR %NAMESPACE%.   #(. would be humanVerse, but any allowed)
	DIR_SEPARATOR %GLOBAL%.;   
	DIR_SEPARATOR;  # should be mute ... seems like R internally handles LINUX forms on windows ... 
	}

# do I need a multivariate normalizePath ... or is that deprecated with what I am doing ... 
tmp.dir = function()
	{
	d = prep.path( tempdir(check=TRUE) );
	d;
	}


