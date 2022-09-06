
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' property.set 
#'
#' This is a function to get/set the "attributes"/"attr"
#'
#' @param key NAME of the attribute
#' @param value VALUE to set NAME attribute
#' @param obj OBJECT to be updated
#'
#' @return obj UPDATED with attribute
#' @export 
#'
#' @examples
# .get has to be key first ... SYSTEM 
# set key on obj with value 
property.set = function(key, obj, value=NULL, 
									property.type="attributes",
									as.null = FALSE,
									recycle = FALSE
									)
	{
	pt = prep.arg(property.type, 1);



	if(is.null(value) && !as.null)  # you may actually want to pass the value NULL in the "setter"
		{
		if(is.list(key))
			{
			k = names(key);
				names(key) = NULL;
			value = unlist(key);
			key = k;
			} else { stop("ERROR with key/val"); }
		}

	# allows multivariate ... keys determine the replacements
	# if value gets to end, it will recycle to beginning
	n.key = length(key);
	n.val = length(value);
	idx.v = 1;
		
	if(pt == "a")
		{
		if(recycle)
			{
			for(i in 1:n.key)
				{
				attributes(obj)[[ key[i] ]] = value[idx.v];
				idx.v = 1 + idx.v; if(idx.v > n.val) { idx.v = 1; }  # allows for looping gracefully if UNEVEN
				}
			} else { attributes(obj)[[ key[1] ]] = value; }
		return (obj);
		}
	if(pt == "s" || pt == "e")  # System Environment
		{
		res = list();
		for(i in 1:n.key)
			{
			str = paste0('Sys.setenv("',key[i],'" = "',value[idx.v],'")');
					eval(parse(text=str));
			res[i] = (Sys.getenv(key[i]));
			idx.v = 1 + idx.v; if(idx.v > n.val) { idx.v = 1; }  # allows for looping gracefully if UNEVEN
			}
		return(res);
		}

	}

 

#' @rdname setAttribute
#' @export
setAttribute = property.set;



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' property.get
#'
#' This is a function to get/set the "attributes"/"attr"
#'
#' @param key NAME of the attribute
#' @param value VALUE to set NAME attribute
#' @param obj OBJECT to be updated
#'
#' @return obj UPDATED with attribute
#' @export
#'
#' @examples
# has to be "key" first, or "system" has to pass in a NULL
property.get = function(key, obj, 
								property.type="attributes",
								wildcard = "keys-values"	
								)
	{
# dput(property.type);  
	pt = prep.arg(property.type, n=1);
#dput(wildcard);
	w  = prep.arg(wildcard, n=1, keep = "-");
#dput(pt);
	if(pt == "a")  # attributes
		{
		res = attributes(obj)[[key]];
#dput(obj); 
#dput(res); 
		return (res);
		}


	if(pt == "s" || pt == "e")  # System Environment
		{
		res = Sys.getenv(key);
		# w == "k-v" means we look normally, than cascade our search from keys to values ... 
		if(res == "")
			{
			# let's try wildcard on Sys.getenv(keys) ... vals?
			vals = Sys.getenv();
			keys = names(vals);
				idxs.keys = regex.wildcardSearch(keys, key);			
			is.k = ( length(idxs.keys) > 0 );
				idxs.vals = regex.wildcardSearch(vals, key);
			is.v = ( length(idxs.vals) > 0 );
			
			if(!is.k && !is.v) { return(""); } # LIKE NORMAL
				res.keys = vals[idxs.keys];
				res.vals = vals[idxs.vals];			
			
			if(w == "k" && is.k) { return( res.keys ); }
			if(w == "v" && is.v) { return( res.vals ); }
			
			# search keys, than vals
			if(w == "k-v" && is.k) { return( res.keys ); }
			if(w == "k-v" && is.v) { return( res.vals ); }
			
			# search vals, then keys
			if(w == "v-k" && is.v) { return( res.vals ); }
			if(w == "v-k" && is.k) { return( res.keys ); }
			
			return(""); # we tried.
			}
			
		# names(s <- Sys.getenv());
		# if key had * and res is empty, # do wildcard search on names 
		# Sys.getenv("RSTUDIO*");
		# s[grep("^L(C|ANG)", names(s))]
		# property.get("RSTUDIO*", NULL, "system");
		# res = (s = Sys.getenv() );
		return (res);
		}
	}




#' @rdname getAttribute
#' @export
getAttribute = property.get;



# as dataframe?
property.getALL = function(obj=NULL, property.type="attributes")
	{
	pt = prep.arg(property.type, 1);
	if(pt == "a")  # attributes
		{
		res = attributes(obj);
		return (res);
		}

	if(pt == "s" || pt == "e")  # System Environment
		{
		res = Sys.getenv();
		return (res);
		}
	}



#' @rdname property.getAll
#' @export
property.getAll = property.getALL;

#' @rdname getAllAttributes
#' @export
getAllAttributes = property.getALL;





#' @rdname getALLAttributes
#' @export
getALLAttributes = property.getALL;

#' @rdname property.getAll
#' @export
property.getAll = property.getALL;







property.remove = function()
	{
# Sys.unsetenv(x)
# attributes(x) <- NULL ... removes all 

	# stop.cat("monte", "say", "hi");
	}








# options(warn=2);
# options("warn");
options.get = function() {}
options.set = function() {}



#' setOption
#'
#' @param myKey
#' @param myValue
#'
#' @return
#' @export
#'
#' @examples
option.set = function(keys, values)
	{
	n = length(keys);
	for(i in 1:n)
		{
		key = keys[i];
		value = values[i];
		options(stats::setNames(list(key), value));
		}
	}
	


	
setOption = option.set;
setOptions = option.set;
options.set = option.set;



#' getOptions
#'
#' @param keys
#'
#' @return
#' @export
#'
#' @examples
option.get = function(keys)
	{
	#  R::base has "getOption" but not "getOptions"	(multivariate)
	n = length(keys);
	res = vector("list", n);
	for(i in 1:n)
		{
		key = keys[i];
		res[[i]] = base::getOption(key);
		}
	list.return(res);
	}

getOptions = option.get;
options.get = option.get;


option.getALL = function()
	{
	options();
	}

option.getAll = option.getALL;
options.getAll = option.getALL;
options.getALL = option.getALL;

option.saveState = function(key="DEFAULT")
	{
	memory.set(key, "OPTIONS", options() );	
	}

options.saveState = option.saveState;
	
option.restoreState = function(key="DEFAULT")
	{
	op = memory.get(key, "OPTIONS");
	if(!is.null(op))
		{
		options(op);
		return(invisible(TRUE));
		}
	warning("There is no memory for key=XXX, nothing updated");
	return(invisible(TRUE));
	}

options.restoreState = option.restoreState;























































#' setPar
#'
#' @param myKey
#' @param myValue
#'
#' @return
#' @export
#'
#' @examples
# bazaar graphics::par opens a par window in RGui
par.set = function(keys, values)
	{
	pnames = names( graphics::par(no.readonly = TRUE) );
	n = length(keys);
	for(i in 1:n)
		{
		key = keys[i];
		value = values[i];
		if( key %in% pnames)
			{
			graphics::par(stats::setNames(list(key), value));
			} else { 
					warning.cat("key", key, " is either *NOT* a par key on this DEVICE or is READONLY"); 
					} 
		}
	}
	


	
setPar = par.set;



#' getPars
#'
#' @param keys
#'
#' @return
#' @export
#'
#' @examples
par.get = function(keys, vals=NULL, no.readonly = TRUE)
	{
	pnames = names( graphics::par(no.readonly = no.readonly) );
	n = length(keys);
	res = vector("list", n);
	for(i in 1:n)
		{
		key = keys[i];
		res[[i]] = graphics::par(key);
		}
	list.return(res);
	}
	
	
getPar = par.get;


par.getALL = function(no.readonly = TRUE)
	{
	par(no.readonly=no.readonly);  # only show ones we can change by default
	}

par.getAll = par.getALL;

par.saveState = function(key="DEFAULT", no.readonly = TRUE)
	{
	memory.set(key, "PAR", par(no.readonly = no.readonly) );	
	}

	
par.restoreState = function(key="DEFAULT")
	{
	old.par = memory.get(key, "PAR");
	if(!is.null(old.par))
		{
		par(old.par);
		return(invisible(TRUE));
		}
	warning("There is no memory for par key=XXX, nothing updated");
	return(invisible(TRUE));
	}















# get/set
# saveState(key)
# restoreState(key)

## similar to par ... see include.dir function 


