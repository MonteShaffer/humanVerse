
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
	pt = functions.cleanKey(property.type, 1);



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
	pt = functions.cleanKey(property.type, 1);
	w  = functions.cleanKey(wildcard, 1, keep = "-");
	if(pt == "a")  # attributes
		{
		res = attributes(obj)[[key]];
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
	pt = functions.cleanKey(property.type, 1);
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


	}
