
##################################################
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
property.set = function(obj, key, value=NULL, property.type="attributes")
	{
	w = functions.cleanKey(property.type, 1);



	if(is.null(value))
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
		
	if(w == "a")
		{
		for(i in 1:n.key)
			{
			attributes(obj)[[ key[i] ]] = value[idx.v];
			idx.v = 1 + idx.v; if(idx.v > n.val) { idx.v = 1; }  # allows for looping gracefully if UNEVEN
			}
		return (obj);
		}
	if(w == "s" || w == "e")  # System Environment
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



##################################################
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
property.get = function(obj, key, property.type="attributes")
	{
	w = functions.cleanKey(property.type, 1);
	if(w == "a")  # attributes
		{
		res = attributes(obj)[[key]];
		return (res);
		}


	if(w == "s" || w == "e")  # System Environment
		{
		res = Sys.getenv(key);
		return (res);
		}
	}




#' @rdname getAttribute
#' @export
getAttribute = property.get;



property.getAll = function(obj, property.type="attributes")
	{
	w = functions.cleanKey(property.type, 1);
	if(w == "a")  # attributes
		{
		res = attributes(obj);
		return (res);
		}

	if(w == "s" || w == "e")  # System Environment
		{
		res = Sys.getenv();
		return (res);
		}
	}


#' @rdname getAllAttributes
#' @export
getAllAttributes = property.getAll;







property.remove = function()
	{
# Sys.unsetenv(x)

	}
