
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
property.set = function(key, value, obj, property.type="attributes")
	{
	w = functions.cleanKey(property.type, 1);
	if(w == "a")
		{
		attributes(obj)[[key]] = value;
		return (obj);
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
property.get = function(key, obj, property.type="attributes")
	{
	w = functions.cleanKey(property.type, 1);
	if(w == "a")
		{
		res = attributes(obj)[[key]];
		return (res);
		}
	}



getAttribute = function(myAttributes, myObj)
	{
	# maybe alias 'getAttributes'
	res = list();
	i = 0;
	for(myAttribute in myAttributes)
		{
		i = 1 + i;
		res[[i]] = attributes(myObj)[[myAttribute]];
		}

	returnList(res);
	}



#' @rdname getAttribute
#' @export
getAttribute = property.get;

