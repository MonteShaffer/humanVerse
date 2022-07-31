



#' get.last.error
#'
#' @return
#' @export
get.last.error = function()
	{
  # ls(".GlobalEnv");
  # geterrmessage()
  # https://stackoverflow.com/questions/36966036/how-to-get-the-last-error
  # options(error = function() {traceback(2, max.lines=100); if(!interactive()) quit(save="no", status=1, runLast=T)})
  # https://stackoverflow.com/questions/7485514/can-you-make-r-print-more-detailed-error-messages

  tr = .traceback()  # Not a typo! .traceback is like traceback except that it doesn't force printing the stack. –
	  if(length(tr) == 0)
  	  {
  		return(NULL);
  	  }
  tryCatch(eval(parse(text = tr[[1]])), error = identity);
	}



#' setParKey
#'
#' @param myKey
#' @param myValue
#'
#' @return
#' @export
#'
#' @examples
# set.par.key
setParKey = function(myKey, myValue)
	{
  # save memory ... restoreState ... pdf
# par(mar=c(0.25, 0.25, 0.25, 0.25)
	# R.O. indicates read-only arguments: These may only be used in queries and cannot be set. ("cin", "cra", "csi", "cxy", "din" and "page" are always read-only.)
	# https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/par

	pnames = names( graphics::par(no.readonly = TRUE) );
	if(is.element(myKey, pnames))
		{
		graphics::par(stats::setNames(list(myValue), myKey))
		}
	}

#' setParKeys
#'
#' @param myKeys
#' @param myValues
#'
#' @return
#' @export
#'
#' @examples
# set.par.keys
setParKeys = function(myKeys, myValues)
	{
	pnames = names( graphics::par(no.readonly = TRUE) );

	i = 0;
	for(myKey in myKeys)
		{
		i = 1 + i;
		# what if myValue is an array?
		myValue = myValues[i]; # we assume they are of equal length, could error check
		if(length(myValue) > 0)
			{
			if(!is.na(myValue))
				{
				if(is.element(myKey, pnames))
					{
					graphics::par(stats::setNames(list(myValue), myKey))
					}
				}
			}
		}
	}

#' getParKey
#'
#' @param myKeys
#'
#' @return
#' @export
#'
#' @
# get.par.key
# get.par.keys
getParKey = function(myKeys)
	{
	# alias ... getParKeys
	pnames = names( graphics::par(no.readonly = FALSE) );

	res = list();
	i = 0;
	for(myKey in myKeys)
		{
		i = 1 + i;
		if(is.element(myKey, pnames))
			{
			res[[i]] = graphics::par(myKey);
			}
		}

	returnList(res);
	}


#' getAllAttributes
#'
#' @param myObj
#'
#' @return
#' @export
#'
#' @examples
# get.attributes
getAllAttributes = function(myObj)
	{
	attributes(myObj);
	}


#' getAttribute
#'
#' @param myAttributes
#' @param myObj
#'
#' @return
#' @export
#'
#' @aliases getAttributes
#'
#' @examples
# get.attribute
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


# delete.attribute
deleteAttribute = function(myAttribute, myObj)
	{
	attributes(myObj)[[myAttribute]] = NULL;
	myObj;  # no object referencing, so I must return
	}
	
	

#' setAttribute
#'
#' @param myAttribute
#' @param myValue
#' @param myObj
#'
#' @return
#' @export
#'
#' @examples
# set.attribute
setAttribute = function(myAttribute, myValue, myObj)
	{
	attributes(myObj)[[myAttribute]] = myValue;
	myObj;  # no object referencing, so I must return
	}

#' setAttributes
#'
#' @param myAttributes
#' @param myValues
#' @param myObj
#'
#' @return
#' @export
#'
#' @examples
# set.attributes
setAttributes = function(myAttributes, myValues, myObj)
	{
	i = 0;
	for(myAttribute in myAttributes)
		{
		i = 1 + i;
		# what if myValue is an array?
		myValue = myValues[[i]]; # we assume they are of equal length, could error check
		if(length(myValue) > 0)
			{
			if(!is.na(myValue))
				{
				attributes(myObj)[[myAttribute]] = myValue;
				}
			}
		}
	myObj;  # no object referencing, so I must return
	}





#' setOption
#'
#' @param myKey
#' @param myValue
#'
#' @return
#' @export
#'
#' @examples
# set.option
setOption = function(myKey, myValue)
	{
	options(stats::setNames(list(myValue), myKey));
	}

#' setOptions
#'
#' @param myKeys
#' @param myValues
#'
#' @return
#' @export
#'
#' @examples
# set.options
setOptions = function(myKeys, myValues)
	{
	# you can set an option that doesn't exist ...
	i = 0;
	for(myKey in myKeys)
		{
		i = 1 + i;
		# what if myValue is an array?
		myValue = myValues[[i]]; # we assume they are of equal length, could error check
		if(length(myValue) > 0)
			{
			if(!is.na(myValue))
				{
				options(stats::setNames(list(myValue), myKey));
				}
			}
		}
	}

#' getOptions
#'
#' @param myKeys
#'
#' @return
#' @export
#'
#' @examples
# get.option
# get.options
getOptions = function(myKeys)
	{
  #  R::base has "getOption" but not "getOptions"	(multivariate)

	res = list();
	i = 0;
	for(myKey in myKeys)
		{
		i = 1 + i;
		res[[i]] = getOption(myKey);
		}
	returnList(res);
	}


#' getElementsInList
#'
#' @param myList
#' @param idx
#'
#' @return
#' @export
#'
#' @examples
getElementsInList = function(myList, idx)
  {
  # if I have a parallel list, get the elements in a vector that are the same location, one layer down

  n = length(myList);
  res = c();
  for(i in 1:n)
    {
    res = c(res, myList[[i]][[idx]]);
    }
  res;
  }


# typeof
# is.logical(z) 
# typeof(z) == "logical"
get.type = function(obj)
	{
	if(is.null(obj)) { return ("NULL"); }
	
	if(is.integer(obj)) { return ("INT"); }
	if(is.character(obj)) { return ("CHAR"); }
	
	if(is.logical(obj)) { return ("BOOL"); }
	if(is.complex(obj)) { return ("COMPLEX"); }
	
	if(is.infinite(obj)) { return ("INF"); }
	if(is.nan(obj)) { return ("NaN"); }
	
	if(is.single(obj)) { return ("SINGLE"); }
	if(is.double(obj)) { return ("DOUBLE"); }
	
	if(is.data.frame(obj)) { return ("DATAFRAME"); }
	if(is.list(obj)) { return ("LIST"); }
	
	if(is.na(obj)) { return ("NA"); }
	
	
	}
	
	
# typeof options?
# "logical", "integer", "double", "complex", "character", "raw" and "list", "NULL", "closure" (function), "special" and "builtin" (basic functions and operators), "environment", "S4" (some S4 objects) and others that are unlikely to be seen at user level ("symbol", "pairlist", "promise", "language", "char", "...", "any", "expression", "externalptr", "bytecode" and "weakref").

# mode?

