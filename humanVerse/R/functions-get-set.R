





# save memory ... restoreState ... pdf
# par(mar=c(0.25, 0.25, 0.25, 0.25)
	# R.O. indicates read-only arguments: These may only be used in queries and cannot be set. ("cin", "cra", "csi", "cxy", "din" and "page" are always read-only.)
	# https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/par
setParKey = function(myKeys, myValues)
	{
	pnames = names( graphics::par(no.readonly = TRUE) );
	
	i = 0;
	for(myKey in myKeys)
		{
		i = 1 + i;
		myValue = myValues[i]; # we assume they are of equal length, could error check
		if(!is.na(myValue))
			{
			if(is.element(myKey, pnames))
				{
				graphics::par(stats::setNames(list(myValue), myKey))
				}
			}
		}
	}

getParKey = function(myKeys)
	{
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


getAllAttributes = function(myObj)
	{
	attributes(myObj);  # maybe name 'getAttributes'
	}


getAttribute = function(myAttributes, myObj)
	{
	res = list();
	i = 0;
	for(myAttribute in myAttributes)
		{
		i = 1 + i;
		res[[i]] = attributes(myObj)[[myAttribute]];
		}
		
	returnList(res);	
	}


setAttribute = function(myAttributes, myValues, myObj)
	{
	i = 0;
	for(myAttribute in myAttributes)
		{
		i = 1 + i;
		myValue = myValues[i]; # we assume they are of equal length, could error check
		if(!is.na(myValue))
			{
			attributes(myObj)[[myAttribute]] = myValue;
			}			
		}
	myObj;  # no object referencing, so I must return
	}


# alias setOptions
#  R::base has "getOption" but not "setOption"
setOption = function(myKeys, myValues)
	{
	# you can set an option that doesn't exist ...
	i = 0;
	for(myKey in myKeys)
		{
		i = 1 + i;
		myValue = myValues[i]; # we assume they are of equal length, could error check
		if(!is.na(myValue))
			{
			options(stats::setNames(list(myValue), myKey));
			}
		}
	}

#  R::base has "getOption" but not "getOptions"	(multivariate)
getOptions = function(myKeys)
	{
	res = list();
	i = 0;
	for(myKey in myKeys)
		{
		i = 1 + i;
		res[[i]] = getOption(myKey);
		}		
	returnList(res);	
	}
	

# if I have a parallel list, get the elements in a vector that are the same location, one layer down
getElementsInList = function(myList, idx)
  {
  n = length(myList);
  res = c();
  for(i in 1:n)
    {
    res = c(res, myList[[i]][[idx]]);
    }
  res;
  }



