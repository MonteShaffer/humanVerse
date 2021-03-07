




# save memory ... restoreState ... pdf
# par(mar=c(0.25, 0.25, 0.25, 0.25)
	# R.O. indicates read-only arguments: These may only be used in queries and cannot be set. ("cin", "cra", "csi", "cxy", "din" and "page" are always read-only.)
	# https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/par
setParKey = function(myKey, myValue)
	{
	pnames = names( par(no.readonly = TRUE) );
	if(is.element(myKey, pnames))
		{
		# par()[[myKey]] = myValue;
		par(setNames(list(myValue), myKey))
		}
	}

getParKey = function(myKey)
	{
	pnames = names( par(no.readonly = FALSE) );
	if(is.element(myKey, pnames))
		{
		# par()[[myKey]];
		par(myKey);
		}
	}


getAttributes = function(myObj)
	{
	attributes(myObj);  # maybe name 'getAllAttributes'
	}

getAttribute = function(myAttribute, myObj)
	{
	attributes(myObj)[[myAttribute]];
	}


setAttribute = function(myAttribute, myValue, myObj)
	{
	attributes(myObj)[[myAttribute]] = myValue;
	myObj;  # no object referencing, so I must return
	}


#  R::base has "getOption" but not "setOption"
setOption = function(myKey, myValue)
	{
	onames = names( options() );
	if(is.element(myKey, onames))
		{
		options(setNames(list(myValue), myKey));
		}
	}

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



