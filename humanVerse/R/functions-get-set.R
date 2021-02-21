
# save memory ... restoreState ... pdf 
setPar = function(key, val)
	{
	# par(mar=c(0.25, 0.25, 0.25, 0.25)
	# R.O. indicates read-only arguments: These may only be used in queries and cannot be set. ("cin", "cra", "csi", "cxy", "din" and "page" are always read-only.)
	# https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/par
	pnames = names( par(no.readonly = TRUE) );
	if(is.element(key, pnames))
		{
		# par()[[key]] = val;
		par(setNames(list(val), key))
		}
	}
	
getPar = function(key)
	{	
	pnames = names( par(no.readonly = FALSE) );
	if(is.element(key, pnames))
		{
		# par()[[key]];	
		par(key);
		}
	}

getAttribute = function(key, obj)
	{
	attributes(obj)[[key]];	
	}
	
setAttribute = function(key, value, obj)
	{
	attributes(obj)[[key]] = value;	
	obj;  # no object referencing, so I must return
	}


