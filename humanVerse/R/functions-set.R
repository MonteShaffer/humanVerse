
na.remove = function(x, method="omit", ...)
	{
	m = functions.cleanKey(method, 1);
	if(m == "o") { return( na.omit(x, ...) )	; }
	if(m == "e") { return( na.exclude(x, ...) )	; }	
	}

set.match = function(searching, set, ...)
	{
	# searching %in% set 
	# set %in% searching   # different things
	match(searching, set, ...);	
	}
	
set.remove = function(elements, set, ...)
	{
	if(is.null(elements)) { return(set); }
	x = na.remove( match(elements, set, ...) );
	if(length(x) == 0) { return(set); }
	set[-c(x)];
	}