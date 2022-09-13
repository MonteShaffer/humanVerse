

JSON.init = function()
	{
	set.seed('123')
myobject <- list(
mynull = NULL,
mycomplex = lapply(eigen(matrix(-rnorm(9),3)), round, 3),
mymatrix = round(matrix(rnorm(9), 3),3),
myint = as.integer(c(1,2,3)),
mydf = cars,
mylist = list(foo='bar', 123, NA, NULL, list('test')),
mylogical = c(TRUE,FALSE,NA),
mychar = c('foo', NA, 'bar'),
somemissings = c(1,2,NA,NaN,5, Inf, 7 -Inf, 9, NA),
myrawvec = charToRaw('This is a test')
);
identical(jsonlite::unserializeJSON(jsonlite::serializeJSON(myobject)), myobject);
	return (myobject);
	}



JSON.pretty = function(jsonstr, indent=4)
	{
	# maybe requires two passes ...
	
	
	
	}


JSON.parse = function(jsonstr)
	{
	jsonlite::unserializeJSON(jsonstr);
	}



JSON.stringify = function(obj, digits=16, prettify=FALSE, indent=5)
	{
	j1 = jsonlite::serializeJSON(obj, digits=digits, pretty=FALSE);
	if(prettify) 
		{ 
		j2 = jsonlite::prettify(j1, indent=indent);
		return (j2);
		}
	return (j1);
	}


