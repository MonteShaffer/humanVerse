





list.prep = function(input)
	{
	# we may have only a vector, not a list 
	if(!is.list(input)) { res = list(); res[[1]] = input; }
	res;
	}


##################################################
#'
#' list.return
#'
#' If list only has one element return it.
#' By default unlist so it is just a vector ... (redundant with above?)
#'
#' @param res
#' @param unlist
#'
#' @return
#' @export
list.return = function(res, unlist=FALSE)
	{
	nr = length(res);
		if(nr == 0) { return (NULL); }
		if(nr == 1) { return (res[[1]]); }
	if(unlist) { unlist(res); } else { res; }
	}


#' @rdname returnList
#' @export
returnList = list.return;

# functions.setDefaultValues(list.toString);
# functions.setDefaultValues("list.toString");
list.toString = function(simpleList, 	sep.keyvalue = "`=`", 
										sep.elements = "`:`",
										sep.type = "`|`"
						)
	{
	str = c();
	n = length(simpleList);
	mynames = names(simpleList); if(is.null(mynames)) { mynames = 1:n; }
	for(i in 1:n)
		{
		# str = str + new;
		element = simpleList[[i]];
		str = c(str, paste0(mynames[i],	sep.keyvalue, 
							element, 	sep.type,
							typeof(element) ));		
		}
	paste0(str, collapse = sep.elements);
	}


list.create = function(keys, vals, types=NULL)
	{
	res = list();
	n = length(keys);
	for(i in 1:n)
		{
		res[[ keys[i] ]] = as.Type(vals[i], types[i]);
		}
	res;
	}

list.fromString = function(str, sep.keyvalue = "`=`", 
								sep.elements = "`:`",
								sep.type = "`|`"
							)
	{
	info = str.explode(sep.elements, str);
	info2 = str.explode(sep.keyvalue, info);
		keys = list.getElements(info2, 1);
		info = list.getElements(info2, 2);
	info2 = str.explode(sep.type, info);
		vals = list.getElements(info2, 1);
		types = list.getElements(info2, 2);
	res = list.map(keys, vals, types);
	}



##################################################
#'
#' list.extract
#'
#' This is not recurive, gets "names" of lists and extracts them to the environment as accessible variables using assign.
#'
#' @param myList the list to be extracted
#' @param envir the environment inwhich to extract it
#'
#' @return updates and assigns the values
#' @export
#'
#' @examples
#'
#' mylist = list("a" = 1, "b" = 2, "c" = 3);
#'		 list.extract(mylist);
#'		 print(a); print(b); print(c);
#'
#'
## envir = .GlobalEnv
list.extract = function(myList, envir = .GlobalEnv, ...)
	{
	n.myList = length(myList);
	myNames = names(myList);  # if names are NULL
	if(n.myList > 0)
	  {
	  for(i in 1:n.myList)
		{
		assign(myNames[i], myList[[i]]);
		}
	  }
	}

# keyed of names or idx?

#' @rdname extractList
#' @export
extractList = list.extract;
 


list.getProperty = function(key, info) 
	{
	n.info = length(info);
	if (!is.list(info)) { return( property.get(key, info) ); }
	# is.list with length of zero
	if(n.info == 0) { return( NULL ); }
	myNames = names(info);
	res = NULL;
	for (i in 1:n.info) 
		{
		res[i] = property.get(key, info[[i]]);
		}
	res;
	}
	
	
list.getLengths = function(info) 
	{
	dput(info);
	n.info = length(info);
	if (!is.list(info)) { return(n.info); }
	# is.list with length of zero
	if(n.info == 0) { return( NULL ); }
	res = NULL;
	for (i in 1:n.info) 
		{
		res[i] = length(info[[i]]);
		}
	res;
	}


list.truncateLength = function(info, n)
	{	
	dput(info);
	n.info = length(info);
	if (!is.list(info)) 
		{ 
		# vector 
		n.t = n.info; 
		if(n.t > n) { n.t = n;}
		return(info[1:n.t]); 
		}
	# is.list with length of zero
	if(n.info == 0) { return( list() ); }
	
	n.lengths = list.getLengths(info);
	if(is.null(n.lengths)) { return( list() ); } # shouldn't this be a list?
	res = NULL;
	for (i in 1:n.info) 
		{
		n.t = n.lengths[i];
		if(n.t > n) { n.t = n; }
		res[[i]] = info[[i]][1:n.t];
		}
	res;
	}
	
	
	
# https://stackoverflow.com/questions/44176908/
# get elements at same key
list.getElements = function(info, n=1)
	{
	n.info = length(info);
	if(!is.list(info)) { return(info[n]); }
	if(n.info == 0) { return(NULL); }
	## sapply(info, "[[", n);  # this doesn't work with MISSING/NULL
	## ... this would be nice ... info[[,2]] or info[[*2]]
	res = NULL;
	for(i in 1:n.info)
		{
		res[i] = info[[i]][n];  # will put NA if missing here
		}
	res;
	}




list.fromError = function(e)
	{
	res = list();
		res[["msg"]] 	= e[1];
		res[["class"]] 	= class(e);
			condition = attributes(e)$condition;
		res[["condition"]] = condition;
		# FROM CONDITION
			extra = attributes(condition)$class;
		res[["call"]] = condition$call;
		res[["message"]] = condition$message;
		res[["classes"]] = extra;
	res;
	}
























