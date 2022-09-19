





check.list = function(input)
	{
	# we may have only a vector, not a list 
	if(is.list(input)) { return(input); }
		res = list(); 
		res[[1]] = input;
	res;
	}

list.prep = check.list;

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
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


list.update = function(res, keys, vals, overwrite=FALSE)
	{
	n = length(keys);
	for(i in 1:n)
		{
		if(overwrite || is.null(res[[ keys[i] ]])) 
			{ 
			res[[ keys[i] ]] = vals[i];
			}
		}
	res;
	}



list.create = function(keys, vals, types=NULL)
	{
	res = list(); 
	n = length(keys);
	for(i in 1:n)
		{
		res[[ keys[i] ]] = vals[i];
		if(!is.null(types))
			{
			res[[ keys[i] ]] = as.Type(vals[i], types[i]);
			}
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


## y = packages.installed("as-list"); str(y);
## yy = unlist(y); str(yy);
## zz = list.collapse(y); str(zz);
## .%$$% ("zz@-names-"); `$$` ("zz@-names-"); access("zz@-names-");
## .%$$% ("zz@-info-");  `$$` ("zz@-info-");  access("zz@-info-");

 
list.collapse = function(res)
	{
	# if I have attributes, they get lost on unlist ... but names get dubplicated ... 
	if(is.list(res))
		{
		getInfo = function(re)
			{
			prop.re = property.getALL(re); # may have key/pairs			
			prop.names = names(prop.re);
			prn = length(prop.re);
			for(j in 1:prn)
				{
				prop.name = prop.names[j];
				prop.vals = prop.re[[j]];
				props[[prop.name]] = c(props[[prop.name]], prop.vals);
				}
			props;
			}
		
		out = NULL;
		rnames = names(res);
		n = length(res);
		rlens = numeric(n);
		props = NULL;
		for(i in 1:n)
			{
			re = res[[i]];
			rlens[i] = length(re);
			out = c(out, re);
			props = getInfo(re);
			}
		out = property.set("-names-", out, rnames);
		out = property.set("-info-", out, props);
		}	
	out;
	}





list.pair = function(res)
	{
	# as a "pair", assume the each (list element) is two-long VECTOR
	# if it is only one, str.explode() didn't split that value 
	# so let's push it to the correct index ... 
	if(is.list(res))
		{
		n = length(res);
		for(i in 1:n)
			{
			if(length(res[[i]]) == 1)
				{
				res[[i]] = c("", res[[i]]);
				}
			}
		}
	res;
	}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
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
	ndim = dim(myList);
	n.myList = length(myList);
	myNames = names(myList);  # if names are NULL
# dput(myNames);
# dput(ndim);
	if(n.myList > 0)
	  {
	  for(i in 1:n.myList)
		{
		if(is.null(ndim))
			{
# cat("\n KCASE 1 \n");
			assign(myNames[i], myList[[i]], envir=envir);
			# assign(key, val, envir = .GlobalEnv);
			} else { 
# cat("\n CASE 2 \n");
					# it's a dataframe 
					assign(myNames[i], myList[, i], envir=envir);
					}
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
# dput(info);   
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




list.fill = function() {} 
list.truncate = function() {}

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





# list.mapNtoOne(dict$search, variants, type);

list.mapNtoOne = function(info, keys, val)
	{
	# inverted from val=>keys ... keys => val (for index lookup)
	for(key in keys)
		{
		info[[key]] = val;
		}
	info;	
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






















# wrap into memory.get ???   STACK ... key ... could have multiple
list.initStack = function(nmax)
	{
	my.stack = list();
	for(idx in 1:nmax)
		{
		my.stack[[idx]] = list();
		}
	my.stack;	
	}
	
	
list.countStack = function(veclist, nmax=length(veclist))
		{
		i = 0;
		for(j in 1:nmax)
			{
			n = length(veclist[[j]]);
			if(n > 0) { i = 1 + i; }
			}
		i;
		}
		
	
# # https://stackoverflow.com/questions/28687806/a-better-way-to-push-and-pop-to-from-lists-in-r
list.push = function() {}
list.push = function(nlist, veclist=NULL, nmax=1+length(veclist), method="FIFO")
  {
 # nc = list.countStack(veclist);  # if we init the stack with empty elements (good practice), this will NOT now work....
 # print(nc);
  
 # if(nc >= nmax)

	  if(method=="FIFO")
		{
		veclist = append(veclist, nlist);
		} else { 
				veclist = append(nlist, veclist);
				}

	# veclist[[nc + 1]] = nlist;  # replace first element or whichever is first empty
	# veclist[[nmax - nc]] = nlist; # replace last element or whichever is last empty 

			
	
	
	n = length(veclist);
	dropped = NULL;
	if(n > nmax)
		{
		if(method=="FIFO")
			{
			dropped = veclist[[1]];  # QUEUING
			veclist[[1]] = NULL;
			
			} else { 
					dropped = veclist[[nmax]];
					veclist[[nmax]] = NULL; # should only be one ...
					}
		}
	# lists of numeric type auto-update indexing?
	# veclist;
	
	details = list("veclist" = veclist, "dropped" = dropped, "nlist"=nlist, nmax=nmax, method=method);
	# veclist = setAttribute("details", details, veclist);
	
	veclist = setAttribute("dropped", dropped, veclist);
	veclist = setAttribute("stack-size", nmax, veclist);
	veclist = setAttribute("method", method, veclist);
	veclist;
  }
  
  # mlist=list(); mlist[[1]] = "monte"; nlist = list(); nlist[[1]] = "alex";
  # (mlist=list.push(nlist,mlist,nmax=4))
  
  # nlist = list(); nlist[[1]] = "alex"; vlist = list.initStack(5);
  # nlist[[1]] = paste0("alex-", rand()); (vlist=list.push(nlist,vlist,nmax=5))
  
  
list.pop = function() {}  
list.pop = function(veclist, n=length(veclist), method="FIFO")
	{
	if(method=="FIFO")
			{
			popped = veclist[[1]];  # QUEUING
			veclist[[1]] = NULL;
			
			} else { 
					popped = veclist[[n]];
					veclist[[n]] = NULL; # should only be one ... by default LAST one
					}
	
	veclist = setAttribute("popped", popped, veclist);
	veclist = setAttribute("method", method, veclist);
	veclist;	
	}
	
	
  
# popList = function(nlist, veclist, n=length(veclist))
  {

  }










