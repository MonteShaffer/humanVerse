








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
	if(!is.list(res)) { return(res); } # it's a vector 
		if(nr == 1) { return (res[[1]]); }
	if(unlist) { unlist(res); } else { res; }
	}



list.removeFillFromEnd = function(info, fill="~")
	{
	n = length(info);  # assumes it is a list ... list IN/list OUT
	nlen = list.getLengths(info);
	vals = list.getElements(info, nlen);
	nvals = str.end(fill, vals, trim=TRUE);
	
	ninfo = list.setElements(info, nlen, nvals);
	ninfo;
	}


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





list.merge = function(pinfo, cinfo)
	{
	return( modifyList(pinfo, cinfo) );
	# maybe just ?modifyList
	# again, does it preserve attributes 
	# https://stackoverflow.com/a/37856431/184614
	# we have cinfo [child] which is a substructure of pinfo [parent]
	# smartly merge ....
	# https://stackoverflow.com/a/18539199/184614
	# not recursively deep 
	# keys = unique(c(names(pinfo), names(cinfo)))
	# setNames(mapply(c, pinfo[keys], cinfo[keys]), keys)
	# https://stackoverflow.com/a/51264788/184614
	# if(is.list(x) && is.list(y) && !is.null(names(x)) && !is.null(names(y)))
		# {
		# ecom 	= intersect(names(x), names(y));
		# enew 	= setdiff(names(y), names(x));
		# res		= x;
		# if(length(enew) > 0) { res = c(res, y[enew]); }
		# if(length(ecom) > 0)
			# {
			# for(i in ecom)
				# {
				# res[i] = list(list.merge(x[[i]], y[[i]]));
				# }
			# }
		# return(res);
		# } else {
				# return(c(x, y));  # doesn't this strip attributes?
				# }
	# 
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
		getInfo = function(re, tol = sqrt(.Machine$double.eps))
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

 
# my function appends to end new val ... what is ?append 
list.append = function(info, val)
	{
	# if the thing is null, create and append to first element 
	if(is.null(info)) 
		{ 
		new = list(); 
		new[[1]] = val; 
		return( new );
		}
	n = 1 + length(info);
	info[[n]] = val;
	info;
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
	
	
	
list.getLastElements = function(info)
	{
	n = length(info);
	if(!is.list(info)) { return(info[n]); }
	if(n == 0) { return(NULL); }
	idx = list.getLengths(info);
	res = NULL; # we don't know the type ...
	for(i in 1:n)
		{
		res[i] = info[[i]][  idx[i]  ];  
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
	if(length(n) != n.info) { n = rep(n, length.out=n.info); }
	for(i in 1:n.info)
		{
		res[i] = info[[i]][  n[i]  ];  # will put NA if missing here
		}
	res; 
	}



list.setElements = function(info, n=1, vals=NULL)
	{
	n.info = length(info); 
	if(!is.list(info)) { return(info[n]); }
	if(n.info == 0) { return(NULL); }
	if(is.null(vals)) { return(NULL); }  # message, bad INPUT ??? 
	## sapply(info, "[[", n);  # this doesn't work with MISSING/NULL
	## ... this would be nice ... info[[,2]] or info[[*2]]
	res = NULL;
	if(length(n) != n.info) { n = rep(n, length.out=n.info); }
	if(length(vals) != n.info) { vals = rep(vals, length.out=n.info); }
	for(i in 1:n.info)
		{
		info[[i]][  n[i]  ] = vals[i];
		}
	info;
	}
   

list.getByIDX = function(info, idx, unused=NULL)
	{
	n.info = length(info); 
	if(!is.list(info)) { return(info[idx]); }
	if(n.info == 0) { return(NULL); }
	
	res = NULL;
	for(i in idx)
		{
		res = c(res, info[[i]]);
		}
	res;  # vector only 
	}
	
list.setByIDX = function(info, idx, vals=NULL)
	{
	n.info = length(info); 
	if(!is.list(info)) { return(info[idx]); }
	if(n.info == 0) { return(NULL); }
	if(is.null(vals)) { return(NULL); }  # message, bad INPUT ??? 

	j = 1;
	for(i in idx)
		{
		info[[i]] = vals[j];  j %++%. 
		}
	info;
	} 


# info = list(1, "alex", c(`0/3 [min]` = 0.1, `1/3 [lower-trecile]` = 0.4, `1/2 [median]` = 0.5, `2/3 [upper-trecile]` = 0.7, `3/3 [max]` = 1 ), 0, 1);

list.flatten = function(info)
	{
	if(!is.list(info)) { return(info); } 
	n = length(info);
	xlen = list.getLengths(info);
	res = list();
	idx = 1;
	for(i in 1:n)
		{
		xlist = info[[i]];
		if(xlen[i] == 1) { res[[idx]] = xlist; idx %++%.; next; }
		
		for(j in 1:xlen[i])
			{
			res[[idx]] = xlist[j]; 
			idx %++%.;
			}
		}	
	list.return(res);
	}  

# list.mapNtoOne(dict$search, variants, type);
# a simple paired list 
list.mapInvert = function(info)
	{
	keys = names(info);
	vals = unname(unlist(info));
	list.create(vals, keys);	
	}
	
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




























