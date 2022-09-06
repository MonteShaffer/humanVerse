



# this is multivariate ... 
v.types = function(vecs, ...)
	{
	# vecs = dots.addTo(vecs, ...);
	n = length(vecs);
	res = character(n);
	for(i in 1:n)
		{
		vec = vecs[i];
		if(is.list(vec)) { vec = vecs[ , i]; } # dataframe
		res[i] = v.type(vec);
		}
	return(res);	
	}






# this is univariate
v.type = function(vec)
	{
	ntype = typeof(vec);		
	if(ntype == "integer" && is.factor(vec))
		{
		ntype = "factor";
		}
	if(ntype == "double")
		{
		# intentional cascade ... if multiple, it will cast as the pointer <ct>
		if(is.POSIXt(vec))   { ntype = "POSIXt";}
		if(is.POSIXlt(vec))  { ntype = "POSIXlt";}
		if(is.POSIXct(vec))  { ntype = "POSIXct";}	
		if(is.Date(vec))     { ntype = "Date";}		
		}
	if(ntype == "closure")
		{
		# my function, not base  
		if(is.function(vec)) { ntype = "function"; }
		}
		# "call", "expression", and "name"  [symbols]
		# environment
	ntype;
	}





v.naTo = function(vec, to="")
	{
	vec[is.na(vec)] = to;
	vec;	
	}
	
v.naTO = v.naTo;

v.between = function(vec, lower, upper, sort = TRUE, ...)
	{
	if(sort) { sort(vec, ...); }
	idx = v.return( which(vec >= lower & vec <= upper) );
	if(is.null(idx)) { return(NULL); }
	vec[idx];
	}

v.return = function(idx)
	{
	if(is.null(idx)) { return(NULL); }
	if(length(idx) == 0) { return(NULL); }
	idx;	
	}


v.which = function(vec, what="")
	{ 
	idx = NULL;
	# type = v.type(what);
	if(is.logical(what))
		{
		if(length(what) == 1)
			{
			idx = which(vec == what);
			} else {
					if(length(what) == length(vec))
						{
						idx = which(what == TRUE);	
						}
					}
		return(v.return(idx));
		}
	if(is.character(what))
		{
		idx = which(vec == what);
		return(v.return(idx));
		}
	if(is.na(what))
		{
		idx = which(is.na(vec));
		return(v.return(idx));
		}
	## DEFAULT
	idx = which(vec == what);
	return(v.return(idx));
	}

# this is univariate
v.remove = function(vec, what="")
	{
	idx = v.which(vec, what=what);
	vec[ -c( idx ) ];
	}





v.shortTypes = function(types)
	{
	n = length(types);
	res = character(n);
	for(i in 1:n)
		{
		type = types[i];
		res[i] = switch(type,					  			
							"character" = "<char>",
							"factor"	= "<factor>",
							"integer"	= "<int>",
							"POSIXlt"	= "<POSIXlt>",
							"POSIXct"	= "<POSIXct>",
							"Date"		= "<Date>",
							"function"	= "<function>",							
				paste0("<",type,">")		# DEFAULT
				);		
		}
	res;
	}
	


.begin = function(x)
	{
	1;
	}
 
.end = function(x)  # could it be from THIS
	{
	length(x);
	}


v.nearest = function(vec, what, howmany=1)
	{
	idx = v.nearest.idx(vec, what, howmany=howmany);
	vec[ idx ];
	}

v.nearest.idx = function(vec, what, howmany=1)
	{
	vec.dev = abs(what-vec);
	if(is.null(howmany)) { return( stats.whichMin(vec.dev) ); } # all results
	stats.whichMin(vec.dev)[1:howmany];
	}
	

v.freq = function(vec, what)
	{
	length(v.which(vec, what));
	}
	
v.mode = function(vec)
	{
	m = stats.mode(vec, force.numeric=FALSE); # values
	set.match(m, vec);	# idx
	}





# original = c("P.1", "P.2", "P.3", "P.4", "P.5", "P.6", "P.7", "P.8", "P.9", "P.10");
# new = c("P.7", "P.1", "P.3", "P.6", "P.5", "P.8", "P.4", "P.9", "P.10", "P.2")
v.arrange = function(orig, new)
	{
	# from 'orig' to 'new'
	# set.match(orig, new);
	set.match(new, orig); # I think this is correct 
	}

computeIndexFromOriginalToNew = v.arrange;











	







vector.appendProperties = function (res, info, keep.source=TRUE)
	{	
	if(keep.source) { res = property.set("s", res, info); }
	# res = property.set("strlen.s", res, strlen(info));
	# res = property.set("strlen.o", res, strlen(res));
	res;	
	}

vector.useNames = function(info)
	{
	n.info = length(info);
	names.info = property.get("names", info);
	names.len = strlen(names.info);
	dput(names.len); 
	if((length(names.info) == n.info))
		{
		res = names.info;
		names(res) = info;
		# set KEY on OBJ to VAL
		res = property.set("strlen", res, names.len);
		res = property.set("names", res, info);
		return(res);
		}
	return(NULL);	
	}
	



































