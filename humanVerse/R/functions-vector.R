



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
	


.begin = function(vec)
	{
	1;
	}
	
.end = function(vec)
	{
	length(vec);
	}

v.begin = function(vec, n=1)
	{
	vec[n];
	}
 
v.end = function(x)  # could it be from THIS
	{
	vec[length(x)];
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











	

# maybe into "functions-vector.R"
v.getLastN = function (vec, n.out = 1) 
	{
    n = length(vec);
    vec[sign(n.out) * (n - abs(n.out) + 1):n]
	}




## do push/pop with FIFO
v.push = function(element, vector)
	{
	
	}
	
v.pop = function(element, vector)
	{
	## TODO
	
	}
	


#' popVector
#'
#' @param vec
#' @param idx
#' @param method
#'
#' @return
#' @export
popVector = function(vec, idx=1, method="FIFO")  # Inf would work for unlimited stack
  {
  # https://stackoverflow.com/questions/2805102/how-is-pushing-and-popping-defined
# vec = 1: 10;
# popVector(vec)
# popVector(vec, method="LIFO-LILO")
  if(method=="FIFO")   # QUEUING
    {
    val = vec[idx];
    vec = vec[-c(idx)];
    } else {
            n = length(vec) + 1 - idx;
            val = vec[n];
            vec = vec[-c(n)];
    }
  # list or "attributes" ?
  list("val" = val, "vec" = vec, "popped" = NULL, method=method); # updated ...
  }


#' pushVector
#'
#' @param val
#' @param vec
#' @param n.max
#' @param method
#'
#' @return
#' @export
#'
#' @examples
pushVector = function() {}
pushVector = function(val, vec, n.max=1+length(vec), method="FIFO")
  {
  # vec = 1: 10;
# popVector(pushVector(13, vec)$vec)
# pushVector(13, vec, n.max=5)
# pushVector(13, vec, n.max=5, method="LIFO-LILO")


  # n.max is max size, so vals popped may return ...
  n = length(vec);
  popped = NULL;
  if(method=="FIFO")  # in this model, new values are added to end
    {
    if(n < n.max)
      {
      vec = c(vec,val);
      } else {
              vec = c(vec,val);
              nn = 1 + n;
              nd = nn - n.max;
              if(nd > 0)
                {
                popped = vec[1:nd];
                vec = vec[(1+nd):nn];
                }
              }
    } else {        # in this model, new values are added to beginning
            if(n < n.max)
              {
              vec = c(val,vec);
              } else {
                      vec = c(val,vec);
                      nn = 1 + n;
                      if(nn > (1+n.max))
                        {
                        popped = vec[(1+n.max):nn];  # off the end ?
                        vec = vec[1:n.max];
                        }
                      }
            }
  # list or "attributes" ?
  list("vec" = vec, "popped" = popped, "val"=val, method=method);
  }



## vector.push_back(element) is C++
v.push_back = function(element, vector)
	{
	c(vector, element);
	}

#' @rdname push_last
#' @export
v.push_last = v.push_back;

v.push_front = function(element, vector)
	{
	c(element, vector);
	}

#' @rdname push.first
#' @export
v.push_first = v.push_front;













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
	



































