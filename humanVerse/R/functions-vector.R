


v.match = function(a, B.len, B.nam)
	{
	# match on KEYS or IDXS 
	# a is subset of B (hopefully)
	# could be comparing on names or idxs ... I don't know 
	# return idxs of set.match ...
	
	if(is.null(a)) { return(NULL); }
	if(is.numeric(a))
		{
		idx = set.match( a, 1:B.len );
		} else {
				idx = set.match( a, B.nam);
				}
	# if(is.na(idx)) { return(NULL); }  # no matches 
	# if(anyNA(idx)) { return(NULL); }  # no matches is one NA ... idx are integers otherwis 
	if(is.na(idx[1])) { return(NULL); }  # no matches 
	idx;	
	}




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
 
# between(x, lower, upper, incbounds=TRUE, NAbounds=TRUE, check=FALSE)
# x %between% y
  
v.between = function(vec, lower, upper, 
							lower.equal = TRUE,
							upper.equal = TRUE,
							by = "value",
							return = "vector",
					sort = FALSE, ...)
	{
	# OPERATES on vec not IDX of vector ... 1:length(vec) to do index ... 
	# return = [v]ector or [i]ndex
	r = prep.arg(return, n = 1);
	# should return idx or vec? ... THIS RETURNS vec, not idx ...
	############## DO I compare the indexes with upper/lower or the values 
	b = prep.arg(by, n = 1); # COMPARISON of "values" or "indexes"
	vecIDX = 1:length(vec);
	vecT = vec; if(b == "i") { vecT = vecIDX; }

	# lower can be NULL ... skip 
	# upper can be NULL ... skip 
	
	# 2 x 2 here ... with null 3 x 2 
	# TRUTH table
	if(!is.null(lower))
		{
		idx1 = NULL;
		# THIS is OR
		if(lower.equal) { idx1 = (vecT == lower); } 
		idx2 = (vecT > lower);
		idx.lower = idx2;
		if(!is.null(idx1)) { idx.lower = idx1 | idx2; }
		}
	if(!is.null(upper))
		{
		idx1 = NULL;
		# THIS is OR
		if(upper.equal) { idx1 = (vecT == upper); } 
		idx2 = (vecT < upper);
		idx.upper = idx2;
		if(!is.null(idx1)) { idx.upper = idx1 | idx2; }
		}
	# COMBINED IS 'AND'
	if(!is.null(lower) && !is.null(upper)) 
		{ 
		idx = idx.lower & idx.upper; 
		} 
	if(!is.null(lower) && is.null(upper)) 
		{ 
		idx = idx.lower; 
		}
	if(is.null(lower) && !is.null(upper)) 
		{ 
		idx = idx.upper; 
		} 
	if(is.null(idx)) { return(NULL); }
	if(r == "i") { return(v.return(which(idx==TRUE))); }   
	
	# indexes are based on lower/upper on VALUES/INDEXES
	# independently, we return VECTOR, not elements below ...
	vec = vec[idx];  # truncate 
	if(sort) { vec = sort(vec, ...); }
	v.return(vec);
	}

v.return = function(idx)
	{
	if(is.null(idx)) { return(NULL); }
	if(length(idx) == 0) { return(NULL); }
	idx;	
	}


v.which = function(vec, what="", invert=FALSE)
	{ 
	idx = NULL;
	# type = v.type(what);
	if(is.logical(what))
		{
		if(length(what) == 1)
			{
			if(is.logical(vec))
				{
				idx = which(vec == what);
				} else { if(what) { idx = 1; } }  # vec is of length one ... TRUE
			} else {
					if(length(what) == length(vec))
						{
						# if you have FALSE, !FALSE to get TRUE on what   
						idx = which(what == TRUE);	
						}
					}
		}
	if(is.null(idx) && is.character(what))
		{
		idx = which(vec == what);
		}
	if(is.null(idx) && is.na(what))
		{
		idx = which(is.na(vec));
		}
	## DEFAULT
	if(is.null(idx))
		{
		idx = which(vec == what); 
		}
		
	if(invert) { IDX = 1:length(vec); idx = IDX[-c(idx)]; }
	return(v.return(idx));
	}

# this is univariate
v.remove = function(vec, what="")
	{
	idx = v.which(vec, what=what);
	if(is.null(idx)) { return(vec); } # nothing to remove
	vec[ -c( idx ) ];  
	}
	 
v.truncate = function(vec, parent)
	{
	# shorten parent by removing vec 
	res = set.diff(parent, vec);
	if(length(res) == 0) { return(NULL); }
	res;
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
v.arrange = function(orig, new, append.missing=TRUE)
	{
	# from 'orig' to 'new'
	# set.match(orig, new);
							# if they missed an element warning and append 
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
	



































