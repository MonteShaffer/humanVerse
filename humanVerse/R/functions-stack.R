


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
