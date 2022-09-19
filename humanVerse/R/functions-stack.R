


# v.queufus # can't spell it ...

# use memory ... e.g., this is stack.init ... 
# "LIFO" = FILO is javascript; "FIFO" = LILO is queueing		
# # https://stackoverflow.com/questions/2805102/
v.stack = function(max.size=Inf,  
							default = NULL,
							type="character", 
							method="LIFO",	 				
							key="-CURRENT_STACK-"
					)
	{
	# max.size can be Inf ...   max.size=Inf
	vec = default;
	vec = as.type(vec, type);
	
	mem = list("vec" = vec, 
						"size" = max.size, "method" = method,
						"type" = type, "default" = default
				);
	memory.set(key, "STACK", mem);	
	minvisible(mem, print="str");
	}  



## do push/pop with FIFO
v.push = function(..., key="-CURRENT_STACK-")
	{
	val = prep.dots(...);
	mem = memory.get(key, "STACK");
	if(is.null(mem)) { stop("You need to configure stack with v.stack() first!"); }
# dput(mem);	
	# push onto end ... LIFO (javascript)
	# push on first ... FIFO (queueing) ... end may truncate ... 
	dropvec = NULL;
	
	if(method == "LIFO")
		{
		# if val is VECTOR, this order is correct, rev(val) for FIFO?
		mem$vec = c(mem$vec, val);
		nv = length(mem$vec);  
		if(nv > mem$size) 
			{ 
			sv = (nv+1-mem$size);  
			newvec = mem$vec[sv:nv];
			dropvec = mem$vec[1:(sv-1)];
			mem$vec = newvec;  
			}		
		} else {
				# mem$vec = c(rev(val), mem$vec);
				mem$vec = c(val, mem$vec);
				nv = length(mem$vec);  
				if(nv > mem$size) 
					{ 
					newvec = mem$vec[1:mem$size];
					sv = (nv+1-mem$size);
					dropvec = mem$vec[sv:nv];
					mem$vec = newvec; 					
					}
				}	
	memory.set(key, "STACK", mem);	
	minvisible(mem, print="str");  #update the memory/history
	minvisible(dropvec, display=TRUE);
	}
	
	
	# just like stack history, R needs a symbol history ...
	
v.pop = function(n=1, key="-CURRENT_STACK-")
	{
	## TODO
	mem = memory.get(key, "STACK");
dput(mem);
	if(is.null(mem)) { stop("You need to configure stack with v.stack() first!"); }
	
	# pop from end ... LIFO (javascript)
	# pop on first ... FIFO (queueing) ... NO truncate ... 
	if(method == "LIFO")
		{
		nv = length(mem$vec); 
			s = (nv-1+n);  if(s < 1) { s = 1; }
			idx = s:nv;
		val = mem$vec[idx];	
		mem$vec = mem$vec[-c(idx)];
		} else {
				nv = length(mem$vec);  
					s = n; if(s > nv) { s = nv; }
					idx = 1:s;
				val = mem$vec[idx];	
				mem$vec = mem$vec[-c(idx)];
				}	
	memory.set(key, "STACK", mem);	#update the memory/history 
	minvisible(mem, print="str");
	minvisible(val, display=TRUE);
	}
	

## EMPTY elements, maintain stack TYPE
v.purge = function(n=1, key="-CURRENT_STACK-")
	{
	mem = memory.get(key, "STACK");
	if(is.null(mem)) { stop("You need to configure stack with v.stack() first!"); }
	
	n = length(mem$vec);  
	if(n > 0) { mem$vec = mem$vec[-c(1:n)]; }
	
	memory.set(key, "STACK", mem);	#update the memory/history 
	minvisible(mem, print="str");
	}



## REVIEW minvisilbe ABOVE, and RECODE below ... #########################




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


