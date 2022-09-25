

# store the original idx and sort, store the new idx as well 
# memoery.sort 
m.sort = function() {}


# v.sort(sample(1:6), "BUBBLE");

# https://github.com/MonteShaffer/humanVerse/blob/main/misc/functions-sort.R
v.sort = function(vec, method="bubble")
	{
	METHOD = prep.arg(method, n=3);
	# msg THIS is for DEMO purposes 
	n = length(vec);
	
	swapMe = function(a,a.idx, b,b.idx, nvec, tol = sqrt(.Machine$double.eps))
		{
		nvec[b.idx] = a;
		nvec[a.idx] = b;
		nvec;
		}
	
	if(METHOD == "bub")		# BUBBLE SORT (swap meet)
		{
		nticks = 0; nswaps = 0;
		nvec = vec; # COPY 
		for(j in 1:n)
			{
			cswap = 0;
			for(i in 2:n)
				{
				nticks = 1 + nticks;
vec = nvec; # original at moment ...
				a.idx = i-1; a = nvec[a.idx];
				b.idx = i;	 b = nvec[b.idx];
				if(a > b) 
					{ 
					nvec = swapMe(a,a.idx, b,b.idx, nvec); 
					cswap = 1 + cswap;
					nswaps = 1 + nswaps;
					}

if(TRUE)
	{
	if(!identical(vec,nvec))
		{
		from = paste0(vec, collapse=",");
		to = paste0(nvec, collapse=",");
		cat("\n", "j -->",j," i: ",i, " ... FROM: ", from, " ==> SWAPPED TO: ",	to );
		} else {
				cat("\n", "j -->",j," i: ",i, " ... " );
				}

	}

			}



			if(cswap == 0) { break; }	 # 3 6 2 4 5 1
			}
		
		bigO = n^2;			# theoretical
		bigO.a = j * n;		# actual:: this is when we break out of the loop
		
if(TRUE)
	{
cat("\n\n", "BUBBLE SORT: ",
	"\n\t\t", "Big O as n^2 : ", bigO,
	"\n\t\t", "For this, BIG O (a) as j*n : ", bigO.a,
	"\n\t\t\t", "There are: ", nswaps, "SWAPS that occurred.",
	"\n\t\t\t", "There are: ", nticks, "COMPARISONS that were made.",
	"\n\n");	
	}
	
		minvisible(nvec);
		}
	
	
	if(METHOD == "sel")		# SELECTION SORT (both MIN/MAX)
		{
		ntimes = ceiling(n/2);  # min/max from both ends ...
		nticks = 0; ncomps = 0;
		
		nvec = NA*vec; # COPY 
		min.idx = 1;
		max.idx = n;
	
		for(j in 1:n)
			{
			ncomps = 1 + ncomps;
			# loop to find min (and max)
			cmin = NA;  idx.cmin = NA;
			cmax = NA;  idx.cmax = NA;


if(TRUE)
	{
cat("\n ####################  HEADER [j] ################# \n");
	from = paste0(vec, collapse=",");
	to = paste0(nvec, collapse=",");

cat("\n", "j -->",j, " ... FROM: ", from, " ==> TO: ",	to );
	}




			for(i in 1:n)
				{
				nticks = 1 + nticks;
				cval = vec[i];






		
				
				if(is.na(cval)) { next; } # put NA at the end ...
								
				if(is.na(cmin)) 
						{ cmin = cval; idx.cmin = i; }
				else 	{
						if(cval < cmin) { cmin = cval; idx.cmin = i; }
						}
				
						
				if(is.na(cmax)) 
						{ cmax = cval; idx.cmax = i; }
				else 	{
						if(cval >= cmax) { cmax = cval; idx.cmax = i; }
						}
				
					
if(TRUE)
	{
cat("\n\t\t", "i:", i, "cval:", cval, " ... cmin[",idx.cmin,"]: ",cmin, 
								" cmax[",idx.cmax,"]: ",cmax	);
	}
	
				

			
			}  # end of i 

		
		
			
		## UPDATE REMAINING
		nvec[min.idx] = cmin;  min.idx = 1 + min.idx;
		vec[idx.cmin] = NA;
		
		nvec[max.idx] = cmax;  max.idx = max.idx - 1;
		vec[idx.cmax] = NA;



if(TRUE)
	{
	from = paste0(vec, collapse=",");
	to = paste0(nvec, collapse=",");
cat("\n\t", " ... FROM: ", from, " ==> TO: ",	to , "\n");
	} 
 


		if(allNA(vec)) { break; }


		} # end of j 
		
		bigO = n^2;			# theoretical
		bigO.a = j * n;		# actual:: this is when we break out of the loop
		
if(TRUE)
	{
cat("\n\n", "SELECTION SORT: ",
	"\n\t\t", "Big O as n^2 : ", bigO,
	"\n\t\t", "For this, BIG O (a) as j*n : ", bigO.a,
	"\n\t\t\t", "There are: ", ntimes, "TIMES (min/max) was computed pairwise.",
	"\n\t\t\t", "There are: ", nticks, "COMPARISONS that were made.",
	"\n\n");	
	}
	
		minvisible(nvec);
		}


	
							# create a new bucket, place in order every time ...
							# sorting laundry... everything out of basket onto bed in sorted "piles" [before/after]
	if(METHOD == "ins")		# INSERTION SORT (laundrey)
		{
		
		nticks = 0; ncomps = 0;
		nvec = c(); 
		for(i in 1:n)
			{
			

if(TRUE)
	{
cat("\n\n ####################  HEADER [i] ################# \n");
	from = paste0(vec, collapse=",");
	to = paste0(nvec, collapse=",");

cat("\n", "i -->",i, " ... FROM: ", from, " ==> TO: ",	to );
	}
	
			cval = vec[i];
			if(length(nvec) == 0) { nvec = cval; next; }
			before = c(); after = c(); found = FALSE; 
			
			for(j in 1:length(nvec))
				{
				nticks = 1 + nticks;
				jval = nvec[j];
				
				if(!isFALSE(found)) { after = c(after, jval); }
				if(isFALSE(found))
					{
					if(cval < jval)
						{
						found = cval;
						after = c(after, jval);
						} else {
								before = c(before, jval);
								}	
					}	

if(TRUE)
	{
cat("\n\t\t", "j:", j, "jval:", jval, " ... before: ",before, " [cval:", cval, "] after: ",after	);
	}

					
				} #%% end j
			nvec = c(before, cval, after );	
							
			} #%% end i
		
		bigO = n^2;			# theoretical
		bigO.a = j * n;		# actual:: this is when we break out of the loop
		
if(TRUE)
	{
	from = paste0(vec, collapse=",");
	to = paste0(nvec, collapse=",");
 
cat("\n", "i -->",i, " ... FROM: ", from, " ==> TO: ",	to );

cat("\n\n", "INSERTION SORT: ",
	"\n\t\t", "Big O as n^2 : ", bigO,
	"\n\t\t", "For this, BIG O (a) as j*n : ", bigO.a,
	"\n\t\t\t", "There are: ", nticks, "COMPARISONS that were made.",
	"\n\n");	
	}
	
		minvisible(nvec);
		}
	
	
	
	
	
	
	
	nvec;
	}


