
				# row.width ... if chars, it truncates row.n 
				# row.n is +/-5 rows from center
				# truncates +/-5 to fit the row.width 
				# row.insert.sep (this is between two indexes)
				# = c(< 1) ... BEGIN
				# = c(1.5) ... BETWEEN 1 and 2
				# = c(1.1430980) ... behaves the same way ... 
				# = c(1) ... this assumes RIGHT side of [idx = 1]
				# = c(> dim(df)[1]) ... END 
pip = function(df, 
				row.n = 5, row.idx = stats.median(dim(df)[1]),
				col.n = 5, col.idx = stats.median(dim(df)[2]),
				row.sep = "-", col.sep = "|", int.sep = "+",
				row.insert.sep = NULL, col.insert.sep = NULL,
				row.width = FALSE,
				show.types = TRUE, 
				show.row.names = FALSE, show.row.numbers = TRUE,
				show.col.names = TRUE,  show.col.numbers = TRUE,
				use.color = FALSE, 
				justify = "left",
				sep = " ",
				...
				)
	{
	
	
	
	}



gget = function(x, ...)
	{
debug = FALSE;
	ginfo = suppressError( get(x, ...), 
								show.notice=debug,
								msg="debugging gget" 
							);
							
	if(is.error(ginfo)) { return(NULL); }
	ginfo;	
	}

functions.stepInto = function(...)
	{
	# fn = str.fromObjectName(...);
	finfo = function.info(...);
	# finfo = function.info("pip");
	n = length(finfo$params);
		# pdf = as.data.frame( cbind(finfo$params$keys, finfo$params$values, finfo$params$types) );
	for(i in 1:n)
		{
		key = finfo$params$keys[i];
		val = finfo$params$values[i];
		typ = finfo$params$types[i];
		
		# glo = get(key, -1);  # _GLOBAL_ as constant 
		# glo = mget(key, ifnotfound=NULL);
		glo = gget(key, -1);  # TRAPS NULL in error
		if(typ == "symbol" && !is.null(glo)) 
			{ 
			value = glo;
			# assign GLOBAL
			# assign(key, value, -1);
			next;
			}
			
		if(typ == "language") 
			{ 
			# no pos -1
			value = eval(parse(text = val), envir=parent.frame(1));  
			# assign GLOBAL
			# assign(key, value, -1);
			next;
			}
		
		value = as.type(val, typ);
		assign(key, value, envir=envir);		
		}
	
	invisible(finfo$params);	
	# not working in RSTUDIO, CLI ... why ?
	# list.extract( formals(fn), ... ); # by default into GLOBAL
	}


# get into weeds, compute strlen(cols), omit some columns
# allow for vertical isolation as well
# print.matrix ...
df.printHead = function(df, n=5, row.idx=10, ...)
	{
	# a 'nibble' ... 
	# a 'pipple' ... amazon funny stories, pizza pipple eagle
	# <chr> <dbl>  6x5

										
	nrow = nrow(df);
	idx = as.integer(row.idx);  		## offset, e.g, SKIP
										
	## if idx = -n ... we are going from the back (tail) ...
	if(is.negative(idx)) { idx = nrow + idx + 1; }
										
	if(is.zero(idx)) { idx = 1; }
	if(idx %>=% nrow) { idx = nrow; }
		
	# tails 			
		lower = (idx - n); 	if(lower < 1) 	 { lower = 1; }
		upper = lower + n; 	if(upper > nrow) { upper = nrow; }
							if(upper >= idx) { upper = idx - 1; }
		diff = upper - lower; 
		
	if(diff < 0) { tails = NULL; } else { 
										tails = df[ lower:upper, ]; 
										rownames(tails) = lower:upper;
										}
										one = df[idx, ]; 
										rownames(one) = idx;
	
	# heads 	
		upper = (idx + n); 	if(upper > nrow) { upper = nrow; }
		lower = upper - n; 	if(lower < 1) { lower = 1; }
							if(lower <= idx) { lower = idx + 1; }
	
	diff = upper - lower; 
	if(diff < 0) { heads = NULL; } else { 
										heads = df[ lower:upper , ]; 
										rownames(tails) = lower:upper;
										}
	res = list("tails"=tails, "one"=one, "heads"=heads);
	# light gray, a pipple 
	# red for NEG, list DIM(original) ... mentions hidden cols
	# https://www.tidyverse.org/blog/2018/01/tibble-1-4-1/
	
	
	
	invisible(res);
	}
	


