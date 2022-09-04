

# cdot ... U+22EF

# u.getSymbol(c("U+1F40C","U+22EF"));
# uu = u.getSymbol(c("U+22EF","U+1F40C","U+22EF"), collapse=TRUE);
#  "⋯🐌⋯" ... > length(uu) ... [1] 1 ..... > str.len(uu) ... [1] 3
## FIXED, something weird about intToUtf8(num); [collapsing]?
## MORE weirdness
# > uu = u.getSymbol(c("U+22EF","U+1F40C","U+22EF"), collapse=FAlSE);
# > uu
# [1] "⋯"  "🐌" "⋯" 
# > char.more = uu[1]
# > char.more
# [1] "⋯"
# > 


# MAYBE ALLOW a key ... 'EGYPTIAN HIEROGLYPH C020' or EGYPTIAN_HIEROGLYPH_C020
# U+13071

# THIS DOES SOMETHING ??? utf8ToInt("U+1F40C")
u.toNum = function(str = "U+22EF", ...)
	{
	str = dots.addTo(str, ...);	
	# if(str.contains("U+", str))
	uinfo = list.pair(str.explode("U+", str));
	utf   = list.getElements(uinfo, 2);
	
	
		# maybe cast \x ... OTHER formats ...
	# utf8ToInt(utf);
	as.integer(as.hexmode(utf));
	}
	
u.fromNum = function(num = 	128012, ..., collapse=FALSE)
	{
	num = dots.addTo(num, ...);
	# res = intToUtf8(num);  # not keeping separate elements ... collapsed
	# str.explode("",res);
	
	intToUtf8(num, multiple=!collapse);
	}
	
u.toSymbol = function(str = "U+22EF", ..., collapse=FALSE)
	{
	str = dots.addTo(str, ...);
	num = u.toNum(str);
	u.fromNum(num, collapse=collapse);
	}


u.getSymbol = u.toSymbol;

# utf8ToInt("U+1F40C")
		# utf8ToInt("\U1F40C"); # 128012; # intToUtf8(128012)
		# U+1F40C [snail]
		# plot(1, pch= intToUtf8(128024) )


				# row.width ... if chars, it truncates row.n 
				# row.n is +/-5 rows from center
				# truncates +/-5 to fit the row.width 
				# row.insert.sep (this is between two indexes)
				# = c(< 1) ... BEGIN
				# = c(1.5) ... BETWEEN 1 and 2
				# = c(1.1430980) ... behaves the same way ... 
				# = c(1) ... this assumes RIGHT side of [idx = 1]
				# = c(> dim(df)[1]) ... END 
pip = function() {}
pip = function(df, 
				row.n = 5, row.idx = as.integer(dim(df)[1]/2),
				col.n = 5, col.idx = as.integer(dim(df)[2]/2),
				row.sep = "-", col.sep = "|", int.sep = "+",
				row.insert.sep = NULL, col.insert.sep = NULL,
				show.types = TRUE, 				
				show.col.names = TRUE,  show.col.numbers = TRUE,
				show.row.names = FALSE, show.row.numbers = TRUE,
				use.color = FALSE, 
				
				equal.width = TRUE, # to min(char.width, number.width)
				char.width = 12, 
				char.more = u.getSymbol(c("U+22EF")),
				number.width = 8,
				number.format = "fixed",  # scientific, engineering
				df.width = 80,			  # EVERYTHING is this CHARS
				df.justify = "center",      # fixed width cols ( )DATA( )
				sep = " ",
				...
				)
	{
	n.f = functions.cleanKey(number.format, 1);
	NUM_FORMAT = switch(n.f, 
							"s" = "sci",
							"e" = "eng",
						"fix"
						);
	df.j = functions.cleanKey(df.justify, 1);
	JUSTIFY = switch(df.j, 
						"l" = "left",
						"r" = "right",
					"center"
					);
				
	# let's calculate the width of everything L-to-R based on 
	# longest strlen 
	cols = list();
	cols$names = colnames(df);
	cols$names.slen = str.len(cols$names);
	cols$types = v.types(df);
	cols$types.short = v.shortTypes( cols$types );
	cols$types.short.slen = str.len(cols$types.short);
	# str(cols); 
	
	cdata = list();
	# let's format each column based on constraint ...
	sdf = df.setColumnType(df, ALL, type="character");
	## we need to format NUMBERS to FIXED in either
	## FIXED, SCI, ENG ...
	
	
	cdata$slen = str.len(sdf);
	cdata$slen.max = unlist(lapply(cdata$slen, max));
	
	
	# show.types = TRUE, 				
				# show.col.names = TRUE,  show.col.numbers = TRUE,
	
	
	}



































# # pdf = as.data.frame( cbind(finfo$params$keys, finfo$params$values, finfo$params$types) );
functions.stepInto = function(...)
	{
debug = FALSE;
	finfo = function.info(...);
	if(is.null(finfo$params$keys)) { return(NULL); }
	n = length(finfo$params$keys);
	for(i in 1:n)
		{
		key = finfo$params$keys[i];
		val = finfo$params$values[i];
		typ = finfo$params$types[i];
if(debug)
	{
cat("\n key ::: ", key, "\t typ ::: ", typ, "\t val ::: ", val, "\n\n");
	}	
		
		if(key == "...") { next; }
		
		if(typ == "symbol")
			{  
			#### why ggget and setback ... just SKIP 
			# glo = ggget(key, -1);  # TRAPS NULL in error
			# if(!is.null(glo))
				# {
				# value = glo;
				# key %GLOBAL% value;
				# }
			next;
			}
			
		if(typ == "language") 
			{ 
			value = eval(parse(text = val));
			key %GLOBAL% value;
			next;
			}
			
		if(typ != "NULL")
			{
			value = as.type(val, typ);
			} else { value = val; }
		key %GLOBAL% value;
		}
	invisible(finfo$params);
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
	


