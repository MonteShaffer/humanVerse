				# row.width ... if chars, it truncates row.n 
				# row.n is +/-5 rows from center
				# truncates +/-5 to fit the row.width 
				# row.insert.sep (this is between two indexes)
				# = c(< 1) ... BEGIN
				# = c(1.5) ... BETWEEN 1 and 2
				# = c(1.1430980) ... behaves the same way ... 
				# = c(1) ... this assumes RIGHT side of [idx = 1]
				# = c(> dim(df)[1]) ... END 
				

	

# quick.source("math", res);
# quick.source("pipple", res);
# pip(iris);
# monte = iris; rownames(monte) =  paste0(sample(c(letters, LETTERS, 0:150), 150), " ", as.character(monte$Species) );
# pip(monte, show.row.names=TRUE)



pip = function() {}
pip = function(df, 
				row.idx = as.integer(dim(df)[1]/2),  # trap RAND or _RAND_
				col.idx = as.integer(dim(df)[2]/2),  # trap RAND or _RAND_ (see curl) ... maybe check.type ... 
				row.n = 5, col.n = 5,
				row.insert.sep = NULL, col.insert.sep = NULL,
				row.sep = "-", col.sep = "|", 
				isolate.row = TRUE, # create space around row to make it easier to read
				row.mark = c("*", "<---"),
				col.mark = c("*", u.toSymbol("U+2191")),  # TODO ... color would be easier
				intersect.sep = "+",				
				show.types = TRUE, 				
				show.col.names = TRUE,  show.col.numbers = TRUE,
				show.row.names = FALSE, show.row.numbers = TRUE,
				use.color = FALSE, 			 	
				char.more = u.toSymbol("U+22EF"),
				col.width = 20, # could set to a VECTOR for EACH col ... if length mismatch, DEFAULT = 12 will be used ... with warning
				number.format = "Natural",	
				df.width = options("width"), # EVERYTHING is this CHARS
				use.max.width = TRUE,
				df.justify = "center",      # fixed width cols ( )DATA( )
				sep = " ", left.pad=5, right.pad=5,
				...
				)
				#   u.toSymbol("U+27FD") ... left arrow 
				#  u.toSymbol("U+2191") ... up arrow 
				## "Fixed: total.width = 10",  # one or matching <double>/<complex> col _FORMAT_ templates
				## "Fixed: total.digits = 5, part=Re / + / Fixed: total.digits = 5, part=Im"
				## "Fixed: total.digits = 10"
				
				#####
				##### MATH FORMATTER with PARAMS may violate number.width ...
				## if you wanted better number formatting (e.g., per column), maybe create yourself ...  
				## or maybe a str template per column 
				## how to do smart NUMBER formatting 
				## fixed, scientific, engineering 
				## # "Fix: MAX_WIDTH, Re+Im  # this is total width Re + Im ... 2 lost to +i as.integer(MAX_WIDTH-2)/2 is length of Re and Im each ...
				# "Fix: MAX_WIDTH, Re 
				# "Fix: 8, Re  # this will OVERRIDE MAX_WIDTH either above or below
				# "Sci: 8 [Re assumed for now] force.scale=FALSE (or NUM)
				# Eng: KEY=VAL; KEY=VAL; 
				
	{
	rtype = toupper(as.character(substitute(row.idx)));
	if(str.contains("RAN", paste0(rtype, collapse="")))
		{
		row.idx = rand(1, dim(df)[1]);
		}
	ctype = toupper(as.character(substitute(col.idx)));
	if(str.contains("RAN", paste0(ctype, collapse="")))
		{
		col.idx = rand(1, dim(df)[2]);
		}
		
	if(!is.numeric(df.width)) { df.width = 120; }
	if(df.width < 60) { df.width = 120; }
		
	df.name = substitute(df);
	o.df = df; # original COPY 
	
	df.j = prep.arg(df.justify, 1);
	JUSTIFY = switch(df.j, 
						"l" = "left",
						"r" = "right",
					"center"
					);
					
					
	### Let's TRUNCATE THE DATA FIRST, SO LESS TOTAL PROCESSING
	
	{
### SHOW WHERE ###	
	ridx = as.integer(row.idx);
	## if idx = -n ... we are counting from the end
	if(is.negative(ridx)) { ridx = dim(o.df)[1] + ridx + 1; }
	if(is.zero(ridx)) { ridx = 1; }
	if(ridx >= dim(o.df)[1]) { ridx = dim(o.df)[1]; }
	
	cidx = as.integer(col.idx);
	## if idx = -n ... we are counting from the end
	if(is.negative(cidx)) { cidx = dim(o.df)[2] + cidx + 1; }
	if(is.zero(cidx)) { cidx = 1; }
	if(cidx >= dim(o.df)[2]) { cidx = dim(o.df)[2]; }
	
	## TAILS
	rlower = (ridx - row.n); if(rlower < 1) { rlower = 1; }
	rupper = rlower + row.n; if(rupper > dim(o.df)[1]) { rupper = dim(o.df)[1]; }
							 if(rupper >= ridx) { rupper = ridx - 1; }
	rdiff = rupper - rlower; 
	
	clower = (cidx - col.n); if(clower < 1) { clower = 1; }
	cupper = clower + col.n; if(cupper > dim(o.df)[2]) { cupper = dim(o.df)[2]; }
							 if(cupper >= cidx) { cupper = cidx - 1; }
	cdiff = cupper - clower; 
	
	# tails = so.df[ rlower:rupper, clower:cupper , drop=FALSE];
	# I just need the indexes ...
		
	# COULD BE NULL 
		# one = so.df[ridx, cidx ]; 
	# heads ... rrlower:rrupper, cclower:ccupper
	# there are four COMBOS with center "ONE"	
		

	rrupper = (ridx + row.n); 
		if(rrupper > dim(o.df)[1]) { rrupper = dim(o.df)[1]; }
	rrlower = rrupper - row.n; 	
		if(rrlower < 1) { rrlower = 1; }
		if(rrlower <= ridx) { rrlower = ridx + 1; }

	ccupper = (cidx + col.n); 
		if(ccupper > dim(o.df)[2]) { ccupper = dim(o.df)[2]; }
	cclower = ccupper - col.n; 	
		if(cclower < 1) { cclower = 1; }
		if(cclower <= cidx) { cclower = cidx + 1; }
	}

# we have 4 quadrants and center element (ridx, cidx)
		rs = unique( c(rlower:rupper, ridx, rrlower:rrupper) );
			rs = v.between(rs, 1, dim(o.df)[1]);  # in case we have 0's
		cs = unique( c(clower:cupper, cidx, cclower:ccupper) );
			cs = v.between(cs, 1, dim(o.df)[2]);  # in case we have 0's
	
	df = o.df[rs,cs];   # truncated tdf ... 
	
				
	# let's calculate the width of everything L-to-R based on 
	# longest strlen 
	cols = list();
	cols$names = colnames(df);
	cols$names.slen = str.len(cols$names);
	cols$types = v.types(df);
	cols$types.short = v.shortTypes( cols$types );
	cols$types.short.slen = str.len(cols$types.short);
	cols$length = length(cols$names);
	cols$length.slen = str.len(cols$length);
	
	cols$width = col.width;
	if(length(col.width) != cols$length) { cols$width = rep(col.width[1], cols$length); }
	# str(cols); 
	
	rows = list();
	rows$names = rownames(df);
	rows$names.slen = str.len(rows$names);
	rows$length = dim(df)[1];
		
	rows$length.slen = str.len(rows$length);
	rows$numbers = as.character(rs);
	rows$numbers.slen = str.len(rows$numbers);
	rows$namesEQUALnumbers = identical(rows$names, rows$numbers);
	
	# let's format each column based on constraint ...
	### UNNECESSARY, happens inside ... 
	## sdf = df.setColumnType(df, ALL, type="character");
	## sdf = df;
	## we need to format NUMBERS to FIXED in either
	## FIXED, SCI, ENG ...
	n = length(cols$types);
	j = 1;  # the paired numbered cols that will be "formatted"
	# or could do a list ... colname = formatter, colname = formatter 
	locked = logical(n);  # is the width LOCKED by formatter?
	for(i in 1:n)
		{
		type = cols$types[i];
		if(type %in% c("double", "complex", "integer"))
			{
			locked[i] = TRUE;
			TEMPLATE = number.format[j]; if(is.na(TEMPLATE)) { TEMPLATE = number.format[1]; }
			x = df[, i];
			y = pip.formatter(x, TEMPLATE);
			df[, i] = y;		
			j = 1 + j;
			} else {
					x = df[, i];
					y = pip.truncator(x, cols$width[i], " ", "BOTH", char.more);
					df[, i] = y;
					} 
		}
	### UNNECESSARY NOW ### 
	cols$slen.max = unlist(lapply( str.len(sdf) , max));
	# this is going to determine max ... 
	cols$locked = locked;
	
	
	### CALCULATE WIDTH NEEDED ###
	{
	sep.slen = str.length(sep);
	cwidth = 0; rwidth = 0;
	row.start = FALSE;
	slen.mark = str.len(row.mark[1]);
	if(show.row.numbers)
		{
		slen = max(rows$numbers.slen);
		# encased in brackets [  n  ], centered ... 
		cwidth = cwidth + (slen.mark + 1) + 1 + slen + 1 + sep.slen;  
		rwidth = rwidth + (slen.mark + 1) + 1 + slen + 1 + sep.slen; 
		row.start = TRUE;
		}
	if(show.row.names && !rows$namesEQUALnumbers)
		{
		slen.mark3 = (slen.mark + 1);
		if(row.start) { slen.mark3 = 0; } # don't double-count 
		
		slen = max(rows$names.slen);
		cwidth = cwidth + slen.mark3 + slen + sep.slen;
		rwidth = rwidth + slen.mark3 + slen + sep.slen;
		}	
	n = cols$length;
	cols$cmax = NULL;
	for(i in 1:n)
		{
		inc = cols$width[i];
		if(show.types) { inc = c(inc, cols$types.short.slen[i]); }
		if(show.col.names) { inc = c(inc, cols$names.slen[i]); }
		if(show.col.numbers) { inc = c(inc, cols$length.slen); }
		cmax = max(inc);
		cols$cmax[i] = cmax;
		cwidth = cwidth + cmax + sep.slen + sep.slen;		
		}
	}

	
	cremaining = df.width - cwidth;
	sep.extra.len = 0;	
	sep.extra = "";
	# keep each column as narrow as it should be 
	# rgb ... r, g, b ... very narrow 
	# take extra info and add to space between each COLUMN ... 
	# the index of numbers for cols [1] seems to be off by one space
	# from correctly aligning with r, <int> on CENTER ... 
	# pip(color.dictionary);
	if(use.max.width)
		{
		while(cremaining > ((sep.slen * left.pad) + 3*cols$length ) )
			{
			cols$cmax[i] = 1 + cols$cmax[i];
			cwidth = cwidth + 1*cols$length;
			cremaining = df.width - cwidth;
			sep.extra.len = sep.extra.len + 1;  # not multivariate
			}
		sep.extra = str.rep(" ",sep.extra.len);
		}
	
		
		

	### DISPLAY TIME ###  
		# if(cremaining < 0) ... we have to RECONFIGURE
		# from the CENTER, we have to remove rows ...
		
		
		
		cpad.left = switch(JUSTIFY,
								"center" = as.integer(cremaining/2),
								"left"   = sep.slen * left.pad,
								"right"  = cremaining - (sep.slen * right.pad),
							as.integer(cremaining/2)
							);
	
	## OVERFLOW, for now ...
		if(cpad.left < left.pad ) { cpad.left = left.pad; }
	
		
	## let's CAT 	
		## maybe cat.color 

	## TOP->DOWN:  A pipple of [df.name] : dim x dim 
	##				[col.num]
	## 			<b>col.name</b> (centered)
	##           <col.type> ... COLLLISION with "colors"? I don't think so 
	## CENTER (x more rows ABOVE )
	## [row.num] <b>row.name<b>  .... each column vertical sep as needed
	## ------------ horizontal ... collision with vertical sep is "+"
	##                                  TODO allow other custom BLOCKS
	## CENTER (x more rows BELOW )
	## AGAIN ... <b>col.name</b> (centered)
	
	## I don't know how I am going to do R-L truncation and display YET 
	##
## TOP->DOWN:  A pipple of [df.name] : dim x dim 
	space = str.rep(sep, as.integer(cpad.left/sep.slen));
	rspace = str.rep(sep, as.integer(rwidth/sep.slen));
	# trail = str.rep(sep, as.integer(right.pad/sep.slen));
		row.sep.slen = str.len(row.sep);
		col.sep.slen = str.len(col.sep);
#dput(cwidth);
#dput(rwidth);

		# times symbol ... u.toSymbol("U+00D7")
		
		cstr = str.rep( row.sep, as.integer((cwidth-rwidth)/row.sep.slen) );
		# one more ... sep.extra.len on RIGHT side
		cstr = str.pad( cstr, (cwidth-rwidth)+(slen.mark + 1)+(2*sep.extra.len), row.sep, "BOTH");  
	cline = paste0(space, rspace, cstr, "\n");
	
		cstr = paste0("A pipple of [", df.name, "]: ", 
						dim(o.df)[1],
						"",u.toSymbol("U+00D7"),"", 
						dim(o.df)[2]);
		cstr = str.pad( cstr, (cwidth-rwidth), " ", "BOTH");
	cat("\n", space, rspace, cstr, "\n");
	cat(cline);

## 			<b>col.name</b> (centered)	
	if(show.col.names)
		{
		cat(space, rspace, sep.extra);
		# cn are actual indexes of entire df ... 
		for(cn in cs)
			{
			cstr = str.pad( cols$names[cn], " ", "BOTH", cols$cmax[cn]); 
			cat(cstr, sep, sep.extra);			
			}
		cat("\n");
		}
	

##           <col.type> ... COLLLISION with "colors"? I don't think so 
	if(show.types)
		{
		cat(space, rspace, sep.extra);
		# cn are actual indexes of entire df ... 
		for(cn in cs)
			{
			cstr = cols$types.short[cn];
			cstr = str.pad( cstr, cols$cmax[cn], " ", "BOTH"); 
			cat(cstr, sep, sep.extra);			
			}
		cat("\n");
		}
	
	
	# show col.mark[1]
		cat(space, rspace, sep.extra);
		# cn are actual indexes of entire df ... 
		ci = 0;
		for(cn in cs)
			{
			ci = 1 + ci;
			cstr = "";
			if(cidx == cs[ci]) { cstr = col.mark[1]; }
			cstr = str.pad( cstr, cols$cmax[cn], " ", "BOTH"); 
			cat(cstr, sep, sep.extra);			
			}
		cat("\n");
	
	
	
 cat(cline);

## CENTER (x more rows ABOVE )
##
	rows.above =  rs[1] - 1;
	if(rows.above > 0)
		{
		cat(space, rspace);
			cstr = paste0("... ",rows.above," records above ...");
			cstr = str.pad( cstr, (cwidth-rwidth), " ", "BOTH");
		cat(cstr);
		cat("\n");
		cat(cline);
		}


##				[col.num]		
	if(show.col.numbers)
		{
		cat("\n");
			# sep.extra = str.rep(" ",sep.extra.len);
			cat(space, rspace, sep.extra);
			# cn are actual indexes of entire df ... 
			for(cn in cs)
				{
				#cstr = paste0(" [", cn ,"]");
				cstr = paste0("(", cn ,")");
				cstr = str.pad( cstr, cols$cmax[cn], " ", "BOTH"); 
				cat(cstr, sep, sep.extra);			
				}
			cat("\n");
		cat("\n");
		}






################ MAIN EVENT #####
	ri = 0;
	for(rn in rs)
		{
		ri = 1 + ri;
	if(isolate.row && (ridx == rs[ri])) {cat("\n"); } # create a blank line
		cat(space);
	
	
	if(show.row.numbers)
		{
		extra = ""; if(ridx == rs[ri]) { extra = paste0(row.mark[1], " "); }
		cstr = paste0(extra, "[", rn, "]");
		cstr = str.pad( cstr, (1+slen.mark) + 2+max(rows$numbers.slen), " ", "LEFT");
		cat(cstr, sep);	
		}

	if(show.row.names && !rows$namesEQUALnumbers)
		{
		cstr = str.pad( rows$names[ri], max(rows$names.slen), " ", "BOTH"); 
		cat(cstr, sep);				
		}			
		
		
	cat(sep.extra);	
		
		
		
		for(cn in cs)
			{
			cstr = df[ri,cn];  # I believe already formatted
								# NUMBERS are right JUSTIFIED based on FORMATTER
			cstr = str.pad( cstr, cols$cmax[cn], " ", "BOTH"); 
			cat(cstr, sep, sep.extra);			
			}
			
		if(ridx == rs[ri])
			{
			cstr = row.mark[2];
			cat(cstr, sep);
			}
		cat("\n");	


		if(isolate.row && (ridx == rs[ri])) {cat("\n"); } # create a blank line
		}









##				[col.num]		
	if(show.col.numbers)
		{
		cat("\n");
			# sep.extra = str.rep(" ",sep.extra.len);
			cat(space, rspace, sep.extra);
			# cn are actual indexes of entire df ... 
			for(cn in cs)
				{
				#cstr = paste0(" [", cn ,"]");
				cstr = paste0("(", cn ,")");
				cstr = str.pad( cstr, cols$cmax[cn], " ", "BOTH"); 
				cat(cstr, sep, sep.extra);			
				}
			cat("\n");
		cat("\n");
		}







## CENTER (x more rows BELOW )
	rows.below =  dim(o.df)[1] - rs[length(rs)];
	if(rows.below > 0)
		{
		cat(cline);
		cat(space, rspace);
			cstr = paste0("... ",rows.below," records below ...");
			cstr = str.pad( cstr, (cwidth-rwidth), " ", "BOTH");
		cat(cstr);
		cat("\n");
		}

	## 			<b>col.name</b> (centered)	
	if(show.col.names)
		{
		cat(cline);
		cat(space, rspace, sep.extra);
		# cn are actual indexes of entire df ... 
		for(cn in cs)
			{
			cstr = str.pad( cols$names[cn], cols$cmax[cn], " ", "BOTH"); 
			cat(cstr, sep, sep.extra);			
			}
		cat("\n");
		} else {
				if(show.col.numbers)
					{
					cat(cline);
					cat(space, rspace, sep.extra);
					# cn are actual indexes of entire df ... 
					for(cn in cs)
						{
						cstr = paste0("[", str.pad(cn, cols$length.slen, " ", "BOTH") ,"]");
						cstr = str.pad( cstr, cols$cmax[cn], " ", "BOTH"); 
						cat(cstr, sep, sep.extra);			
						}
					cat("\n");
					}		
				}
	
	cat("\n");
	# show col.mark[2]
	cm2 = str.trim(str.explode("", col.mark[2]));
	cm2n = length(cm2);
	for(cm2i in 1:cm2n)
		{
		cat(space, rspace, sep.extra);
		# cn are actual indexes of entire df ... 
		ci = 0;
		for(cn in cs)
			{
			ci = 1 + ci;
			cstr = "";
			if(cidx == cs[ci]) { cstr = cm2[cm2i]; }
			cstr = str.pad( cstr, cols$cmax[cn], " ", "BOTH"); 
			cat(cstr, sep, sep.extra);			
			}
		cat("\n");
		}
	
	
	
	
###  END
	cat("\n\n");
























	}




 
pip.truncator = function(x, cwidth=22, sep=" ", side="BOTH", trunc.sym = ">")
	{
	x = as.character(x); # assuming string, but may be FACTOR
	x.slen = str.len(x);
	idx = (x.slen <= cwidth);
	x[idx] = str.pad(x[idx], cwidth, sep, side);
	x[!idx] = paste0( substring(x[!idx], 1, (cwidth-1) ), trunc.sym);
	x;
	}

pip.truncater = pip.truncator;
					
					
pip.numFunction = function(format="Natural")
	{
	f = prep.arg(format, n=1, case="upper");
	NUM_FUNCTION = switch(f, 
							"E" = "num.toEng",
							"F" = "num.toFix",
							"N" = "num.toNat",
							"S" = "num.toSci",
						"num.toNat"
						);
	NUM_FUNCTION;
	}

pip.formatter = function(x, TEMPLATE)
	{
#cat("\n in formatter \n");	
#dput(TEMPLATE);
	clist = list(x);  # list for do.call 
	fo = str.explode(":", TEMPLATE);
#dput(fo);
	## THIS is key on what function to call 
	## [F]ixed => num.toFixed; [S]ci => num.toSci; [E]ng => num.toEng
	fn.name = pip.numFunction(fo[1]);
	
#dput(clist);	
	if(is.na(fo[2])) { return( do.call(fn.name, clist) ); }
		
	# put everything BACK but the removed key 
	fr = str.replace( paste0(fo[1],":"), "",  str.implode(":", TEMPLATE));
	## key=val, key2=val2, and so on ...
	fpar = str.explode("," , str.trim(fr) );
#dput(fpar);
	fpkv = str.trim(str.explode("=", fpar) );
	fpkeys = list.getElements(fpkv, 1);
	fpvals = list.getElements(fpkv, 2);
#cat("\n KEYS/VALS \n");
#dput(fpkeys);
#dput(fpvals);
		idx = v.which(fpvals, NA); 		
#dput(clist);	
	if(!is.null(idx)) 
		{ 
		cat.warning("\n KEY->VAL pairs don't match, calling fn.name with DEFAULT params \n");
		return( do.call(fn.name, clist) ); 
		}
	 
	# maybe cast back into format required from INFO based on types
	finfo = function.info(fn.name, character.only=TRUE)$params;
	n = length(fpkeys);
	for(i in 1:n) 
		{
		key = fpkeys[i];		
		val = fpvals[i];
		idx = v.which(finfo$keys, key);
		if(!is.null(idx))
			{
			typ = finfo$types[idx];
			if(!(typ %in% c("symbol", "language")))
				{
				# if(typ == "NULL") { mlist[[key]] = NULL; }
				clist[[key]] = as.type(val, typ);
				}			
			}
		}
#dput(clist);		
	do.call(fn.name, clist);
	}



pip.formattor = pip.formatter;



















xdf = structure(list(V1 = c("     df     ", "   row.n    ", "  row.idx   ", "   col.n    ", "  col.idx   ", "  row.sep   ", "  col.sep   ", "  int.sep   ", "row.insert.⋯", "col.insert.⋯", " row.width  ", " show.types ", "show.row.na⋯", "show.row.nu⋯", "show.col.na⋯", "show.col.nu⋯", " use.color  ", "  justify   ", "    sep     ", "    ...     "), V2 = c("            ", "     5      ", "stats.media⋯", "     5      ", "stats.media⋯", "     -      ", "     |      ", "     +      ", "    NULL    ", "    NULL    ", "   FALSE    ", "    TRUE    ", "   FALSE    ", "    TRUE    ", "    TRUE    ", "    TRUE    ", "   FALSE    ", "    left    ", "            ", "            "), V3 = c("   symbol   ", "   double   ", "  language  ", "   double   ", "  language  ", " character  ", " character  ", " character  ", "    NULL    ", "    NULL    ", "  logical   ", "  logical   ", "  logical   ", "  logical   ", "  logical   ", "  logical   ", "  logical   ", " character  ", " character  ", "   symbol   "), num = c("      0.00", "      0.00", "     -0.00", "      0.00", "      1.43", "      8.63", "      1.89", "    839.21", "  -2009.39", "-135429.88", "      0.00", "     -0.00", "      0.00", "     -0.09", "     -0.44", "      6.10", "   -111.14", "  -1190.51", "   1636.14", "-106248.89")), row.names = c(NA, -20L), class = "data.frame");
