magicFunction = function(FUN.OBJ.OR.STR)
	{
	# do something here ...
	FUN.AS.STR;
	}

v.math = function(data=c("#abcdef","#123456"), FUN, param="hi", FUN.pre="hex2dec", FUN.post=dec2hex)
	{
	# takes input [whether a str/obj] and returns a string.
	fn.str = magicFunction(FUN);
	fn.pre = magicFunction(FUN.pre);
	fn.post = magicFunction(FUN.post);

	# get to the main event 
	}


v.math = function(..., fn="sum", fn.pre="hex2dec", fn.post="dec2hex")
	{
	# get names / values ... 
	# xlist = list(...);
	# dots = match.call(expand.dots = FALSE)$...
	# names = as.character(dots);
	###### THIS is a vec function ... 
	vec = dots.addTo(NULL, ...);
# dput(vec);
	vec = do.call(fn.pre, list(vec));
# dput(vec); 
	vec = do.call(fn, list(vec));
# dput(vec);
	vec = do.call(fn.post, list(vec));
# dput(vec);
	minvisible(vec, print=TRUE);	
	}


 

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
 
 
 
 
 
 
 
 
 
 
 
 

# oper.which("!= 3");   # NO x 
# oper.which("3 != ");
# oper.which("== 3");
# oper.which("= 3");

oper.which = function(str)
	{
	kname = NULL;
	if(str.contains("<=", str)) 
		{ 
		o = key = "<=";
		kname = "LEQ";
		rkey = ">";
		rname = "G";
		}
	if(is.null(kname) && str.contains("<>", str)) 
		{ 
		o = key = "<>";  # Lotus 1-2-3
		kname = "NEQ";
		rkey = "=";
		rname = "EQU";
		}
	if(is.null(kname) && str.contains("<", str)) 
		{ 
		o = key = "<";
		kname = "L";
		rkey = ">=";
		rname = "GEQ";
		}
	if(is.null(kname) && str.contains(">=", str)) 
		{ 
		o = key = ">=";
		kname = "GEQ";
		rkey = "<";
		rname = "L";
		}
	if(is.null(kname) && str.contains(">", str)) 
		{ 
		o = key = ">";
		kname = "G";
		rkey = "<=";
		rname = "LEQ";
		}
	if(is.null(kname) && str.contains("!=", str)) 
		{ 
		# != BECOMES <> so no collision with "=" in str.contains 
		o = "!=";
		key = "<>";  # Lotus 1-2-3
		kname = "NEQ";
		rkey = "=";
		rname = "EQU";
		}
	if(is.null(kname) && str.contains("==", str)) 
		{ 
		o = key = "==";
		kname = "EQU";  # can't contain "L" 	
		rkey = "<>";
		rname = "NEQ";
		}
	if(is.null(kname) && str.contains("=", str)) 
		{ 
		o = "=";
		key = "==";
		kname = "EQU";	
		rkey = "<>";
		rname = "NEQ";
		}
		
	otmp = str.trim(str.explode(o, str));	
	r = as.numeric(str.trim(str.implode("", str.explode(o, str))));
	nfirst = FALSE; if(otmp[1] != "") { nfirst = TRUE; }
	
	if(is.null(kname)) { return(NULL); }
	list("key" = key, "keyname" = kname, "remaining" = r, "original" = o, "reverse.key" = rkey, "reverse.keyname" = rname, "number.first" = nfirst);
	}




## by.idx or by.value of the VEC 
# return final.idx or vec[final.idx] 
v.smart = function(vec, test = " x <= 12 ", varname="x", 
							by="value", return = "vector"
					)
	{
debug = FALSE;

#dput(vec);
	b = prep.arg(by, n = 1); # COMPARISON of "values" or "indexes"
	vecIDX = 1:length(vec);
#dput(vecIDX);
#dput(b);
	vecT = vec; if(b == "i") { vecT = vecIDX; }
	r = prep.arg(return, n = 1); # RETURN of "values" or "indexes"
	
	## v.smart(1:30, " 3 >= x != 5   ")
#dput(vecT);
	
	
	## parse EQUALITY as generic function???
	## searching for up to 3 things ... x can be in different places?
	## x <= 12 ... 12 < x ... 12 < x < 22 [for 3, x has to be in middle]
	# looks like READING, R-L parser ...
	{
		# force univariate 
	one = str.trim(str.explode(varname, str.trim(test[1]) ));  
	
	lower = NULL; upper = NULL; lower.equal = FALSE; upper.equal = FALSE;
	lower.is.equal = upper.is.equal = FALSE;
	lower.is.NOTequal = upper.is.NOTequal = FALSE;
	n.one = length(one); 
	o.first = o.last = NULL;
	# f = c("OPER", "VAR");  f = c("VAR", "OPER"); 
	if(n.one == 1 || (n.one==2 && one[1] == "") ) 
		{ 
		newone = paste0(one, collapse="");
		o.first = oper.which(newone);
if(debug)
	{
cat("\n newone : '",newone,"' \n");
cat("\n\t\t str( oper.which('",newone,"') ); \n\n");
	}
			SIGN1_G = str.contains("G", o.first$keyname);
			SIGN1_L = str.contains("L", o.first$keyname);
			SIGN1_N = str.contains("NEQ", o.first$keyname);
			SIGN1_E = str.contains("EQU", o.first$keyname);
			# PART is EQUAL: ==, >=, <=
			SIGN1_PE = str.contains("=",o.first$key);  
			NUMSTART1 = o.first$number.first;
		
if(debug)
	{
cat("\n ONE ----> DEFAULT \n");	
	}		
			# DEFAULT ... 
			lower 				= o.first$remaining;
			lower.equal 		= SIGN1_PE;
			lower.is.NOTequal 	= SIGN1_N;
			lower.is.equal 		= SIGN1_E;
			
			# " 3 < x " ... " x < 3 "
			# " 3 <= x " ... " x <= 3 "

		if( (SIGN1_G && NUMSTART1) || 
			(SIGN1_L && !NUMSTART1) 
			)
			{
if(debug)
	{
cat("\n ONE ----> REVERSE \n");
	}
			# " 3 > x " ===> " x < 3 "
			# " 3 >= x " ===> " x <= 3 "
			# let's recast into upper terms ... 
			upper 				= lower;
			upper.equal 		= lower.equal;
			upper.is.equal 		= FALSE;
			upper.is.NOTequal 	= lower.is.NOTequal;
			# RESET lower 
			lower 				= NULL; 
			lower.equal 		= FALSE; 
			lower.is.equal 		= FALSE; 
			lower.is.NOTequal 	= FALSE;
			}			 

		}  else {
				# we have a COMPOUND ... 
				
				o.first = oper.which(one[1]);
					SIGN1_G = str.contains("G", o.first$keyname);
					SIGN1_L = str.contains("L", o.first$keyname);
					SIGN1_N = str.contains("NEQ", o.first$keyname);
					SIGN1_E = str.contains("EQU", o.first$keyname);
					# PART is EQUAL: ==, >=, <=
					SIGN1_PE = str.contains("=",o.first$key);  
					NUMSTART1 = o.first$number.first;					
				o.last = oper.which(one[2]);
					SIGN2_G = str.contains("G", o.last$keyname);
					SIGN2_L = str.contains("L", o.last$keyname);
					SIGN2_N = str.contains("NEQ", o.last$keyname);
					SIGN1_E = str.contains("EQU", o.last$keyname);
					# PART is EQUAL: ==, >=, <=
					SIGN1_PE = str.contains("=",o.last$key);  
					NUMSTART2 = o.last$number.first;
				
				# test = NUM1 SIGN1 x SIGN2 NUM2
				# test = " 9 <= x <= 15 ";
				#            LEQ  LEQ ... works as expected ...
				
				# P(9 ≤ x ≤ 15) 
				# test = " 9 <= x <= 15 ";
				# test = " 9 <= x <> 15 ";
				# test = " 9 <> x <= 15 ";
				# # # test = " 9 <> x <> 15 "; 
				if( (SIGN1_L && SIGN2_L) || 
					(SIGN1_L && SIGN2_N) || 
					(SIGN1_N && SIGN2_L) || 
					(SIGN1_N && SIGN2_N)
					)
					{
if(debug)
	{
cat("\n TWO ----> CASE 1 \n");
	}
					lower = o.first$remaining;
					lower.equal = str.contains("=",o.first$key);
					lower.is.NOTequal = (o.first$key == "<>");
					lower.is.equal = (o.first$key == "==");
					
					upper = o.last$remaining;
					upper.equal = str.contains("=",o.last$key);
					upper.is.NOTequal = (o.last$key == "<>");
					upper.is.equal = (o.last$key == "==");
					}
				
				# test = " 9 >= x >= 15 ";
				#            GEQ  GEQ ... x > 15 and x < 9 == NULL ...
				# test = " 15 >= x >= 9 ";
				#                         x > 9  and x < 15 should work 
				# test = " 9 >= x <> 15 "; # 15 does nothing 
				# test = " 9 <> x >= 15 "; # 9 does nothing 
				# test = " 15 >= x <> 9 "; # removes 9
				# test = " 15 <> x >= 9 "; # removes 15 
				if( (SIGN1_G && SIGN2_G) || 
					(SIGN1_G && SIGN2_N) || 
					(SIGN1_N && SIGN2_G)
					)
					{
if(debug)
	{
cat("\n TWO ----> CASE 2 \n");
	}
					upper = o.first$remaining;
					upper.equal = str.contains("=",o.first$key);
					upper.is.NOTequal = (o.first$key == "<>");
					upper.is.equal = (o.first$key == "==");
					 
					lower = o.last$remaining;
					lower.equal = str.contains("=",o.last$key);
					lower.is.NOTequal = (o.last$key == "<>");
					lower.is.equal = (o.last$key == "==");
					}
				
				
				
				
				# test = " 9 >= x <= 18 ";
				#            GEQ  LEQ ... x <= 9 and x < 18 SO just x <= 9 ...
				# test = " 18 >= x <= 9 ";  # x <= 9 and x < 18 so just x <= 9
				# x < 9 as in x < min(9,18) 
				# test = " 9 > x <= 18 "; x < 9
				
				# # test = " 18 >= x <= 9 ";
				
				
				# test = " 9 <= x >= 15 ";
				#            LEQ  GEQ ... x > 9 and x > 15 so just x > 15 
				# test = " 15 <= x >= 9 ";
				#            LEQ  GEQ ... x > 15 and x > 9 so just x > 15 
				# x > 15 as in x > max(9,15)
				
				if( (SIGN1_G && SIGN2_L) || 
					(SIGN1_L && SIGN2_G)
					)
					{
					if( (SIGN1_L && SIGN2_G))
						{ 
if(debug)
	{
cat("\n TWO ----> CASE 3a \n");
	}
						marker = max(o.first$remaining, o.last$remaining);
						} else {
if(debug)
	{
cat("\n TWO ----> CASE 3b \n");
	}
								marker = min(o.first$remaining, o.last$remaining);
								}
					if(marker == o.first$remaining)
						{						
						SIGN_G 	= SIGN1_G;
						SIGN_L 	= SIGN1_L;
						SIGN_N 	= SIGN1_N;
						SIGN_E 	= SIGN1_E;
						SIGN_PE = SIGN1_PE;
						} else {
								SIGN_G 	= SIGN2_G;
								SIGN_L 	= SIGN2_L;
								SIGN_N 	= SIGN2_N;
								SIGN_E 	= SIGN2_E;
								SIGN_PE = SIGN2_PE;
								}
					
					upper 				= marker;
					upper.equal 		= SIGN_PE;
					upper.is.NOTequal 	= SIGN_N;
					upper.is.equal 		= SIGN_E;
					
					}
				
				
								
				
				# v.smart(1:30, test = " 9 >= x >= 18 ")
				# v.smart(1:30, test = " 9 >= x <= 18 ")

				# nonsensical??
				# test = " 9 <= x == 15 ";
				# test = " 9 == x <= 15 ";
	if(is.null(lower) && is.null(upper))
		{
if(debug)
	{
	cat("\n TWO ----> CASE ??UNKNOWN?? \n");
	}
		}
				
				}
	# parse to  lower LSIGN x USIGN upper ... possible NULL 
	}

if(debug)
	{
cat("\n", " test : ", test, "\n\t\t\t lower : ", lower, 
							"\n\t\t\t lower.equal : ", lower.equal, 
							"\n\t\t\t lower.is.equal : ", lower.is.equal, 
							"\n\t\t\t lower.is.NOTequal : ", lower.is.NOTequal,
							"\n\t\t\t upper : ", upper, 
							"\n\t\t\t upper.equal : ", upper.equal, 
							"\n\t\t\t upper.is.equal : ", upper.is.equal, 
							"\n\t\t\t upper.is.NOTequal : ", upper.is.NOTequal,
	"\n\n");
	}
	
	# compute final.idx 
	# everything is now *ANCHORED* to vecT ... by=by, return=return 
	{
	final.idx = NULL;
	if(lower.is.equal && upper.is.equal) { stop("what ... two operators both can't be equal ... 3 = x = 5 ... nonsensical"); }
	# v.which if EQUAL 
	# v.between if LE, L, GE, GE 
	if(lower.is.equal)
		{
		final.idx = v.which(vecT, what=lower);		
		}		
	if(is.null(final.idx) && upper.is.equal)
		{
		final.idx = v.which(vecT, what=upper);		
		}	

	if(is.null(final.idx) && lower.is.NOTequal && upper.is.NOTequal) 
		{ 
		# 3 != x != 5 
		# ALL but two elements ...
		f.lower = v.which(vecT, what=lower);
		f.upper = v.which(vecT, what=upper);
		f.join = set.union(f.lower,f.upper);
		final.idx = v.return(vecT[-c(f.join)]);
		}
		
	if(is.null(final.idx) && lower.is.NOTequal) 
		{ 
		# 3 != x >= 2 
		# ALL but one elements ...
		f.lower = v.which(vecT, what=lower);
		f.other = NULL;
		
		if(is.null(upper))
			{
			final.idx = vecIDX[-c(f.lower)];
			} else {
					# f.other = v.between(vec, lower=NULL, upper=upper, lower.equal=FALSE, upper.equal=upper.equal, by=by, return="indexes");	
					f.other = v.between(vecT, lower=NULL, upper=upper, lower.equal=FALSE, upper.equal=upper.equal, by="value", return="index");
					final.idx = v.return(set.diff(f.other, f.lower));
					
					}
if(debug)
	{
	cat("\n OUT ----> f.other,f.lower : ", final.idx, " \n");
	cat("\n\t\t\t f.other : ", f.other, " \n");
	cat("\n\t\t\t f.lower : ", f.lower, " \n");
	}
		}
		
	if(is.null(final.idx) && upper.is.NOTequal) 
		{ 
		# 3 >= x != 5 
		# ALL but one elements ...
		f.upper = v.which(vecT, what=upper);
		f.other = NULL;
		if(is.null(lower))
			{
			final.idx = vecIDX[-c(f.upper)];
			} else {
					#f.other = v.between(vec, lower=lower, upper=NULL, lower.equal=lower.equal, upper.equal=FALSE, by=by, return="indexes");	
					f.other = v.between(vecT, lower=lower, upper=NULL, lower.equal=lower.equal, upper.equal=FALSE, by="value", return="index");	
					final.idx = v.return(set.diff(f.other, f.upper));
					}
if(debug)
	{
	cat("\n OUT ----> f.other,f.upper : ", final.idx, " \n");
	cat("\n\t\t\t f.other : ", f.other, " \n");
	cat("\n\t\t\t f.upper : ", f.upper, " \n");
	}
		}

	if(is.null(final.idx))
		{
		# final.idx = v.between(vec, lower=lower, upper=upper, lower.equal=lower.equal, upper.equal=upper.equal, by=by, return="indexes");
		final.idx = v.between(vecT, lower=lower, upper=upper, lower.equal=lower.equal, upper.equal=upper.equal, by="value", return="index");
if(debug)
	{
	cat("\n OUT ----> v.between : ", final.idx, " \n");
	}
		}	
	}
	
	if(is.null(final.idx)) { return(NULL); }
	
	
	
	
	if(b == "i" && r == "i") { res = (v.return(final.idx)); } 
	if(b == "i" && r == "v") { res = (v.return(vec[final.idx])); }
	
	if(b == "v" && r == "i") { res = (v.return(final.idx)); }   
	if(b == "v" && r == "v") { res = (v.return(vec[final.idx])); }
	  
if(debug)
	{
cat("\n b: ", b, " \t r: ", r, 
				"\n\n\t\t\t vec[: ", vec, 
				"\n\n\t\t\t vecIDX[: ", vecIDX,
				"\n\n\t\t\t final.idx: ", v.return(final.idx), 
				"\n\n\t\t\t vec[final.idx: ", vec[final.idx], 
				"\n\n\t\t\t vecIDX[final.idx: ", vecIDX[final.idx],
	"\n\n");
	}
	
	return( v.return(res) );
	}
 
 
 
 
 
 
 
 
 
 
 
 
# between(x, lower, upper, incbounds=TRUE, NAbounds=TRUE, check=FALSE)
# x %between% y
  
v.between = function(vec, lower, upper, 
							lower.equal = TRUE,
							upper.equal = TRUE,
							by = "value",
							return = "vector")
	{
debug = FALSE;
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
if(debug)
	{
cat("\n v.between() \n");
	}


		
	if(b == "i" && r == "i") { res = (v.return(which(idx==TRUE))); } 
	if(b == "i" && r == "v") { res = (v.return(vec[idx])); }
	
	if(b == "v" && r == "i") { res = (v.return(which(idx==TRUE))); }   
	if(b == "v" && r == "v") { res = (v.return(vec[idx])); }
	  
if(debug)
	{
cat("\n b: ", b, " \t r: ", r, 
				"\n\n\t\t\t vec[: ", vec, 
				"\n\n\t\t\t vecIDX[: ", vecIDX,
				"\n\n\t\t\t idx: ", v.return(which(idx==TRUE)), 
				"\n\n\t\t\t vec[idx: ", vec[idx], 
				"\n\n\t\t\t vecIDX[idx: ", vecIDX[idx],
	"\n\n");
	}
	
	v.return(res);
	} 


# res as idx mostly
v.return = function(res)
	{
	if(is.null(res)) { return(NULL); }
	if(length(res) == 0) { return(NULL); }
	res;	
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
		
	if(invert) { idx = v.invert(vec,idx); }
	v.return(idx);
	}

v.invert = function(vec, idx)
	{
	IDX = 1:length(vec); idx = IDX[-c(idx)];
	v.return(idx);
	}

v.remove = function(vec, what="", invert=FALSE)
	{
	idx = v.which(vec, what=what, invert=invert);
	if(is.null(idx)) { return(vec); } # nothing to remove
	v.return(vec[ -c( idx ) ]);  
	}
	 


# subtract vec from parent ... 
v.truncate = function(vec, parent, by="value", invert=FALSE)  
	{
	BY = prep.arg(by, n=1);
	# shorten parent by removing vec 
	idx = set.match(vec, parent); 
	if(invert) { idx = v.invert(vec,idx); }
	res = (1:length(parent))[-c(idx)]; 
	if(BY=="v") { res = parent[-c(idx)]; }
	# res = set.diff(parent, vec);
	v.return(res);
	}




v.shortTypes = function(types, force.odd=TRUE)
	{
	n = length(types);
	res = character(n);
	for(i in 1:n)
		{
		type = types[i];
		# SWITCH is ***NOT*** multivariate 
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

		if(force.odd)
			{
			rn = str.len(res[i]);
			if(rn %% 2 == 0) { res[i] = str.replace("<"," <",res[i]); }
			}
		}
	res;
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
	

v.freq = function(vec, what="", invert=FALSE)
	{
	length(v.which(vec, what, invert=invert));
	}
	
v.mode = function(vec, invert=FALSE)
	{
	m = stats.mode(vec, force.numeric=FALSE); # values
	idx = set.match(m, vec);	# idx
	if(invert) { IDX = 1:length(vec); idx = IDX[-c(idx)]; }
	v.return(idx);
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
v.push = function(val, ..., key="-CURRENT_STACK-")
	{
	val = dots.addTo(val, ...);
	mem = memory.get(key, "STACK");
	if(is.null(mem)) { stop("You need to configure stack with v.stack() first!"); }
dput(mem);	
	# push onto end ... LIFO (javascript)
	# push on first ... FIFO (queueing) ... end may truncate ... 
	
	if(method == "LIFO")
		{
		# if val is VECTOR, this order is correct, rev(val) for FIFO?
		mem$vec = c(mem$vec, val);
		nv = length(mem$vec);  
		if(nv > mem$size) { mem$vec = mem$vec[(nv+1-mem$size):nv]; }		
		} else {
				mem$vec = c(rev(val), mem$vec);
				nv = length(mem$vec);  
				if(nv > mem$size) { mem$vec = mem$vec[1:mem$size]; }
				}	
	memory.set(key, "STACK", mem);	
	minvisible(mem, print="str");  #update the memory/history 
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
	minvisible(val, print=TRUE);
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
	



































