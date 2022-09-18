magicFunction = function(KEY, to="character")
	{
	TO = prep.arg(to, n=4);
	
	key = NULL;
	ct.KEY = check.type(KEY);
	if(!ct.KEY || !is.character(KEY) )	
		{ key = deparse(substitute(KEY)); }  # valid objects are stringed
		
	if(TO == "char")
		{
		if(is.null(key))  { return(KEY); }
		if(!is.null(key)) { return(key); }
		} 
	
	if(ct.KEY) { return(KEY); } # already an object ...
	
	# I have a string ... and need an object 
	return( eval(parse(text = KEY)) );
	
	stop("how did I get here");
	return(NULL);
	}
	
	
parse.syscall = function(syscall)
	{
	str = lang2str(syscall);
	info = str.explode("(" , str);
	fn = str.trim(info[1]);
		f 	 = as.list(formals(fn));
		keys = names(f);
		vals = as.character(f);		
		
	# put everything back but the function call 
	nstr = str.implode("(", info[-c(1)] );
	nstr = str.replace(")", "", nstr);
	ninfo = check.list(str.explode('=', str.trim(str.explode("," , nstr))));
	n = length(ninfo);
	pkeys = str.replace('"', "", str.trim(list.getElements(ninfo, 1)));
	pvals = str.trim(list.getElements(ninfo, 2));
	params = list();
	for(i in 1:n)
		{
		pval = pvals[i];
		if(!is.na(pval))
			{
			params[[ pkeys[i] ]] = eval(parse(text=pval));
			}
		# params[[ pkeys[i] ]] = NA;  # you can't always trap NULL ... 
		} 
		
	missing = length(keys) - length(pkeys);
	
	list(
		"fn" = fn, 
		"pkeys"  = pkeys,
		"params" = params, 
		"missing" = missing, 
		"formals" = f
		);
	}
   
# v.chain(vec, hex2dec, mean, dec2hex, hex.prepend)
v.chain = function(vec, ...)   
	{ 
	# this gives me sys.call and envir ... and now the fn.name with params 
	.%THIS%. ;  minvisible(THIS, display=none); 
	# fn = match.call()[[1]];  
	
	# make this a generic message
	if(THIS$fn.info$missing > 0) { print(str(THIS)); stop("looks like you have a [1] missing param in functon"); }
	pkeys = THIS$fn.info$pkeys;
	np = length(pkeys);
	if(np < 2) { stop("looks like there is nothing to do"); }

	# get to the main event 
	for(i in 2:np)
		{
		vec = do.call(pkeys[i], list(vec));
		}
	vec;
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



# toNA ... assigns elements as NA 
v.toNA = function(vec, idx)
	{
	vec[idx] = NA;
	vec;	
	}
	

	
# naTO, not NATO  

v.TO = function(vec, what="NA", to="")
	{
	if(what == "NA" || is.na(what)) {	vec[is.na(vec)] = to; }
	if(what == "Inf" || is.infinite(what)) { vec[is.infinite(vec)] = to; }
	vec;	
	}
	


	
 
 
 
 
 
 
 
 
 
 
 
 

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
	if(is.null(what))
		{
		# lists can trap NULLS, vectors can't 
		return(NULL);
		}
		
	if(is.null(idx) && is.na(what))
		{
		idx = which(is.na(vec));
		}	
	# type = v.type(what);
	if(is.null(idx) && is.logical(what))
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
	 
	 
prep.distribution = function(METHOD)
	{
	IN.init();
	key = NULL;
	if(is.null(key) && METHOD %IN% c("Uniform Distribution", "unif", "unif-dist")) 
		{ key = "unif"; }
	if(is.null(key) && METHOD %IN% c("Normal Distribution",  "norm", "norm-dist")) 
		{ key = "norm"; }
	if(is.null(key) && METHOD %IN% c("t Distribution",  "t", "stud-t", "t-dist")) 
		{ key = "t"; }
	if(is.null(key) && METHOD %IN% c("F Distribution",  "f", "f-dist")) 
		{ key = "f"; }
	if(is.null(key) && METHOD %IN% c("Chi-Squared Distribution",  "chisq", "chi-squa", "chi-dist")) 
		{ key = "chisq"; }
	if(is.null(key) && METHOD %IN% c("Beta Distribution",  "beta", "beta-dist")) 
		{ key = "beta"; }
	if(is.null(key) && METHOD %IN% c("Gamma Distribution",  "gamma", "gamm", "gamm-dist")) 
		{ key = "gamma"; }
	if(is.null(key) && METHOD %IN% c("Cauchy Distribution",  "cauchy", "cauc", "cauc-dist")) 
		{ key = "cauchy"; }
	if(is.null(key) && METHOD %IN% c("Exponential Distribution",  "exp", "expo", "exp-dist", "expo-dist")) 
		{ key = "exp"; }
	if(is.null(key) && METHOD %IN% c("Binomial Distribution",  "binom", "bino", "bino-dist")) 
		{ key = "binom"; }
	if(is.null(key) && METHOD %IN% c("Negative-Binomial Distribution",  "nbinom", "nbin", "nbin-dist", "nega-bino-dist", "neg-bino", "neg-bin", "neg-bino-dist", "neg-bin-dist")) 
		{ key = "nbinom"; }
	if(is.null(key) && METHOD %IN% c("Poisson Distribution",  "pois", "pois-dist")) 
		{ key = "pois"; }
	if(is.null(key) && METHOD %IN% c("Log-Normal Distribution",  "lnorm", "lnor", "lnor-dist", "logo-norm", "logo-norm-dist", "log-", "log-norm", "log-norm-dist")) 
		{ key = "lnorm"; }
		
	if(is.null(key) && METHOD %IN% c("Multinomial Distribution",  "multinom", "mult", "mult-dist", "mult-nom", "mult-nomi")) 
		{ key = "multinom"; }
	if(is.null(key) && METHOD %IN% c("Logistic Distribution",  "logis", "logi", "logi-dist")) 
		{ key = "logis"; }
		
	if(is.null(key) && METHOD %IN% c("Weibull Distribution",  "weibull", "weib", "weib-dist")) 
		{ key = "weibull"; }
		
	if(is.null(key) && METHOD %IN% c("Geometric Distribution",  "geom", "geom-dist")) 
		{ key = "geom"; }
	if(is.null(key) && METHOD %IN% c("HyperGeometric Distribution",  "hyper", "hype", "hype-dist","hype-geo", "hype-geom", "hype-geo-dist", "hype-geom-dist")) 
		{ key = "hyper"; }
		
	if(is.null(key) && METHOD %IN% c("Signed Rank (Wilcoxon) Distribution",  "signrank", "sign", "sign-dist", "sign-rank", "sign-rank-dist")) 
		{ key = "signrank"; }
		
	if(is.null(key) && METHOD %IN% c("Wilcoxon Rank Sum Distribution",  "wilcox", "wilc", "wilc-dist", "wilc-rank-dist", "wilc-rank-sum-dist", "wilc-sum-dist")) 
		{ key = "wilcox"; }

	## only has an rWishart function ...
	if(is.null(key) && METHOD %IN% c("Wishart Distribution",  "Wishart", "wish", "wish-dist")) 
		{ key = "Wishart"; }
							

	
	if(is.null(key)) { key = "--NULL--"; }
	
	df = IN.df();
	IN.clear();
	minvisible(df, print=FALSE);
	key = property.set("IN", key, df);
	key;
	}
	
	
	
# call-list ... do.call()
prep.clist = function(clist, dots)
	{
	keys = names(dots);
	vals = unname(dots);
	nk = length(keys);
	for(i in 1:nk)
		{
		key = keys[i]; val = unlist(vals[i]);  # vectored?
		clist[[key]] = val;
		}
	clist;
	}


# complement of ERF 
ERF.C = function(z, ...)
	{
	# pracma::erfc ... 2*pnorm(-sqrt(2)*x)
	z = dots.addTo(z, ...);
	1 - ERF(z);	
	}
	
ERF = function(z, ...) 
	{
	z = dots.addTo(z, ...);
	if(!is.complex(z)) 
		{ 
		z = math.cleanup(z);
		# erf (−z) = −erf z 
		# math.sign just calls cleanup ... 
		erf = sign(x) * pchisq(2 * z^2, 1);
		return(erf);
		}
	# if complex, where was that code ...
	stop("TODO: complex, where was that code??? Taylor Series?");
	}
	
ERF.inv = function(zinv, ...) 
	{
	# inverse in [-1,1] nicely
	zinv = dots.addTo(zinv, ...);
	if(!is.complex(zinv)) 
		{ 
		zinv = math.cleanup(zinv);
		 
		zinv.abs = abs(zinv);
		
		zinv = v.toNA(zinv, (zinv.abs > 1));
		erfinv = sign(zinv) * sqrt(qchisq(zinv.abs, 1)/2);

		return(erfinv);
		}
	# if complex, where was that code ...
	# pracma::erfz?
	stop("TODO: complex, where was that code??? Taylor Series?");
	# extend the taylor series ... better precisions?  Adebo bug? what was it?
	}



# what is erfi?  imaginary but not complex?

	
ERF.fn = function() {}
ERF.num = function() {} # numerical integration?




PDF.inv = function() {} 
PDF.fn = function() {}


PDF = function(x, method="norm", ...)
	{
	# probability density function ... height at point x ... 
	dots = match.call(expand.dots = FALSE)$...
	clist = list(x=x); 
	if(!is.null(dots)) 
		{
		dots = list(...);
		clist = prep.clist(clist, dots);
		}
	ct.method = check.type(method);
	if(!ct.method || !is.character(method)) 
		{ method = deparse(substitute(method)); }
	METHOD = prep.arg(method, n=4, keep="-");
	# http://127.0.0.1:23214/library/stats/html/Distributions.html
		
	KEY = prep.distribution(METHOD);
	if(KEY == "--NULL--")
		{
		df = property.get("IN", KEY);
		msg = msg.badOption("method", method, METHOD);	
		cat("\n\n"); minvisible( df, display=TRUE ); cat("\n\n"); 
		IN.clear();	
		cat.stop(msg);
		}
	fn.name = paste0("d", as.character(KEY));
	
	
	res = do.call(fn.name, clist);	
	res = property.set("params", res, clist);
	res = property.set("fn.name", res, fn.name);
	minvisible(res, print="str");
	invisible(res);
	}


CDF.fn = function() {}

CDF = function(q, method="norm", ...)
	{
	# cumulative distribution function ... cumulative area from -Inf to x 
	dots = match.call(expand.dots = FALSE)$...
	clist = list(q=q); 
	if(!is.null(dots)) 
		{
		dots = list(...);
		clist = prep.clist(clist, dots);
		}
	ct.method = check.type(method);
	if(!ct.method || !is.character(method)) 
		{ method = deparse(substitute(method)); }
	METHOD = prep.arg(method, n=4, keep="-");
	# http://127.0.0.1:23214/library/stats/html/Distributions.html
		
	KEY = prep.dist(METHOD);
	if(KEY == "--NULL--")
		{
		df = property.get("IN", KEY);
		msg = msg.badOption("method", method, METHOD);	
		cat("\n\n"); minvisible( df, display=TRUE ); cat("\n\n"); 
		IN.clear();	
		cat.stop(msg);
		}
	fn.name = paste0("p", as.character(KEY));
	
	res = do.call(fn.name, clist);	
	res = property.set("params", res, clist);
	res = property.set("fn.name", res, fn.name);
	minvisible(res, print="str");
	invisible(res);
	}
	

CDF.inv = function(p, method="norm", ...)
	{
	# inverse cumulative distribution function ...
	#	cumulative area from -Inf to x 
	# give me the area (a as probability [0,1]), I will give you the x value 
	dots = match.call(expand.dots = FALSE)$...
	clist = list(p=p); 
	if(!is.null(dots)) 
		{
		dots = list(...); 
		clist = prep.clist(clist, dots);
		}
	ct.method = check.type(method);
	if(!ct.method || !is.character(method)) 
		{ method = deparse(substitute(method)); }
	
	METHOD = prep.arg(method, n=4, keep="-");
	# http://127.0.0.1:23214/library/stats/html/Distributions.html
		
	KEY = prep.dist(METHOD);
	if(KEY == "--NULL--")
		{
		df = property.get("IN", KEY);
		msg = msg.badOption("method", method, METHOD);	
		cat("\n\n"); minvisible( df, display=TRUE ); cat("\n\n"); 
		IN.clear();	
		cat.stop(msg);
		}
	fn.name = paste0("q", as.character(KEY));
	# https://www.stat.umn.edu/geyer/old/5101/rlook.html
	
	# dnorm is "mean" not "mu" ... how to trap this and deliver SMARTLY?
	
	res = do.call(fn.name, clist);	
	res = property.set("params", res, clist);
	res = property.set("fn.name", res, fn.name);
	minvisible(res, print="str");
	invisible(res);
	}
	


CDF.between = function(p.lower, p.upper, method="norm", ...)
	{
	ct.method = check.type(method);
	if(!ct.method || !is.character(method)) 
		{ method = deparse(substitute(method)); }
	
	
	lower = CDF(p.lower, method=method, ...);
	upper = CDF(p.upper, method=method, ...);
	
	res = as.numeric(upper-lower);
	
	minvisible(res, print="str");
	invisible(res);
	}



stats.test = function(X.stat, method="norm", ..., tail="both", alpha=0.05)
	{
	# for given X.stat and alpha ... compute X.crit and pvalue 
	# based on a distribution with its needed parameters 
	# tail = "both", "lower", "upper" ... what to do with alpha 
	
	ct.method = check.type(method);
	if(!ct.method || !is.character(method)) 
		{ method = deparse(substitute(method)); }
	
	# just call the generic function PDF/CDF/inverseCDF to solve the problem 
	# here you would do the appropriate 1-p if necessary 
	# should I add the multivariate chi-square to these ...
	# I have a p and a q?
	
	}
	 
v.random = function(n=100, method="norm", ..., seed=NULL)
	{
	dots = match.call(expand.dots = FALSE)$...
	clist = list(n=n); 
	if(!is.null(dots)) 
		{
		dots = unlist(list(...));
		clist = prep.clist(clist, dots);
		}
	ct.method = check.type(method);
	if(!ct.method || !is.character(method)) 
		{ method = deparse(substitute(method)); }
	METHOD = prep.arg(method, 4);
	# http://127.0.0.1:23214/library/stats/html/Distributions.html
	fn.name = switch(METHOD,
						"unif" 	= "runif",		# unif 		
						"norm" 	= "rnorm",		# norm
						"t"		= "rt",			# t
						"f"		= "rf",			# f 
						"chis"	= "rchisq",		# chisq 
						"beta"	= "rbeta",		# beta 
						"gamm"	= "rgamma", 	# gamma
						"exp"	= "rexp",		# exp 
						"cauc"	= "rcauchy",	# cauchy 
						
						"bino"	= "rbinom", 	# binom
						"nbin"	= "rnbinom",	# nbinom
						"pois"	= "rpois",		# pois
						
						"lnor"	= "rlnorm", 	# lnorm
						"mult"	= "rmultinom", 	# multinom
						"logi"	= "rlogis", 	# logis
						
						"weib"	= "rweibull",	# weibull						
						"geom"	= "rgeom", 		# geom
						"hype"	= "rhyper",		# hyper
						
						"rsig"	= "rsignrank", 	# signrank 
						"wilc"	= "rwilcox", 	# wilcox 
						"wish"	= "rWishart",	# Wishart
					
					stop("not found, METHOD wrapped in msg")
					);
	
	# paste0("r",method);		
		
	s = seed.set(seed);
	res = do.call(fn.name, clist);	
	res = property.set("seed", res, as.integer(s));	
	res = property.set("params", res, clist);
	res = property.set("fn.name", res, fn.name);
	minvisible(res, print="str");
	invisible(res);
	}
 
# https://cplusplus.com/reference/vector/vector/
# https://www.educba.com/c-plus-plus-shuffle/
# Inf just MEANS "ALL OF THEM" in vec ... 
v.shuffle = function(vec, n=length(vec), seed=NULL) 
	{
	# shuffle implies no replacement ...  
	nv = length(vec); if(n > nv) { n = nv; } 
	s = seed.set(seed);	
	res = sample(vec, n);
	res = property.set("seed", res, as.integer(s));
	res;
	}

v.sample = function(vec, size, replace=FALSE, prob=NULL, seed=NULL) 
	{
	s = seed.set(seed); 
	res = sample(vec, size, replace=replace, prob=prob);
	res = property.set("seed", res, as.integer(s));
	res;
	}





v.fill = function(vec, to.length=5, with=NA)
	{
	n = length(vec);
	r = to.length - n;
	if(r >= 1) { add = rep(with, r); } else { add = NULL; }
	c(vec, add);	
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
	idx = v.nearestIDX(vec, what, howmany=howmany);
	vec[ idx ];
	}




v.nearestIDX = function(vec, what, howmany=1)
	{
	vec.dev = abs(what-vec); 			# deviation
	idx.min = stats.whichMin(vec.dev);	# minimum
	if(is.null(howmany) || is.Inf(howmany) || is.negative(howmany))
		{
		return(idx.min);  # will have a min 
		}
	idx.min[1:howmany];  # this will append NA's if too long ...
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

# e = yt - y.hat
# xls.RMSE(e) ... xls.ME ... xls.MAD ... xls.MPE ... xls.MAPE 
# xls.RSQ ... xls.INTERSEPT ... xls.SLOPE ... 
# rewrite TRENDLINE function ... trendline ... use only xls.COMMANDS 
# transform the x, y to call SLOPE/INTERSEPT 
# fn = B0 + B1x1 + B2x2
# linear prob / logistic / 2-param Richards (floor=0, ceil=1) ... equivalent?
# sigma() ... fitted() ... 
# accuracy() ... forecast::forecast()
# matching coefficient ... jaccards coefficient
# misscaliffication aka error
# sensitivity ... aka recall
# 

set.info = function(A, B)
	{
	all = c(A,B);
	all.u = unique(all);
	
	A = c(1,1,3,4); # table(A);  FREQ ,,, 
	B = c(5,3,3,1); # table(B);  FREQ ,,,
	
	# match is like which.min ... just returns the first element 
	# set.info do everything ... dataframe ... duplicates yes or no version 
	# 
	
	}
	
set.union = function(A, B, allow.duplicates=FALSE)
	{
	all = c(A,B);
	
	}
	
# R set theory is wrong ... based on unique indexes?
# allow.duplicates=FALSE (replicate R behavior)
# pair theory ... not useful in data 
# https://www.youtube.com/watch?v=AAJB9l-HAZs
# complemtn of A/B from universal 
# x = 1,2,3 ... y = 3,1,3 ... 
# union would be collection with matches removed as duplicates
# unique or duplicates are not pair-matching ... bad design
# we don't assume in set theory that members of x are UNIQUE, do we?
# set.subtract (a from b)
# str.subtract (a from b)
# x = set* first 10 primes AND first 10 odd numbers ... duplicate entries
# y = set* first 3 primes, 8th prime ... AND third/fifth odd number 
# what does UNION imply?  INTERSECT, etc.  unique/duplicate seems wrong.

	
	


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















# v.sort(sample(1:6), "BUBBLE");

# https://github.com/MonteShaffer/humanVerse/blob/main/misc/functions-sort.R
v.sort = function(vec, method="bubble")
	{
	METHOD = prep.arg(method, n=3);
	# msg THIS is for DEMO purposes 
	n = length(vec);
	
	swapMe = function(a,a.idx, b,b.idx, nvec)
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
	



































