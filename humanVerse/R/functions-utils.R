

smart.sep = function() {}
smart.sep = function(x, search.order=c("'", "-", ":", ",", "^", ".", "ft", "f"))
	{
	n = length(search.order);
	o = search.order[1];  # first is default, if not found ...
	for(i in 1:n)
		{
		sep = search.order[i];
		if(allTRUE(str.contains( sep , x )))	{ return ( sep ); }		
		}
	o;
	}
	
	
# # dots = structure(list(now.1 = structure(1664158355.03661, class = c("POSIXct", "POSIXt")), action.2 = "set", MEMORY.3 = "-IN-", key.4 = "-CURRENT_IN-",     test = list(monte = structure(123, prop = list(a = 1, `b=3` = 7)),         alex = structure(-7, m = 33, b = list(44, 33, structure(-7, m = 33),             structure(123, prop = list(a = 1, `b=3` = 7)))))), fn.info = list(    fn = "df.row", dot.keys = c("now", "action", "MEMORY", "key"    ), params = list(use.names = TRUE), map = list(... = c("now",     "action", "MEMORY", "key"), use.names = "use.names"), formals = list(        ... = "--EMPTY--", use.names = FALSE, character.only = FALSE)), original = list(    structure(1664158355.03661, class = c("POSIXct", "POSIXt"    )), "set", "-IN-", "-CURRENT_IN-"))

  
smart.access = function(objstr, a.sep=ATTRIBUTE_KEY)
	{
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.OS = check.type(objstr);
	if(!ct.OS || !is.character(objstr))	
		{ objstr = deparse(substitute(objstr)); } 
##########################################################
	o.seps = c("[[", "[", "$");
	o.map = list("[[" = "]]", "[" = "]", "$" = "");
#cat("\n\n");	
#dput(objstr);
#cat("\n\n");
#stop("monte");
	objstr = str.trim(objstr);
		# str.trimFromAny ... doesn't seem to work as expected
	objstr = str.trimFromAny(objstr, "()", "both");
	
	o.sep = "$"; 	alen = str.len(a.sep);
					olen = str.len(o.sep);
	# objstr = "dots@fn.info$dot.keys";
	# objstr = "dots$test$alex@b[4]@prop$b=3"
	aIDX = str.pos(a.sep, objstr);	aIN = length(aIDX);
	# nothing in attributes, just parse it ...
	if(is.null(aIDX)) { return( eval(parse(text=objstr)) ); }

	wrapBackTick = function(x, trimLeft=FALSE)
		{
		if(str.contains(o.sep, x))
			{
			x = str.replace("$", "`$`", x);
			x = paste0("`",x, "`");
			# in case we OVERDID it ...
			x = str.replace("``", "`", x);
			# last element 
			if(trimLeft) { x = str.trimFromFixed(x, "`", "LEFT"); }
			}
		x;
		}
	
	prepAttribute = function(x)
		{
		paste0("[['",x,"']]");  # maybe jsut make $ 
		#paste0("$`",x,"`");
		}
	wrapAttribute = function(fstr,key,rem)
		{
		# attributes(`dots`$`test`$`alex`)[["b"]]
		paste0("attributes(",fstr,")",key,rem);
		}
	
	# first one just needs wrappers ...
	# count $ in info[1]
#	info[1] = str.replace("$", "`$`", info[1]);
#	info[1] = paste0("`", info[1], "`");
#	info[1] = str.replace("``", "`", info[1]); # in case we messed up
	# 2:n ... we need to separate the attribute from any possible
	# downstream tags $ [[name]] [[number]] [name] [number] (mistakes)
	# need attribute keys ... 
	

	info = str.explode(a.sep, objstr);
	# oIDX = strpos(objstr, o.sep);	oIN = length(oIDX);
	
	fstr = "";
	n = length(info);
	for(i in 1:n)
		{
		# we need to replace the '@key' with 'attributes(key)'
		# need to get the 'val@key' element, not 'val$val@key' ?
		# regex might work, but I have my wheelhouse 
		# when did they add @ as a slot operator, ridiculous
		# http://127.0.0.1:12967/library/base/html/slotOp.html
		# "attributes(dots)$`fn.info`$dot.keys"
		# dots$test$alex@b[4]@prop$b=3
		# attributes(dots$test$alex)$`b`
		# for CLARITY, I want to do it this way 
		#  attributes(`dots`$`test`$`alex`)[["b"]][[4]]
		#  attributes(attributes(`dots`$`test`$`alex`)[["b"]][[4]])[["prop"]]$`b=3`

		# `dots`$`test`$`alex`
		# substring(objstr, 1, (aIDX[1] - alen ))
		
		sub = info[i];
		 
		if(fstr == "")
			{
			fstr = wrapBackTick(sub);
			next;
			}
			## we need to separate the @attr$from or @attr[[from]]
		sep = smart.sep(sub, o.seps);
			# should be in the sep list ... maybe ERROR check ...
		sinfo = str.explode(sep, sub);
		key = prepAttribute(sinfo[1]);
			# put everything back 
		rem = paste0(sep, str.implode(sep, sinfo[-c(1)])); 
		if(i == n) { rem = wrapBackTick(rem, TRUE); }
		if(sub == sinfo[1]) { rem = ""; } # we had nothing left ...
		# could dig into smart.sep ... return NULL ? # more checks ...
		# this works ....
			# cleanup rem noise ... [4] should be [[4]]
			# let is pass, could be a potential TRUE key, how do I know 
			# [[4]][4]@next ... 
		fstr = wrapAttribute(fstr, key, rem);		
		}
	eval(parse(text=fstr));
	}



 

### 
###	res = str.trimFromAny(res, "()");  # so .%$$%(obj) WORKS (remove)
# I am pretty sure this is UNIVARIATE 
# should be a character already and have length == 1
## (.%$$% dots@fn.info)$dot.keys;
## ABOVE works, I would like it to be 
## .%$$% dots@fn.info$dot.keys;
access = function(str)
	{	
	str = str.trimFromFixed(str, "(", "left");
	str = str.trimFromFixed(str, ")", "right");
	E = str.explode("@", str);
	k = length(E);
		if(k==1)
			{
			cat("\n\n"); dput.one(str); cat("\n\n");
			eval(parse(text=str));
			} else  {
					# k == 2
					nstr = paste("attributes(", E[1] ,")", sep="");
					nstr = paste(nstr,'$', "`", E[2], "`", sep="");
					if(k > 2) 
						{
						for(i in 3:k)
							{
							nstr = paste("attributes(",nstr,")",sep="");
							nstr = paste(nstr,'$',"`",E[i],"`",sep="");
							}
						}
					# recursion 
					access(nstr);
					}
	}
	 

	


allNA = function(x)
	{
	idx = v.which(x, NA);
	if(is.null(idx)) { return(FALSE); }
	return( length(idx) == length(x) );
	}


	
	
anyTRUE = function(x)
	{
	idx = v.which(x, TRUE);
	if(is.null(idx)) { return(FALSE); }
	return(TRUE);
	}
	
allTRUE = function(x)
	{
	idx = v.which(x, TRUE);
	if(is.null(idx)) { return(FALSE); }
	return( length(idx) == length(x) );
	}

anyFALSE = function(x)
	{
	idx = v.which(x, FALSE);
	if(is.null(idx)) { return(FALSE); }
	return(TRUE);
	}
	
allFALSE = function(x)
	{
	idx = v.which(x, FALSE);
	if(is.null(idx)) { return(FALSE); }
	return( length(idx) == length(x) );
	}

# any/all ... NULL ? (on list) ... 
#         ... Inf ... infinite  



ggget = function(x, ...)
	{
debug = FALSE;
	ginfo = suppressError( get(x, ...), 
								show.notice=debug,
								msg="debugging gget" 
							);
							
	if(is.error(ginfo)) { return(NULL); }
	ginfo;	
	}


gggassign = function(key, val)
	{
# cat("\n gggassing key ::: ", key, "\t val ::: ", val, "\n\n");
	assign(key, val, envir = .GlobalEnv);
	invisible(NULL);  # vs.   return(invisible(NULL));
	}
	
	
# TODO
# constants ... flood GLOBAL space or quietly 
# how does pi work 
## define = function(KEY, VALUE) {}
# http://127.0.0.1:11303/library/base/html/Constants.html
## John Machin (ca 1706) computed pi to over 100 decimal places # [how many correct?]
## using the Taylor series expansion of the second term of
# pi - 4*(4*atan(1/5) - atan(1/239))
	
.GLOBAL. = function(KEY, VALUE)
	{
	ct.KEY = check.type(KEY);
	ct.VAL = check.type(VALUE);
	
	# key %GLOBAL% .
	# grab name/val from it 
	if(ct.KEY && !ct.VAL)
		{
		val = KEY;
		key = deparse(substitute(KEY));
		return(gggassign(key,val));
		}
		
	# . %GLOBAL% val
	# grab name/val from it 
	if(!ct.KEY && ct.VAL)
		{
		val = VALUE;
		key = deparse(substitute(VALUE));
		return(gggassign(key,val));
		}
		
	# "key" %GLOBAL% val 
	key = KEY;
	# what if "key" is not a string yet?
	if(!is.character(KEY)) { key = deparse(substitute(KEY)); }
	gggassign(key,VALUE);	
	}
	



  
 
 
 
  
 
 
.TO. = function() {}
.TO. = function(WHAT, WHERE=parent.frame(2))
	{
###cat("\n\n MONTE \n\n");
# cat("\n");
# print(environment());
# print(parent.env(environment()));
# print(parent.env(parent.env(environment())));
	#DEFAULT = parent.frame(1); # caller? parent
	DEFAULT = parent.frame(2); # one above caller? grandparent
	# allows x %TO% . 
	ct.WHERE = check.type(WHERE);
	if(!ct.WHERE) { WHERE = DEFAULT; }
		
	val = WHAT;
	key = deparse(substitute(WHAT));		
	assign(key, val, envir=WHERE );
	invisible(val);
	}
	
	
	
# x = 44;
# y = function(envir = .GlobalEnv) { x=NULL; x %TO% envir; }
# y();
# x; 








# this = function(...) { print(sys.call(1)); }
#  .%THIS%.
.THIS. = function() {}
.THIS. = function(KEY, VALUE, WHERE = parent.frame(1), deparse.call = TRUE ) 
	{
	# KEY is FRAME, VALUE is CALL 
	
 	DEFAULT_FRAME 	= 1;	# parent.frame(n);
	DEFAULT_CALL 	= 1;	# sys.call(n); 
		
	## KEY is the FRAME 
	ct.KEY = check.type(KEY);
	key = NULL;
	if(is.null(key) && !ct.KEY) 
		{
		key = parent.frame(DEFAULT_FRAME);		
		}
	if(is.null(key) && is.environment(KEY))
		{
		key = KEY;
		}
	if(is.null(key) && is.numeric(KEY))
		{		
		key = parent.frame(KEY);
		}

	ct.VAL = check.type(VALUE);
	## VALUE is the CALL
	val = NULL;
	if(is.null(val) && !ct.VAL)
		{
		val = sys.call(DEFAULT_CALL);		
		}
	if(is.null(val) && is.numeric(VALUE))
		{		
		val = sys.call(VALUE);
		}

	# parse key/val ... get what we want 
	# key is envir 
	# val is call with parameters 
	# 

	fn = sys.calls()[[sys.nframe()-1]];  # close 
	# parent.call = sys.call(sys.nframe() - 1L);  # equivalent?
	finfo = parse.syscall(fn);
	# can we get the df = iris ... 
  
	if(deparse.call)
		{
		res = list("envir" = deparse(key), 
					"call" = deparse(val), 
				 "fn.info" = finfo
					);
		} else {
				res = list("envir" = key, 
							"call" = val, 
				 		 "fn.info" = finfo
							);		
				}
		
	# WHERE=parent.frame(1); # or WHERE = env?
	assign("THIS", res, envir=WHERE );
	invisible(res);
	}
	













	
.ADD. = function(a,b)
	{ 
	a = as.numeric(a); 
	b = as.numeric(b);
	na = length(a);
	nb = length(b);
	out = matrix(0, nrow=na, ncol=nb);
	
	if(na >= nb)
		{
		# vector by column
		for(i in 1:nb)
			{
			out[,i] = a + b[i];
			}
		} else {
				# vector by row
				for(i in 1:na)
					{
					out[i,] = a[i] + b;
					}
				}
	invisible(math.cleanup(out));
	}
	
	
	
	
	
			# dot.operator  ... append "|" by default ... 
			# %.=% ... "|" . THING 
			# %=.% ... THING . "|" 
			# %.=.% ... THING only ... but appended  	
			# %.% ... ccould you chain these ? like NORMAL PHP 
			# like complex addition, parentheses are your friend ...
			# CL was faster than me at multipliers, except 7
			# student-teacher (algebra x ... what are you talking about)
			# people (let me explain atob)... what are you talking about 
			# CL shift ... 
			# MR experience (two-meanings... ice and car)
			# MS treatment (love and affection, triangle)
			# DJ wisdom (2nd / 12th)
			# DT goodness 
			# MC craftiness
			# JA embarrassment (woody)
			# BP swagger (white lightning)
			# JG backlines (chess, rooks)
			# MM moment (CL, 8th grade)
			# CR shift (sweather, 10th grade) 
			# JE BA (they tried)
			# Teddy effect (belt)
			# Schm first name (get outta here)
			# Hol (smoking)
			# memorizing maps of mt ... hofst
			# bullet in your brain (PE study hall)
			# SH moment (double off the wall) ... dad 4th grade 
			# boundaries 150% A+ vs tradition A- border ...
			# simulation, created shitty software ... ATONEMENT ... FIX IT ... 
		

.EQUAL_PIPE. = function() {}  # %=|% ==> THING . "|" 
.EQUAL_PIPE. = function(KEY, VALUE, sep="|", WHERE=parent.frame(1))
	{
	# these functions are much simpler than the one's already written
	key = KEY; val = VALUE;
	key = paste0(key, val, sep);
	 
	KEY = deparse(substitute(KEY));
	assign(KEY, key, envir=WHERE );
	# minvisible(key, key=KEY, display=none);
	key;
	}
	
.PIPE_EQUAL. = function() {}  # %|=% ==> "|" . THING 
.PIPE_EQUAL. = function(KEY, VALUE, sep="|", WHERE=parent.frame(1))
	{
	# these functions are much simpler than the one's already written
	key = KEY; val = VALUE;
	key = paste0(key, sep, val);
	
	KEY = deparse(substitute(KEY));
	assign(KEY, key, envir=WHERE );
	# minvisible(key, key=KEY, display=none);
	key;
	}


.PIPE. = function() {}		 # %|%  ==>  a | b | c  ... 
.PIPE. = function(KEY, VALUE, sep="|", WHERE=parent.frame(1))
	{
	# these functions are much simpler than the one's already written
	key = KEY; val = VALUE;
	key = paste0(key, sep, val);
	# minvisible(key, key=KEY, display=none);
	key;
	}




# %.=% ... THING only ... but appended  # "%.=.%" = .DOT_APPEND.;
.DOT_EQUAL. = function() {}
.DOT_EQUAL. = function(KEY, VALUE,  sep="", WHERE=parent.frame(1))
	{
	# these functions are much simpler than the one's already written
	key = KEY; val = VALUE;
	key = paste0(key, sep, val);  	# order is same as .DOT_PIPE.
									# just change the separator
									# don't want to deal with WHERE
									# DRY ... DO-REPEAT-YOURSELF when R call stack demands it ... 
	KEY = deparse(substitute(KEY));
	assign(KEY, key, envir=WHERE );
	# minvisible(key, key=KEY, display=none);
	key;
	}



.DOT. = function() {}		 # %.%  ==>  a . b . c  ... 
.DOT. = function(KEY, VALUE, sep="", WHERE=parent.frame(1))
	{
	# these functions are much simpler than the one's already written
	key = KEY; val = VALUE;
	key = paste0(key, sep, val);
	# minvisible(key, key=KEY, display=none);
	key;
	}	

.DOT_SPACE. = function() {}		 # %. %  ==>  a . b . c  ... 
.DOT_SPACE. = function(KEY, VALUE, sep=" ", WHERE=parent.frame(1))
	{
	# these functions are much simpler than the one's already written
	key = KEY; val = VALUE;
	key = paste0(key, sep, val);
	#minvisible(key, key=KEY, display=none);
	key;
	}	
		
		
.PLUS_EQUAL. = function() {}	# "%+=%"
.PLUS_EQUAL. = function(KEY, VALUE, WHERE=parent.frame(1))
	{
	# these functions are much simpler than the one's already written
	key = KEY; val = VALUE;
	key = key + val;
	
	KEY = deparse(substitute(KEY));
	assign(KEY, key, envir=WHERE );
	# minvisible(key, key=KEY, display=none);
	key;
	}




.MINUS_EQUAL. = function() {}	# "%-=%"
.MINUS_EQUAL. = function(KEY, VALUE, WHERE=parent.frame(1))
	{ 
	# these functions are much simpler than the one's already written
	key = KEY; val = VALUE;
	key = key - val;
	
	KEY = deparse(substitute(KEY));
	assign(KEY, key, envir=WHERE );
	# minvisible(key, key=KEY, display=none);
	key;
	}
	

.PLUS_PLUS. = function() {}
# x = function() { print(environment()); print(parent.env(environment())); print(parent.frame(1)); print(parent.frame(2)); i = 0; a = 0; b = 0; for(j in 1:3) { a = i%++%.; cat("\n ja -->", j, "\t a:",a, "\t b:",b, "\t i:",i); b = .%++%i; cat("\n jb -->", j, "\t a:",a, "\t b:",b, "\t i:",i); }; cat("\n"); }

.PLUS_PLUS. = function(KEY, VALUE, WHERE=parent.frame(1))
	{
# cat("\n");
# print(environment()); 
# print(parent.env(environment())); 
# print(parent.frame(1));
# print(parent.frame(2));
	ct.KEY = check.type(KEY);
	ct.VAL = check.type(VALUE);
	
# https://stackoverflow.com/questions/16583211/how-to-get-environment-of-a-variable-in-r
# pryr::where("mean")
# dput(KEY);
# dput( deparse(substitute(KEY)) );
# dput( eval(parse(text = deparse(substitute(KEY)) ) ) );
# print(WHERE);
# dput(environmentName(WHERE));
# 871dcc472a3a1b66f8a266c330d56907d725fc8c
# git log --all --full-history -- "**/functions-integrate.*"

	# i %++% .
	if(ct.KEY && !ct.VAL)
		{
		val = KEY;
		nval = val + 1;
		key = deparse(substitute(KEY));
		# snails.x[n] %++%.
		str = paste0(key, " = ", nval, ";");
# dput(str);
		eval(parse(text = str), envir=WHERE);
		return(nval);
		# return( minvisible(nval, key=PLUS, display=none) );
		}
		
	# . %++% i
	if(!ct.KEY && ct.VAL)
		{
		val = VALUE;
		nval = val + 1;
		key = deparse(substitute(VALUE));
		#  .%++% snails.x[n]
		str = paste0(key, " = ", nval, ";");
# dput(str);
		eval(parse(text = str), envir=WHERE);		
		return(val);
		# return( minvisible(val, key=PLUS, display=none) );
		}
	
	stop("how did you get here");
	}


	
	


.MINUS_MINUS. = function() {}
.MINUS_MINUS. = function(KEY, VALUE, WHERE=parent.frame(1))
	{
	ct.KEY = check.type(KEY);
	ct.VAL = check.type(VALUE);

	# i %--% .
	if(ct.KEY && !ct.VAL)
		{
		val = KEY;
		nval = val - 1;
		key = deparse(substitute(KEY));
		# snails.x[n] %--%.
		str = paste0(key, " = ", nval, ";");
# dput(str);
		eval(parse(text = str), envir=WHERE);
		return(nval);
		#return( minvisible(nval, key=MINUS, display=none) );
		}
		
	# . %--% i
	if(!ct.KEY && ct.VAL)
		{
		val = VALUE;
		nval = val - 1;
		key = deparse(substitute(VALUE));
		#  .%--% snails.x[n]
		str = paste0(key, " = ", nval, ";");
# dput(str);
		eval(parse(text = str), envir=WHERE);		
		return(val);		
		# return( minvisible(val, key=MINUS, display=none) );
		}
	
	stop("how did you get here");
	}

	
	














IN.init = function(mem.key = "-CURRENT_IN-")
	{
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.MEMKEY = check.type(mem.key);
	if(!ct.MEMKEY || !is.character(mem.key))	
		{ mem.key = deparse(substitute(mem.key)); } 
##########################################################

	memory.set(mem.key, "-IN-", list()); 
	}
	
IN.clear = function(mem.key = "-CURRENT_IN-")
	{
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.MEMKEY = check.type(mem.key);
	if(!ct.MEMKEY || !is.character(mem.key))	
		{ mem.key = deparse(substitute(mem.key)); } 
##########################################################
	memory.set(mem.key, "-IN-", list()); 
	}

IN.get = function(mem.key = "-CURRENT_IN-")
	{
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.MEMKEY = check.type(mem.key);
	if(!ct.MEMKEY || !is.character(mem.key))	
		{ mem.key = deparse(substitute(mem.key)); } 
##########################################################
	memory.get(mem.key, "-IN-");	
	}
	
	
IN.df = function(mem.key = "-CURRENT_IN-")
	{
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.MEMKEY = check.type(mem.key);
	if(!ct.MEMKEY || !is.character(mem.key))	
		{ mem.key = deparse(substitute(mem.key)); } 
##########################################################
	info = memory.get(mem.key, "-IN-");
	
	n 		= length(info);
	keys 	= names(info);
	klen	= max(list.getLengths(info));
	df = NULL;
	
	
	for(i in 1:n)
		{
		key = keys[i];
		val = v.fill( info[[key]], klen, ""); 
		row = df.row(c(key, val), use.names=FALSE, character.only=TRUE);
dput(row); 
		df = rbind(df, row);
		}
	cnames = c("-OPTION-", "ideal => shortcodes", paste0("ALT-", 1:(klen-1)) ); 	
	colnames(df) = cnames;
	# rownames(df) = rep("", n);
		
	df;
	}

IN = function(KEY, VALUE, mem.key = "-CURRENT_IN-")
	{
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.MEMKEY = check.type(mem.key);
	if(!ct.MEMKEY || !is.character(mem.key))	
		{ mem.key = deparse(substitute(mem.key)); } 
##########################################################
	# perform %in% with memory 	
	info = memory.get(mem.key, "-IN-");
	if(is.null(info)) { info = list(); }
	
	ele = as.vector(KEY);
	set = unique(as.vector(VALUE)); 
	key = set[1];
	
	info[[key]] = set[-c(1)];	
	memory.set(mem.key, "-IN-", info); 
	
	# ?el 
	
	(match(ele, set, 0L) > 0L);
	}
   
 








# THIS IS A BLOCK OF LOGIC, 
# do I want to separate them into "is" and "utils"
# if I build a network "leaf" from path.summary, it won't matter 


dir.exists_ = function(path=getwd(), ...) 
	{ r = path.summary(path, ...); (r$type == "dir" & r$exists); }
file.exists_ = function(path=getwd(), ...) 
	{ r = path.summary(path, ...); (r$type == "file" & r$exists); }
	
path.exists_ = function(path=getwd(), ...) 
	{ r = path.summary(path, ...); ( r$exists ); }



# Euclidean fractions ... (depth = 12, tol = ) ... 

## PHP https://kinsta.com/blog/is-php-dead/
# https://kinsta.com/blog/php-vs-javascript/
# 

# https://en.wikipedia.org/wiki/Inverse_function











#' charAt
#'
#' Get the character of a string at position [idx]
#'
#' @param str String
#' @param idx position to get character
#'
#' @return single character
#' @export
#'
#' @examples
#'
#' charAt("Alex", 2);
#' charAt(c("Hello","there","Alex"), 2); 
#' charAt("Alex", 8);
#' charAt("Alexander", 8);
#'
charAt = function(str="Welcome to the humanVerse",idx=21)
  {
  substr(str,idx,idx);  # or substring?
  }
  
indexOf = function(vec, what, explode=FALSE, zero.idx=TRUE)
	{
	if(explode) { vec = str.explode("", vec); } # it's just characters 
	v.which(str.explode("",str), what);	
	}

#' lastChar
#'
#' Get the last character of a string
#'
#' @param str String
#' @param trim should the string be trimmed first
#'
#' @return single character
#' @export
#'
#' @examples
#'
#' lastChar("Alex");
#' lastChar(c("Hello","there","Alex"));
#' lastChar("Sasha");
#' lastChar("Alexander");
#'
lastChar = function(str, pre.trim=FALSE)
	{
	# this also works:: ... # .substr(str, -1)
	if(pre.trim){ str = str.trim(str); }
	slen = str.len(str);
	charAt(str, s.len);
	}


#' charCodeAt
#'
#' Get the ASCII character code of a string at position [idx]
#'
#' @param str String
#' @param idx position to get character
#'
#' @return
#' @export
#'
#' @examples
#'
#' charCodeAt("Alex", 2);
#' charCodeAt(c("Hello","there","Alex"), 2);
#' charCodeAt("Alex", 8);
#' charCodeAt("Alexander", 8);
#'
charCodeAt = function(str,idx)
  {
  charCode ( charAt(str,idx) ); 
  #  as.numeric( iconv( charAt(str,idx), from="ASCII", to="unicodeFFFE", toRaw=TRUE)[[1]][2] );
  }




#' charCode
#'
#' @param svec A vector of characters
#'
#' @return ASCII character code for each character
#' @export
#'
#' @examples
#'
#' s = c("Alexander", "Jay"); 
#' ## svec = strsplit(s,"",fixed=TRUE)[[1]];
#' charCode(svec);
#'
String.fromCharCode = function(n)
	{
	# 0 is AA== in base64 ??? # null not allowed?
	# ifelse({n == 0}, { '\x00'; }, { chr(n); });
	chr(n);
	}


charCode = function(s)
	{
	ord(s);
	}
	



raw.toString = function(raw, collapse="")
	{
	paste0(as.character(raw), collapse=collapse);
	} 
	
raw.fromString = function(str, splitN=TRUE)
	{
	if(splitN) { str = bin(str, n=2); }
	# https://stackoverflow.com/a/11617990/184614
	as.raw ( as.hexmode ( str ) )
	}






	


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
	# lets create a NULL one ...
	# str = paste0(prep.evalKey(KEY), " = character(0); ");
	# eval(parse(text = str))
	 
	# return( eval(parse(text = KEY)) );
	
	#stop("how did I get here");
	# doesn't exist, so let's just return NULL 
	return(NULL);
	}
	 
	

# showMethods("coerce")
# could I pass ... dots
as.type = function(vals, types="character", ...) 
	{
	n = length(vals);
	nt = length(types); if( (n != nt) && (nt != 1) ) { stop("lenghts must match or types must be of length 1;"); }
	
	# NULL => null
		w = which(is.null(types));
	# "POSIXct.POSIXlt" ... upper case 
	if(length(w) > 0)
		{
		# NULL is lower is.null
		types[w] = str.tolower(types[w]);
		}

	# as.complex, as.double, as.null, as.single, as.integer 
	# seems like it is of the form as.{typeof(vals))
	if(nt == 1)
		{
		# one type on a vector
		cmd = paste0("vals = as.",types,"(vals);"); # could I eval with ... (dots)
		} else {
				str = character(n);
				for(i in 1:n)
					{
					# pairwise
					str[i] = paste0("vals[" , i , "] = as.",
										types[i],"(vals[" , i , "]);");
					}
				# we could create a vector or one long string, parse(text is multivariate
				cmd = paste0(str, collapse=" ");
				}
	# maybe trap this?
	# maybe a function?  
	eval(parse(text = cmd));
	vals;
	}


# This is obviously tedious, violates DRY, and introduces bloat.
# @rdname vs @alias
# https://stackoverflow.com/questions/57770755/
# https://stackoverflow.com/a/30773843/184614
# @describeIn ... @rdname ... map to \alias{} ... 
# @export ??? 
# https://stackoverflow.com/a/22556198/184614
# https://stackoverflow.com/a/71453653/184614
# The @export docstring informs Roxygen to to put the function name in the package NAMESPACE file









#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' strip.tags
#'
#'
#------------------------------------------------#
strip.tags = function(...)
	{
	str = prep.dots(...);
	return(gsub("<.*?>", "", str));
	}
	
	



touch = function(f)
	{
	# for now, we assume touch means ... empty ...
	# just MODIFY the timestamps, create INODE if it doesn't exist
	# maybe tie-in to system or system2 
	if(!file.exists_(f))
		{
		# does this work, empty?
		cat("", file=f, sep="");  # maybe do ftouch with fopen?
		}
	
	}


# basename doesn't play nice ... 
basename_ = function(path, trailing=TRUE)
	{
	d = prep.path(path, trailing=trailing);
	
	# basename doesn't play nice ... 
	tmp = str.explode(DIR_LINUX, d);
	stem = list.getLastElements(tmp);
	stem;
	}
   
  


dput.one = function(x, ...)
	{
	str = capture.output(dput(x, ...));
	cat("\n\n", str, "\n\n", sep = "");	
	minvisible(str, print=FALSE);
	}
	 

# if a matrix gets to length one in rows (or cols)???
# for some reason R truncates it to a vector ... 
keep.matrix = function(X, nrow=3, ...)
	{
		# unlist of matrix is by.col 
	X.matrix = as.numeric( unlist( X ) );
		# force input to [3 x n] matrix
		# default is by.col 
	X.matrix = matrix(X.matrix, nrow=nrow, ...); 
	X.matrix;
	}
	



strlang.RFormat = function(str)
	{
	lang2str(str2lang(str));
	}
	
	
#base::str2lang; 
	# str = "x + 2*y"; slang = str2lang(str); str2 = lang2str(slang); 
	# str; str2; identical(str, str2);
lang2str = function(lang.obj) 
	{
	deparse(lang.obj); 		# eval(parse(text = lang.obj));
	}
	
	
# symbols ...  is.symbol 
# ?is.symbol ... "mode"
symb2str = function(symb) 
	{
	as.character(symb);
	}
str2symb = function(str="alex") 
	{
	as.symbol(str);
	}




# any filename that contains text ...
# passes through readChars ... 
# obj (raw) are all derived from TXT 
checksum.textInFile = function(path.to.files.TXT)
	{
	# tools::md5sum(fileTXT); # are these different ??? ...
	# identical(checksum.textInFile( c(f,f) ), tools::md5sum( c(f,f) ) );  # [1] TRUE
	# f = "C:/_git_/github/MonteShaffer/humanVerse/humanVerse/inst/R/config/--.old.--/ZZ-test.ini"
	# "c183a00977d8605add80976979cc5cbf" 
	# THIS depends on readChars ... binary ---> TXT ... 

	n = length(path.to.files.TXT);
	res = character(n);
	for(i in 1:n)
		{
		res[i] = str.toMD5( readTextFile(path.to.files.TXT[i]) );
		}
	names(res) = path.to.files.TXT;
	res;
	}
	
checksum.file = function(path.to.files)
	{
	# C function ... obfuscated ... .Call(C_Rmd5, files),
	# https://unix.stackexchange.com/questions/343900/
	# \R-4.2.1\src\library\tools\src\Rmd5.c and md5.c (stream)
	# very well-written code ... 
	tools::md5sum(path.to.files); 	
	}
	
