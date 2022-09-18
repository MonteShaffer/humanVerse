

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
	math.cleanup(out);
	}
	
"%+%" = .ADD.;


# dataframe(x, y) : could not find function "dataframe"
 
dataframe = data.frame;

as.dataframe = as.data.frame;

#' @rdname ceil
#' @export
ceil = ceiling;

# natural log as log ... log10 is other, AS-IS so no collision ... 
ln = log; 

axes = axis;

#' @rdname nchars
#' @export
nchars = nchar;

ord = utf8ToInt;  # mb_ord ?
chr = intToUtf8;

 
dots.addTo = function(key, ..., by="column")
	{
	BY = prep.arg(by, n=3);
	
	a = is.atomic(key);  	# what type is the key ...
	v = is.vector(key);		# maybe list append
	l = is.list(key); 		# maybe cbind or rbind
	m = is.matrix(key);
	d = is.data.frame(key);
	
	if((a || v) && !m) 
		{
		more = unlist(list(...));
		res = c(key, more);
		return(res);
		}
	if(m || d)
		{
		dm = dim(key);  # 3 x 1 ... match or transpose more 
		mores = list(...);
		n = length(mores);
		if(n > 0)
			{
			for(i in 1:n)
				{
				more = mores[[i]];
				di = dim(more);
				if(all.equal(di,dm))
					{
					# 3x1 and 3x1 ... 3x2 or 6x1
					if(BY == "col") { key = cbind(key, more); }
					if(BY == "row") { key = rbind(key, more); }
					} else {
							if(di[1] == dm[1])
								{
								# 3x2 and 3x1 ... rbind 
								key = rbind(key, more);
								}
							if(di[2] == dm[2])
								{
								# 3x2 and 1x2 ... cbind  
								key = cbind(key, more);
								}							
							}
				}
			}
		return(key);
		}
	
cat("\n key \n");
dput(key);
cat("\n list(...) \n");
dput(list(...));
cat("\n unlist(list(...)) \n");
dput(unlist(list(...)));
stop("monte");		
	}

#' @rdname dots.addToKey
#' @export
dots.addToKey = dots.addTo;


# testit = function() { if(rand(0,1) == 1) {stop("ERROR");} else {return(1); }}
# x = suppressError(testit);
# x = suppressError(testit(), show.notice=FALSE); str(x);
# x = suppressError(testit(), msg="-HI-"); str(x); if(is.error(x)) { list.fromError(x); }
suppressError = function(expression, show.notice = TRUE, msg = "")
	{
	if(show.notice)
		{
		if(msg == "") 
			{
			msg = "\n\n tldr; \n\n\n\t R-dev believes this is poor programming practice to allow you to \n\t\t suppressError( so they have not included it in base R.  \n\t\t It is probably true, but 'git-r-done' first, and then \n\t\t figure out the minutia such as why this function is \n\t\t throwing an error.  That is why I have past such a \n\t\t VERBOSE message to you, dear reader. \n\n\t By altering this function [set msg to something else, not empty ''], \n\t\t you can reduce the length of this message.  \n\n\t Or you can set the flag show.notice=FALSE to prevent it from printing. \n\t\t  THIS my friends is how choice architecture works!  Cheers and Aloha! \n\n\n";
			}
		# cat(msg);
		warning(msg, call. = FALSE, immediate. = TRUE);
		}
	try( expression , silent = TRUE);
	}






### 
###	res = str.trimFromAny(res, "()");  # so .%$$%(obj) WORKS (remove)
# I am pretty sure this is UNIVARIATE 
# should be a character already and have length == 1
access = function(str)
	{	
	str = str.trimFromFixed(str, "(", "left");
	str = str.trimFromFixed(str, ")", "right");
	E = str.explode("@", str);
	k = length(E);
		if(k==1)
			{
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
	 
	
# leading . will work
"%$$%" = function(r="HI", ...) 
			{ 
			str = str.fromObjectName(...);
			access(str);			
			}
			
#### THIS is *SPECIAL FUNCTION* ... (PARANTHESES) is optional			
# .%$$% "dcf$Depends@dependencies" ;
# .%$$% dcf$Depends@dependencies ;			
# .%$$%("dcf$Depends@dependencies");
# .%$$%(dcf$Depends@dependencies);				
			
			
`$$`   = function(...) 
			{ 
			str = str.fromObjectName(...);
			access(str);			
			}

### THIS IS FUNCTION, MUST HAVE (PARANTHESES)
# `$$`("dcf$Depends@dependencies");
# `$$`(dcf$Depends@dependencies);



allNA = function(vec)
	{
	n = length(vec);
	nna = stats.countNA(vec);
	(n == nna);
	}




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
	return(invisible(NULL));
	}
	
	
.PLUSPLUS. = function() {}
# x = function() { print(environment()); print(parent.env(environment())); print(parent.frame(1)); print(parent.frame(2)); i = 0; a = 0; b = 0; for(j in 1:3) { a = i%++%.; cat("\n ja -->", j, "\t a:",a, "\t b:",b, "\t i:",i); b = .%++%i; cat("\n jb -->", j, "\t a:",a, "\t b:",b, "\t i:",i); }; cat("\n"); }

.PLUSPLUS. = function(KEY, VALUE, WHERE=parent.frame(1))
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
		nval = 1+val;
		key = deparse(substitute(KEY));
		# snails.x[n] %++%.
		str = paste0(key, " = ", nval, ";");
# dput(str);
		eval(parse(text = str), envir=WHERE);
		return(nval);
		}
		
	# . %++% i
	if(!ct.KEY && ct.VAL)
		{
		val = VALUE;
		nval = 1+val;
		key = deparse(substitute(VALUE));
		#  .%++% snails.x[n]
		str = paste0(key, " = ", nval, ";");
# dput(str);
		eval(parse(text = str), envir=WHERE);		
		return(val);
		}
	
	stop("how did you get here");
	}

"%++%" = .PLUSPLUS.;	
	
	
	
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
	
"%GLOBAL%" = .GLOBAL.;



IN.init = function(mem.key = "-CURRENT_IN-")
	{
	memory.set(mem.key, "-IN-", list()); 
	}
	
IN.clear = function(mem.key = "-CURRENT_IN-")
	{
	memory.set(mem.key, "-IN-", list()); 
	}

IN.get = function(mem.key = "-CURRENT_IN-")
	{
	memory.get(mem.key, "-IN-");	
	}
	
	
IN.df = function(mem.key = "-CURRENT_IN-")
	{
	info = memory.get(mem.key, "-IN-");
	
	n 		= length(info);
	keys 	= names(info);
	klen	= max(list.getLengths(info));
	df = NULL;
	
	
	for(i in 1:n)
		{
		key = keys[i];
		val = v.fill( info[[key]], klen, ""); 
		row = df.row(c(key, val), use.names=FALSE);
		df = rbind(df, row);
		}
	cnames = c("-OPTION-", "ideal => shortcodes", paste0("ALT-", 1:(klen-1)) ); 	
	colnames(df) = cnames;
	# rownames(df) = rep("", n);
		
	df;
	}

IN = function(KEY, VALUE, mem.key = "-CURRENT_IN-")
	{
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
   
"%IN%" = IN;
 
# this = function(...) { print(sys.call(1)); }
#  .%THIS%.
.THIS. = function(KEY, VALUE) 
	{
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
	finfo = parse.syscall(fn);
  
	res = list("envir" = key, "call" = val, "fn.info" = finfo);
		
	WHERE=parent.frame(1); # or WHERE = env?
	assign("THIS", res, envir=WHERE );
	}
	
"%THIS%" = .THIS.; 
  
 
 
 
dput.one = function(x, ...)
	{
	str = capture.output(dput(x, ...));
	cat("\n\n", str, "\n\n", sep = "");	
	minvisible(str, print=FALSE);
	}

 
.TO. = function() {}
.TO. = function(WHAT, WHERE=parent.frame(2))
	{
cat("\n");
print(environment());
print(parent.env(environment()));
print(parent.env(parent.env(environment())));
	#DEFAULT = parent.frame(1); # caller? parent
	DEFAULT = parent.frame(2); # one above caller? grandparent
	# allows x %TO% . 
	ct.WHERE = check.type(WHERE);
	if(!ct.WHERE) { WHERE = DEFAULT; }
		
	val = WHAT;
	key = deparse(substitute(WHAT));		
	assign(key, val, envir=WHERE );
	}
	
"%TO%" = .TO.;	
	
	
# x = 44;
# y = function(envir = .GlobalEnv) { x=NULL; x %TO% envir; }
# y();
# x; 



























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
#' s = "Alexander"; svec = strsplit(s,"",fixed=TRUE)[[1]];
#' charCode(svec);
#'
charCode = function(svec)
  {
  #v1 = iconv( svec, from="ASCII", to="unicodeFFFE", toRaw=TRUE);
	v1 = iconv( svec, from="UTF-8", to="unicodeFFFE", toRaw=TRUE);
	v2 = as.integer( unlist(v1) )
	v2[v2 > 0];

	# https://coolbutuseless.github.io/2021/12/04/base64-encoding/decoding-in-plain-r/

	#unname(vapply(as.character(svec), utf8ToInt, integer(1)))
  }
	












prep.eval = function(value)
	{
	nv = length(value);
	if(is.character(value) && nv==1) 
		{ 
		value = paste0('"',value,'"'); 
		} else { 
				value = deparse(value);
				}
	value;
	}

eval.fromTemplate = function(TEMPLATE, key, value)
	{
	TEMPLATE = str.replace("{key}", key, TEMPLATE);
	
	value = prep.eval(value);
	
	# str.replace failed here trying to be smart ... force=1
	TEMPLATE = gsub("{value}", value, TEMPLATE, fixed=TRUE);
	
	eval(parse(text=TEMPLATE));
	}

	


minvisible.get = function(key="LAST")
	{
	memory.get(key, "-MINVISIBLE-");
	}

minvisible = function(x, key="LAST", display=TRUE)
	{
	memory.set(key, "-MINVISIBLE-", x);
	# also store to ANS variable ... 
	# I could do ANS = Ans = x;   ANS %GLOBAL%. ; Ans %GLOBAL%. ;
	# ANS %GLOBAL% x;  # undefined ANS ... treated as "." (dot)
	"ANS" %GLOBAL% x; 
	"Ans" %GLOBAL% x; 
	
	
	ct.DISPLAY = check.type(display);
	if(!ct.DISPLAY || !is.character(display))	
		{ 
		display = deparse(substitute(display)); 
		display = prep.arg(display, n=3);
		}
	# str may be object ... str may be a var in GLOBAL ... str="hello world";

	has.displayed = NULL;
	if(is.null(has.displayed) && display == "str") 
		{ (has.displayed = print(str(x)) ); }
	if(is.null(has.displayed) && display == TRUE) 
		{ (has.displayed = print(x) ); }	

	
	invisible(x);	
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


#' @rdname as.Type
#' @export
as.Type = as.type;

#' @rdname readChars
#' @export
readChars = readChar;

# This is obviously tedious, violates DRY, and introduces bloat.
# @rdname vs @alias
# https://stackoverflow.com/questions/57770755/
# https://stackoverflow.com/a/30773843/184614
# @describeIn ... @rdname ... map to \alias{} ... 
# @export ??? 
# https://stackoverflow.com/a/22556198/184614
# https://stackoverflow.com/a/71453653/184614
# The @export docstring informs Roxygen to to put the function name in the package NAMESPACE file


