



# dataframe(x, y) : could not find function "dataframe"
 
dataframe = data.frame;

as.dataframe = as.data.frame;

#' @rdname ceil
#' @export
ceil = ceiling;

# natural log as log ... log10 is other, AS-IS so no collision ... 
ln = log; 

e = E = exp(1);
PI = 3.1415926535897932384626
PHI = (1+sqrt(5))/2;

axes = axis;

#' @rdname nchars
#' @export
nchars = nchar;

ord = utf8ToInt;  # mb_ord ?
chr = intToUtf8;



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








# this = function(...) { print(sys.call(1)); }
#  .%THIS%.
.THIS. = function() {}
.THIS. = function(KEY, VALUE, WHERE = parent.frame(1) ) 
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
	# parent.call = sys.call(sys.nframe() - 1L);  # equivalent?
	finfo = parse.syscall(fn);
  
	res = list("envir" = key, "call" = val, "fn.info" = finfo);
		
	# WHERE=parent.frame(1); # or WHERE = env?
	assign("THIS", res, envir=WHERE );
	}
	
"%THIS%" = .THIS.; 













	
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
		nval = val + 1;
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
		nval = val + 1;
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
	
	
	



.MINUSMINUS. = function(KEY, VALUE, WHERE=parent.frame(1))
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
		}
	
	stop("how did you get here");
	}

"%--%" = .MINUSMINUS.;	
	
	














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
	
	
striptags = strip.tags;
str.striptags = strip.tags;
strip_tags = strip.tags;




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


