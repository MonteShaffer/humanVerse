

fn.norm = function(method="euclidean")
	{
	ct.METHOD = check.type(method);
	if(!ct.METHOD || !is.character(method))	
		{ method = deparse(substitute(method)); }
	
	NORM = list(
		"sum" = function(x) { x/sum(x); },
		"manhattan" = function(x) { x/sum(abs(x)); },  		# L1
		"euclidean" = function(x,k=2) { xa = abs(x); xam = max(xa); norm = xam * ( (sum((xa / xam)^k))^(1/k) ); x/norm; },  	# L2+ (overflow prevention
		"min" = function(x) { x/min(x); },
		"max" = function(x) { x/max(x); },
		"length" = function(x) { x/length(x); },
		"divide" = function(x,f=1) { x/f; },
		"multiply" = function(x,f=1) { x*f; },
		"reverse" = function(x, lo=min(x), hi=max(x) ) { (hi + lo) - x; },
		"min-max" = function(x) { lo=min(x); hi=max(x); d = (hi-lo); ( x - lo) / d; },
		"custom-range" = function(x, from=c(min(x), max(x)), to=c(-1,1) ) { to[1] + diff(to) * (x-from[1]) / diff(from); },	 	
		"z-scores" = function(x) { (x-mean(x))/sd(x); },
		"m-scores" = function(x) { m = stats.median(x); (x-m)/stats.MAD(x, m=m); }
		);
		
	# if ALL, return the LIST ... NULL ... otherwise just the function ... 
	# load function into MATRIX call, so it is just looping ... not having to check which method ...

	if(is.null(method)) { return (NORM); }
	fn = NORM[[method]]; 
	if(is.null(fn)) { return (NORM); }
	fn;
	}

.norm = function(vec, method="sum", ...)
	{
	# no checks, this better be well-formatted 
	fn = fn.norm(method); # get the appropriate function 	

cat("\n .norm(vec, method=\"",method,"\", ...) \n\n", sep="");
cat(fn.replaceParameters(fn, list(...) ), sep="\n"); 
cat("\n\n");
	
	
	if(method %in% c("divide", "multiply"))
		{
		if( !exists("f", list(...), inherits = FALSE ) )			
			{ 
			f = 1;
			cat.warning(msg.missingParam("f",1));
			return( fn(x, f=1) );			
			}		
		}
	
	fn(x, ...);	
	}	
	
prep.norm = function(METHOD)
	{
	IN.init();
	key = NULL;
	# add is.null(key) && 
	if(is.null(key) && METHOD %IN% c("Sum", "sum"))
		{ key = "sum"; }
	if(is.null(key) && METHOD %IN% c("Minimum", "min"))
		{ key = "min"; }
	if(is.null(key) && METHOD %IN% c("Maximum", "max"))
		{ key = "max"; }
	if(is.null(key) && METHOD %IN% c("Length", "length", "len"))
		{ key = "length"; }
	if(is.null(key) && METHOD %IN% c("Reverse-code", "reverse", "rev-cod", "rev-sco", "rev-lik"))
		{ key = "reverse"; }
	# # # bounded between [0,1] # # # 
	if(is.null(key) && METHOD %IN% c("Minimum-Maximum", "minimum-maximum", "min-max", "max-min"))
		{ key = "min-max"; }
	if(is.null(key) && METHOD %IN% c("Custom-Range", "custom-range", "cus-ran", "r", "ran", "c-r"))
		{ key = "custom-range"; }
	if(is.null(key) && METHOD %IN% c("zScores", "z-scores", "zsc", "z", "z-s", "z-sco", "mea-sca", "sca"))	
		{ key = "z-scores"; }
	if(is.null(key) && METHOD %IN% c("mScores", "m-scores", "msc", "m", "m-s", "m-sco", "med-sco", "med-sca"))
		{ key = "m-scores"; }
	if(is.null(key) && METHOD %IN% c("Divide", "divide", "div"))
		{ key = "divide"; }
	if(is.null(key) && METHOD %IN% c("Multiply", "multiply", "mul", "factor", "fac"))
		{ key = "multiply"; }
	if(is.null(key) && METHOD %IN% c("Euclidean", "euclidean", "euc", "ecu"))
		{ key = "euclidean"; }
	if(is.null(key) && METHOD %IN% c("Manhattan", "manhattan", "man", "abs", "abs-sum", "sum-abs"))
		{ key = "manhattan"; }
		
		
	if(is.null(key)) 
		{ 
		key = "--NULL--"; 
		df = IN.df();
		minvisible(df, print=FALSE);
		key = property.set("IN", key, df);
		}
		
	IN.clear();	
	key;
	}
	
	

v.norm = function(vec, method="sum", ..., force.abs=FALSE, na.rm=TRUE, show.warning=na.rm)
	{
	# this works naturally, AS-IS on vectors/matrices
	if(is.dataframe(vec)) { vec = as.matrix(vec); }
	
	ct.METHOD = check.type(method);
	if(!ct.METHOD || !is.character(method))	
		{ method = deparse(substitute(method)); }
	
	METHOD = prep.arg(method, n=3, keep="-");
	KEY = prep.norm(METHOD);
	if(KEY == "--NULL--")
		{
		df = property.get("IN", KEY);
		msg = msg.badOption("method", method, METHOD);	
		cat("\n\n"); minvisible( df, print=TRUE ); cat("\n\n"); 
		IN.clear();	
		cat.stop(msg);
		}
	
	vec = stats.warningNA(vec, show.warning=show.warning); 
	if(force.abs) { vec = abs(vec); }
	
	return( .norm(vec, as.character(KEY), ...) );
	}
	
