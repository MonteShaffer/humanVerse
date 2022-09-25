


fn.distance = function(method="euclidean")
	{
	ct.METHOD = check.type(method);
	if(!ct.METHOD || !is.character(method))	
		{ method = deparse(substitute(method)); }
	
	DISTANCE = list(
		"euclidean" = function(a,b) { sqrt(sum((a - b)^2)); }, # norm-euclidean?
		"manhattan" = function(a,b) { sum(abs(a - b)); },
		"minkowski" = function(a,b,p=2) { (sum(abs(a - b)^p))^(1/p); },
		"chebyshev" = function(a,b) { max(abs(a - b)); },
		"min-chebyshev" = function(a,b) { min(abs(a - b)); },
		"gower" = function(a,b) { (sum(abs(a - b)))/(length(a)); },
		"canberra" = function(a,b) { (sum(abs(a - b)))/(abs(a)+abs(b)); },
		"lorentzian" = function(a,b) { (sum(ln(1 + abs(a - b)))); }
		);
		
		
	if(is.null(method)) { return (DISTANCE); }
	fn = DISTANCE[[method]]; 
	if(is.null(fn)) { return (DISTANCE); }
	fn;
	}

.distance = function(a, b, method="euclidean", ...)
	{
	# a and b are rows where columns may have x,y,z or lat/lon/alt
	
	# This assumes the method variable has already been cleansed, 
	# see v.dist or matrix.dist 
	# 
	fn = fn.distance(method);
	

cat("\n .distance(a, b, method=\"",method,"\", ...) \n\n", sep="");
cat(fn.replaceParameters(fn, list(...) ), sep="\n"); 
cat("\n\n");	

	fn(a, b, ...);
	}


prep.distance = function(METHOD)
	{
	IN.init();
	key = NULL;
	# add is.null(key) && 
	if(is.null(key) && METHOD %IN% c("Euclidean (Pythagorean) Distance", "euclidean", "eucl", "eulc", "eucl-dist", "euc", "eul", "eu", "e", "p"))
		{ key = "euclidean"; }
	if(is.null(key) && METHOD %IN% c("Manhattan Distance", "manhattan", "manh", "manh-dist"))   
		{ key = "manhattan"; }
	if(is.null(key) && METHOD %IN% c("Minkowski Distance", "minkowski", "mink", "minsk", "mins", "mink-dist"))   
		{ key = "minkowski"; }
	if(is.null(key) && METHOD %IN% c("Chebyshev Distance {stats::dist('maximum');}", "chebyshev", "cheb", "cheb-dist", "maxi-dist", "max-dist", "maximum"))    
		{ key = "chebyshev"; }
	if(is.null(key) && METHOD %IN% c("Minimum Chebyshev Distance", "min-chebyshev", "mini-cheb", "min-cheb", "mini-cheb-dist", "min-cheb-dist"))  
		{ key = "min-chebyshev"; }
	
	if(is.null(key) && METHOD %IN% c("Gower Distance", "gower", "gowe", "gowe-dist"))  
		{ key = "gower"; }
	
	if(is.null(key) && METHOD %IN% c("Canberra Distance", "canberra", "canb", "canb-dist"))  
		{ key = "canberra"; }
	if(is.null(key) && METHOD %IN% c("Lorentzian Distance", "lorentzian", "lore", "lore-dist"))   
		{ key = "lorentzian"; }
		
		
	# binary is for bits ... meaningless in general ...
	# "Jaccard" may be useful for proportion matching ... 
		
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

v.dist = function(vec1, vec2, method="Euclidean", ..., na.rm=TRUE, show.warning=na.rm)
	{
	ct.METHOD = check.type(method);
	if(!ct.METHOD || !is.character(method))	
		{ method = deparse(substitute(method)); }
	
	METHOD = prep.arg(method, n=4, keep="-");
	KEY = prep.distance(METHOD);
	if(KEY == "--NULL--")
		{
		msg = msg.badOption("method", method, METHOD);		
		cat("\n\n"); minvisible( IN.df(), display=TRUE ); cat("\n\n"); 
		IN.clear();	
		cat.stop(msg);
		}
	
	# why call the function if you have na ... ?
	vec1_ = stats.warningNA(vec1, show.warning=show.warning); 
	vec2_ = stats.warningNA(vec2, show.warning=show.warning); 
	if(length(vec1_) != length(vec2_)) { stop("vector lengths are unqueal!"); }

	
	
	return( .distance(vec1_,vec2_, method=as.character(KEY), ...) );
	}

matrix.dist = function(m, method="euclidean", ...)
	{
	ct.METHOD = check.type(method);
	if(!ct.METHOD || !is.character(method))	
		{ method = deparse(substitute(method)); }
	
	m = as.matrix(m);
	# rows are OBSERVATIONS (a) ... cols are DIMENSIONS of a ... (b)
	# if(anyNA(m)) { stop("we have missing values!"; }
	
	METHOD = prep.arg(method, n=4, keep="-");
	KEY = prep.distance(METHOD);
	if(KEY == "--NULL--")
		{
		msg = msg.badOption("method", method, METHOD);	
		cat("\n\n"); minvisible( IN.df(), display=TRUE ); cat("\n\n"); 
		IN.clear();	
		cat.stop(msg);
		}
	
	fn = fn.distance(as.character(KEY));
	
cat("\n matrix.dist(m, method=\"",as.character(KEY),"\", ...) \n\n", sep="");
cat(fn.replaceParameters(fn, list(...) ), sep="\n"); 
cat("\n\n");		

# if a%*%b is slower than crossprod(a,b), why not replace it?
# "%*%" = crossproduct;
	
	n = nrow(m);
	m.names = rownames(m);
	d = matrix(0, nrow=n, ncol=n, dimnames = list(m.names, m.names));
	for(i in 1:n)
		{
		a = m[i, ];
		for(j in i:n)
			{			
			b = m[j, ];
			d[i, j] = d[j, i] = fn(a, b, ...);
			}
		}
	# s = 1-d;  # similarity is 1-distance 
	# d = property.set("similarity", d, s);
	d;
	}



