

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
ERF.C = function(...)
	{
	# pracma::erfc ... 2*pnorm(-sqrt(2)*x)
	z = prep.dots(...);
	1 - ERF(z);	
	}
	
ERF = function(...) 
	{
	z = prep.dots(...);
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
	
ERF.inv = function(...) 
	{
	# inverse in [-1,1] nicely
	zinv = prep.dots(...);
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

