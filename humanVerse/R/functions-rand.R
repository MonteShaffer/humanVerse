
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' rand
#'
#' Generate random integers between two values (min, max).
#'
#' @param min By default -1*as.integer(Sys.time()), what is the minimum?  Could be negative.
#' @param max By default as.integer(Sys.time()), what is the maximum?
#' @param n By default 1, how many random elements do you want?
#' @param method By default "high-low" which slightly outperforms "floor" which both outperform "sample"
#' @param seed By default NULL, meaning we are not worried about tracking the seed here.
#' @param sample.replace For method "sample", will allow replace = FALSE (make certain n, min/max are comformable for this)
#'
#' @return
#' @export
#'
#' @examples
#' rand();            # positive or negative integer
#' rand(0);           # non-negative integer only
#' rand(1);           # positive integer only
#' rand(1, n=3);      # returns 3 positive integers
#' rand(1,10, n=5, method="floor");  # Uses the floor method
#' rand(1,10, n=5, method="sample"); # Uses the sample method (available, but why?)
#' rand(1,10, n=5, method="sample", sample.replace=FALSE); # min, max, n must be comformable "with replacement = FALSE"
#' rand(1,10, n=5, seed=10);  # fixed seed requires the min/max to be known
rand = function() {}
rand = function(min = as.integer(-1*as.numeric(Sys.time())), max = as.integer(1*as.numeric(Sys.time())), n = 1, method = "high-low", sample.replace = TRUE, seed = NULL, attributes=NULL, to.integer=TRUE) 
    {
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.METHOD = check.type(method);
	if(!ct.METHOD || !is.character(method))	
		{ method = deparse(substitute(method)); } 
##########################################################
	METHOD = prep.rand(method);
	
	n = as.integer ( n );
	# min/max can be non-integer, but 'rand' is generally considered an integer operation
	if(is.na(n) || n < 1)
		{
		msg = prep.msg( paste0('Bad input for parameter n "', n, '" in function [rand]', "\n", "Setting n=1") );
		cat.warning(msg);
		n = 1;
		}
	
	FNS = list(
			"high-low" 		= function() { min + (max + 1 - min) * stats::runif(n)  } , 
			"floor" 	= function() { floor( stats::runif(n, min = min, max = (max + 1) ) ); } ,
			"sample" 		= function() { sample(min:max, n, replace = check.sample(sample.replace, min,max, n)); }
			);
			
	if(METHOD != "first")
		{
		# AS-IS, no checks 
		if(!is.null(seed)) { set.seed(seed); }
		res = FNS[[METHOD]]();
		} else {
				if(!is.null(seed)) { set.seed(seed); }
				res = FNS[["high-low"]]();
				}
		
	if(to.integer) { res = as.integer(res); }
	if(!is.null(attributes))
		{
		vals = list('min' = min, 'max' = max, 'n' = n, 
								'seed' = seed, 'method' = METHOD);
		if(METHOD == "sample") { vals$sample.replace = sample.replace;}
		res = property.set("setup", res, vals);
		}	 
	res;
	}
