
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
rand = function(min = -1*as.integer(Sys.time()), max = as.integer(Sys.time()), n = 1, method = "high-low", sample.replace = TRUE, seed = NULL, attributes=NULL)
    {
	# if(is.null(seed)) { setSeed(NULL, "rand"); my.seed = getSeed("rand"); } else { my.seed = seed; }
    me = substr( trimMe( tolower(method) ), 1, 2);
	n = as.integer ( n );
	if(is.na(n) || n < 1)
		{
		warning( paste0('Bad value for n "', n, '" in function [rand]', "\n", "Setting n=1") );
		n = 1;
		}
	res = NULL;
    if(me == "fl")  # floor method
      {
	  if(!is.null(seed)) { set.seed(seed); }
      res = ( as.integer( floor( stats::runif(n, min = min, max = (max + 1) ) ) ) );
      }
    else if(me == "sa")  # sample method
      {
      if(!sample.replace)
        {
        len = (max - min) + 1;
        if(len < n)
          {
          warning( "sample.replace forced to TRUE" );
          sample.replace = TRUE;
          }
        }
	  if(!is.null(seed)) { set.seed(seed); }
      res = ( sample(min:max, n, replace = sample.replace) );
      }
	else
		{
		if(me != "hi")  # high-low method
			{
			warning( paste0('Bad value for method "', method, '" in function [rand]', "\n", "Setting method='high-low'") );
			}
		# DEFAULT will run 
		if(!is.null(seed)) { set.seed(seed); }
		res = ( as.integer(( (max + 1) - min) * stats::runif(n) + min) );
		}
		
	if(!is.null(attributes))
		{
		vals = list('min' = min, 'max' = max, 'seed' = seed, 'method' = method);
		res = property.set(res, vals);
		}
	
	res;
	}
