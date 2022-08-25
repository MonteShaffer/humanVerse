
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' tukey.summary
#'
#' behaves summary() with TUKEY flavor
#'
#'
#' @param x numeric vector
#'
#' @return numeric vector with names of TUKEY summary
#' @export
#'
#' @examples
#' 
#' tukey.summary( c(87, presidents[1:30], 87) );
#'
tukey.summary = function() {}
tukey.summary = function(x, type=1, signif.digits=5, return="five", 
							snames = c("min", "-Q1-", "median", "Mean", "-Q3-", "max"), 
							tnames = c("min", "-Hinge1-", "median", "-Hinge3-", "max"), 
						... )
	{
	r = functions.cleanKey(return, 1);
	
	res = summary(x, digits=signif.digits, quantile.type=type, ...);
	res = res[c(1:3,5:6)];  # tukey 
	names(res) = snames;
	
	myfive = fivenum(x);
	names(myfive) = tnames;
	
	# [s]ummary form 
	if(r == "s")
		{
		res = property.set("five", res, myfive );
		return(res);
		}
	
	# [f]ive or [t]ukey ... [d]efault
	out = myfive;
	out = property.set("summary", out, res);
	return(out);
	}

#' @rdname tukeySummary
#' @export
tukeySummary = tukey.summary;


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' stats.warningNA
#'
#'
#' @param x vector of numbers
#' @return TRUE or FALSE from `anyNA`
#' @export
#'
#' @examples
stats.warningNA = function(x)
	{
	res = anyNA(x);
	if(res) 
		{ 
		howMany = sum(is.na(x));
		values = if(howMany == 1) { "value" } else { "values" }
		warning(paste0("Your data has ", howMany, 
						" missing ", values," [NA] ",
						" :: omitted from analysis"
						)
				); 
		}
	res;
	}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' stats.sum
#'
#' Ignoring "NA" by default ...
#'
#' @param x vector of numbers
#' @param na.rm This will remove missing [NA] values with a warning
#'
#' @return a numeric result (one number)
#' @export
#'
#' @examples
#' x = c(1, NA, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, NA);
#' 
stats.sum = function(x, na.rm=TRUE)
	{
	warning = stats.warningNA(x);
	res = sum(x, na.rm=na.rm)
	res;
	}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' stats.median
#'
#' Applying 'quantile' to median to give more flexibility
#'
#' @param x vector of numbers
#' @param na.rm This will remove missing [NA] values with a warning
#' @param names if TRUE, will append '50%' to the result
#' @param type Default is number 1, meaning smallest value of 'tie'; a member of the set x
#'
#' @return a numeric result (one number)
#' @export
#'
#' @examples
#' x = c(1, NA, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, NA);
#' 
stats.median = function(x, type=1, na.rm=TRUE, names=FALSE, ...)
	{
	warning = stats.warningNA(x);
	# sm.type = typeof(x);  # if we lose the type, we could restore 
	res = stats::quantile(x, prob=c(0.5), type=type, na.rm=na.rm, names=names, ...);
	res;
	}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' stats.quantile
#'
#' Applying 'quantile' to type=1
#'
#' @param x vector of numbers
#' @param na.rm This will remove missing [NA] values with a warning
#' @param names if TRUE, will append '50%' to the result
#' @param type Default is number 1, meaning smallest value of 'tie'; a member of the set x
#'
#' @return a numeric result (one number)
#' @export
#'
#' @examples
#' x = c(1, NA, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, NA);
#' 
stats.quantile = function(x, qs=c(0, 0.25,0.5,0.75, 1), type=1, na.rm=TRUE, names=FALSE, ..., tag.me=FALSE)
	{
	warning = stats.warningNA(x);
	res = stats::quantile(x, prob=qs, type=type, na.rm=na.rm, names=names, ...);
	res;
	}

#' @rdname getQuantiles
#' @export
getQuantiles = stats.quantile;



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' stats.mean
#'
#' Applying by default MEMBERSHIP of the set to mean(x)
#'
#' @param x vector of numbers
#' @param na.rm This will remove missing [NA] values with a warning
#' @param is.member if TRUE, find nearest member of data (x) that is closest to traditional mean
#'
#' @return a numeric result (one number)
#' @export
#'
#' @examples
#' x = c(1, NA, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, NA);
#' 
stats.mean = function(x, na.rm=TRUE, is.member=TRUE, ...)
	{
	warning = stats.warningNA(x);
		# by default, na.rm happens inside b/c na.last=NA
	x.sort = sort(x, na.last=TRUE);  
		# this returns [NA] if there are NA's ... WHY?!?
	m = mean(x.sort, na.rm=na.rm, ...); 

		# plain vanilla with 'sort' overhead
	if(!is.member) { return(m); }  

	m.deviations = abs(x.sort-m);
	m.mins = stats.whichMin(m.deviations);
		# this returns the first (smallest) if deviations tied
	x.sort[ m.mins[1] ]; 
	}

#' @rdname doMean
#' @export
doMean = stats.mean;


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' stats.whichMin
#'
#' behaves like which.min(x) but returns multiple indexes if required.
#'
#'
#' @param x numeric vector
#'
#' @return numeric vector that contains the indexes of *all* min elements, not just the *first*
#' @export
#'
#' @examples
#' which.min( c(23, presidents[1:30], 23) );
#' stats.whichMin( c(23, presidents[1:30], 23) );
#'
stats.whichMin = function(x, na.rm=TRUE)
	{
	# behaves like which.min(x) but returns multiple
	x.min = min( x, na.rm=na.rm ); 
	which(x == x.min);
	}

#' @rdname whichMin
#' @export
whichMin = stats.whichMin;


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' stats.whichMax
#'
#' behaves like which.max(x) but returns multiple indexes if required.
#'
#'
#' @param x numeric vector
#'
#' @return numeric vector that contains the indexes of *all* max elements, not just the *first*
#' @export
#'
#' @examples
#' which.max( c(87, presidents[1:30], 87) );
#' stats.whichMax( c(87, presidents[1:30], 87) );
#'
stats.whichMax = function(x, na.rm=TRUE)
	{
	# behaves like which.max(x) but returns multiple
	x.max = max( x, na.rm=na.rm ); 
	which(x == x.max);
	}

#' @rdname whichMax
#' @export
whichMax = stats.whichMax;





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' stats.whichMinFrequency
#'
#' behaves like the "inverse" of a stats.mode
#'
#'
#' @param x numeric vector
#'
#' @return numeric vector that contains the indexes of *all* min FREQ elements, not just the *first*
#' @export
#'
#' @examples
#' 
#' stats.whichMinFrequency( c(23, presidents[1:30], 23) );
#' whichMinFrequency( c(1:9) );
#' whichMinFrequency( c(1, 1:9, 9) );
#' whichMinFrequency( c(1, 1:9, 9, 9) );
#' stats.whichMinFrequency( c("monte", "says", "hi", "Alex", "likes", "and", "says", "hi", "monte") );
#'
stats.whichMinFrequency = function(x)
	{
	x.table = as.data.frame( table(x) );
		freq.min = min( x.table$Freq );
	x.list = x.table[x.table$Freq==freq.min,];
	xs = as.vector (x.list$x) ;
	xs;
	}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' stats.mode
#'
#' gets the most frequent element in the set (multiple allowed)
#'
#'
#' @param x numeric vector
#'
#' @return numeric vector that contains *all* mode elements (bimodal)
#' @export
#'
#' @examples
#' 
#' stats.mode( c(23, presidents[1:30], 23) );
#' stats.mode( c(1:9) );
#' stats.mode( c(1, 1:9, 9) );
#' stats.mode( c(1, 1:9, 9, 9) );
#' stats.mode( c("monte", "says", "hi", "Alex", "likes", "and", "says", "hi", "monte") );
#'
stats.mode = function(x, force.numeric=TRUE)
	{
	# NA?
	x.table = as.data.frame( table(x) );
		freq.max = max( x.table$Freq );
	x.list = x.table[x.table$Freq==freq.max,];
	xs = as.vector (x.list$x);
	if(force.numeric){ xs = as.numeric(xs);}
	xs;
	}

#' @rdname stats.whichMaxFrequency
#' @export
stats.whichMaxFrequency = stats.mode;



#' whichMinFreq( c(1:9) );
#' whichMinFreq( c(1, 1:9, 9) );
#' whichMinFreq( c(1, 1:9, 9, 9) );
#'
whichMinFreq = function(x) # opposite of doMode
	{
	x.table = as.data.frame( table(x) );
		freq.min = min( x.table$Freq );
	x.list = x.table[x.table$Freq==freq.min,];
	xs = as.numeric( as.vector (x.list$x) );
	xs;
	}

#' @rdname whichMinFrequency
#' @export
whichMinFrequency = stats.whichMinFrequency;




stats.zScores = function(x, x.bar = NULL, s.hat = NULL, method="base")
	{
	if(is.numeric(x.bar) && is.numeric(s.hat)) { return ( (x - x.bar) / s.hat); }
	m = functions.cleanKey(method, 1);
	if( is.null(x.bar) || is.null(s.hat) )
      {
      warning("Only one value was entered for x.bar / s.hat ... Computing from the data instead.")
      }
	
	# [b]ase method ... missing values, definition of mean?
	if( m == "b" )
		{
		return( (x - mean(x)) / sd(x) );
		}
	
	x.bar = stats.mean(x);
	x.sd = sd(x, na.rm = TRUE);	
	return ( (x - x.bar) / s.hat);
	}

calculateZscores = stats.zScores;



stats.summary = function(x, type=1, sort.ASC = FALSE,
								outlier.z = c(-3, 3), 
								outlier.m = c(-3, 3),
								outlier.IQR = c(1.5, 3),
								sharpe.R = 0
						)
	{
	res = list();
	n = length(x);
	xx = stats::na.omit(x);
	n2 = length(xx);
	res$length = list("n" = n, "omit" = (n-n2), "good" = n2);
	res$base = list(	"sum" 		= base::sum(xx),
						"mean" 		= base::mean(xx),	
						"mean.t5" 	= base::mean(xx, trim=0.05),
						"mean.t20" 	= base::mean(xx, trim=0.20),
						"var"		= stats::var(xx),  	# sample as /(n-1)
						"sd"		= stats::sd(xx),	# sample as /(n-1)
						"median"	= stats::median(xx),
						"mad"		= stats::mad(xx),			
						"five"		= stats::fivenum(xx)
					);
					
					# stats::mad() defaults 
					# ?mad hi/low as type?
					# constant as NORMAL assumption:  1/qnorm(3/4) * 2
					
					# https://www.programmingr.com/statistics/skewness/
					# Base R does not contain a function that will allow you to calculate Skewness in R. [LOL!]
					# library(methods) keeps computing the mean() ?!ARGH!?
					# https://en.wikipedia.org/wiki/Skewness#Sample_skewness
					# This is form skewness.g1 
					# [I like oldschool] ... In the older notion of nonparametric skew, defined as {\displaystyle (\mu -\nu )/\sigma ,}(\mu -\nu )/\sigma , where {\displaystyle \mu }\mu  is the mean, {\displaystyle \nu }\nu  is the median, and {\displaystyle \sigma }\sigma  is the standard deviation, the skewness is defined in terms of this relationship
					# If the distribution is both symmetric and unimodal, then the mean = median = mode. 
					# how can mean/median/mode be equal if they are not members of the same set?
					# https://en.wikipedia.org/wiki/Sharpe_ratio
					# https://en.wikipedia.org/wiki/Coefficient_of_variation#Estimation
					
	res$extended = list(
						"mean.mad" 	= mean( abs( xx - res$base$mean ) ),
						"mean.se"	= ( res$base$sd / sqrt(n2) ),
						"skew"		= (sum(( xx - res$base$mean )^3)/n2)/(sum((xx - res$base$mean)^2)/n2)^(3/2),
						"kurtosis"	=  n2 * sum( ( xx - res$base$mean )^4 ) / ( sum( ( xx - res$base$mean )^2)^2 ),  # [space]^4 was bug?
						"sharpe"	= ( res$base$mean - sharpe.R ) / res$base$sd,
						"CV" 		= res$base$sd / res$base$mean
						);

						# matrixStats::weightedMad(xx);
						# matrixStats::weightedMedian(xx);

	 
	# think about redundancy on sorting in all of the functions ...
	res$sorted = sort( x, decreasing=!sort.ASC, na.last=TRUE );
	res$Qs = stats::quantile(xx, prob=( (0:4)/4 ), type=type);  # includes min/max 
		res$IQR = as.numeric(res$Qs[4] - res$Qs[2]); # inner half the data 
	res$Fs = stats::quantile(xx, prob=( (0:5)/5 ), type=type);
	res$Ns = stats::quantile(xx, prob=( (0:9)/9 ), type=type);
		res$ITR = as.numeric(res$Ns[7] - res$Ns[4]); # inner third of the data 
	
	res$mean 	= stats.mean(xx);					# members of the set
	res$median 	= stats.median(xx);
	res$mad		= stats.median( abs( xx - res$median ) ); # members of diff(set)
	res$mode 	= stats.mode(xx);
	res$min 	= as.numeric( res$Ns[1] ); 			# drop names 
	res$max 	= as.numeric( res$Ns[10]);
	res$range	= as.numeric( res$max - res$min ); 	# https://en.wikipedia.org/wiki/Range_(statistics)
	res$xlim	= c(res$min, res$max);  			# this is base::range 
	 
	 
	
		x.bar = res$mean; 				# anchored to set membership
		s.hat = res$base$sd;
	res$zScores = (x - x.bar) / s.hat;  # maybe different that base::scale()
		# the probem: neither x.bar or s.hat are trimmed ... bias in outlier detection
		#  https://en.wikipedia.org/wiki/Grubbs%27s_test
	res$zOutliers = list(
						"lower" = 	which( res$zScores < outlier.z[1] ),
						"upper" = 	which( res$zScores > outlier.z[2] )
						);
	
	# distribution-less on all assumptions ... TBD: outlier.m 
	res$mScores = (x - res$median) / res$mad; 					
	res$mOutliers = list(
						"lower" = 	which( res$mScores < outlier.m[1] ),
						"upper" = 	which( res$mScores > outlier.m[2] )
						);
		
	# TUKEY ... inner / outer into basic outliers ... "far out" homage 
		fence.inner.lower = res$median - outlier.IQR[1] * res$IQR;
		fence.inner.upper = res$median + outlier.IQR[1] * res$IQR;
		fence.outer.lower = res$median - outlier.IQR[2] * res$IQR;
		fence.outer.upper = res$median + outlier.IQR[2] * res$IQR;
	res$outliers = list(
						"lower" = 	which( x < fence.inner.lower ),
						"upper" = 	which( x > fence.inner.upper )
						);
	res$far.out = list(
						"lower" = 	which( x < fence.outer.lower ),
						"upper" = 	which( x > fence.outer.upper )
						);
	
	# maybe do plot at end with NORMAL, t and z-cuts?
	invisible(res); 
	}


