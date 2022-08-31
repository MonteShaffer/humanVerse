
	
	







# https://www.r-project.org/foundation/members.html

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


# maybe stats.countWhere = function(x, "x > 3") {}
# analagous to COUNTIF ... 
stats.countNA = function(x)
	{
	sum(is.na(x));  # adding TRUEs as 1's in binary (0,1) sense 
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
	res = base::sum(x, na.rm=na.rm)
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
		# sort not really necessary?
	m = mean(x.sort, na.rm=na.rm, ...); 

		# plain vanilla with 'sort' overhead
	if(!is.member) { return(m); }  

	# this returns the first (smallest) if deviations tied
	m.dev = abs(x.sort - m);	 			m.mins = stats.whichMin(m.dev);
											x.bar = x.sort[ m.mins[1] ]; 
	x.bar; 
	}

#' @rdname doMean
#' @export
doMean = stats.mean;


stats.sd = function(x, na.rm=TRUE, is.member=TRUE, ...)
	{
	warning = stats.warningNA(x);
	if(!is.member) { sd(x, na.rm=na.rm); }  # plain vanilla 
	n = length(x);
	n2 = n - stats.countNA(x);
		# by default, na.rm happens inside b/c na.last=NA
	x.sort = sort(x, na.last=TRUE);  
	
	x.sum = x.sum2 = 0;
	# NAIVE algorithm, so-called
	for(i in 1:n2)
		{
		x.sum 	= x[i] + x.sum;
		x.sum2	= x[i]*x[i] + x.sum2;
		}
	
	x.mean	= x.sum/n2;
	x.var	= (x.sum2 - (x.sum*x.sum)/n2)/(n2-1);
	x.sd	= sqrt(x.var);
	
	m.dev 	= abs(x.sort - x.mean); 			m.mins = stats.whichMin(m.dev);
												x.bar = x.sort[ m.mins[1] ]; 
	# this is mean centered ... +/- one SD is 68% of the data ?, trecile (33%)
	#m2.dev 	= abs(x.sort - x.bar);
	s.dev 	= abs(x.sort - x.sd ); 				s.mins = stats.whichMin(s.dev);
												s.hat = abs(x.sort[ s.mins[1] ]);
	# this is the element that is closest to computed sd in the set 
						
	# 457 - 300; 457 + 300; # pracma::primes(1000); 
	# maybe create a trecile rule 
	# x.tri = stats::quantile(xx, prob=( (0:3)/3 ), type=1);
	# element nearest lower (x.tri[2]) or nearest upper (x.tri[3])
	# isn't that what x.sort - x.sd is technically doing ?
	
	s.hat;
	}


# buy me a coffee ... popup, not a WIKI annoyance or interrupted, you clicked
# pi simple ... CNTRL-SHIFT CLICK ... v, date 
# https://www.fileformat.info/info/unicode/char/13268/index.htm # 8 divisions
# https://www.fileformat.info/info/unicode/block/egyptian_hieroglyphs/utf8test.htm
# https://www.fileformat.info/info/unicode/char/131fc/browsertest.htm # 5 star
# https://www.fileformat.info/info/unicode/char/1328d/index.htm # ASC/DESC
# https://www.fileformat.info/info/unicode/char/13416/browsertest.htm # median (3)
# https://www.fileformat.info/info/unicode/char/13254/index.htm # persian rug 
# https://www.fileformat.info/info/unicode/char/13028/browsertest.htm # boxplot IQR?
# CENSORSHIP on penis
# https://www.fileformat.info/info/unicode/char/130B8/browsertest.htm
# https://unicode-explorer.com/c/130B8#:~:text=%F0%93%82%B8%20penis%2C%20phallus%20%7C%20EGYPTIAN%20HIEROGLYPH%20D052&text=The%20Unicode%20character%20depicting%20a,the%20Egyptian%20hieroglyphics%20unicode%20block.


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
	warning = stats.warningNA(x);
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
	warning = stats.warningNA(x);
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
	warning = stats.warningNA(x);
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
	warning = stats.warningNA(x);
	# R is a programming language for statistical computing and graphics supported by the R Core Team and the R Foundation for Statistical Computing. Created by statisticians Ross Ihaka and Robert Gentleman, R is used among data miners, bioinformaticians and statisticians for data analysis and developing statistical software. (WIKIPEDIA.com).
	# R is a free software environment for statistical computing and graphics. It compiles and runs on a wide variety of UNIX platforms, Windows and MacOS. (r-project.org) 
	# NA?
	x.table = as.data.frame( table(x) );
		freq.max = max( x.table$Freq );  # why named 'Freq', hopefully it doesn't change its name 
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



stats.mScores = function(x, x.median = NULL, x.mad = NULL, method="base")
	{
	warning = stats.warningNA(x);
	if(is.numeric(x.median) && is.numeric(x.mad)) { return ( (x - x.median) / x.mad ); }
	m = functions.cleanKey(method, 1);
	if( is.null(x.median) || is.null(x.mad) )
      {
      warning("Only one value was entered for x.median / x.mad ... Computing from the data instead.")
      }
	
	# [b]ase method ... missing values, definition of mean?
	if( m == "b" )
		{
		# base::mad has a constant != 1 by default ...
		return( (x - median(x, na.rm=TRUE)) / mad(x, na.rm=TRUE) );
		}
		
	x.median = stats.median(x);
	x.mad	 = stats.median( abs( x - x.median ) );
	
	return ( (x - x.median) / x.mad );
	}
	
	

stats.zScores = function(x, x.bar = NULL, s.hat = NULL, method="base")
	{
	if(is.numeric(x.bar) && is.numeric(s.hat)) { return ( (x - x.bar) / s.hat); }
	warning = stats.warningNA(x);
	m = functions.cleanKey(method, 1);
	if( is.null(x.bar) || is.null(s.hat) )
      {
      warning("Only one value was entered for x.bar / s.hat ... Computing from the data instead.")
      }
	
	# [b]ase method ... missing values, definition of mean?
	if( m == "b" )
		{
		return( (x - mean(x, na.rm=TRUE)) / sd(x) );
		}
	
	x.bar = stats.mean(x);
	x.sd = sd(x, na.rm = TRUE);	
	return ( (x - x.bar) / s.hat);
	}

calculateZscores = stats.zScores;


stats.min = function(x, na.rm=TRUE)
	{
	warning = stats.warningNA(x);
	base::min(x, na.rm=na.rm);
	}
	
stats.max = function(x, na.rm=TRUE)
	{
	warning = stats.warningNA(x);
	base::max(x, na.rm=na.rm);
	}	
	
stats.range = function(x, na.rm=TRUE)
	{
	warning = stats.warningNA(x);
	base::diff( base::range(x, na.rm=na.rm) );
	}

# are these all not univariate?
# x = c(-0.457320423426742, 0.718822930133582, 0.913246703178334, 1.7489769470399, -0.435465367752817, -0.094406449218146, -1.29106291718895, -1.81526173485784, 0.246525347218095, -0.942997917267053, -0.00183948250677812, -0.19456458422759, 1.30012685580887, -0.286935598888774, 1.75036394522621, 1.20020292972812, 0.0872539337664133, 1.38539706854182, 1.85078707399169, 0.0629809109473718, -0.199309995765987, -1.0603974913912, 0.999818789291627, 1.56204778042194, 0.0229749429268058, 1.55599419104352, -1.61627827655638, 2.50204258589797, 1.17878626750971, -0.555160121843776, 0.486708062967901, -0.296390985434472, -0.356519522480354, 0.576064971994066, -0.526271633773905, 2.32994442689201, 1.55933903938102, 0.520621633705236, 0.277102682760275, 1.89017076338778, -0.831309252521779, 0.174731362485081, -1.76979571867116, 0.322617878673464, 1.22647626113366, 0.679615634830971, 0.959971889567321, 0.952391473234687, 0.429646833044911, 2.2402408720535, 1.85154085855492, 0.235088249191772, 0.510886122411297, 1.21555381652151, -1.78997554451195, -0.915385327412029, 0.854750205156582, 1.34335688999449, 0.495875033824638, 0.113341956475536, -0.499171025341094, -0.514522135855301, -1.15059136357202, 1.12563598134349, -0.277864709411034, 0.677044261028789, -0.148662161455589, -0.104440858011832, -0.492614646070104, -0.862547048838231, -0.304471715174949, 1.23759263561903, 0.712062306092214, 1.64070305189485, -1.33675055746022, -1.60301825221837, 1.28710227218544, -0.249302392930578, -1.24772834671796, 0.0702606721622734, 0.712865930106855, 1.08571484024967, -0.542589647418648, -0.258300920730575, -0.575242684071675, 1.62201031630678, 0.0768877651412813, 1.19956015385064, 0.424021358146621, 1.03882219075107, -1.0836688677529, 0.935204617976028, -1.31459218916934, -0.779253539443086, -0.939860443677011, -0.893448119796482, 0.429425929894907, -1.00920713262212, -0.477890588313549, 0.156767929541687);
# x.info = stats.summary(x);
# str(x.info);
stats.summary = function() {}
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
	
		t.var = stats::var(xx);
		t.var.p = t.var*(n2-1)/n2;
		t.mean = base::mean(xx);
		
	res$length = list("n" = n, "omit" = (n-n2), "good" = n2);
	res$base = list(	"sum" 		= base::sum(xx),
						"mean" 		= t.mean,	
						"mean.t5" 	= base::mean(xx, trim=0.05), 
						"mean.t20" 	= base::mean(xx, trim=0.20),
						"var"		= t.var,  			# sample as /(n-1)
						"var.p"		= t.var.p,	# population
						"sd"		= sqrt(t.var),		# UNBIASED /(n-1)
						"sd.p"		= sqrt(t.var.p), 	# population
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
					
					
		t.mean.d = xx - res$base$mean;
	res$extended = list(
						"mean.deviation" = t.mean.d,
						"mean.mad" 	= mean( abs( t.mean.d ) ),
						"mean.se"	= ( res$base$sd.p / sqrt(n2) ),
						"sharpe"	= ( res$base$mean - sharpe.R ) / res$base$sd.p, #Sharpe ratio
						"CV" 		= res$base$sd.p / res$base$mean  # Coefficient of variation
						);
						
						
	# https://en.wikipedia.org/wiki/Method_of_moments_(statistics)
	# not the same as moments::all.moments() ?
	res$moments = list();
		for(i in 0:5)
			{
			mo = paste0("m",i);
			res$moments[[mo]] = sum(t.mean.d^i)/n2;
			}

						# matrixStats::weightedMad(xx);
						# matrixStats::weightedMedian(xx);

	 
	# think about redundancy on sorting in all of the functions ...
	res$sorted = sort( x, decreasing=!sort.ASC, na.last=TRUE );
	
	res$Qs = stats::quantile(xx, prob=( (0:4)/4 ), type=type);  # includes min/max 
		names(res$Qs) = paste0( (0:4), "/4" );
	res$IQR = as.numeric(res$Qs[4] - res$Qs[2]); # inner half the data 
	
	res$Fs = stats::quantile(xx, prob=( (0:5)/5 ), type=type);
		names(res$Fs) = paste0( (0:5), "/5" );
	
	res$Ns = stats::quantile(xx, prob=( (0:9)/9 ), type=type);
		names(res$Ns) = paste0( (0:9), "/9" );
	res$ITR = as.numeric(res$Ns[7] - res$Ns[4]); # inner third of the data 
	
	res$mean 	= stats.mean(xx);					# members of the set
	res$sd		= stats.sd(xx);
	res$var		= (res$sd)^2;
	res$var.p	= res$var*(n2-1)/n2;
	res$sd.p 	= sqrt(res$var.p);	
	res$median 	= stats.median(xx);
	res$mad		= stats.median( abs( xx - res$median ) ); # members of diff(set)
	res$mode 	= stats.mode(xx);
	res$min 	= as.numeric( res$Ns[1] ); 			# drop names 
	res$max 	= as.numeric( res$Ns[10]);
	res$range	= as.numeric( res$max - res$min ); 	
		# https://en.wikipedia.org/wiki/Range_(statistics)
	res$xlim	= c(res$min, res$max);  			# this is base::range 
		# https://bookdown.org/dli/rguide/descriptive-statistics-for-a-vector.html
		# In R, the function, range( ) shows the minimum and maximum value in the dataset. It is not the same idea of range used in statistics.
		# [maybe diff(range()) = stats.range 
	 
	
		x.bar = res$base$mean; 				# NOT anchored to set membership
		s.hat = res$base$sd.p;				# sd (population)
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
	
	
	
	if(n2 < 4) 
		{ 
		warning("Need at least 4 data for complete analysis, returning what has been calculated."); 
		return(invisible(res)); 
		}
	
	# see moments::skewness or e1071::skewness

	skew.g1 = res$moments$m3 / ( (res$moments$m2)^(3/2) );
	skew.G1 = skew.g1 * sqrt(n2 * (n2-1)) / (n2 - 2) ;
	skew.b1 = skew.g1 * ((n2-1)/n) ^ (3/2);
	
	res$skewness = list(
						"g1" = skew.g1,
						"G1" = skew.G1,
						"b1" = skew.b1
						);
	
	info = "Skewness refers to a distortion or asymmetry that deviates from the symmetrical bell curve, or normal distribution, in a set of data. If the curve is shifted to the left or to the right, it is said to be skewed. Skewness can be quantified as a representation of the extent to which a given distribution varies from a normal distribution. A normal distribution has a skew of zero, while a lognormal distribution, for example, would exhibit some degree of right-skew. (https://www.investopedia.com/terms/s/skewness.asp).  \n Skewness is defined base the tails of the distribution, NOT the center of MASS of the data (defined by the histogram: *more* numbers on the right or left side [not the weighting of the numbers defined by the mean, but the counts]).  If the center of MASS appears to be shifted to the LEFT (|:.), it is 'called right-skewed' as the 'long-tail of the data is to the right' and has positive skewness (skewness > 0).  If the center of MASS appears to be shifted to the RIGHT (.:|), it is called \"left-skewed\" as the 'long-tail of the data is to the left' and has negative skewness (skewness < 0).  If the center of MASS is balanced (.:|:.) and symmetric (e.g., a 'Normal' distribution), then (skewness = 0).  The 'confusion' is exacerbated by changing statistical norms from the nonparametric assumptions of the data; specifically the relationship between the median and the mean.  For more info, please see (https://en.wikipedia.org/wiki/Skewness#Relationship_of_mean_and_median)."
	
	res$skewness = property.set("info", res$skewness, info);
	

	kurt.g2 = res$moments$m4 / (res$moments$m2)^2 - 3;
	kurt.G2 = ((n2+1)*kurt.g2 + 6)*(n2-1)/((n2-2)*(n2-3));
	kurt.b2 = (kurt.g2 + 3) * (1 - 1/n2)^2 - 3;
	
	res$kurtosis = list(
						"g2" = kurt.g2,
						"G2" = kurt.G2,
						"b2" = kurt.b2
						);
	

	info = "Kurtosis is a measure of the “tailedness” of the probability distribution. A standard normal distribution has kurtosis of 3 and is recognized as mesokurtic. An increased kurtosis (>3) can be visualized as a thin “bell” with a high peak whereas a decreased kurtosis corresponds to a broadening of the peak and “thickening” of the tails. Kurtosis >3 is recognized as leptokurtic and <3 as platykurtic (lepto=thin; platy=broad). There are four different formats of kurtosis, the simplest is the population kurtosis; the ratio between the fourth moment and the variance. (https://www.sciencedirect.com/topics/neuroscience/kurtosis).  Often the 'excess kurtosis' is reported meaning they already subtracted 3 from the result.";
	
	res$kurtosis = property.set("info", res$kurtosis, info);
	
	
	
	
	
	# maybe do plot at end with NORMAL, t and z-cuts?
	# maybe box.tukey (univariate)
	# maybe box.niner (univariate)
	invisible(res); 
	}




# contigency tables ... partial and absolute probabilities
# pivot tables ... 
# scatterplot with bubble as size 
# scatterplot with categories 
# heat map
# all of excel bivariate R^2 trendlines ...
# =AVERAGE ... mean()
# =STDEV.S ... sd()
# =VAR.S ... var()
# =AVE ... mean.mad() inside stats.summary
# =QUARTILE.INC ... quantile( prob = c(0, 0.25, 0.5, 0.75, 1)
# =PERCENTILE.INC ... quantile (prob = (0:100)/100) ... notice 101
# skewness/kurtosis msg 


