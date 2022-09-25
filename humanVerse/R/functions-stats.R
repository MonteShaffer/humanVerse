
	
	

# https://cran.r-project.org/web/packages/pvaluefunctions/vignettes/pvaluefun.html
# https://stackoverflow.com/questions/57231854/substitute-p-values-for-stars-in-data-frame-in-r
stats.stars = function(... , 
							cuts =c(0.001, 0.01, 0.05, 0.10, 1), 
							stars=c("***", "**", "*", ".", "N.S.")
						)
	{ 
	pvals = prep.dots(...);
	n = length(pvals);
	nc = length(cuts);
	ns = length(stars);
	res = character(n);
	# check.compatibleLength?
	
	# findInterval is zero-offset, the "-NULL-" accounts for this ... 
	idx = 1 + findInterval(pvals, cuts, left.open=TRUE);  
	stars[idx];	
	}
	
	# gtools::stars.pval
	# unclass(symnum(p.value, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")))
		
	## stats::symnum ... CONTORTIONS !!
	# stop("number of 'cutpoints' must be one less than number of symbols")
	# stop("'symbols' must be unique


 




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
	r = prep.arg(return, 1);
	
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
stats.warningNA = function(x, key="na", show.warning=TRUE)
	{
	KEY = prep.arg(key, n=2, keep="-");
	if(KEY == "na")
		{
		idx = v.which(x, NA);
		}
	if(KEY == "le-eq-ze")  # key = "less-equal-zero"
		{ 
		idx = v.which(x, (x <= 0));
		}
	
	
	test = is.null(idx);
	xx = x;	howMany = 0; strval = "values";
	if(!test)
		{
		howMany = length(idx);
		if(howMany == 1) { strval = "value"; }
		xx = v.return(x[!idx]);
		}
	
	if(show.warning && !test)
		{
		if(KEY == "na")
			{
			# how to translate grammaticalNumber ?
			msg = prep.msg("Your data has ", howMany, "missing", str.grammaticalNumber("value", n=howMany, type="noun"), " [NA] ", " :: omitted from the analysis.");
			}
		if(KEY == "le-eq-ze")  # key = "less-equal-zero"
			{
			msg = prep.msg("Your data has ", howMany, str.grammaticalNumber("value", n=howMany, type="noun"), "less than or equal to zero", " [x <= 0] ", " :: omitted from the analysis.");
			}
		
		cat.warning(msg);
		}
	
	xx;
	}


# maybe stats.countWhere = function(x, "x > 3") {}
# analagous to COUNTIF ... 
stats.countNA = function(x, ...)
	{
	x = prep.dots(...);
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
stats.sum = function(..., na.rm=TRUE, show.warning=na.rm)
	{
	x = prep.dots(...);
	xx = stats.warningNA(x, show.warning=show.warning);  
	x_ = stats.whichX(x, xx, na.rm);
	res = base::sum(x_, na.rm=na.rm)
	res; 
	}

stats.whichX = function(x, xx, test=TRUE)
	{
	nx = x;
	if(test) { nx = xx; }
	nx;
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
stats.median = function(..., type=1, na.rm=TRUE, show.warning=na.rm, names=FALSE, digits=7)
	{
	x = prep.dots(...);
	xx = stats.warningNA(x, show.warning=show.warning);
	x_ = stats.whichX(x, xx, na.rm);
	# how to pass q.args into quantile ... 
	res = stats::quantile(x_, prob=c(0.5), type=type, 
							na.rm=na.rm, names=names, digits=digits);
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
stats.quantile = function(..., qs=c(0, 0.25,0.5,0.75, 1), type=1, na.rm=TRUE, names=FALSE, digits=7)
	{
	x = prep.dots(...);
	xx = stats.warningNA(x, show.warning=show.warning);
	x_ = stats.whichX(x, xx, na.rm);
	# allows the USER to throw an error on BELOW 
	res = stats::quantile(x_, prob=qs, type=type, na.rm=na.rm, names=names, ...);
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
stats.mean = function(..., na.rm=TRUE, is.member=FALSE, show.warning=na.rm, trim=0)
	{
	x = prep.dots(...);
	xx = stats.warningNA(x, show.warning=show.warning);
	x_ = stats.whichX(x, xx, na.rm);
	m = mean(x_, na.rm=na.rm, trim=trim);	
	if(!is.member) { return(m); } # plain vanilla 
	
	
	
	# this returns the first (smallest) if deviations tied
	m.dev = abs(x_ - m);		m.mins = stats.whichMin(m.dev);
								x.bar = x_[ m.mins[1] ]; 
	x.bar; 
	}

#' @rdname doMean
#' @export
doMean = stats.mean;


stats.sd = function(..., na.rm=TRUE, is.member=FALSE, show.warning=na.rm)
	{
	x = prep.dots(...);
	xx = stats.warningNA(x, show.warning=show.warning);
	x_ = stats.whichX(x, xx, na.rm);
	if(!is.member) { sd(x_, na.rm=na.rm); }  # plain vanilla 
	
	
	# I don't know about sd and MEMBERSHIP ... maybe a z-score of 1 (from the data) ... 
	
	n = length(x);
	# n2 = n - stats.countNA(x); 
	n2 = length(xx);
	
	if(!na.rm && (n != n2)) { stop("can't continue with na.rm"); }
	
	x.sum = x.sum2 = 0;
	# NAIVE algorithm, so-called
	for(i in 1:n2)
		{
		x.sum 	= xx[i] + x.sum;
		x.sum2	= xx[i]*xx[i] + x.sum2;
		}
	
	x.mean	= x.sum/n2;
	x.var	= (x.sum2 - (x.sum*x.sum)/n2)/(n2-1);
	x.sd	= sqrt(x.var);
	
	m.dev 	= abs(xx - x.mean); 			m.mins = stats.whichMin(m.dev);
												x.bar = xx[ m.mins[1] ]; 
	# this is mean centered ... +/- one SD is 68% of the data ?, trecile (33%)
	#m2.dev 	= abs(x.sort - x.bar);
	s.dev 	= abs(xx - x.sd ); 				s.mins = stats.whichMin(s.dev);
												s.hat = abs(xx[ s.mins[1] ]);
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




stats.geomean = function(..., na.rm=TRUE, trim=0, show.warning=na.rm, zero.rm=TRUE, show.warning2=zero.rm)
	{
	x = prep.dots(...);	
	xx = stats.warningNA(x, show.warning=show.warning);
	x_ = stats.whichX(x, xx, na.rm);
	xxx = stats.warningNA(xx, key = "less-equal-zero", show.warning=show.warning2);
	x_ = stats.whichX(x_, xxx, zero.rm);
	# we've alreay removed everything, no?
	exp( mean(log(x_), na.rm=na.rm, trim=trim) );
	}



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
stats.whichMin = function(..., na.rm=TRUE, show.warning=na.rm)
	{
	x = prep.dots(...);
	warning = stats.warningNA(x, show.warning=show.warning);
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
stats.whichMax = function(..., na.rm=TRUE, show.warning=na.rm)
	{
	x = prep.dots(...);
	warning = stats.warningNA(x, show.warning=show.warning);
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
stats.whichMinFrequency = function(..., force.numeric=TRUE)
	{
	x = prep.dots(...);
	stats.mode(x, force.numeric=force.numeric, do.min=TRUE);
	}

whichMinFrequency = stats.whichMinFrequency;
whichMinFreq = stats.whichMinFrequency;

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
stats.mode = function(..., force.numeric=TRUE, do.min=FALSE)
	{
	x = prep.dots(...);
	# NA maybe the "mode"
	# R is a programming language for statistical computing and graphics supported by the R Core Team and the R Foundation for Statistical Computing. Created by statisticians Ross Ihaka and Robert Gentleman, R is used among data miners, bioinformaticians and statisticians for data analysis and developing statistical software. (WIKIPEDIA.com).
	# R is a free software environment for statistical computing and graphics. It compiles and runs on a wide variety of UNIX platforms, Windows and MacOS. (r-project.org) 
	# NA?
	x.table = as.data.frame( table(x) );
		freq = x.table[, 2];
		freq.search = stats.max(freq);
		if(do.min) { freq.search = stats.min(freq); }
		idx = which(freq == freq.search);
		
	res = x[idx];
	if(force.numeric) { res = as.numeric(res);}
	res;
	}

#' @rdname stats.whichMaxFrequency
#' @export
stats.whichMaxFrequency = stats.mode;
stats.whichMaxFreq = stats.mode;
whichMaxFrequency = stats.mode;
whichMaxFreq = stats.mode;



stats.mScores = function(x, x.median = NULL, x.mad = NULL, method="base")
	{
	warning = stats.warningNA(x, show.warning=show.warning);
	if(is.numeric(x.median) && is.numeric(x.mad)) { return ( (x - x.median) / x.mad ); }
	m = prep.arg(method, 1);
	if( is.null(x.median) || is.null(x.mad) )
      {
      warning("Only one value was entered for x.median / x.mad ... Computing from the data instead.")
      }
	
	# [b]ase method ... missing values, definition of mean?
	if( m == "b" )
		{
		# base::mad has a constant != 1 by default ...
		return( (x - median(x, na.rm=TRUE, show.warning=TRUE)) / mad(x, na.rm=TRUE, show.warning=TRUE) );
		}
		
	x.median = stats.median(x);
	x.mad	 = stats.median( abs( x - x.median ) );
	
	return ( (x - x.median) / x.mad );
	}
	
	

stats.zScores = function(x, x.bar = NULL, s.hat = NULL, method="base")
	{
	if(is.numeric(x.bar) && is.numeric(s.hat)) { return ( (x - x.bar) / s.hat); }
	warning = stats.warningNA(x, show.warning=show.warning);
	m = prep.arg(method, 1);
	if( is.null(x.bar) || is.null(s.hat) )
      {
      warning("Only one value was entered for x.bar / s.hat ... Computing from the data instead.")
      }
	
	# [b]ase method ... missing values, definition of mean?
	if( m == "b" )
		{
		return( (x - mean(x, na.rm=TRUE, show.warning=TRUE)) / sd(x) );
		}
	
	x.bar = stats.mean(x);
	x.sd = sd(x, na.rm = TRUE);	
	return ( (x - x.bar) / s.hat);
	}

calculateZscores = stats.zScores;




stats.MAD = function(..., m=NULL, na.rm=TRUE, show.warning=na.rm)
	{ 
	# median absolute deviation
	x = prep.dots(...);
	warning = stats.warningNA(x, show.warning=show.warning);
	if(is.null(m)) { m = stats.median(x); }
	stats.median( abs( x - m ) );
	}


stats.min = function(..., na.rm=TRUE, show.warning=na.rm)
	{
	x = prep.dots(...);
	warning = stats.warningNA(x, show.warning=show.warning);
	base::min(x, na.rm=na.rm);
	}
	
stats.max = function(..., na.rm=TRUE, show.warning=na.rm)
	{
	x = prep.dots(...);
	warning = stats.warningNA(x, show.warning=show.warning);
	base::max(x, na.rm=na.rm);
	}	
	
stats.range = function(..., na.rm=TRUE, show.warning=na.rm)
	{
	x = prep.dots(...);
	warning = stats.warningNA(x, show.warning=show.warning);
	base::diff( base::range(x, na.rm=na.rm) );
	}

# are these all not univariate?
# x = c(-0.457320423426742, 0.718822930133582, 0.913246703178334, 1.7489769470399, -0.435465367752817, -0.094406449218146, -1.29106291718895, -1.81526173485784, 0.246525347218095, -0.942997917267053, -0.00183948250677812, -0.19456458422759, 1.30012685580887, -0.286935598888774, 1.75036394522621, 1.20020292972812, 0.0872539337664133, 1.38539706854182, 1.85078707399169, 0.0629809109473718, -0.199309995765987, -1.0603974913912, 0.999818789291627, 1.56204778042194, 0.0229749429268058, 1.55599419104352, -1.61627827655638, 2.50204258589797, 1.17878626750971, -0.555160121843776, 0.486708062967901, -0.296390985434472, -0.356519522480354, 0.576064971994066, -0.526271633773905, 2.32994442689201, 1.55933903938102, 0.520621633705236, 0.277102682760275, 1.89017076338778, -0.831309252521779, 0.174731362485081, -1.76979571867116, 0.322617878673464, 1.22647626113366, 0.679615634830971, 0.959971889567321, 0.952391473234687, 0.429646833044911, 2.2402408720535, 1.85154085855492, 0.235088249191772, 0.510886122411297, 1.21555381652151, -1.78997554451195, -0.915385327412029, 0.854750205156582, 1.34335688999449, 0.495875033824638, 0.113341956475536, -0.499171025341094, -0.514522135855301, -1.15059136357202, 1.12563598134349, -0.277864709411034, 0.677044261028789, -0.148662161455589, -0.104440858011832, -0.492614646070104, -0.862547048838231, -0.304471715174949, 1.23759263561903, 0.712062306092214, 1.64070305189485, -1.33675055746022, -1.60301825221837, 1.28710227218544, -0.249302392930578, -1.24772834671796, 0.0702606721622734, 0.712865930106855, 1.08571484024967, -0.542589647418648, -0.258300920730575, -0.575242684071675, 1.62201031630678, 0.0768877651412813, 1.19956015385064, 0.424021358146621, 1.03882219075107, -1.0836688677529, 0.935204617976028, -1.31459218916934, -0.779253539443086, -0.939860443677011, -0.893448119796482, 0.429425929894907, -1.00920713262212, -0.477890588313549, 0.156767929541687);
# x.info = stats.summary(x);
# str(x.info);
stats.summary = function() {}
stats.summary = function(..., type=1, sort.ASC = FALSE,
								outlier.z = c(-3, 3), 
								outlier.m = c(-3, 3),
								outlier.IQR = c(1.5, 3),
								sharpe.R = 0
						)
	{
	x = prep.dots(...);
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
	
	res$mean 	= stats.mean(xx, is.member=TRUE);		# members of the set
	res$sd		= stats.sd(xx, is.member=TRUE);
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
	skew.b1 = skew.g1 * ((n2-1)/n2) ^ (3/2);
	 
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


















# n.FUNCTIONS ... 
## is.vector vs is.atomic ... 
## list(500) is no, vector("list", 500) is yes ...
## n.dim(X) and n.length(X) ... deal with issues of dim not found 
## maybe a function '`count`' ... sizeof => size 




 
myfive = function(..., na.rm=TRUE, show.warning=na.rm)
	{
	x = prep.dots(...);
	xx = stats.warningNA(x, show.warning=show.warning);
	x_ = stats.whichX(x, xx, na.rm);
	res = stats::quantile(x_, prob=c(0/3, 1/3, 1/2, 2/3, 3/3), type=1);			
	names(res) = c("0/3 [min]", "1/3 [lower-trecile]", 
					"1/2 [median]", "2/3 [upper-trecile]", "3/3 [max]");
	res;
	}
 


stats.test = function(X.stat, method="norm", ..., tail="both", alpha=0.05)
	{
	# for given X.stat and alpha ... compute X.crit and pvalue 
	# based on a distribution with its needed parameters 
	# tail = "both", "lower", "upper" ... what to do with alpha 
	
	ct.method = check.type(method);
	if(!ct.method || !is.character(method)) 
		{ method = deparse(substitute(method)); }
	
	# just call the generic function PDF/CDF/inverseCDF to solve the problem 
	# here you would do the appropriate 1-p if necessary 
	# should I add the multivariate chi-square to these ...
	# I have a p and a q?
	
	}
	