
#
# doStatsSummaryDataFrame = function(df)
# 	{
# 	# pastec?
#
# 	}

#' doStatsSummary
#'
#' @family Stats
#'
#' @param x a numeric vector
#'
#' @return a list of various statistical summaries
#' @export
#'
#' @examples
#' x.norm = doStatsSummary ( rnorm(100,0,1) );
#' x.unif = doStatsSummary ( runif(100,0,1) );
#'
doStatsSummary = function(x)
	{
	# getAttribute("na.action", xx)
	# getAttribute("class", getAttribute("na.action", xx) )
	result = list();
		result$length = length(x);
	xx = stats::na.omit(x);
		result$length.na = length(x) - length(xx);
		result$length.good = length(xx);
	result$mean = mean(xx);
	result$mean.trim.05 = mean(xx, trim=0.05);
	result$mean.trim.20 = mean(xx, trim=0.20);

	result$sorted = sort(xx);

	result$median = stats::median(xx);
	result$MAD = stats::mad(xx);
	result$IQR = stats::IQR(xx);
	result$quartiles = stats::quantile(xx, prob=c(.25,.5,.75), type=1); # tries to use actual data, not averages ...
	# https://stats.stackexchange.com/questions/430391/are-there-99-percentiles-or-100-percentiles-and-are-they-groups-of-numbers-or
	# https://math.stackexchange.com/questions/1419609/are-there-3-or-4-quartiles-99-or-100-percentiles
		probs.deciles = seq(0.1,0.9,by=0.1);
		#probs.deciles = seq(0.1,1.0,by=0.1);
	result$deciles = stats::quantile(xx, prob=probs.deciles, type=1 );
	result$decile.members = cutMe(xx, probs.deciles, lower.equal = TRUE);
	  probs.centiles = seq(0.01,0.99,by=0.1);
	result$centiles = stats::quantile(xx, prob=probs.centiles, type=1 );
	# ?cut   cut(xx, 10) ... will be usefull for a histogram-ish device ...
	# https://stackoverflow.com/questions/11728419/using-cut-and-quartile-to-generate-breaks-in-r-function
		probs.niniles = (1:8)/9;
	result$niniles = stats::quantile(xx, prob=probs.niniles, type=1 );
	result$niniles.members = cutMe(xx, probs.niniles, lower.equal = TRUE);


	result$median.weighted = matrixStats::weightedMad(xx);
	result$MAD.weighted = matrixStats::weightedMedian(xx);

	# value from data
	result$myMedian = doMedian(xx,1);
	result$myMean   = doMean(xx);

	result$max = max(xx);
	result$min = min(xx);
	result$range = result$max - result$min;
	result$xlim = range(xx);

	result$max.idx = whichMax(x);
	result$min.idx = whichMin(x);

	result$mode = result$freq.max = doMode(x);  # elements with highest frequency
	result$which.min.freq = doModeOpposite(x);

	result$ylim = c( findFrequencyMinimumInVector(xx), findFrequencyMaximumInVector(xx) );

	# you could later get indexes of each mode(freq.max)/freq.min using findAllIndexesWithValueInVector

	result$sd = stats::sd(xx);
	result$var = stats::var(xx);

	result$se.mean = result$sd / sqrt(result$length.good);
	result$IDR.3 = doIDR(xx, 1/3);

	result$var.naive = doSampleVariance(x,"naive");
	result$var.2step = doSampleVariance(x,"2step");

	result$outliers.z = findOutliersUsingZscores(x);
	result$outliers.IQR = findOutliersUsingIQR(x);

	# result$z = calculateZscores(x); # works same as scale ...
	# append "attributes" ...

	result;
	}



#' zCutOverlay
#'
#' @param z.table
#' @param steps.z
#' @param verbose
#' @param myColor
#' @param ...
#'
#' @return
#' @export
zCutOverlay = function(z.table, steps.z = 1/2, verbose = FALSE, myColor = "blue", ...)
	{

	# z = calculateZscores(x);
	  # range.z = c(-3,3);
	  # steps.z = 1/2;
	# z.cut = cutZ(z, range.z, steps.z, verbose=TRUE);
	# z.table = table(z.cut$member);

	# how to deal with -Inf, +Inf ... put them one step above/below the range.z values ... maybe two steps ...
	# currently, I think they will grpah, just at Inf

	keys = as.numeric(names(z.table));
	vals = as.numeric(z.table); # these are the counts as height

	xleft = keys ;
	xright = keys + steps.z;
	ybottom = 0 * keys;
		vals.sum = standardizeToSum(vals);
		vals.max = standardizeToMax(vals.sum);
		vals.normmax = standardizeToFactor(vals.max, 0.4);
	ytop = vals.normmax

	graphics::rect(xleft, ybottom, xright, ytop, col=myColor);
	}



#' cutZ
#'
#' @param z
#' @param range.z
#' @param steps.z
#' @param verbose
#'
#' @return
#' @export
cutZ = function(z, range.z = c(-3,3), steps.z = 1/2, verbose = FALSE)
	{
	# allows overlay of rectangles on normal graph
	zz = z;
	nz = length(zz);

	zmin = range.z[1];
	zmax = range.z[2];

	df = as.data.frame(zz);
		df$member = 0 * zz;

	i = 1;
	buckets = c();
	breaks = c();
		which.z = which(zz < zmin);
			buckets[[i]] = length(which.z);
			breaks[[i]] = -Inf;
		if(length(which.z) > 0)
			{
			df$member[which.z] = -Inf;
			zz[which.z] = NA; # set to NA so won't count any more
			}

	i = i + 1;



	zlower = zmin;
		if(verbose)
			{
			cat("\n", "zmin: ", zmin, "  ...  ", "zmax: ", zmax, "\n");
			}
	while(zlower < zmax)
		{
		if(verbose)
			{
			cat("\n", " == WHILE == ", "zlower: ", zlower, "  <  ", "zmax: ", zmax, "\n");
			}

		zupper = zlower +  steps.z;

		which.z = which(zz < zupper);
			buckets[[i]] = length(which.z);
			breaks[[i]] = zlower;
		if(length(which.z) > 0)
			{
			df$member[which.z] = zlower;
			zz[which.z] = NA; # set to NA so won't count any more
			}

		i = i + 1;
		zlower = zupper;
		}

	which.z = which(!is.na(zz));
		buckets[[i]] = length(which.z);
		breaks[[i]] = Inf;
	if(length(which.z) > 0)
			{
			df$member[which.z] = Inf;
			}

	df;
	}




#' cutMe
#'
#' @param x
#' @param qs
#' @param type
#' @param lower.equal
#'
#' @return
#' @export
cutMe = function(x, qs, type=1, lower.equal=TRUE)
	{
  	# freq.df = as.data.frame( cbind( breaks, buckets ) );
	#	colnames(freq.df) = c("break", "count");

	# df = setAttribute("cuts", q.cuts, df);  # set KEY to VAL in OBJ

	# I can get freq.df by just calling table(df)
	# colnames(df) = c("z", "member");



# # create a 2 by 5 matrix
# x <- 1:10
# attr(x,"dim") <- c(2, 5)
# https://stackoverflow.com/questions/27546901/how-to-set-attributes-for-a-variable-in-r



	# xx = na.omit(x);
	xx = x;
	df = as.data.frame(xx);
		df$yy = 0 * xx;
	nx = length(xx);
	q.cuts = getQuantiles(xx, qs, type);
		# attributes(df)[["cuts"]] = q.cuts;
		df = setAttribute("cuts", q.cuts, df);  # set KEY to VAL in OBJ
		# getAttribute("cuts", df);

	# buckets = length(qs) + 1;
	# yy = list();
	for(b in 1:length(qs) )
		{
		idxb = which(xx <= q.cuts[b]);
		df$yy[idxb] = b;
		xx[idxb] = NA;
		# yy[[b]] = idxb;
		}
		idxb = which(!is.na(xx)); # leftovers
	# yy[[b+1]] = idxb;
		df$yy[idxb] = b+1;
	colnames(df) = c("x", "member");
	df;

	}


#' cutN
#'
#' @param x
#' @param ...
#' @param n
#'
#' @return
#' @export
cutN = function(x, ..., n=2)
	{
	more = unlist(list(...));
	x = c(x, more);

	# create a list of elements with "n"
	out = list();
	i = 0;
	while(length(x) > 0)
		{
		i = 1 + i;
		sub = x[1:n];
			out[[i]] = sub;
		x = x[-c(1:n)];
		}
	out;
	}


#' getQuantiles
#'
#' @param x
#' @param qs
#' @param type
#'
#' @return
#' @export
getQuantiles = function(x, qs, type=1)
	{
  # quantile(data, probs = c(.37, .53, .87))
	xx = stats::na.omit(x);
	as.numeric(stats::quantile(xx, prob=qs, type=type));
	}


#' doIDR
#'
#' @param x
#' @param lower
#' @param upper
#' @param type
#'
#' @return
#' @export
doIDR = function(x, lower = 0.25, upper = 1 - lower, type=1)
	{
	xx = stats::na.omit(x);
	q.lower = getQuantiles(xx, lower, type);
	q.upper = getQuantiles(xx, upper, type);
	q.lim = c(q.lower, q.upper);
	q.range = abs(q.upper - q.lower); # in case they enter them backwards

	list("call" = list("lower" = lower, "upper" = upper, "type" = type),
		"IDR" = list("lower" = q.lower, "upper" = q.upper, "xlim" = q.lim, "range" = q.range)
		);
	}

#' doSampleVariance
#'
#' Computes the sample variance with (n-1) ...
#'
#' @family Stats
#'
#' @param x numeric vector
#' @param method "two-pass" prevents "naive" floating-point issues
#'
#' @return list (x.bar, s.var, s.sd)
#' @export
#'
#' @examples
#' doSampleVariance( c(1) ); # returns null
#' doSampleVariance( 1:2 );
#'
doSampleVariance = function(x, method="two-pass")
	{
	x = stats::na.omit(x);
	if(method=="naive")
		{
		n = 0;
		sum = 0;
		sum2 = 0;

		for(i in 1:length(x))  ## stats::na.omit(x)
			{
			n = n + 1;
			sum = sum + x[i];
			sum2 = sum2 + x[i]*x[i];
			}

		if(n < 2) { return(NULL);} #
			x.bar = sum/n;
			s.var = (sum2 - (sum*sum)/n)/(n-1);

		} else	{
				# two-pass algorithm # testing
				n = sum = sum2 = 0;
				## first pass
				for(i in 1:length(x))  ## stats::na.omit(x)
					{
					n = n + 1;
					sum = sum + x[i];
					}
		if(n < 2) { return(NULL);} #
				x.bar = sum/n;
				## second pass
				for(i in 1:length(x))  ## stats::na.omit(x)
					{
					deviation = x[i] - x.bar;
					sum2 = sum2 + deviation * deviation;
					}
				s.var = sum2/(n-1);
				}

		s.sd = sqrt(s.var);
	list("x.bar"=x.bar,"s.var"=s.var,"s.sd"=s.sd);
	}


#' calculateZscores
#'
#' Calculate z-scores using formula:  (x - x.bar) / s.hat;
#'
#'
#' @family Stats
#'
#' @param x numeric vector
#' @param x.bar default NULL, you can pass in the parameter
#' @param s.hat default NULL, you can pass in the parameter
#'
#' @return z (numeric vector)
#' @export
#'
#' @examples
#' calculateZscores( 1:5 );
#' calculateZscores( 1:9 );
#'
#' calculateZscores( 1:5, x.bar=3, s.hat=1 );
#' calculateZscores( 1:9, x.bar=3, s.hat=1 );
#'
calculateZscores = function(x, x.bar=NULL, s.hat=NULL)
	{
  if(is.numeric(x.bar) && is.numeric(s.hat)) { return ((x - x.bar) / s.hat);}
  # maybe throw a warning if one is null, but not the other
  if( (is.null(x.bar) + is.null(s.hat)) == 1)
      {
      warning("Only one value was entered for x.bar / s.hat ... Computing these values instead.")
      }


	dsv = doSampleVariance(x);

	x.bar = dsv$x.bar;
	s.hat = dsv$s.sd;

	if(is.null(s.hat)) { return (NULL); }  # we take care of division by zero in our custom sampleVarianceFunction

	(x - x.bar) / s.hat;
	}


#' findOutliersUsingZscores
#'
#'
#' [assuming normality.  Is that a good assumption?]
#' \url{https://statisticsbyjim.com/basics/outliers/}
#'
#' @family Stats
#'
#'
#' @param x numeric vector
#' @param zmin what is the lower-bound cutoff to label an outlier
#' @param zmax what is the upper-bound cutoff to label an outlier
#'
#' @return list of various features of the outliers
#' @export
#'
#' @examples
#' findOutliersUsingZscores( c(-5, rep(1:3,9), 9) );
#' findOutliersUsingZscores( c(-5,-4, rep(1:3,9), 8,9) );
#' findOutliersUsingZscores( c(-5, rep(1:3,9), 9), -2, 2);
#' findOutliersUsingZscores( c(-5,-4, rep(1:3,9), 8,9), -2, 2 );
#'
findOutliersUsingZscores = function(x, zmin=-3, zmax=3)
	{
  result = list();
  result$z = z = calculateZscores(x);
  result$z.min = zmin;
  result$z.max = zmax;

	outliers = x[z < zmin | z > zmax];
	  outliers.lower.which = which(z < zmin);
	  outliers.lower = x[outliers.lower.which];
			v.lower = rep("lower", length(outliers.lower));
		outliers.upper.which = which(z > zmax);
		outliers.upper = x[outliers.upper.which];
			v.upper = rep("upper", length(outliers.upper));

	df.lower = cbind(outliers.lower, v.lower);
	df.upper = cbind(outliers.upper, v.upper);

	df = as.data.frame(rbind(df.lower,df.upper));
		colnames(df) = c("value","direction");

	result$df = df;

	result$z.lower = outliers.lower.which;  # indexes
	result$z.upper = outliers.upper.which;  # indexes

	result;
	}


#' findOutliersUsingIQR
#'
#' [assuming nothing about the data]
#' \url{https://statisticsbyjim.com/basics/outliers/}
#'
#' @family Stats
#'
#'
#' @param x numeric vector
#' @param innerFenceFactor typically 1.5 * IQR
#' @param outerFenceFactor typically 3.0 * IQR
#'
#' @return list of various features of the outliers
#'
#' @export
#'
#' @examples
#' findOutliersUsingIQR( c(-5, rep(1:3,9), 9) );
#' findOutliersUsingIQR( c(-5,-4, rep(1:3,9), 8,9) );
#' findOutliersUsingIQR( c(-5, rep(1:3,9), 9), 1, 2);  # unchanging
#' findOutliersUsingIQR( c(-5,-4, rep(1:3,9), 8,9), 1, 2 );  # unchanging
#' findOutliersUsingIQR( c(-5, rep(1:3,9), 9), 2, 4);  # unchanging
#' findOutliersUsingIQR( c(-5,-4, rep(1:3,9), 8,9), 2, 4 );  # unchanging
#'
findOutliersUsingIQR = function(x, innerFenceFactor=1.5, outerFenceFactor=3)
	{
  result = list();

	result$IQR = myIQR = stats::IQR(x, na.rm=TRUE);
	result$Quartiles = myQuartiles = as.numeric( stats::quantile(x, na.rm=TRUE, prob=c(.25,.5,.75)) );

	result$inner.fence = innerFence = myIQR * innerFenceFactor;
	result$outer.fence = outerFence = myIQR * outerFenceFactor;

	result$Q1.inner = Q1.inner = myQuartiles[1] - innerFence;
	result$Q1.outer = Q1.outer = myQuartiles[1] - outerFence;

	result$Q3.inner = Q3.inner = myQuartiles[3] + innerFence;
	result$Q3.outer = Q3.outer = myQuartiles[3] + outerFence;


	# values that fall inside the two inner fences are not outliers ...
	result$inner.which = inner.which = which(x < Q1.inner | x > Q3.inner);
	          inner.which.lower = which(x < Q1.inner);
	          inner.which.upper = which(x > Q3.inner);
	  result$inner = inner = x[inner.which];	# circles
	result$outer.which = outer.which = which(x < Q1.outer | x > Q3.outer);
	          outer.which.lower = which(x < Q1.outer);
	          outer.which.upper = which(x > Q3.outer);
	  result$outer = outer = x[outer.which];	# * in boxplot
	# outer and inner may have duplicates, let's remove from inner so they are disjoint ...
	result$inner.u = inner = setdiff(inner,outer);
	# I could separate into lower and upper for later manipulation

	v.inner = rep("inner", length(inner));
		inner.lower = inner[inner < Q1.inner];
			v.inner.lower = rep("lower", length(inner.lower));
		inner.upper = inner[inner > Q3.inner];
			v.inner.upper = rep("upper", length(inner.upper));
	v.outer = rep("outer", length(outer));
		outer.lower = outer[outer < Q1.outer];
			v.outer.lower = rep("lower", length(outer.lower));
		outer.upper = outer[outer > Q3.outer];
			v.outer.upper = rep("upper", length(outer.upper));

	df.inner = cbind( c(inner.lower, inner.upper), v.inner, c(v.inner.lower, v.inner.upper) );

	df.outer = cbind( c(outer.lower, outer.upper), v.outer, c(v.outer.lower, v.outer.upper) );

	df = as.data.frame(rbind(df.inner,df.outer));
		colnames(df) = c("value","fence","direction");

	df$value = as.numeric(df$value);

	result$df = df;

    # indexes
	result$inner.lower = setdiff(inner.which.lower,outer.which.lower);
	result$inner.upper = setdiff(inner.which.upper,outer.which.upper);

	result$outer.lower = outer.which.lower;
	result$outer.upper = outer.which.upper;

	#list("df" = df, "inner" = c(i.lower,i.upper), "outer" = c(o.lower,o.upper) ); ;
	result;
	}




#' doMode
#'
#' Returns \code{mode} of a numeric vector x
#'
#' \code{mode} is the most frequent value(s) in a set of data
#'
#'
#' @family Stats
#'
#' @param x numeric vector
#'
#' @return numeric vector that contains _all_ values that are modal (could be bimodal)
#' @export
#'
#' @examples
#' doMode( c(1:9) );
#' doMode( c(1, 1:9, 9) );
#' doMode( c(1, 1:9, 9, 9) );
#'
doMode = function(x) # alias ?
	{
	whichMaxFreq(x);
	}



doMedian = function(x, type=1)
  {
  xx = stats::na.omit(x);
  as.numeric(stats::quantile(xx, prob=c(0.5), type=type));
  }

doMean = function(x)
  {
  xx = stats::na.omit(x);
  m = mean(xx);
  deviationFromMean = abs(xx-m);
  xx[ whichMin(deviationFromMean)[1] ];  # value from data
  }

#' doModeOpposite
#'
#' Returns \code{!mode} of a numeric vector x
#'
#' \code{!mode} is the least frequent value(s) in a set of data
#'
#' @family Stats
#'
#' @param x numeric vector
#'
#' @return numeric vector that contains _all_ values that are least modal
#' @export
#'
#' @examples
#' doModeOpposite( c(1:9) );
#' doModeOpposite( c(1, 1:9, 9) );
#' doModeOpposite( c(1, 1:9, 9, 9) );
#'
doModeOpposite = function(x)  # alias ?
	{
	whichMinFreq(x);
	}








