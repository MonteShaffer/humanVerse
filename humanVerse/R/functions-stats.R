
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' stats.summary
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
#' stats.summary( c(87, presidents[1:30], 87) );
#'
stats.summary = function(x, tukey=TRUE, type=1, ... )
	{
	# maybe build internal caching mechanism based on a digest of the obj
	# stats.summary = function(x, tukey=TRUE, type=1, signif.digits=options("digits")$digits, names = c("min", "-Q1-", "median", "Mean", "-Q3-", "max"), ... )
				signif.digits=5; names = c("min", "-Q1-", "median", "Mean", "-Q3-", "max");
	res = summary(x, digits=signif.digits, type=type...);
	names(res) = names;
	if(tukey) { res[c(1:3,5:6)]; } else { res; }
	}

#' @rdname tukeySummary
#' @export
tukeySummary = stats.summary;


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




