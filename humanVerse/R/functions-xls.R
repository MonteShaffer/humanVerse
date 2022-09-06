
# https://bert-toolkit.com/

xls.AVERAGE = function(x, na.rm=TRUE, show.warning=TRUE)
	{ 
	warning = stats.warningNA(x, show.warning=show.warning);
	mean(x, na.rm=na.rm);
	}
	
xls.SUM = function(x, na.rm=TRUE, show.warning=TRUE)
	{
	warning = stats.warningNA(x, show.warning=show.warning);
	sum(x, na.rm=na.rm);
	}
	
xls.COUNT = function(x, na.rm=TRUE, show.warning=TRUE)
	{
	warning = stats.warningNA(x, show.warning=show.warning);
	if(na.rm)
		{
		length(na.omit(x));
		} else { length(x); }
	}
	
xls.COUNTBLANK = function(x)
	{
	warning = stats.warningNA(x, show.warning=show.warning);
	n = length(x);
	n2 = length(na.omit(x));
	n - n2;
	}

xls.VAR.P = function(x, na.rm=TRUE, show.warning=TRUE)
	{
	warning = stats.warningNA(x, show.warning=show.warning);  # doubled
	n2 = xls.COUNT(x, na.rm=na.rm);
	var(x, na.rm=na.rm)*(n2-1)/n2;
	}
	
xls.VAR.S = function(x, na.rm=TRUE, show.warning=TRUE)
	{
	warning = stats.warningNA(x, show.warning=show.warning);
	var(x, na.rm=na.rm);
	}

xls.STDEV.P = function(x, na.rm=TRUE, show.warning=TRUE)
	{
	warning = stats.warningNA(x, show.warning=show.warning);
	sqrt( xls.VAR.P(x, na.rm=na.rm) );
	}
	
xls.STDEV.S = function(x, na.rm=TRUE, show.warning=TRUE)
	{
	warning = stats.warningNA(x, show.warning=show.warning);
	sqrt( xls.VAR.S(x, na.rm=na.rm) );
	}


xls.MIN = function(x, na.rm=TRUE, show.warning=TRUE)
	{
	warning = stats.warningNA(x, show.warning=show.warning);
	min(x, na.rm=na.rm);
	}

xls.MAX = function(x, na.rm=TRUE, show.warning=TRUE)
	{
	warning = stats.warningNA(x, show.warning=show.warning);
	max(x, na.rm=na.rm);
	}

xls.MEDIAN = function(x, na.rm=TRUE, show.warning=TRUE)
	{
	warning = stats.warningNA(x, show.warning=show.warning);
	median(x, na.rm=na.rm);  # looks like type=6 or type=7	
	}
	
	
xls.PERCENTILE.INC = function(x, prob=0.88, na.rm=TRUE, show.warning=TRUE)
	{
	warning = stats.warningNA(x, show.warning=show.warning);
	quantile(x, prob=prob, type=7, na.rm=na.rm);
	}
	
xls.PERCENTILE.EXC = function(x, prob=0.88, na.rm=TRUE, show.warning=TRUE)
	{
	warning = stats.warningNA(x, show.warning=show.warning);
	quantile(x, prob=prob, type=6, na.rm=na.rm);
	}
	
xls.QUARTILE.INC = function(x, q=1, na.rm=TRUE, show.warning=TRUE)
	{
	warning = stats.warningNA(x, show.warning=show.warning);
	if(!(q %in% 1:3)) { stop("q must be 1 (Q1), 2 (Q2), 3 (Q3)");}
	quantile(x, prob=0.25*q, type=7, na.rm=na.rm);
	}
	
xls.QUARTILE.EXC = function(x, q=1, na.rm=TRUE, show.warning=TRUE)
	{
	warning = stats.warningNA(x, show.warning=show.warning);
	if(!(q %in% 1:3)) { stop("q must be 1 (Q1), 2 (Q2), 3 (Q3)");}
	quantile(x, prob=0.25*q, type=6, na.rm=na.rm);
	}
	


xls.COMBINE = function(n, r)
	{
	n %nCr% r;	
	}

xls.PERMUT = function(n, r)
	{
	n %nPr% r;	
	}
	
xls.T.DIST = function(x, df, cdf=TRUE)
	{
	if(cdf) 	{ return(pt(x, df)); }  # cdf 
	if(!cdf) 	{ return(dt(x, df)); }  # pdf 	
	}
	
xls.T.DIST.2T = function(x, df)
	{
	2*pt(x, df, lower.tail=FALSE);	
	}
	
xls.T.DIST.RT = function(x, df)
	{
	pt(x, df, lower.tail=FALSE);	
	}
	
xls.T.INV = function(prob, df)
	{
	qt(prob, df);
	}
	
xls.T.INV.2T = function(prob, df)
	{
	qt(prob/2, df);
	}
	
	
xls.T.TEST = function(x, y, tails=1, type=1)
	{
	paired = FALSE; 	if(type == 1)  { paired = TRUE; }
	var.equal = TRUE; 	if(type == 3)  { var.equal = FALSE; }
	alternative = NULL;	if(tails == 2) { alternative = "two.sided"; } 
	# alternative ... a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less". You can specify just the initial letter
	
	res = t.test(x, y, alternative = alternative, paired = paired, var.equal = var.equal);
	res$p.value;
	}
	
## not directly fns in EXCEL but needed to do "HW"
xls.TCRIT.SAMPLE = function(x, mu=0, ...)
	{
	# t-critical from data ...  ( HO mu - mean(x) ) / ( sd.p(x) / sqrt(n))
	x.bar = xls.AVERAGE(x, ...);
	s.hat = xls.STDEV.S(x, ...);
	n     = xls.COUNT(x, ...);  # mu = 1 has test.stat of 4.1 
	xls.TCRIT2.SAMPLE(x.bar, mu, s.hat, n);
	## 2* (1-pt(t.crit, n-1)) ... to match t.test(df$x) output
	## xls.T.DIST.2T(t.crit, n-1) ... 
	}
	
xls.TCRIT2.SAMPLE = function(x.bar, mu=0, s.hat, n)
	{
	# t-value from data ...  ( HO mu - mean(x) ) / ( sd.p(x) / sqrt(n))
	(x.bar - mu) / (s.hat/sqrt(n));
	#  ( mean(df$x) - 1 ) / (sd(df$x)/sqrt(n))
	}
	
# df = structure(list(x = c(3, 4, 5, 8, 9, 1, 2, 4, 5), y = c(6, 19, 3, 2, 14, 4, 5, 17, 1)), row.names = c(NA, -9L), class = "data.frame")	
	
	
xls.SKEW = function(x, ...)
	{
	n = xls.COUNT(x, ...);
	x.bar = xls.AVERAGE(x, ...);
	x.dev = (x - x.bar);
	
	# See stats.summary(x) for more info about moments 
	m2 = sum(x.dev ^2)/n;
	m3 = sum(x.dev ^3)/n;
	
	# G1
	skew.g1 = m3 / ( (m2)^(3/2) );
	skew.G1 = skew.g1 * sqrt(n * (n-1)) / (n - 2) ;
	skew.b1 = skew.g1 * ((n-1)/n) ^ (3/2);

	skew.G1;
	}
	
	
	 
	
xls.KURT = function(x, ...)
	{
	n = xls.COUNT(x, ...);
	x.bar = xls.AVERAGE(x, ...);
	x.dev = (x - x.bar);
	
	# See stats.summary(x) for more info about moments 
	m2 = sum(x.dev ^2)/n;
	m4 = sum(x.dev ^4)/n;
	
	# G2 
	kurt.g2 = m4 / (m2)^2 - 3;
	kurt.G2 = ((n+1)*kurt.g2 + 6)*(n-1)/((n-2)*(n-3));
	kurt.b2 = (kurt.g2 + 3) * (1 - 1/n)^2 - 3;

	kurt.G2;
	}	


xls.AVEDEV = function(x, ...)
	{
	n = xls.COUNT(x, ...);
	x.bar = xls.AVERAGE(x, ...);
	x.dev = (x - x.bar);
	 
	xls.AVERAGE( xls.ABS(x.dev), ... );
	}
	
xls.ABS = function(x)
	{
	abs(x);
	}
	
	
xls.COVARIANCES.S = function(x, y, ...)
	{
	cov(x,y);
	}
	
xls.COVARIANCES.P = function(x, y, ...)
	{
	nx = xls.COUNT(x, ...);
	ny = xls.COUNT(y, ...);  # equal lengths N (nx == ny == N)
	cov.s = cov(x,y);
	cov.s*(nx-1)/nx;
	}
	
	
xls.CORREL = function(x, y, ...)
	{
	cor(x,y);
	}

xls.AVERAGEIF = function(data, group="Sex", group.order=NULL, include.cols=NULL)
	{
	# unnecessary on "==" comparison 
	# data[[group]] = as.factor(data[[group]]); 
	group.keys = unique(data[[group]]);		# Male, Female 
	# 'by' ... I tried ...
	
	data.keys = set.diff( colnames(data), group ); # all keys BUT group 
													# must be numeric 
	if(!is.null(include.cols))
		{
		idxs = v.match(include.cols, ncol(data), colnames(data));
		if(is.null(idxs)) { stop("error with include.cols"); }
		data.keys = colnames(data)[idxs];
		}
		
	if(!is.null(group.order))
		{
		# this will TRUNCATE and put in desired order based on INPUT
		idxs = v.match(group.order, length(group.keys), group.keys);
		if(is.null(idxs)) { stop("error with group.order"); }
		group.keys = group.keys[idxs];
		}
	
	
	ngroups = length(group.keys);
	ncolumns = length(data.keys);
	result = NULL;
	for(i in 1:ngroups)
		{
		row = NULL;
		data.sub = subset(data, data[[group]] == group.keys[i]);
		for(j in 1:ncolumns)
			{
			# this is in order of the INPUT 
			data.key = data.keys[j];
			cdata = data.sub[[data.key]];
			cdata = as.numeric(cdata);
			cmean = mean(cdata);
			row = c(row, cmean);			
			}
		result = rbind(result, row)
		}
		
	result = as.data.frame(result);
		# all.numerics, no problem ... 
		rownames(result) = group.keys;
		colnames(result) = data.keys;
	result; 
	}
 
 # pip( xls.AVERAGEIF(data,"Sex"), show.row.names=TRUE, number.format="Fixed: total.width=5");
 # fs # data = structure(list(Sex = structure(c(1L, 2L, 1L, 2L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 2L), levels = c("Female", "Male"), class = "factor"), Clothing = c(246, 171, 95, 125, 368, 148, 48, 147, 91, 324, 258, 79, 84, 48, 399, 126, 364, 306, 94, 315, 217, 14, 176, 351, 348, 14, 67, 335, 144), Health = c(185, 78, 15, 16, 100, 139, 74, 108, 46, 80, 142, 55, 146, 48, 174, 29, 69, 118, 12, 18, 168, 127, 24, 159, 113, 174, 140, 11, 90), Tech = c(64, 345, 47, 493, 82, 347, 108, 532, 86, 12, 92, 13, 522, 383, 94, 462, 40, 92, 12, 57, 91, 10, 81, 11, 26, 525, 579, 23, 560), Misc = c(75, 10, 90, 13, 109, 107, 176, 146, 182, 51, 119, 296, 184, 61, 25, 74, 208, 242, 54, 138, 67, 226, 123, 291, 204, 183, 72, 200, 149)), row.names = c(NA, -29L), class = "data.frame");

xls.UNIQUE = function(x)
	{
	unique(x);
	}
	
	

# do this like data conversion ... from and to 

# BIN2DEC
# BIN2HEX
# BIN2OCT

 

xls.POISSON.DIST = function(x, mean, cdf=TRUE)
	{
	# mean as lambda ... 
	if(cdf) 	{ return(ppois(x, mean)); }  # cdf 
	if(!cdf) 	{ return(dpois(x, mean)); }  # pdf 	
	}

xls.POISSON.INV = function(prob, mean)
	{
	# mean as lambda ... 
	qpois(prob, mean); 
	}
	

xls.BINOM.DIST = function(successes, trials, prob.success, cdf=TRUE)
	{
	if(cdf) 	{ return(pbinom(successes, trials, prob.success)); }  # cdf 
	if(!cdf) 	{ return(dbinom(successes, trials, prob.success)); }  # pdf 	
	}

xls.BINOM.INV = function(prob, trials, prob.success)
	{
	qbinom(prob, trials, prob.success); 
	}
	
	

xls.NORM.DIST = function(x, mean, sd, cdf=TRUE)
	{
	if(cdf) 	{ return(pnorm(x, mean, sd)); }  # cdf 
	if(!cdf) 	{ return(dnorm(x, mean, sd)); }  # pdf 	
	}
	
xls.NORM.S.DIST = function(z, cdf=TRUE)
	{
	if(cdf) 	{ return(pnorm(z, 0, 1)); }  # cdf 
	if(!cdf) 	{ return(dnorm(z, 0, 1)); }  # pdf 	
	}


xls.NORM.INV = function(prob, mean, sd)
	{
	qnorm(prob, mean, sd); 
	}
	
xls.NORM.S.INV = function(prob)
	{
	qnorm(prob, 0, 1); 
	}
	









# xls.COMBINE = function(n, r) {}
# xls.NORMDIST = xls.NORMINV = xls.POISSON = xls.BINOMIAL = xls.SUMPRODUCT
# POISSON/BINOMAL ... E[X], var[X] ... n, p 
# xls has "mean" not AVERAGE ... LOL
# P(A), P(A and B), P(A|B), P(A or B) ... parse/eval ... 
# BAYES's THEOREM in simple FORM ...
# P(X = x); 								=> 1
# P(X > x); P(X >= x); P(X <=x); P(X < x); 	=> 4
# P(a </= x </= b); P(a >/= x >/= b);		=> 8?
# p's and q's ... q = 1-p 		

