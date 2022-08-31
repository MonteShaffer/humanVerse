
xls.AVERAGE = function(x, na.rm=TRUE)
	{ 
	warning = stats.warningNA(x);
	mean(x, na.rm=na.rm);
	}
	
xls.SUM = function(x, na.rm=TRUE)
	{
	warning = stats.warningNA(x);
	sum(x, na.rm=na.rm);
	}
	
xls.COUNT = function(x, na.rm=TRUE)
	{
	warning = stats.warningNA(x);
	if(na.rm)
		{
		length(na.omit(x));
		} else { length(x); }
	}
	
xls.COUNTBLANK = function(x)
	{
	warning = stats.warningNA(x);
	n = length(x);
	n2 = length(na.omit(x));
	n - n2;
	}

xls.VAR.P = function(x, na.rm=TRUE)
	{
	warning = stats.warningNA(x);  # doubled
	n2 = xls.COUNT(x, na.rm=na.rm);
	var(x, na.rm=na.rm)*(n2-1)/n2;
	}
	
xls.VAR.S = function(x, na.rm=TRUE)
	{
	warning = stats.warningNA(x);
	var(x, na.rm=na.rm);
	}

xls.STDEV.P = function(x, na.rm=TRUE)
	{
	warning = stats.warningNA(x);
	sqrt( xls.VAR.P(x, na.rm=na.rm) );
	}
	
xls.STDEV.S = function(x, na.rm=TRUE)
	{
	warning = stats.warningNA(x);
	sqrt( xls.VAR.S(x, na.rm=na.rm) );
	}


xls.MIN = function(x, na.rm=TRUE)
	{
	warning = stats.warningNA(x);
	min(x, na.rm=na.rm);
	}

xls.MAX = function(x, na.rm=TRUE)
	{
	warning = stats.warningNA(x);
	max(x, na.rm=na.rm);
	}

xls.MEDIAN = function(x, na.rm=TRUE)
	{
	warning = stats.warningNA(x);
	median(x, na.rm=na.rm);  # looks like type=6 or type=7	
	}
	
	
xls.PERCENTILE.INC = function(x, prob=0.88, na.rm=TRUE)
	{
	warning = stats.warningNA(x);
	quantile(x, prob=prob, type=7, na.rm=na.rm);
	}
	
xls.PERCENTILE.EXC = function(x, prob=0.88, na.rm=TRUE)
	{
	warning = stats.warningNA(x);
	quantile(x, prob=prob, type=6, na.rm=na.rm);
	}
	
xls.QUARTILE.INC = function(x, q=1, na.rm=TRUE)
	{
	warning = stats.warningNA(x);
	if(!(q %in% 1:3)) { stop("q must be 1 (Q1), 2 (Q2), 3 (Q3)");}
	quantile(x, prob=0.25*q, type=7, na.rm=na.rm);
	}
	
xls.QUARTILE.EXC = function(x, q=1, na.rm=TRUE)
	{
	warning = stats.warningNA(x);
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

