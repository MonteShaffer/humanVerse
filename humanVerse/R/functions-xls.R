
xls.AVERAGE = function(x, na.rm=TRUE)
	{
	mean(x, na.rm=na.rm);
	}
	
xls.SUM = function(x, na.rm=TRUE)
	{
	sum(x, na.rm=na.rm);
	}
	
xls.COUNT = function(x, na.rm=TRUE)
	{
	if(na.rm)
		{
		length(na.omit(x));
		} else { length(x); }
	}
	
xls.COUNTBLANK = function(x)
	{
	n = length(x);
	n2 = length(na.omit(x));
	n - n2;
	}

xls.VAR.P = function(x, na.rm=TRUE)
	{
	n2 = xls.COUNT(x, na.rm=na.rm);
	var(x, na.rm=na.rm)*(n2-1)/n2;
	}
	
xls.VAR.S = function(x, na.rm=TRUE)
	{
	var(x, na.rm=na.rm);
	}

xls.STDEV.P = function(x, na.rm=TRUE)
	{
	sqrt( xls.VAR.P(x, na.rm=na.rm) );
	}
	
xls.STDEV.S = function(x, na.rm=TRUE)
	{
	sqrt( xls.VAR.S(x, na.rm=na.rm) );
	}


xls.MIN = function(x, na.rm=TRUE)
	{
	min(x, na.rm=na.rm);
	}

xls.MAX = function(x, na.rm=TRUE)
	{
	max(x, na.rm=na.rm);
	}

xls.MEDIAN = function(x, na.rm=TRUE)
	{
	median(x, na.rm=na.rm);  # looks like type=6 or type=7	
	}
	
	
xls.PERCENTILE.INC = function(x, prob=0.88, na.rm=TRUE)
	{
	quantile(x, prob=prob, type=7, na.rm=na.rm);
	}
	
xls.PERCENTILE.EXC = function(x, prob=0.88, na.rm=TRUE)
	{
	quantile(x, prob=prob, type=6, na.rm=na.rm);
	}
	
xls.QUARTILE.INC = function(x, q=1, na.rm=TRUE)
	{
	if(!(q %in% 1:3)) { stop("q must be 1 (Q1), 2 (Q2), 3 (Q3)");}
	quantile(x, prob=0.25*q, type=7, na.rm=na.rm);
	}
	
xls.QUARTILE.EXC = function(x, q=1, na.rm=TRUE)
	{
	if(!(q %in% 1:3)) { stop("q must be 1 (Q1), 2 (Q2), 3 (Q3)");}
	quantile(x, prob=0.25*q, type=6, na.rm=na.rm);
	}
	

