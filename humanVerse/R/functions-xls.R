
xls.pasteFrom = function()
	{
	df = read.delim("clipboard");
	str(df);
	minvisible(df);
	}
	
xls.copyTo = function(df, row.names=FALSE, col.names=TRUE, ...)
	{
	write.table(df, "clipboard", sep="\t",
					row.names=row.names,
					col.names=col.names,
					...
				);
	}





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



xls.groupBy = function(df, 
								group="Sex", group.order = NULL, 
								custom.cols = NULL
						)
	{
	# returns a list of sub.df's 
	xls.prepGroup(df, group=group, group.order=group.order, custom.cols=custom.cols);
	
	## subset by group.keys 
	ngroups = length(group.keys);
	## truncate cols by data.keys 
	ncolumns = length(data.keys);
	res = vector("list",ngroups);
	for(i in 1:ngroups)
		{
		row = NULL;
		df.sub = subset(df, df[[group]] == group.keys[i]);
		df.col = df.sub[, data.keys];
		res[[i]] = df.col;
		}
	names(res) = group.keys;
	minvisible(res);
	}


xls.prepGroup = function() {}
xls.prepGroup = function(df, 
								group="Sex", group.order = NULL, 
								custom.cols = NULL,
								envir = parent.env(environment())
						)
	{
	# unnecessary on "==" comparison 
	# df[[group]] = as.factor(df[[group]]); 
	group.keys = unique(df[[group]]);		# Male, Female 
	# 'by' ... I tried ...
	
	# by default OTHER cols are everything but the group ...
	data.keys = set.diff( colnames(df), group ); # all keys BUT group 
													# must be numeric 
	if(!is.null(custom.cols))
		{
		idxs = v.match(custom.cols, ncol(df), colnames(df));
		if(is.null(idxs)) { stop("error with custom.cols"); }
		data.keys = colnames(df)[idxs];
		}
	assign("data.keys", data.keys, envir=envir);

	
	if(!is.null(group.order))
		{
		# this will TRUNCATE and put in desired order based on INPUT
		idxs = v.match(group.order, length(group.keys), group.keys);
		if(is.null(idxs)) { stop("error with group.order"); }
		group.keys = group.keys[idxs];
		}
	assign("group.keys", group.keys, envir=envir);
	# could return list, etc, but I will assign back to the envir 
	}


xls.AVERAGEIF = function() {}
xls.AVERAGEIF = function(df, 
								group="Sex", group.order = NULL, 
								custom.cols = NULL
							)
	{  
	xls.prepGroup(df, group=group, group.order=group.order, custom.cols=custom.cols);
	
	ngroups = length(group.keys);
	ncolumns = length(data.keys);
	result = NULL;
	for(i in 1:ngroups)
		{
		row = NULL;
		df.sub = subset(df, df[[group]] == group.keys[i]);
		for(j in 1:ncolumns)
			{
			# this is in order of the INPUT 
			data.key = data.keys[j];
			vec = df.sub[[data.key]]; 		# vec of data key (column)
			vec = as.numeric(vec);
			vec.mean = mean(vec);  
			row = c(row, vec.mean);			# AVERAGEIF ... 
			}
		result = rbind(result, row)
		}
		
	result = as.data.frame(result);
		# all.numerics, no problem ... 
		rownames(result) = group.keys;
		colnames(result) = data.keys;
	# print(result); 
	minvisible(result);
	}
 
 # res = xls.AVERAGEIF(data,"Sex"); res;
 # res = xls.AVERAGEIF(data,"Sex",group.order=c(2,1)); res;
 # res = xls.AVERAGEIF(data,"Sex",group.order=c("Male","Female")); res; 
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
	
# P(x ≤ 12)
# P(x > 11)
# P(x ≥ 10)
# P(9 ≤ x ≤ 15)
# P(x = 10)

test = c("12 < x", "x <= 12", "x > 11", "x >= 10", "9 <= x <= 15", "x = 10")

ggg.barplot = function(X)
	{
	# barplot of EVERYTHING with OPACITY 
	# par(new = TRUE)
	# barplot of test RANGE highlighted ...
		bnames = property.get("x", X);
		nb = length(bnames);
	
	# make 20% higher for PROBS to be printed ... 
	# rotate at angle of 30/60/90 degrees, up to the right rotation 
		xoffset = 2;
		xlim = c(1-xoffset, nb+xoffset);  xdiff = diff(ylim);
		

		# we want y range expanded to allow 20% for LEGEND in TOP
	ylim = c(min(X), max(X));
	  ydiff = diff(ylim); yadd = 1.20 * ydiff;
	ylim.new = c(ylim[1], ylim[1] + yadd);
	
	
		pnames = num.toFixed(as.vector(X), total.width=6);
	# https://stackoverflow.com/questions/10286473/rotating-x-axis-labels-in-r-for-barplot  # las=2, ROTATE labels 
	# https://stackoverflow.com/questions/12481430/how-to-display-the-frequency-at-the-top-of-each-factor-in-a-barplot-in-r
	
	# make angle of function of xdiff/ydiff ?
	angle = 90;
	xx = barplot( as.vector(X), names.arg = bnames, xlim=xlim, ylim=ylim.new );
	
	text(x=xx,y=as.numeric(pnames)+0.10*ydiff, pnames, srt=angle)
	
	
	}


oper.which = function(str)
	{
	kname = NULL;
	if(str.contains("<=", str)) 
		{ 
		key = "<=";
		kname = "LEQ";		
		}
	if(is.null(kname) && str.contains("<", str)) 
		{ 
		key = "<";
		kname = "L";		
		}
	if(is.null(kname) && str.contains(">=", str)) 
		{ 
		key = ">=";
		kname = "GEQ";		
		}
	if(is.null(kname) && str.contains(">", str)) 
		{ 
		key = ">";
		kname = "G";		
		}
	if(is.null(kname) && str.contains("=", str)) 
		{ 
		key = "=";
		kname = "EQ";		
		}
		
	r = as.numeric(str.trim(str.implode("", str.explode(key, str))));
	if(is.null(kname)) { return(NULL); }
	list("key" = key, "keyname" = kname, "remaining" = r);
	}


#

 

# by.idx or by.value of the VEC 
# return final.idx or vec[final.idx] 
v.smart = function(vec, test = " x <= 12 ", varname="x", 
							by="value", return = "vector"
					)
	{
	b = prep.arg(by, n = 1); # COMPARISON of "values" or "indexes"
	vecIDX = 1:length(vec);
	vecT = vec; if(b == "i") { vecT = vecIDX; }
	
	## parse EQUALITY as generic function???
	## searching for up to 3 things ... x can be in different places?
	## x <= 12 ... 12 < x ... 12 < x < 22 [for 3, x has to be in middle]
	# looks like READING, R-L parser ...
		# force univariate 
	one = str.trim(str.explode(varname, str.trim(test[1]) ));  
	
	lower = NULL; upper = NULL; lower.equal = FALSE; upper.equal = FALSE;
	lower.is.equal = upper.is.equal = FALSE;
	n.one = length(one); 
	o.lower = o.upper = NULL;
	if(n.one == 1) 
		{ 
		# OPER then variable 
		f = c("OPER", "VAR");
		o.lower = oper.which(one[1]);
			lower = o.lower$remaining;
			lower.equal = str.contains("=",o.lower$key);
			lower.is.equal = (o.lower$key == "=");
		}  
	if(n.one == 2)
		{
		if(one[1] == "") 
			{ 
			o.upper = oper.which(one[2]);
				upper = o.lower$remaining;
				upper.equal = str.contains("=",o.upper$key);
				upper.is.equal = (o.upper$key == "=");
			f = c("VAR", "OPER"); 
			} else {
					o.lower = oper.which(one[1]);
						lower = o.lower$remaining;
						lower.equal = str.contains("=",o.lower$key);
						lower.is.equal = (o.lower$key == "=");
					f = c("OPER", "VAR", "OPER");
					o.upper = oper.which(one[2]);
						upper = o.upper$remaining;
						upper.equal = str.contains("=",o.upper$key);
						upper.is.equal = (o.upper$key == "=");
					}
		}
	# parse to  lower LSIGN x USIGN upper ... possible NULL 
	final.idx = NULL;
	if(lower.is.equal && upper.is.equal) { stop("what ... two operators both can't be equal ... 3 = x = 5 ... nonsensical"); }
	# v.which if EQUAL 
	# v.between if LE, L, GE, GE 
	if(lower.is.equal)
		{
		final.idx = v.which(vecT, what=lower);		
		}		
	if(!is.null(final.idx) && upper.is.equal)
		{
		final.idx = v.which(vecT, what=upper);		
		}		
	if(!is.null(final.idx))
		{
		final.idx = v.between(vec, lower=lower, upper=upper, lower.equal=lower.equal, upper.equal=upper.equal, by=by, return="indexes");		
		}	
	
	
	if(is.null(final.idx)) { return(NULL); }
	if(r == "i") { return(final.idx); }
	
	# indexes are based on lower/upper on VALUES/INDEXES
	# independently, we return VECTOR, not elements below ...
	vec = vec[final.idx];  # truncate 
	vec;
	}


xls.binom.test = function(trials, prob.success, test=" x <= 12")
	{
	X = xls.binom.build(trials, prob.success); 
	ggg.barplot(X); 
	
	
	


v.between = function(vec, lower, upper, 
							lower.equal = TRUE,
							upper.equal = TRUE,
							return = "vector",
					sort = FALSE, ...)
	{
	
	
	
	prob.range;	
	}
	
	
xls.binom.build = function(trials, prob.success)
	{
	n = trials;
	p = prob.success;
	q = (1-p);
	
	# https://en.wikipedia.org/wiki/Binomial_distribution
	E.X = n*p;
	var.X = n*p*q;
	skew.X = (q-p) / (sqrt(n*p*q));
	kurt.X = (1-6*p*q) / (n*p*q);
	
	# build all x probabilities ...
	res = numeric(n);
	for(x in 1:n)
		{
		res[x] = xls.BINOM.DIST(x, trials, prob.success, FALSE);
		}
		
	names(res) = 1:n; # names can get removed easily 	
	res = property.set("x", res, 1:n);
	res = property.set("-info-", res, list(	"E[X]" = E.X,
											"var[X]" = var.X,
											"skew[X]" = skew.X,
											"kurt[X]" = kurt.X
											)
						);
	minvisible(res);	
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
# xls has "mean" not AVERAGE withing DIST functions ... LOL
# P(A), P(A and B), P(A|B), P(A or B) ... parse/eval ... 
# BAYES's THEOREM in simple FORM ...
# P(X = x); 								=> 1
# P(X > x); P(X >= x); P(X <=x); P(X < x); 	=> 4
# P(a </= x </= b); P(a >/= x >/= b);		=> 8?
# p's and q's ... q = 1-p 		

 
# df = structure(list(x = c(246L, 171L, 95L, 125L, 368L, 148L, 48L, 147L, 91L, 324L, 258L, 79L, 84L, 48L, 399L, 126L, 364L, 306L, 94L, 315L, 217L, 14L, 176L, 351L, 348L, 14L, 67L, 335L, 144L, 140L, 313L), y = c(185L, 78L, 15L, 16L, 100L, 139L, 74L, 108L, 46L, 80L, 142L, 55L, 146L, 48L, 174L, 29L, 69L, 118L, 12L, 18L, 168L, 127L, 24L, 159L, 113L, 174L, 140L, 11L, 90L, 91L, 53L)), class = "data.frame", row.names = c(NA, -31L)); list.extract(df);  functions.stepInto(xls.TRENDLINE);

# Lotus - V    	# http://www.mit.edu/~mbarker/formula1/f1help/01-intr8.htm
# Cairn (WP)	# http://www.columbia.edu/~em36/wpdos/WP6XCHMN.pdf
# jquery features, not one huge application like timycc or fckedit ... leaning toward commercial too much ...
# could I do debian/windows looks like it ... install gitlabs(FREE) and have internal version control on the one computer/laptop ...
# jquery build nice table ALOHA is deprecated ...
# ability to copy/paste to a BLANK space of a webpage and it builds a mini-excel like TABLE ... it creates a TEXTAREA where your mouse is when you click paste ... how to know if ACTIVE window vs HOVER window?
# send only small data to/from the WEB APP ... why waste life on 'pip' when it is so 1970's ... good for ZEN garden, but let's keep things modular 
# I need a HIGHCHARTS.STATS.js library eventually.  If I build out the mockups, will they implement?
# objects can move around the PAGE canvas, be "detached or undocked"
# objects start simple but can become more advanced as needed ...
# simple XLS look like datatables alternative .... headers go above the A, B, C at the top ... customize style with "under the hood reference" ... 
# sort/filter, subset ... make simple and intuitive ... 
# it only works with small portions, sends AJAX requests to perform OPERATION on the data, create the objects (e.g., subset), and return what is needed to display (partial rows and cols) ... 
# salmon (steak or filet)
# sphinxsearch REPLACE searcheverything to index things ...
# build fuzzy search with TIES or whatever (tree)
# build index of files, filesystem 
# build index of R files and other elements 







# df = structure(list(x = 0:10, y = c(3L, 4L, 11L, 24L, 43L, 68L, 100L, 136L, 179L, 228L, 283L)), class = "data.frame", row.names = c(NA, -11L)); list.extract(df);  functions.stepInto(xls.TRENDLINE);

# can't live on same line with ";" ... weird stepInto behavior ... no LHS?
## WEIRD ???
#   # old STEP INTO ???

# Exponential, Linear, Logarithmic, Polynomial-2, Power, MovingAverage-2
# Intercept = 0, Show Equation, Display R^2
# Name Linear(y) 

# https://chem.libretexts.org/Courses/BethuneCookman_University/B-CU%3A_CH-345_Quantitative_Analysis/Book%3A_Analytical_Chemistry_2.1_(Harvey)/05%3A_Standardizing_Analytical_Methods/5.06%3A_Using_Excel_and_R_for_a_Linear_Regression
# =slope(A1:A10, B1:B10)
# =intercept(...)


xls.TRENDLINE = function(x, y, type="Linear", set.intercept=FALSE, intercept.val=0, order.num = 2, digits=5)
	{
	# https://www.statology.org/power-regression-in-r/
	# https://www.statology.org/power-regression-in-excel/
	## https://www.statology.org/polynomial-regression-excel/
	## https://www.statology.org/polynomial-regression-r/
	## # https://stackoverflow.com/a/7333292/184614
		
	TYPE = prep.arg(type, n=3, case="upper");
dput(TYPE);
	if(TYPE == "MOV") { warning.cat("MOVING AVERAGE changes the dataframe, reducing it in size.  It is an important function and R has many ways to deal with lags() and rolling averages (or medians) in other functions.  Sorry, for now, it will not be implemented here.  As I believe it doesn't have an R^2 or FIT formula in EXCEL.  It can be developed, just not right now.  Don't see the benefit."); stop("Have a nice day!");}
	
	if(!set.intercept) { INT = 1; } else { INT = 0; }
	if(set.intercept && (TYPE=="POW" || TYPE == "EXP") )
		{
		INT = 1;
		intercept.val = 0; # so it doesn't change the Y - values 
		warning.cat("\n FOR the type: [",type,"] you can't set the intersept to a fixed value.  Resetting to default 'set.intercept=FALSE' and proceeding. \n");
		}
		
	# if(any == 0 in certain TYPES ... warning (data for y has 0 ln ... x has ln ) ... POLITE directon on what is wrong 
	
	df = data.frame(x, y);
	n = nrow(df);
	
	if(TYPE == "POL")
		{
		pol = c("y ~ ");
		for(i in order.num:2)
			{
			pol = c(pol, paste0("{c}I(x^", i, ") + ") );
			}
		pol = c(pol, "{c}x + {INT}");
		}

	model.str = switch(TYPE,
						"EXP" = "log(y) ~ {c}x + {INT}",
						"LIN" = "y ~ {c}x + {INT}",
						"LOG" = "y ~ {c}log(x) + {INT}",
						"POL" = paste0(pol, collapse=""),
						"POW" = "log(y) ~ {c}log(x) + {INT}",
						"MOV" = paste0("y ~ ",INT," + x"),
					"y ~ {x} + {INT}"  # DEFAULT is linear
					)
					
	if( ( TYPE == "EXP" || TYPE == "POW") && any(df$y<=0) )
		{
		cat.warning("\n\n\t\t\t For TRENDLINE type [",type,"] you cannot have any data (currently [",n,"] rows) \n\t\t\t\t in [y] that is <= 0 (less than or equal to zero) ... \n\t\t\t\t REMOVING ROWS and trying to COMPUTE \n\n");
		df = subset(df, y > 0);
		n = nrow(df);
		cat.warning("\n\n\t\t\t\t Proceeding with only [",n,"] rows! \n\n");
		}
	if( ( TYPE == "LOG" || TYPE == "POW") && any(df$x<=0) )
		{
		cat.warning("\n\n\t\t\t For TRENDLINE type [",type,"] you cannot have any data (currently [",n,"] rows) \n\t\t\t\t in [x] that is <= 0 (less than or equal to zero) ... \n\t\t\t\t REMOVING ROWS and trying to COMPUTE \n\n");
		df = subset(df, x > 0);
		n = nrow(df);
		cat.warning("\n\n\t\t\t\t Proceeding with only [",n,"] rows! \n\n");
		}
dput(model.str);	
	model.f = str.replace(c("{INT}", "{c}"), c(INT, ""), model.str);
		
	model = eval(parse(text = model.f));  # now a language ...
	
		ndf = df; if(set.intercept) { ndf$y = df$y - intercept.val; }
	m.fit = lm(model, data=ndf);
	m.fitsum = summary(m.fit);
	m.cor = cor(ndf$x,ndf$y);  # cor of input (x,y) is just linear ... ? alternative to R^2 is sample correlation, how to address in POLY or exp ... cor(ln(x),y) ... etc. ... TODO to think about this as 
dput(m.cor);	
	m.R2 = round( m.fitsum$r.squared, digits);
	m.aR2 = round( m.fitsum$adj.r.squared, digits);
	m.F = as.numeric(m.fitsum$fstatistic);
	m.pvalue = 1-pf(m.F[1], m.F[2], m.F[3]);
	m.stars = stats.stars(m.pvalue);
print(m.fitsum);
# R2 is not the same on set.intercept OPTIONS in LINEAR ... coefficients are 

	# getAnywhere("print.lm")
	coef = round( as.numeric(m.fit$coefficients), digits);
	coef.pvalues = as.numeric(m.fitsum$coefficients[, 4]);
	coef.stars = stats.stars(coef.pvalues);
	
	rcoef = coef;  # remaining coefficients ... 
	mINT = INT; 
		if(set.intercept) { mINT = intercept.val; } # don't put in MODEL, just shift it ... 
		if(!set.intercept) { mINT = coef[1]; rcoef = coef[-c(1)]; }
		
	# this is default ... 
	fn.str = str.replace(	c("{INT}", 				"{c}"), 
							c( round(mINT,digits), 	paste0(round(rcoef[1],digits),"*")), 
						model.str
						); 
	if(TYPE == "EXP")
		{
		# "EXP" = "log(y) ~ {c}x + {INT}",
		# "EXP" = "y ~ exp({c}x) + exp({INT})",
		fn.str = paste0( c("y ~ ", round(exp(mINT),digits), "*exp(", round(rcoef[1],digits), "*x)"), collapse="");
		}
		
		
	if(TYPE == "POW")
		{
		# "POW" = exp(y) = exp(3.6) + 1.86(x)
		# POW = y = 3.6*x ^ 1.86
		fn.str = paste0( c("y ~ ", round(exp(mINT),digits), "*x^(", round(rcoef[1],digits), ")"), collapse="");
		}
		
	if(TYPE == "POL")
		{
		pcoef = c(rcoef, mINT); # descending order ... 
		pol.len = length(pol);
		polc = pol; # copy 
		for(i in 2:pol.len)
			{
			polc[i] = str.replace("{c}", paste0(round(pcoef[i-1],digits),"*"), polc[i]);
			}
		polc = str.replace(c("I(",")"), "", polc); 
		polc = str.replace("{INT}", round(mINT,digits), polc);  # THE PARSER THINKS this is on the SAME LINE AS BELOW (BASED ON ERROR) ...
		fn.str = paste0( polc , collapse="");
		}
dput(fn.str);
						
	fn = function.fromString(str.replace("y ~", "", fn.str), x);  # need bogus data for x to be symbol
	xlim = c(min(x), max(x));

		# we want y range expanded to allow 20% for LEGEND in TOP
	ylim = c(min(y), max(y));  ydiff = diff(ylim); yadd = 1.20 * ydiff;
	ylim.new = c(ylim[1], ylim[1] + yadd);
		

	# dxi ... # 100 determines smoothness of 'model' as fn.curve
	xfn = seq(xlim[1], xlim[2], length.out=100);  
	
par.saveState();

# http://rfunction.com/archives/1302	
# mar – A numeric vector of length 4, which sets the margin sizes in the following order: bottom, left, top, and right. The default is c(5.1, 4.1, 4.1, 2.1).  y,x ... y, x .... not x,y ... x,y 

	# keystrokes ... par("mar" = c(2, 2, 0, 0)); 
	# getter/setter
	# I treat it like a "property" [with memory] not a function ...
	par.set("mar", c(2, 2, 0, 0));
	{
	plot(x, 	y, 		xlim = xlim, ylim = ylim.new, 
			axes = FALSE, frame.plot = FALSE
		);
	axis(1); # x 
	axis(2); # y  
	par(new=TRUE);
	plot(xfn, fn(xfn), 	xlim = xlim, ylim = ylim.new, 
			axes = FALSE, frame.plot = FALSE,	
			col="darkgreen", lwd = 2, 
			type="l", xlab = "", ylab = "", main = ""
		);
	}
	
	
	yline = (ylim.new[2] - ylim[2])/3;  # 3 lines
	
	## abline(intercept, coef(fit))
	# text 
	# align stars below "y" for MODEL.fit, INT and x for PARAM.fit 
	# show R^2 ...  m.R2 
	# https://lukemiller.org/index.php/2012/10/adding-p-values-and-r-squared-values-to-a-plot-using-expression/
	mylabel = bquote(italic(R)^2 == .(format(m.R2, digits = digits)));
	text(x = xlim[1], y = ylim[2] + 3*yline, labels = mylabel, pos=4);
	
	# show fitted EQU ... fn.str 
	# this formatter puts exact "R spacing"
	fn.str.f = strlang.RFormat(fn.str);
	text(x = xlim[1], y = ylim[2] + 2*yline, labels = fn.str.f, pos=4);
		
	# show stars ... coef.stars; m.stars
	stars = paste0( c(m.stars, coef.stars), collapse=" ");
	text(x = xlim[1], y = ylim[2] + 1*yline, labels = stars, pos=4);
	# if(set.intercept), STARS needs to put FIXED 

	par(new=FALSE);

	
par.restoreState();	
	
	# what to return?
	# a df or list of key elements of the analysis ... maybe a 'updated' object that mirrors how EXCEL now shows a regression.
	# a way to have the list or data frame be prepped so I call
	# a fn `excel.paste`(obj) ... go to EXCEL and it is tab delimited and ready to be added to the SPREADSHEET manually.
	}

	
	
	# "There is a war a-brewing"... the civil war has been going on for a long time.  Time to kill the posers, but be wary of the curmundgeons.  Not an ally per se, have to proceed with the idea of a potential FORK from R to V.
	# 3 in the CORE engine:  FORTRAN -> C -> R ... solid
	# C++ for install/compile has led to strong ties to Debian/Rcpp ... solid
	# what is missing is the API level (humanVerse) to allow for extensiblity ... mostly written in R, some HVcpp and maybe HVc if necessary.
	# what is missing is an application level (kill the beast first).  Modern, extensible with design rules.  FEATURES should play where they belong:  js is client side, DAMP (debian, apache, mysql, php) is server side.  NO BLOAT.  Think elegeance and tick-tocks ... standalone libraries as classes, no autovendor bull shit ...
	# RhV as notebooks. [SANDBOX] for development and [FORMAL] for writeup.
	# allow inline WYSIWYG with shortcuts that generate what is needed as "SOURCE CODE a la WORDPERFECT" ... if the user types ```dfslkjkj``` under the hood it stores <code multiline>fadslkj</code> but the user sees what they type.
	# the user wants 3 colums to display content, they have to type a shortcut or find a button to click on to start that mode (CNTR-T)
	# smart help ... I want to ... and it shows only the things they need ...
	# ONE FORMAT to rule them all <HTML> with custom codes?  <HV fg> or just <fg></fg>
	# conversion ... everyting to/from HTML reduces dependencies ... allow for document type formatting in HTML interface (pagination, shrink to fit) ...
	# allows component previews in other outputs ... this BLOCK I want to see as Latex ... not worry about full build, just quick and easy PREVIEW 
	# markdown is inline text that gets translated to system codes.  Basic markdown with #HEADER *bold* or _whatever_ ... SYSTEM / BLOCK feature to enable or disable ... 
	# ```{r params}``` This is reasonable for code, but, ...
	# <code multiline>
	#    lang = r 
	#        would enable param, options drop downs 
	# with possibility to APPLY this BLOCK parameter results to other BLOCKS in section or entire document be able to save the keys into memory ... use \section as HTML section <section>
	# https://www.geeksforgeeks.org/naming-convention-in-c/ # C++
	# https://www.php.net/manual/en/functions.user-defined.php
	# Function names follow the same rules as other labels in PHP. A valid function name starts with a letter or underscore, followed by any number of letters, numbers, or underscores. As a regular expression, it would be expressed thus: ^[a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]*$.
	
	 

	
# function(...)
# # I could do group by eventually 
	# dots			= match.call(expand.dots = FALSE)$... ;
	# form_ls			= rep(list(bquote()), length(dots));
	# names(form_ls)	= as.character(dots);
	
# dput(form_ls);
 
# df = pdf;
# slist = xls.groupBy(df, "Personality", group.order=c("Analyst","Diplomat","Explorer","Sentinel"), custom.cols="Salary" );
# sdf = df.fromList(slist);
# sample from unique lists to build a ANOVA frame?
# adf = sdf[1:1000, ];

xls.ANOVA.SIMPLE = function(df, alpha=0.05, posthoc="THSD")
	{
	# data is already grouped ... AS-IS based on excel's current functions
	
	# output TABLE in LINES ... 
	TABLE = c("Anova: Single Factor", "", "SUMMARY");
	TABLE = c(TABLE, paste0("   Groups   ","\t", " Count ", "\t", " Sum ", "\t", " Average ", "\t", " Variance ", "\t", " [SSD] ") );
	
	groups = names(df);
	ngroups = length(groups);  # dim(df)[2];
	ndata = dim(df)[1];
	res = vector("list", ngroups);
	within.SSD = 0;
	for(i in 1:ngroups)
		{ 
		group = groups[i];
		data = df[[group]];
			Count 	 = xls.COUNT(data);
			Sum 	 = xls.SUM(data);
			Average  = xls.AVERAGE(data);
			Variance = xls.VAR.S(data);
			SSD 	 = xls.DEVSQ(data);
			
		TABLE = c(TABLE, paste0("   ",group,"   ","\t", Count, "\t", Sum, "\t", Average , "\t",  Variance , "\t", SSD ) );
		
		res[[group]] = list(	"Count" 	= Count,
								"Sum"   	= Sum,
								"Average"	= Average,
								"Variance"	= Variance,
								"SSD" 		= SSD
								);	
								
		within.SSD = within.SSD + SSD;
		}
		
	TABLE = c(TABLE, "", "", "ANOVA");
	TABLE = c(TABLE, paste0(" Source of Variation","\t", " SS[D] ", "\t", " df ", "\t", " MS ", "\t", " F.stat ", "\t", " P-value ", "\t", " F.crit ") );
	
	
	# SSD of entire data ...	
	all = as.numeric(as.matrix(df));
	
	total.SSD 	= xls.DEVSQ(all);
	total.N 	= xls.COUNT(all);
	total.df 	= total.N - 1;
	total.MS 	= total.SSD / total.df;
	
	# within 
	# within.SSD we already have ... from loop above 
	between.SSD = total.SSD - within.SSD;
	between.N 	= ngroups;
	between.df 	= between.N - 1;
	between.MS 	= between.SSD / between.df;
	
	within.df 	= total.df - between.df;
	within.MS 	= within.SSD / within.df;
	
	F.crit 		= xls.FINV.RT(alpha, between.df, within.df);
	F.stat  	= between.MS / within.MS;
	pval 		= xls.FDIST.RT(F.stat, between.df, within.df);
	
	TABLE = c(TABLE, paste0("Between Groups","\t", between.SSD, "\t", between.df, "\t", between.MS, "\t", F.stat, "\t", pval, "\t", F.crit) );
	TABLE = c(TABLE, paste0("Within Groups","\t", within.SSD, "\t", within.df, "\t", within.MS) );
	TABLE = c(TABLE, "");
	
	TABLE = c(TABLE, paste0("Total","\t", total.SSD, "\t", total.df, "\t", total.MS) );
	
	# HO: groups are the same ... mu_1 = mu_2 = ... mu_gn
	reject = (p <= alpha);  # reject = (F.stat <= F.crit);
	
	# MAYBE DO a RA.FISCHER.plot ... farmland, means in the centers, with posthoc differences ... 
	
	msg = "[Fail to Reject H0]: There is not enough evidence to conclude that the group means are different from one another ... ";
	if(reject) { msg = "[Reject H0]:  There is sufficient evidence at alpha=0.05 to reject the null hypothesis and conclude that *AT LEAST* one group mean is different from the others ... "; }
	
	# POSTHOC Bonferroni ... Tukey HSD ... 
	 
	}
	 
	
xls.F.INV = function(prob=1-0.05, df1, df2)
	{
	
	qf(prob, df1, df2);
	}
	
xls.F.INV.RT = function(alpha=0.05, df1, df2)
	{
	# # alpha = 0.05; df1 = 2; df2 = 27;
	xls.F.INV( 1-alpha, df1, df2);
	}
	
	
xls.F.DIST = function(x, df1, df2, cdf=TRUE)
	{
	if(cdf) 	{ return(pf(x, df1, df2)); }  # cdf 
	if(!cdf) 	{ return(df(x, df1, df2)); }  # pdf 	
	}
	

xls.F.DIST.RT = function(x, df1, df2)
	{
	# x = 2.5; df1 = 2; df2 = 27;
	pf(x, df1, df2, lower.tail=FALSE);	
	}
	
	
xls.SUMSQ = function(x)
	{
	xls.SUM(x^2);
	}
	
xls.DEVSQ = function(x)
	{
	dev = (x - xls.AVERAGE(x));
	xls.SUM(dev^2);
	}
	
	
### 1066 	