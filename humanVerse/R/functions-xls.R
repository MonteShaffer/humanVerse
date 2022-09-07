
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


# df = structure(list(x = c(246L, 171L, 95L, 125L, 368L, 148L, 48L, 147L, 91L, 324L, 258L, 79L, 84L, 48L, 399L, 126L, 364L, 306L, 94L, 315L, 217L, 14L, 176L, 351L, 348L, 14L, 67L, 335L, 144L, 140L, 313L), y = c(185L, 78L, 15L, 16L, 100L, 139L, 74L, 108L, 46L, 80L, 142L, 55L, 146L, 48L, 174L, 29L, 69L, 118L, 12L, 18L, 168L, 127L, 24L, 159L, 113L, 174L, 140L, 11L, 90L, 91L, 53L)), class = "data.frame", row.names = c(NA, -31L)); list.extract(df);  functions.stepInto(xls.TRENDLINE);



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


xls.TRENDLINE = function(x, y, method="Exp", set.intercept=FALSE, intercept.val=0, no = 2)
	{
	df = data.frame(x, y);
	METHOD = prep.arg(method, n=3, case="upper");
	if(!set.intercept) { INT = 1; } else { INT = 0; }
	# https://stackoverflow.com/a/7333292/184614
	
	# ?lm lm.D90 <- lm(weight ~ group - 1) # omitting intercept ??? 0 ?
	# if intercept.val ... I could do it with forced to ORIGIN then just add/subtract ? 
	
	# https://answers.microsoft.com/en-us/msoffice/forum/all/excel-formula-for-logarithmic-and-polynomial/54d75ca2-a598-4483-bca5-775325c333f3
	# y = a*ln(x) + b 
						# 
						# "EXP" = "y ~ exp({c}x) + exp({INT})",
	
	model.str = switch(METHOD,
						"EXP" = "log(y) ~ {c}x + {INT}",
						"LIN" = "y ~ {c}x + {INT}",
						"LOG" = "y ~ {c}log(x) + {INT}",
						"POL" = paste0("y ~ ",INT," + x"),
						"POW" = paste0("y ~ ",INT," + x"),
						"MOV" = paste0("y ~ ",INT," + x"),
					"y ~ {x} + {INT}"  # DEFAULT is linear
					)
	
	model.f = str.replace(c("{INT}", "{c}"), c(INT, ""), model.str);
		
	model = eval(parse(text = model.f));  # now a language ...
		ndf = df; if(set.intercept) { ndf$y = df$y - intercept.val; }
		# if (LN) ndf$y = log(y)
	m.fit = lm(model, data=ndf);
	m.fitsum = summary(m.fit);
	
	m.R2 = round( m.fitsum$r.squared, 5);
	m.F = as.numeric(m.fitsum$fstatistic);
	m.pvalue = 1-pf(m.F[1], m.F[2], m.F[3]);
	m.stars = stats.stars(m.pvalue);

	# getAnywhere("print.lm")
	coef = round( as.numeric(m.fit$coefficients), 3);
	coef.pvalues = as.numeric(m.fitsum$coefficients[, 4]);
	coef.stars = stats.stars(coef.pvalues);
	
	# plot data ... easy
	# plot fn as line with PARAM estimates 
	# if INT is forced to, REPLACE WITH INT, not coef[1]
	rcoef = coef;  # remaining coefficients ... 
	mINT = INT; 
		if(set.intercept) { mINT = intercept.val; } # don't put in MODEL, just shift it ... 
		if(!set.intercept) { mINT = coef[1]; rcoef = coef[-c(1)]; }
		
	# this is default ... 
	fn.str = str.replace(	c("{INT}", 	"{c}"), 
							c( mINT, 	paste0(rcoef[1],"*")), 
						model.str
						); 
	if(METHOD == "EXP")
		{
		# "EXP" = "log(y) ~ {c}x + {INT}",
		# "EXP" = "y ~ exp({c}x) + exp({INT})",
		fn.str = paste0( c("y ~ ", round(exp(mINT),3), "*exp(", rcoef[1], "*x)"), collapse="");
		}
						
	fn = function.fromString(str.replace("y ~", "", fn.str), x);  # need bogus data for x to be symbol
	xlim = c(min(x), max(x));
		# we want y range expanded to allow 20% for LEGEND in TOP
	ylim = c(min(y), max(y));  ydiff = diff(ylim); yadd = 1.20 * ydiff;
	ylim.new = c(ylim[1], ylim[1] + yadd);
		
		 
	
	# dxi ... # 100 determines smoothness of curve
	xfn = seq(xlim[1], xlim[2], length.out=100);  
	
par.saveState();

# http://rfunction.com/archives/1302	
# mar – A numeric vector of length 4, which sets the margin sizes in the following order: bottom, left, top, and right. The default is c(5.1, 4.1, 4.1, 2.1).  y,x ... y, x .... not x,y ... x,y 
	
	
	
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
	mylabel = bquote(italic(R)^2 == .(format(m.R2, digits = 3)));
	text(x = xlim[1], y = ylim[2] + 3*yline, labels = mylabel, pos=4);
	
	# show fitted EQU ... fn.str 
	fn.str.f = strlang.RFormat(fn.str);
	text(x = xlim[1], y = ylim[2] + 2*yline, labels = fn.str.f, pos=4);
		
	# show stars ... coef.stars; m.stars
	stars = paste0( c(m.stars, coef.stars), collapse=" ");
	text(x = xlim[1], y = ylim[2] + 1*yline, labels = stars, pos=4);
	# if(set.intercept), STARS needs to put FIXED 

	par(new=FALSE);

	
par.restoreState();	
	
	}

	