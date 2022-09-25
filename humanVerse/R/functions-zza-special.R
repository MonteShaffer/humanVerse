

# 30  %deg%. 
# pi  %rad%.  # dot means default... or you could put pi %rad% "G"
# 100 %gon%. 


	
"%deg%" = function(deg, to="R") 
	{ 
	# default is to="R"
	# traps the single dot (.) as option 
	if(!check.type(to)) { to="R"; } 
	if(is.na(to) || is.null(to)) { to = "R"; }
	angle.convert(deg, from="D", to=to); 
	}

"%rad%" = function(rad, to="D") 
	{ 
	# default is to="D"
	# traps the single dot (.) as option 
	if(!check.type(to)) { to="D"; } 
	if(is.na(to) || is.null(to)) { to = "D"; }
	angle.convert(rad, from="R", to=to); 
	}	
	

"%gon%" = function(gon, to="D") 
	{ 
	# default is to="D"
	# traps the single dot (.) as option 
	if(!check.type(to)) { to="D"; } 
	if(is.na(to) || is.null(to)) { to = "D"; }
	angle.convert(gon, from="G", to=to); 
	}



# 4 nCr 2 ... choose vs lchoose ?
# ?utils::combn  ?choose 
# library(combinat); library(gtools);
# choose(4, 2) ... with replacement 
# make %nCr% and %nPr% functions ... 
# https://davetang.org/muse/2013/09/09/combinations-and-permutations-in-r/
# https://www.calculatorsoup.com/calculators/discretemathematics/permutationsreplacement.php
"%nCr%" = function(n, r, replace=FALSE) 
	{ 
	# same function (FALSE, with n+r-1)
	if(replace) { return( nCr( (n+r-1), r, replace=FALSE ) ); } 
	factorial(n) / ( factorial(r) * factorial(n-r) ); 
	}
	

"%!%" = function(n, r=NULL) { factorial(n); }

"%nPr%" = function(n, r, replace=FALSE) 
	{ 
	if(replace) { return( n^r ); }
	factorial(n) / factorial(n-r); 
	}
	
	
"%tall%" = function(ft,cm) 
	{ 
	if(!check.type(ft)) { return( height.cm2ft(cm) ); }
	if(!check.type(cm)) { return( height.ft2cm(cm) ); }
	} 
	

"%^T%" 	= function(m,r=NULL) { matrix.transpose(m); } 
"%^-1%" = function(m,r=NULL) { matrix.inverse(m); } 
 
"%rand%" = function(seed = NULL, n)
	{
	# TODO write checkType ...   . %rand% 5
	# 							seed.create() %rand% 5
	rand(n=n, seed=seed);	
	}
	
"%DRG% = function(r=NULL, mode) { trig.setMode(mode); }


# takes num/den 
"%frac%" = function(num, den, expand=TRUE) 
			{ 
			if(!expand) { return (num/den); }
			nn = length(num);
			nd = length(den);			
			# normal recycling
			if(nn == 1 || nd == 1) { return (num/den); }
			
			# 0:10 %frac% 1:100
			# this is expand == TRUE
			# I want 0...10 / 1:100 ... all of them ...
			# 0:10/1 THEN 0:10/2 THEN 0:10/3 ...
			res = NULL;
			for(i in 1:nd)					# could have done nn 
				{
				res = c(res, num / den[i]); # could have done num[i]
				}
			res;
			}


"%SCI%" = function(x, r=NULL) { num.toScientific(x); }
"%NAT%" = function(x, r=NULL) { num.toNatural(x); }
"%FIX%" = function(x, r=NULL) { num.toFixed(x); }
"%ENG%" = function(x, r=NULL) { num.toEngineering(x); }



# leading . will work
"%$$%" = function(r="HI", ...) 
			{ 
			str = str.fromObjectName(...);
			access(str);			
			}
			
#### THIS is *SPECIAL FUNCTION* ... (PARANTHESES) is optional			
# .%$$% "dcf$Depends@dependencies" ;
# .%$$% dcf$Depends@dependencies ;			
# .%$$%("dcf$Depends@dependencies");
# .%$$%(dcf$Depends@dependencies);				
			
			
`$$`   = function(...) 
			{ 
			str = str.fromObjectName(...);
			access(str);			
			}

### THIS IS FUNCTION, MUST HAVE (PARANTHESES)
# `$$`("dcf$Depends@dependencies");
# `$$`(dcf$Depends@dependencies);





