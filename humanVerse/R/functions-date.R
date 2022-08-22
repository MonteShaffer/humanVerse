

date.calculateLeapDays = function() {}
date.calculateLeapDays = function(cyear, 
									ytype="gregorian",
									return = "logical"  # boolean
									)
	{
	# cyear is 1AD => 1, 1BC => 0, 2BC => -1
	ytyp = functions.cleanKey(ytype, 4);
	ret = functions.cleanKey(return, 3);
	# ytype="julian"
	if(ytyp == "juli")
		{
		i = (cyear %% 4 == 0); 			
		if(ret == "int") { i = as.integer(i); } # removes names 
		names(i) = cyear;
		return(i);
		}
		
	if(ytyp == "xela")
		{
		## stop("what, TODO::");
			# if(YEAR == 0) { return(0); } # RULE: YEAR ZERO HAS ZERO leap days 
			# sum( as.integer( YEAR / c(5, 25, 500, 5000) ) ); # TOTAL LEAP DAYS SINCE YEAR 0
		z = as.numeric(!(cyear == 0));  # put a zero at any cyear == 0
										# multiply away ... multivariate  
		
		# NON-ZERO YEAR HAS ... 
		h = as.numeric(cyear %% 5 == 0);
		i = as.numeric(cyear %% 25 == 0);	
		j = as.numeric(cyear %% 500 == 0);	
		k = as.numeric(cyear %% 5000 == 0);
		
		r = h + i + j + k;
		r = r * z;
		names(r) = cyear;
				
		if(ret != "int") { warning("Returning integers as BOOLEAN is nonsensical for Xelian"); }
		# remove year zeroes by pairwise multiplication.
		return( r );
		}
	
	
	# gregorian is default
	k = (cyear %% 400 == 0);		
	j = (cyear %% 100 == 0);		
	i = (cyear %% 4 == 0);	
		# multivariate, as a set 
		r = i - j + k;
		r = as.logical(r);
		if(ret == "int") { r = as.integer(r); }
		names(r) = cyear;
	r;
	}


#' @rdname is.leapYear
#' @export
is.leapYear = date.calculateLeapDays;



# defaults from memory? OR independent of memory?  INTENTIONALLY independent ...
date.convertString = function(str,	from.format = "%Y-%m-%d %H:%M:%S",
									from.tz = "UTC", from.numeric=TRUE,
									
									to.format = "", to.numeric=FALSE,
									to.tz = "UTC", to.print.tz = FALSE
							)
	{
	# strptime as STRING POSIX TIME
	res = strptime(strvec, format=from.format, tz=to.print.tz);
	if(to.format == "") 
		{ 
		if(from.numeric) { res = as.numeric(res); } 
		return(res); 
		}
	# strftime as STRING FORMAT TIME 
	res = strftime(res, format=to.format, tz=to.tz, usetz=print.tz);
	if(to.numeric) { res = as.numeric(res); } 
	return(res);
	}
								
	
	# install.packages("swirl")
	# https://cran.r-project.org/web/packages/swirl/index.html
# %j ... day of year
# %U	Week    01-53    with Sunday as first day of the week	22, 27
# %W	Week 00-53 with Monday as first day of the week
# %w	Weekday 0-6 Sunday is 0	
# %u	Weekday 1-7 Monday is 1

# mydate = as.POSIXlt('2005-4-19 7:01:00')
# names(mydate);
# ISOdate(2005,10,21,18,47,22,tz="PDT")

date.showDateCodes = function()
	{
	# cache in memory as dataframe
	# https://css-tricks.com/snippets/php/php-date-parameters/
	# use URL / parse to build ... TODO ... 
	# formatter = POSIX, PHP, MYSQL ... mapped ... 
	# https://www.r-bloggers.com/2020/04/a-comprehensive-introduction-to-handling-date-time-in-r/
	# guess-formats ... not TIMELIB sophisticated
	# https://pubs.opengroup.org/onlinepubs/9699919799/
	# https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/time.h.html
	# https://pubs.opengroup.org/onlinepubs/9699919799/functions/strptime.html 
	## strptime has the CODES ... 
	# https://publications.opengroup.org/catalogsearch/result/?q=POSIX
	
	}	
	


#  strftime('%Y-%m-%d', time, 'unixepoch', 'localtime')

date.getNow = function(format="MySQL", out.tz="")
	{
	now = Sys.time();	
	}

date.now = date.getNow

	
	
	
	