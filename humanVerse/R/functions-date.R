



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

# date.time = function(datePOSIX) 
# date.




#  date.calculateLeapDays(100*15:18, "julian", "integer");
#  date.calculateLeapDays(100*15:18);

date.calculateLeapDays = function() {}
date.calculateLeapDays = function(cyear, 
									ctype="gregorian",
									return = "logical"  # boolean
									)
	{
	# cyear is 1AD => 1, 1BC => 0, 2BC => -1
	ctyp = functions.cleanKey(ctype, 4);
	ret = functions.cleanKey(return, 3);
	# ctype="julian"
	if(ctyp == "juli")
		{
		i = (cyear %%4 == 0); 			
		if(ret == "int") { i = as.integer(i); } # removes names 
		names(i) = cyear;
		return(i);
		}
	if(ctyp == "xela")
		{
		stop("what, TODO::");
		}
	
	
	# gregorian is default
	k = (cyear %%400 == 0);		
	j = (cyear %%100 == 0);		
	i = (cyear %%4 == 0);	
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



# date.getTimeZoneOffset(OlsonNames()[123])
date.getTimeZoneOffset = function(tz = "UTC")
	{
	info = stringi::stri_timezone_info(tz);
	info$RawOffset;
	}
	

date.setOrigin = function(origin)
	{
	memory.init();
	memory.set("tz.origin", "DATE", origin);
	invisible(origin);
	}
	
date.getOrigin = function()
	{
	memory.init();
	x = memory.get("tz.origin", "DATE");
	if(is.null(x))
		{
		# x = structure(0, class = c("POSIXt", "POSIXct") ); # list and compact
		x = structure(0, class = "POSIXct" ); # compact only
		memory.set("tz.origin", "DATE", x);
		}
	x;
	}
	
date.init = function()
	{
	memory.init();
	
	# ?OlsonNames ... interesting
	# As from R 3.5.0, when a time zone location is first found in a session, its value is cached in object .sys.timezone in the base environment.
	# https://data.iana.org/time-zones/tz-link.html#tzdb
	# https://time.is/ [your clock is 0.6 seconds behind ... latency?]
	
	memory.set("tz.names", 	"DATE", OlsonNames() );  # doesn't have map to offsets?  stringi
	memory.set("tz.out", 	"DATE", "UTC");
		s.tz = Sys.timezone();
	memory.set("tz.in", "DATE", s.tz);
	memory.set("tz.local", "DATE", s.tz);
	memory.set("tz.local", "DATE", date.getTimeZoneOffset(s.tz) );
	# 1970 EPOCH 
	memory.set("tz.origin", "DATE", structure(0, class = c("POSIXt", "POSIXct") ) );
	
		i.tz = stringi::stri_timezone_info();
	memory.set("tzi.local", "DATE", i.tz);
	memory.set("tzi.local.offset", "DATE", i.tz$RawOffset);
	
	# x = stri_timezone_list(); y = OlsonNames(); stopifnot(identical(x,y));
	# stri_timezone_list(offset=5.5)
	# stri_timezone_list(region='US', offset=-10)
	
		cat("\n", " DATE INIT:  unless otherwise specified, all dates ",
					" will be outputed to [UTC] time.  Your local timezone is: ",
					Sys.timezone(), " \n\n\t ", "You can change these default ",
					" paramaters any time with `memory.set` and access them ",
					" with `memory.get`.  Please look inside this function ",
					" for more details. ", "\n");
	
	}
	

date.getVar = function(key)
	{
	ke = functions.cleanKey(key, 2, keep=".");
	if(key = "l.t")  # local.tz
		{
		
		}
	
	}



# ISO 8601 date format
# https://en.wikipedia.org/wiki/Proleptic_Gregorian_calendar
# https://en.wikipedia.org/wiki/Gregorian_calendar
# Meeus # https://articles.adsabs.harvard.edu/pdf/1992JBAA..102...40M
# Frank 

# G = 365.2425
# J = 365.25
# X = 365.2422 ?
# The mean tropical year is 365.24219 days long
date.getDaysInTropicalYear = function()
	{
	# Meeus shows the fluctuations ... this is the mean 
	days = 365.24219;
	days;
	}


## everything to "UTC" than back to your format 
## sometimes UTC is tz.in, sometimes tz.out 

date.getTimeZone = function(which.tz="in")
	{
	wt = functions.cleanKey(which.tz, 2);
	# if from INIT, we have other parameters, use them 
	
	## [lo]cal ... 
	
	# CURRENTLY
	if(wt == "ou") { return("UTC"); }
	Sys.timezone();
	}
		
date.checkTimeZone = function() {} 		
date.checkTimeZone = function(tz)
	{
	tz %in% OlsonNames();
	}
	
	
date.convertFromUTC = function(datePOSIX.UTC, out.tz="local")
	{
	if(out.tz == "local") { out.tz = 
	if(!is.null(out.tz)) 
		{ 
		res = 	as.POSIXct(res, tz = out.tz, origin=origin); 
		}
	
	}


date.searchForTimeZone = function(tzw = "*Cairo*")
	{	
	memory.init();
	timezones = memory.get("timezones");
	res = regex.wildcardSearch(timezones, tzw);
	if(length(res) < 1) { return(NULL); }
	
	out = timezones[res];
	names(out) = res;
	out;
	}


date.checkPOSIXct = function(datePOSIX, in.tz, origin, out.tz)
	{
	# recasting, or actually check elementwise?
	# if(multivariate) ... could do which but doesn't seem to work on a partial vector ... 
	res = datePOSIX;
	if(!is.POSIXct(datePOSIX[1]))
		{	
		res = as.POSIXct(datePOSIX, tz = in.tz,  origin=origin);
		}
	if(!is.null(out.tz)) 
		{ 
		res = 	as.POSIXct(res, tz = out.tz, origin=origin); 
		}
		
	res;
	}
	
	
		
	
# args = list(.dot.keys. = c("in.tz","out.tz", "origin"), .dot.vals. = list("monte", "natalya", "alex"));
	
date.defaults = function(dots)
	{
	# cat("\n\n ==== DEFAULTS ===== \n\n");
	# dput(dots);  
	# cat("\n\n ==== DEFAULTS ===== \n\n");
	
	
	in.tz = if("in.tz" %in% dots) { dots$in.tz; } else { date.getTimeZone("in"); }
	out.tz = if("out.tz" %in% dots) { dots$out.tz; } else { date.getTimeZone("out"); }
	origin = if("origin" %in% dots) { dots$origin; } else { date.getOrigin(); }
	
	pf = parent.frame(1);
	assign("in.tz", in.tz, envir=pf);
	assign("out.tz", out.tz, envir=pf);
	assign("origin", origin, envir=pf);
	}
				
### TODO ... date.defaults() ... list extract ... set like str.MD5
					
date.toUnix = function(datePOSIX = date.now(), ...)
	{
	dots = functions.getParameterInfo("dots");
	# date.defaults will select dots info, fall back to a default ... 
	date.defaults(dots); # assign in date.defaults return here ... 
	
	res = date.checkPOSIXct(datePOSIX, in.tz, origin, out.tz);
	as.numeric(res);
	}
	
# https://www.neonscience.org/resources/learning-hub/tutorials/dc-convert-date-time-posix-r
# eg.lt = as.POSIXlt("2015-01-19 10:15"); str(eg.lt); str(unclass(eg.lt));
# eg.ct = as.POSIXct("2015-01-19 10:15"); str(eg.ct); str(unclass(eg.ct));	
date.fromUnix = function(unixNumeric, ..., return="lt")
	{
	args = functions.getParameterInfo();  # why is this not directly built in ??? JS 1995
	# date.defaults will choose the default, or value from dots ... 
	date.defaults(args$dots); # assign in date.defaults return here ... 
	
	re = functions.cleanKey(return, 2);  # "lt" or "ct"
	
	if(re == "ct") 
		{ 
		res = as.POSIXlt(unixNumeric, tz=in.tz, origin=origin);
		# rollback to in.tz / out.tz [REVERSE?]
		return(res);
		}
	
	res = as.POSIXct(numvec, tz=in.tz, origin=origin);
	
	if(which != "ct")
		{
		as.POSIXlt(numvec, origin=origin);
		} else	{
				as.POSIXct(numvec, origin=origin);
				}
	}
	
	
	
	

## accessors 

date.get = function() {}	
date.get = function(datePOSIX = date.now(), 
							in.tz  = Sys.timezone(), 
							origin = date.getOrigin(),
							out.tz = NULL,

						use.numeric = FALSE,
						format = "%Y-%m-%d", 
						formatter="R")
	{
	res = date.checkPOSIXct(datePOSIX, in.tz, origin, out.tz);
	res = format(res, format = format);
	if(use.numeric) { res = as.numeric(res); }
	res;
	}



					  
# YYYYMMDDHHMMSS
date.getYMDHMS = function() {}
date.getYMDHMS = function(datePOSIX = date.now(), 
							in.tz  = Sys.timezone(), 
							origin = date.getOrigin(),
							out.tz = NULL,

						use.numeric = FALSE,
						format = "%Y%m%d%H%M%S",
						formatter="R")
	{
	res = date.checkPOSIXct(datePOSIX, in.tz, origin, out.tz);
	res = format(res, format = format);
	if(use.numeric) { res = as.numeric(res); }
	res;
	}

date.getYear = function() {}	
date.getYear = function(datePOSIX = date.now(), 
							in.tz  = Sys.timezone(), 
							origin = date.getOrigin(),
							out.tz = NULL,

						use.numeric = TRUE,
						format = "%Y",
						formatter="R")
	{
	res = date.checkPOSIXct(datePOSIX, in.tz, origin, out.tz);
	res = format(res, format = format);
	if(use.numeric) { res = as.numeric(res); }
	res;
	}





	
date.getMonth = function(datePOSIX = date.now(), 
							in.tz  = Sys.timezone(), 
							origin = date.getOrigin(),
							out.tz = NULL,

						use.numeric = TRUE,
						format = "%m",
						formatter="R")
	{
	res = date.checkPOSIXct(datePOSIX, in.tz, origin, out.tz);
	res = format(res, format = format);
	if(use.numeric) { res = as.numeric(res); }
	res;
	}
	
date.getDay = function(datePOSIX = date.now(), 
							in.tz  = Sys.timezone(), 
							origin = date.getOrigin(),
							out.tz = NULL,

						use.numeric = TRUE,
						format = "%d",
						formatter="R")
	{
	res = date.checkPOSIXct(datePOSIX, in.tz, origin, out.tz);
	res = format(res, format = format);
	if(use.numeric) { res = as.numeric(res); }
	res;
	}
	

# date.getDaysInTropicalYear()

# http://www.webexhibits.org/calendars/calendar-christian.html
# modified Julian day number (MJD)
# MJD 0 thus started on 17 Nov 1858 (Gregorian) at 00:00:00 UTC.
# The Lilian day number is similar to the Julian day number, except that Lilian day number 1 started at midnight on the first day of the Gregorian calendar, that is, 15 October 1582.
## different OFFSETS ...

# https://articles.adsabs.harvard.edu/pdf/1992JBAA..102...40M
# refers to EPOCH 1900 for NASA?
## Frank ... https://www.mreclipse.com/Special/quotes2.html
## SOLAR eclipses, history

# this builds a propleptic calendar 
date.daysFromEpoch = function() {}
date.daysFromEpoch = function(datePOSIX, 
									epoch.key="G",	# [G]regorian, [J]ulian, [M]odified Julian, [L]ilian,  [X]elian 
									epoch.start, 	# 1572, 46 BC (when was 4 cycle), 3/21/1973
									epoch.offset	# 1752, ???, -1980
							)	
	{
	# https://www.sciencedirect.com/topics/engineering/julian-day-numbe
	# The Julian day number is the number of days since noon UT on January 1, 4713 BCE
	# Notice that there is no year zero in the Julian or Gregorian calendars. The day that precedes January 1, 1 A.D. is December 31, 1 B.C. If you use the formula above to determine a Julian day number of a B.C. date, you must convert to negative year numbers, as, for instance, 10 B.C. which must be entered as -9
	# http://www.webexhibits.org/calendars/week.html
	# http://www.webexhibits.org/calendars/calendar-christian.html
	
	# from.calendar="[G]"  to.days=[J]
	# Try this one (the divisions are integer divisions, in which remainders are discarded):

	month = date.getMonth(datePOSIX)

	a = as.integer( (14-month)/12 );
	y = year+4800-a
	m = month + 12*a - 3
	
	

	
	
	
	}








#In Roman times, the hour was defined as 1/12 of the time period between sunrise and sunset. Since this interval varies with seasons, the “hour” was longer in the summer than in the winter.

#At the latitude of Rome, about N, 1 hour would last anywhere between 44.7 modern minutes (in the end of December) and 75.3 (in mid-June). 





#' getDate
#'
#' Wrapper for 'format' function
#'
#' @param how See options using ?as.Date [?strptime for codes]
#' @param when You can pass in a time, or let it grab current time
#'
#' @return date string
#' @export
#'
#' @examples
#' getDate();
#' getDate("%Y-%m-%d");
#' getDate("%Y-%m-%d %H:%M:%S");
#' getDate("%Y-%V");  # week ISO 8601
#'
#' getDate("%Y-%m-%d", strptime("23mar1973", "%d%b%Y") );
# get.date
# date.get

getDate = function(how="%Y-%m", when = Sys.time())
	{
  # php date syntax is a bit different
  # doesn't allow for microtime
  # this needs to be updated to a modern version of the ISO standard (R::base)
	format(when, how);
	}





#' convertDateStringToFormat
#'
#' basically wraps strptime and strftime into this single call
#'
#' I have this vector of dates in string format;
#' I want to convert it to this format (numeric),
#' and currently, they are of this format.
#'
#' @family Date-Time
#'
#' @param strvec one or more strings, such as \code{"3/24/2010 18:33"}
#' @param to how you want to return the date \code{"\%Y"} is just Year
#' @param to.name name(s) given to the \code{to} column(s)
#' @param from format of input, default \code{"\%Y-\%m-\%d \%H:\%M:\%S"}
#' @param num if TRUE (default), will return numeric form.
#'
#' @return dataframe same length as strvec, with one or more columns
#' @export
#' @examples
#'
#' date.strings = c("3/24/2010 18:33", "9/3/2009 17:28", "10/14/2009 11:40",
#'                  "7/3/2015 11:16", "11/18/2010 1:29", "4/23/2011 0:08",
#'                  "10/6/2010 11:13", "7/26/2009 13:23","4/9/2008 13:40",
#'                  "8/20/2008 11:32");
#'
#' years = convertDateStringToFormat(date.strings,
#'                              "%Y", "years",
#'                                                            "%m/%d/%Y %H:%M");
#'
#' weeks = convertDateStringToFormat(date.strings,
#'                              "%W","weeks",
#'                                                             "%m/%d/%Y %H:%M");
#'
#' days = convertDateStringToFormat(date.strings,"
#'                               %j", "days",
#'                                                            "%m/%d/%Y %H:%M");
#'
#' ywd = convertDateStringToFormat( date.strings,
#'                              c("%Y","%W","%j"), c("year","week","day"),
#'                                                            "%m/%d/%Y %H:%M");
#'
#'
#' Ymd = convertDateStringToFormat(date.strings,
#'                               "%Y-%m-%d","ISO8601",
#'                                                  "%m/%d/%Y %H:%M",num=FALSE);
#'
convertDateStringToFormat = function (strvec,to="%Y",to.name="years",from="%Y-%m-%d %H:%M:%S",num=TRUE)
	{
  nt = length(to);
  result = NULL;
  for(i in 1:nt)
    {
	  p.obj = strptime(strvec, format=from );
	  o.obj = strftime(p.obj,  format=to[i] );
	  n.obj = if(num) { as.numeric(o.obj); } else { o.obj; }
	  result = cbind(result, n.obj);
    }
    colnames(result) = to.name;
	as.data.frame(result);
	}


	
	