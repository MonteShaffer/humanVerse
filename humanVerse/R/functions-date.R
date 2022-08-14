


# DEFAULT anchors to JULIAN CALENDAR DATE: ... proleptic
#				August 5, 1600 (Tuesday) ==> RUTHVEN EPOCH 
# Historical Date Found in Documents (saying Tuesday)
# cdoy (Current Day of Year) ... important for first year calculations 
# 		## FROM https://www.timeanddate.com/date/weekday.html
# Above link also has Aug 5, 1600 as a Tuesday 
# date.generateProleptic(999); date.generateProleptic(999, "BACKWARD");
date.generateProleptic = function() {}
date.generateProleptic = function(n, dir="FORWARD", 
									path = getwd(),
									filename = "RUTHVEN_{n}_{dir}.txt",
									ctype="julian", 
									cyear = 1600,
									cmonth = 8,   # August 
									cday = 5,
									str.cday = "Tue",
									cdoy = 218 
								)
	{	
	ctyp = functions.cleanKey(ctype, 1);
	# Jan, Feb, Mar, ...
	MONTHS_ = format(ISOdate(2000, 1:12, 1), "%b");
	LENS_ = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
	# Mon, Tue, Wed, ...
	DAYS_ = format(ISOdate(2000, 1, 1:10), "%a")[1:7];

	
	# current idx of str.cday (name in week)
	idx.cday = which(DAYS_ == str.cday);
	# current leap year day (0 or 1)... extensible to allow others
	cleapdays = date.calculateLeapDays(cyear, ctype, "integer");
	LENS_[2] = 28 + cleapdays; # 28 or 29 .. 
	# current length of a month
	clen = LENS_[cmonth];
	
	
	DIRE = toupper(functions.cleanKey(dir, 4));
	dir = "FORWARD"; if(DIRE == "BACK") { dir = "BACKWARD"; }
		filename = str.replace( "{n}", n, filename );
		filename = str.replace( "{dir}", dir, filename );
		filename = paste0(path, "/", filename);  # is trailing slash required, will it break?
	## HEADER 		
	row = c("IDX", "YYYY", "MM", "DD", "DOW", "DOY");
	cat( paste0(row, collapse="|"), "\n", sep="", 
			file=filename, append=FALSE);
			
	if(DIRE == "FORW") # FORWARD in TIME, ASCENDING
		{
		if(is.negative(n)) { n = -1* n; }  # n is POSITIVE
		i = 0;
		while(i < n)
			{
			# write current row 
			row = c(i, cyear, cmonth, cday, str.cday, cdoy);
			cat( paste0(row, collapse="|"), "\n", sep="", 
					file=filename, append=TRUE);
					
			# increment	
			i = 1+i;
			cdoy = 1 + cdoy;
			if(i %% 365 == 0) 
				{ 
				cat("\n =====   ", cyear, " ===== \n"); 
				flush.console(); 
				}
				
			cday = 1 + cday;
			if(cday > clen) 
				{ 
				# NEW MONTH 
				cday = 1; 
				cmonth = 1 + cmonth;
				if(cmonth > 12)
					{
					# NEW YEAR 
					cmonth = 1;
					cyear = 1 + cyear;
					cdoy = 1;
					cleapdays = date.calculateLeapDays(cyear, ctype, "integer");
					LENS_[2] = 28 + cleapdays; # 28 or 29 .. 
					}
				clen = LENS_[cmonth];
				}
			# day name of week [DOW] on continuous 7-day loop	
			idx.cday = 1 + idx.cday;
			if(idx.cday > 7) { idx.cday = 1;}
			str.cday = DAYS_[idx.cday];
			}		
		}
		
	if(DIRE == "BACK") # BACKWARD in TIME, DESCENDING
		{
		if(is.positive(n)) { n = -1* n; }  # n is NEGATIVE
		i = 0;
		while(i > n)
			{
			row = c(i, cyear, cmonth, cday, str.cday, cdoy);
			cat( paste0(row, collapse="|"), "\n", sep="", 
					file=filename, append=TRUE);
			
			# DECREMENT 
			i = i-1;
			cdoy = cdoy-1;
			if(i %% 365 == 0) 
				{ 
				cat("\n =====   ", cyear, " ===== \n"); 
				flush.console(); 
				}
				
			cday = cday - 1;
			if(cday < 1) 
				{ 
				# NEW MONTH 
				cmonth = cmonth - 1;
				if(cmonth < 1)
					{
					# NEW YEAR 
					cmonth = 12;
					cyear = cyear - 1;
					cleapdays = date.calculateLeapDays(cyear, ctype, "integer");
					LENS_[2] = 28 + cleapdays; # 28 or 29 .. 
					cdoy = sum(LENS_);
					}				
				clen = LENS_[cmonth];
				cday = clen;  # last day of given month 
				}
			# if(cdoy < 1) { cdoy = sum(LENS_); }  ## why again?

			idx.cday = idx.cday - 1;
			if(idx.cday < 1) { idx.cday = 7;}
			str.cday = DAYS_[idx.cday];
			}		
		}

		cat("\n RESULTS are STORED HERE: \n\n\t", filename, "\n\n");
	invisible(filename);
	}




































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
	


		#  -587094
		# Ruthven offset ...  -4237058 
		# Julian ... +2305675 FROM Ruthven ... -1931383
		# MOD ... Julian - 2400000.5 ...  -4331383.5
		# LILIAN 
		# CUSTOM ... 
# https://www.slideshare.net/chenshuo/datetime-julian-date SLIDE 8

# why is mod this date, BRITAIN was a different date 
# date.toJulianDayNumber(1858, 11, 17-12, "modified"); 	# GREG 
# date.toJulianDayNumber(1600, 8, 5, "ruthven");
# date.toJulianDayNumber(-4712, 1, 1, "julian");		# anchor to special alignment of 3 cycles
# date.toJulianDayNumber(1582, 10, 15-12, "lilian"); 		# GREG
# date.toJulianDayNumber(-7, 3, 21, "equ");


date.computeOffset = function(NUM, dir=-1, 
								offset.for="julian", 
								offset=NULL
								)
	{
	off = functions.cleanKey(offset.for, 3);
	NUM = as.numeric(NUM);
	# dir == -1 ... this is toJulianDayNumber
	# dir == 1 ... this is fromJulianDayNumber
	
	# do offset logic here?
	if(off == "rut")
		{
		NUM = NUM + dir*4237058;
		return(NUM);
		}
	if(off == "equ")
		{
		NUM = NUM + dir*3649964;  # EQUINOX, MARCH 21, 8BC [-7, 03, 21]
		return(NUM);
		}
	if(off == "jul")
		{
		NUM = NUM + dir*1931383;
		return(NUM);
		}
	if(off == "mod")
		{
		# MJD 0 thus started on 17 Nov 1858 (Gregorian) at 00:00:00 UTC.
		NUM = NUM + dir*4331383;
		if(dir == -1) { NUM = NUM - 0.5; } # don't do 1/2 on return 
		return(NUM);
		}	
	if(off == "lil")
		{
		# Lilian day number 1 started at midnight 15 October 1582 (Gregorian).
		NUM = NUM + dir*4230541;
		if(dir == -1) { NUM = NUM - 0.5; } # don't do 1/2 on return 
		return(NUM);
		}
	if(off == "cus")
		{
		if(is.null(offset)) { offset = dir*1931383; }
		NUM = NUM + as.numeric(offset);
		return(NUM);
		}
		
	warning("invalid offset.for, returning NUM for offset = 0");
	NUM;	
	}
	
	
	
date.toJulianProlepticNumber = function(jyear, jmonth=1, jday=1, 
										offset.for="julian",
										offset = NULL
										)
	{	
	# if jyear is dataframe or list, decompose 
	
	jyear = as.integer(jyear);
	jmonth = as.integer(jmonth);
	jday = as.integer(jday);
	
	IN_THE_BEGINNING = 10000 	# e.g., ~10,000 BC ... 
								# year = 1 => 1 AD/CE
								# year = 0 => 1 BC/BCE
								# year = -1 => 2 BC/BCE
								# 4800 is traditional number 
								#   --> DEFAULT offset is -32045
								# will alter offset
								# OLD-SCHOOL ALGO - unsigned INTEGERS
								# New Year was March 1, JULIAN CALENDAR
	
	# [a] MAPS Jan/Feb as 11/12 months
	# [y] places Jan/Feb in previous year 
	a = as.integer( (14 - jmonth)/12 );		
	y = as.integer( jyear + IN_THE_BEGINNING - a );
	m = as.integer( jmonth + 12 * a - 3); 
	
	# https://www.slideshare.net/chenshuo/datetime-julian-date # SLIDE 13
	# daysBeforeMonth(March) = 0 
	mm = as.integer( (153*m + 2)/5 );
	
	JPN =  jday + mm + 365*y + as.integer( y/4 );  # JULIAN LEAP YEAR 
	
	return(date.computeOffset(JPN, -1, offset.for, offset));	
	}
	
	

date.fromJulianProlepticNumber = function(JPN,  
										offset.for="julian",
										offset = NULL
										)
	{	
	JPN = as.numeric(JPN);

	IN_THE_BEGINNING = 10000;
		# FOUR CENTURIES
	CENTURY_ = 365.25*400;  # (GREG leap days)?  140697; # 
		# FOUR YEARS  
	YEARS_   = 365.25*4;	#  1461; # 
	
	a = date.computeOffset(JPN, 1, offset.for, offset) - 1; 
	b = as.integer((4*a + 3) / CENTURY_);		# four century phase
	c = a - as.integer((b*CENTURY_)/4);			# one century phase 
	d = as.integer((4*c + 3) / YEARS_);			# within century phase
	e = c - as.integer((YEARS_ * d) / 4);		# days in year 
	m = as.integer((5*e + 2) / 153);			# shifted months

	# day of month (offset - 1), now + 1
	jday = e - as.integer((153*m + 2)/5) + 1;	
	
	# month of year
	jmonth =  m + 3 - 12 * as.integer(m / 10);					
	
	# year
	jyear = b*100 + d - IN_THE_BEGINNING + as.integer(m / 10); 	

	
	list(	jyear 	= as.integer( jyear ), 
			jmonth 	= as.integer( jmonth ), 
			 jday 	= as.integer( jday) 
		);
		
		
		
		
		
		
		
	}
	
	
	
	
	
# memory.init()
# setOrigin, setInTZ, setOutZ
## less passing into functions


# set.seed(123); year = sample(1900:2000, 10, replace = TRUE);
# set.seed(123); month = sample(1:12, 10, replace = TRUE);
# set.seed(123); day = sample(1:28, 10, replace = TRUE);
 
date.fromJulianDay = function() {}
date.fromJulianDay = function(JD, 
								input.is="Gregorian",
								output.is="DEFAULT" # [L]ilian, [M]odified
							)
		{
		inp = functions.cleanKey(input.is, 3);
		out = functions.cleanKey(output.is, 3);
		
		if(inp == "gre" || inp == "ggg" )  # Good Game Gabriel G. Gandzjuk
			{			
			a = JD + 32044;
			b = as.integer( (4*a+3)/146097 );
			c = a - as.integer( (b*146097)/4 );
			}
			
		if(inp == "jul")
			{
			b = 0;
			c = JD + 32082;
			}
	
		d = as.integer( (4*c+3)/1461 );
		e = c - as.integer( (1461*d)/4 );
		m = as.integer( (5*e+2)/153 );

# http://www.webexhibits.org/calendars/calendar-christian.html
		day = e - as.integer( (153*m+2)/5 ) + 1;
		month = m + 3 - as.integer( 12*(m/10) );
		year = b*100 + d - 4800 +as.integer(  m/10 );

	

	
		
		
		list(year = as.integer( year ), 
			month = as.integer( month ), 
			  day = as.integer( day) );
		}

































# ISO 8601 date format
# https://en.wikipedia.org/wiki/Proleptic_Gregorian_calendar
# https://en.wikipedia.org/wiki/Gregorian_calendar
# Meeus # https://articles.adsabs.harvard.edu/pdf/1992JBAA..102...40M
# Frank 

# G = 365.2425
# J = 365.25
# X = 
# The mean tropical year is 365.24219 days long
date.getDaysInTropicalYear = function()
	{
	# Meeus shows the fluctuations ... this is the mean 
	days = 365.24219;
	days;
	}


date.getOrigin = function() {} 
date.getOrigin = function(method="R", o.tz = Sys.timezone())
	{
	origin = structure(0, class = c("POSIXt", "POSIXct") );
	origin;
	}
		
date.checkTimeZone = function() {} 		
date.checkTimeZone = function(tz)
	{
	tz %in% OlsonNames();
	}


date.checkPOSIXct = function(datePOSIX, in.tz, origin, out.tz)
	{
	# recasting, or actually check elementwise?
	# if(multivariate) ... could do which but doesn't seem to work on a partial vector ... 
	
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
	
	
date.init = function()
		{
		date.setFeatures();
		}
		
## key this like "timer"   [[key]]	, only univariate ... 
	
date.setFeatures = function(key = "DEFAULT",
							+-in.tz  = Sys.timezone(), 
							origin = date.getOrigin(),
							out.tz = NULL)
		{
		memory.init();
		
		}
		
		# univariate ... 
date.getFeatures = function(key)  
		{
		
		}
		
		
date.toUnix = function() {}	
date.toUnix = function(datePOSIX, 
							in.tz  = Sys.timezone(), 
							origin = date.getOrigin(),
							out.tz = NULL
						)
						
						
						
date.toUnix = function(datePOSIX, ...)
	{
	date.defaults();  # list.extract in.tz, origin, out.tz ... 
	res = date.checkPOSIXct(datePOSIX, in.tz, origin, out.tz);
	as.numeric(res);
	}
	
date.fromUnix = function() {} 
date.fromUnix = function(unixNumeric, 
							in.tz  = Sys.timezone(), 
							origin = date.getOrigin(),
							out.tz = NULL
							)
	{
	# https://www.epochconverter.com/
	res = date.checkPOSIXct(datePOSIX, in.tz, origin, out.tz);
	res;
	}

date.get = function() {}	
date.get = function(datePOSIX, 
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

date.getYear = function() {}	
date.getYear = function(datePOSIX, 
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



# %j ... day of year
# %U	Week    01-53    with Sunday as first day of the week	22, 27
# %W	Week 00-53 with Monday as first day of the week
# %w	Weekday 0-6 Sunday is 0	
# %u	Weekday 1-7 Monday is 1

	
date.getMonth = function(datePOSIX, 
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
	
date.getDay = function(datePOSIX, 
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















# month names ... 
# Kimberleigh, Karey, Kathy, Khristine, Kandie, Korrie
# Karsen, Katelyn, Karen, Kimberly, Vera, Natalya, 
# XELA is out of time




# https://docs.oracle.com/cd/E17952_01/mysql-8.0-en/date-and-time-type-syntax.html#:~:text=MySQL%20displays%20DATETIME%20values%20in,using%20either%20strings%20or%20numbers.
# A date and time combination. The supported range is '1000-01-01 00:00:00.000000' to '9999-12-31 23:59:59.999999'. MySQL displays DATETIME values in 'YYYY-MM-DD hh:mm:ss[.fraction]'

# https://en.wikipedia.org/wiki/ISO_8601
# ISO 8601:2004 fixes a reference calendar date to the Gregorian calendar of 20 May 1875 as the date the Convention du Mètre (Metre Convention) was signed in Paris

# + 1 month ... "calendarMonth [anchored to modern calendar FEB 28], averageMonth, monthLengthStart [previous, current, next], monthLengthEnd [previous, current, next], 4 'weeks'?
# what does PHP do with + 1 month
# how does strftaim in PHP work?


# holiday ... "as text"
# first day of SPRING in NORTH ==> EQUINOX, first day of MAY, first Monday of September
# full moon something (CHINESE NEW YEAR), Persian Nasraw ... NISSAN (NEW YEAR)
# https://pear.php.net/package/Date_Holidays
# https://stackoverflow.com/questions/6780536/failed-to-download-pear-http-request2-within-preferred-state-stable
# https://en.wikipedia.org/wiki/Public_holidays_in_Argentina
# https://ru.wikipedia.org/wiki/%D0%9F%D1%80%D0%B0%D0%B7%D0%B4%D0%BD%D0%B8%D0%BA%D0%B8_%D0%A0%D0%BE%D1%81%D1%81%D0%B8%D0%B8
# https://en.wikipedia.org/wiki/Public_holidays_in_Russia
            

# date = Sys.time()
# structure(0, class = c("POSIXt", "POSIXct"))
# seq(date, length = 2, by = "day")[2]
# seq(date, length = 2, by = "-1 day")[2]
 
date.findFormat

# input, output ... R, PHP, MYSQL, JavaScript 
# ... output "equivalent" to another language 

# March 21 is the 80th day of the year (81st in leap years) in the Gregorian calendar; 285 days remain until the end of the year.
# universal days ... XELA days 
# anchor March 21, 2000 (at 12:00 noon at GIZA)
# UPDATE March 21, 1973 (at 12:00 noon at GIZA)
## UPDATE March 21, 1972 [CUPS] ... sets Chinese to RAT 
##  UTC ==>   XTC (GiZA)
## XTC:  March 21, 1973 (at solar noon at GIZA) ... FINAL ANSWER?
# call the calendar XELA ... out-of-time (not months) are XELA days
# tz = "" a function of location [NOT based on GOVT rules]
# set.hours = 24; or 36
# set.mins = 60; or 40 (HEBREW?)
# set.seconds = 60 ... blinks of eye or 1/3 of minute (HEBREW)
# set.week = 5 or 10
# set.month = lunar, 30, arbitrary rules 
# new.year = January 1 ... or moon rules 
# set.YEAR0 = 1973 GREG ... -8 JULIAN 
# YEARS start at 0
# offsetYEAR0 = -1980 ... this will set as YEAR 0, (5,25, 500, 5000) rule applies to THAT offsetYEAR0


# For example, the Julian day number for the day starting at 12:00 UT (noon) on January 1, 2000, was 2 451 545
# https://en.wikipedia.org/wiki/Universal_Time#Adoption_in_various_countries
# UTC.modern.long = 0 ... historically these also changed 
# UTC.diff (long from GIZA)
# 29.987, 31.2118
# 29° 59′ 13.2″ N, 31° 12′ 42.48″ E
# https://stackoverflow.com/questions/21414847/convert-a-date-vector-into-julian-day-in-r

# JDN = (1461 * (Y + 4800 + (M - 14)/12))/4 +(367 * (M - 2 - 12 × ((M - 14)/12)))/12 - (3 * ((Y + 4900 + (M - 14)/12)/100))/4 + D - 32075 // julian day number algorithm
# TOD = JDN + ((H-12)/24) + (MN/1440) + (S/86400) // JDN to JD algorithm
# https://www.sciencedirect.com/topics/engineering/julian-day-number#:~:text=The%20Julian%20day%20count%20is,from%20that%20of%20the%20other.
# The astronomical Julian day number is such a system. We will define it as the day count starting with the number 2,400,000 corresponding to November 16, 1858.
# The day that precedes January 1, 1 A.D. is December 31, 1 B.C.


# https://en.wikipedia.org/wiki/Julian_day
# 


# changeover in 1582 or 1752
x1 = date.mktime(0,0,0,10,15,1582);
x2 = date.mktime(0,0,0,10,4,1582);
# windows says 11 days 
#  x1 - x2

x1 = date.mktime(0,0,0,9,2,1752);
x2 = date.mktime(0,0,0,9,14,1752);

# use.calendar = "G/J" ... gregorian or julian
# greg.adoption = 10,4,1582 ... 10,15,1582
#		... OR ... 9,2,1752 ... 9,14,1752
#      ...  OR ... arbitrary (different countries)
# DST daylight savings is not worth it ...

# apply julian / gregorian rules backward in time indefinitely
# use AD/BC to convert to a universal time 

# https://eclipse.gsfc.nasa.gov/SEhelp/dates.html
# https://www.hermetic.ch/cal_stud/cal_art.html
# https://web.archive.org/web/20141130225140/http://astro.nmsu.edu/~lhuber/leaphist.html

# This is illustrated by the adoption of the birth of Christ as the initial epoch of the Christian calendar. This epoch was established by the sixth-century scholar Dionysius Exiguus, who was compiling a table of dates of Easter. An existing table covered the nineteen-year period denoted 228-247, where years were counted from the beginning of the reign of the Roman emperor Diocletian. Dionysius continued the table for a nineteen-year period, which he designated Anni Domini Nostri Jesu Christi 532-550. Thus, Dionysius' Anno Domini 532 is equivalent to Anno Diocletian 248. In this way a correspondence was established between the new Christian Era and an existing system associated with historical records. What Dionysius did not do is establish an accurate date for the birth of Christ. Although scholars generally believe that Christ was born some years before A.D. 1, the historical evidence is too sketchy to allow a definitive dating.

# https://en.wikipedia.org/wiki/Chinese_calendar#Solar_calendars

# One version of the solar calendar is the five-elements calendar (五行曆; 五行历), which derives from the Wu Xing. A 365-day year was divided into five phases of 73 days, with each phase corresponding to a Day 1 Wu Xing element. A phase began with a governing-element day (行御), followed by six 12-day weeks. Each phase consisted of two three-week months, making each year ten months long. Years began on a jiǎzǐ (甲子) day (and a 72-day wood phase), followed by a bǐngzǐ day (丙子) and a 72-day fire phase; a wùzǐ (戊子) day and a 72-day earth phase; a gēngzǐ (庚子) day and a 72-day metal phase, and a rénzǐ day (壬子) followed by a water phase.[1] Other days were tracked using the Yellow River Map (He Tu).

# Another version is a four-quarters calendar (四時八節曆; 四时八节历; 'four sections, eight seasons calendar', or 四分曆; 四分历). The weeks were ten days long, with one month consisting of three weeks. A year had 12 months, with a ten-day week intercalated in summer as needed to keep up with the tropical year. The 10 Heavenly Stems and 12 Earthly Branches were used to mark days.[2]
#  第40篇四時 [Chapter 40: Four Sections]. 管子 [Guanzi] (in Chinese).


# A third version is the balanced calendar (調曆; 调历). A year was 365.25 days, and a month was 29.5 days. After every 16th month, a half-month was intercalated. According to oracle bone records, the Shang dynasty calendar (c. 1600 – c. 1046 BCE) was a balanced calendar with 12 to 14 months in a year; the month after the winter solstice was Zhēngyuè.[3]

# https://eclipse.gsfc.nasa.gov/SEhelp/calendar.html
# The Julian calendar is used for all dates up to 1582 Oct 04. After that date, the Gregorian calendar is used. Due to the Gregorian Calendar reform, the day after 1582 Oct 04 (Julian calendar) is 1582 Oct 15 (Gregorian calendar). Note that Great Britain did not adopt the Gregorian calendar until 1752. For more information, see Calendars.

# The Julian calendar does not include the year 0, so the year 1 BCE[1] is followed by the year 1 CE. This is awkward for arithmetic calculations. All pages in this web site employ the astronomical numbering system for dates (they use the year 0). Years prior to the year 0 are represented by a negative sign. Historians should note that there is a difference of one year between astronomical dates and BCE dates. Thus, the astronomical year 0 corresponds to 1 BCE, and year -100 corresponds to 101 BCE, etc.. (See: Year Dating Conventions )

# There is some historical uncertainty as to which years from 43 BCE to 8 CE were counted as leap years. For the purposes of this web site, we assume that all Julian years divisible by 4 are be counted as leap years.


date.daysSince(

September 2, 1752 was followed by September 14, 1752



# Pope Gregory shortened October of A.D. 1582 by ten days. Thursday, October 4, 1582 (Julian) was followed immediately by Friday, October 15, 1582 (Gregorian).
# https://libguides.ctstatelibrary.org/hg/colonialresearch/calendar
# n 45 B.C., Julius Caesar ordered a calendar consisting of twelve months based on a solar year.  This calendar employed a cycle of three years of 365 days, followed by a year of 366 days (leap year).  When first implemented, the "Julian Calendar" also moved the beginning of the year from March 1 to January 1.  However, following the fall of the Roman Empire in the fifth century, the new year was gradually realigned to coincide with Christian festivals until by the seventh century, Christmas Day marked the beginning of the new year in many countries. 

# Because the year began in March, records referring to the "first month" pertain to March; to the second month pertain to April, etc., so that "the 19th of the 12th month" would be February 19.  In fact, in Latin, September means seventh month, October means eighth month, November means ninth month, and December means tenth month.  Use of numbers, rather than names, of months was especially prevalent in Quaker records.

# 1752 MARCH -> JANUARY ...
# The changeover involved a series of steps:

#December 31, 1750 was followed by January 1, 1750 (under the "Old Style" calendar, December was the 10th month and January the 11th)
#March 24, 1750 was followed by March 25, 1751 (March 25 was the first day of the "Old Style" year)
#December 31, 1751 was followed by January 1, 1752 (the switch from March 25 to January 1 as the first day of the year)
#September 2, 1752 was followed by September 14, 1752 (drop of 11 days to conform to the Gregorian calendar)

# ANCHOR to JANUARY 1, 2000 
# count backwards from then, using -days ... (fractions of days could be converted to hours, etc.)



# http://www.webexhibits.org/daylightsaving/

# cyear = c(100 * 16:21)
date.isLeapYear = function(cyear, calendar="gregorian")
	{
	# cyear is 1AD => 1, 1BC => 0, 2BC => -1
	cale = functions.cleanKey(calendar, 4);
	# calendar="julian"
	if(cale == "juli")
		{
		i = (cyear %%4 == 0); 	
		names(i) = cyear;
		return(i);
		}
	if(cale == "xela")
		{
		stop("what, TODO::");
		}
	
	
	# gregorian is default
	k = (cyear %%400 == 0);		# if(k) { return(TRUE); }
	j = (cyear %%100 == 0);		# if(j) { return(FALSE); }
	i = (cyear %%4 == 0);	
		# multivariate, as a set 
		r = i - j + k;
		r = as.logical(r);
		names(r) = cyear;
	r;
	}
	

date.mktime = function (sec = 0, min = 0, hour = 12, 
						month = 10, day = 15, year = 1582, tz="")
	{
	ISOdatetime(year, month, day, hour, min, sec, tz="");
	}

date.ISOdate = function(year, month, day, 
						hour = 12, min = 0, 
						sec = 0, tz = "GMT"
						)

https://adodb.org/dokuwiki/doku.php?id=v5:datetime:datetime_index
adodb_mktime(0,0,0,10,15,1582)

ISOdatetime(year, month, day, hour, min, sec, tz = "")
ISOdate(year, month, day, hour = 12, min = 0, sec = 0, tz = "GMT")

date.secondsBeforeOrigin

# how does adobe::date deal with NEGATIVE 

# fCalendar 
# new years (US, CH) ... easter (W, E) 
# fathers/mothers ... labor/memorial ... 
# "holiday" wrapper function 
# map from ISO and POSIX 
# ?strptime

#' date.formatTime();
#' date.formatTime("%Y-%m-%d");
#' date.formatTime("%Y-%m-%d %H:%M:%S");
#' date.formatTime("%Y-%V");  # week ISO 8601 [may vary on system] ?
#'
#' date.formatTime("%Y-%m-%d", strptime("23mar1973", "%d%b%Y") );
date.formatTime = function(out="%Y-%m-%d %H:%M:%S", time = Sys.time() )
	{
	# multivariate on both allowed, recycles
	format(time, out);
	}


x3 = date.mktime(0,0,0,8,5,1600);
date.formatTime("%Y-%V-%A", x3);



# If you want to extract specific aspects of a time (such as the day of the week) just convert it to class "POSIXlt" and extract the relevant component(s) of the list, or if you want a character representation (such as a named day of the week) use the format method.


# here are two POSIX date/time classes, which differ in the way that the values are stored internally. The POSIXct class stores date/time values as the number of seconds since January 1, 1970, while the POSIXlt class stores them as a list with elements for second, minute, hour, day, month, and year, among others.

# ISOdate(1977,7,13)
# date.toPOSIXlt = function(
# ISOdate(-200,7,13)
# NEGATIVE VALUES DON'T WORK 

# write functions to deal with negative YEARS
# with NEGATIVE YEARS, we could set ORIGIN ... Julian APR 5, -7 (8BC) and [5] leap year rule 

# https://stackoverflow.com/questions/39526527/is-there-a-reliable-way-to-detect-posixlt-objects-representing-a-time-which-does
# The value of as.POSIXct(test) seems to be platform dependent



# mydate = as.POSIXlt('2005-4-19 7:01:00')
# doesn't have attributes


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





#' date.isLeapYear
#'
#' @param year
#'
#' @return
#' @export
#'
#' @examples
date.isLeapYear = function(year)
	{
	# _adodb_is_leap_year

	if (year %% 4 != 0) 					        { return(FALSE); }
	if (year %% 400 == 0) 					      { return(TRUE);  }
	if (year %% 100 == 0  && year > 1582) { return(FALSE); }

	return(TRUE);
	}


date.fromString = function(





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


# https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/now
# The static Date.now() method returns the number of milliseconds elapsed since January 1, 1970 00:00:00 UTC.
# The MySQL function NOW() gives the dateTime value in this format: 'YYYY-MM-DD HH:MM:SS' . 
# PHP ... $date =  date("Y-m-d H:i:s");
# 

date.now = function(format="MySQL", tz="")
	{
	now = Sys.time();
	
	}


	> now = Sys.time()
> now 
[1] "2022-08-11 07:37:36 EDT"
> str(now)
 POSIXct[1:1], format: "2022-08-11 07:37:36"
> class(now)
[1] "POSIXct" "POSIXt"

out <- if (!is.POSIXct(time)) as.POSIXct(time) else time
  attr(out, "tzone") <- tzone
  if (is.POSIXlt(time)) {
    out <- as.POSIXlt(out)
  }
  out
  
#  origin = structure(0, class = c("POSIXt", "POSIXct"))




	
# https://www.r-bloggers.com/2018/07/a-tour-of-timezones-troubles-in-r/
# In any programming tool, dates, times, and timezones are hard. Deceptively hard. They’ve been shaped by politics and whimsy for hundreds of years: timezones can shift with minimal notice, countries have skipped or repeated certain days, some are offset by weird increments, some observe Daylight Saving Time, leap years, leap seconds, the list goes on. Luckily, we rarely need to worry about most of those details because other teams of very smart people have spent a lot of time providing nice abstractions for us that handle most of the weird edge cases
# OlsonName()	
	
	
	
	
	
	
	
	
	
	
#' toSystemTime
#'
#' @param numvec
#' @param which
#' @param origin
#'
#' @return
#' @export
#'
#' @examples
toSystemTime = function(numvec, which="ct", origin="1970-01-01")
	{
	# reverses as.numeric(Sys.time());

	# https://rstudio-pubs-static.s3.amazonaws.com/28038_1bcb9aa80ca84f27ace07d612872861a.html
	# now <- Sys.time(); class(now);
	# https://stat.ethz.ch/R-manual/R-devel/library/base/html/as.POSIXlt.html
	# try formats ...

	if(which != "ct")
		{
		as.POSIXlt(numvec, origin=origin);
		} else	{
				as.POSIXct(numvec, origin=origin);
				}
	}


#' asDateTime
#'
#' @param strvec
#' @param from
#' @param to
#' @param num
#'
#' @return
#' @export
#'
#' @examples
asDateTime = function(strvec, from="%Y-%m-%d %H:%M:%S", to="", num=TRUE)
	{
	# ?as.Date
	# https://www.r-bloggers.com/2013/08/date-formats-in-r/
	p.obj = strptime(strvec, format=from );
	res = p.obj;
	if(to != "") { o.obj = strftime(p.obj,  format=to); res = o.obj; }
	if(num) { res = as.numeric(res);}

	res;
	}




















date.now = function(format="MySQL", tz="")
	{
	now = Sys.time();
	
	}

date.toUnix = function(time=NULL, ...)
	{
	if(is.null(time)) { time = date.now(...); }
	# as.POSIXlt ... class(
	# as.numeric(time);
	}
	
	

# http://www.webexhibits.org/calendars/calendar-christian.html 
## TODO ... dots.addToKey
## unlist(  date.toJulianDay(1858,11,16) )  - 2400000

date.toJulianDay = function() {}
# date.toJulianDay(year, month, day);
# date.toJulianDay(0, 0, 0);
# date.toJulianDay( list(year = year, month = month, day = day) );
date.toJulianDay = function(YMDlist, ...,  
								input.is="Gregorian",
								output.is="DEFAULT" # [L]ilian, [M]odified
							)
	{	
	# year, month, day,
	if(!is.list(YMDlist))
		{
		more = list(...);
		year = YMDlist;
		month = more[[1]];
		day = more[[1]];
		} else { list.extract(YMDlist); }
		# should be keyed mylist[["year"]]; mylist[["month"]]; mylist[["day"]]
		
		year = as.integer(year);
		month = as.integer(month);
		day = as.integer(day);
	
	inp = functions.cleanKey(input.is, 3);
	out = functions.cleanKey(output.is, 3);
	
	a = as.integer( (14 - month) / 12 );
	y = year + 4800 - a;
	m = month + 12 * a - 3; 

	if(inp == "gre" || inp == "ggg" )  # Good Game Gabriel G. Gandzjuk
		{
		# JD = 	{
				# day 	+ as.integer( (153*m+2)/5 ) 
						# + y*365 
						# + as.integer(y/4)		# leap year [J]
						# - as.integer(y/100)		# [G] adjust
						# + as.integer(y/400) 	# [G] adjust
						# - 32045;
				# }
				
		JD =  day 	+ as.integer( (153*m+2)/5 )  + y*365 + as.integer(y/4) - as.integer(y/100) + as.integer(y/400) - 32045;
		}

	if(inp == "jul")
		{
		# JD = 	{
				# day  	+ as.integer( (153*m+2)/5 ) 
						# + y*365 
						# + as.integer(y/4)		# leap year [J]
						# - 32083;
				# }
				
		JD = day  	+ as.integer( (153*m+2)/5 )  + y*365 + as.integer(y/4) - 32083;
		}
		
		
	## do MODS here ...
	if(!is.set(JD)) { stop("what are you doing here!"); }
		
	JD;
	}

x = list( "year" = as.integer( c(1914, 1946, 2010, 2007) ),
			"month" = as.integer( c(8, 4, 9, 10) ),
			"day" = as.integer( c(14, 18, 1, 16) )
			); ( y = date.toJulianDay(x) );
			
# off by a bit ... date.fromJulianDay( date.toJulianDay(0,0,0) )
#  year month   day 
#    0     1    27 
# https://www.sciencedirect.com/topics/engineering/julian-day-numbe


## MAYBE correct ?
## 
## unlist( date.fromJulianDay( date.toJulianDay(0,0,0) ) )
## unlist( date.fromJulianDay( date.toJulianDay(-1,2,30) ) )
## unlist( date.fromJulianDay( date.toJulianDay(-1,1,1) ) )
## unlist( date.fromJulianDay( date.toJulianDay(-1,1,1) ) )
## CONVERGED ... 
## Notice that there is no year zero in the Julian or Gregorian calendars. The day that precedes January 1, 1 A.D. is December 31, 1 B.C. 
## November 16, 1858 == 2,400,000  
## unlist( date.fromJulianDay( date.toJulianDay(1858,11,16) ) )
	
	
	
# MJD 0 thus started on 17 Nov 1858 (Gregorian) at 00:00:00 UTC.
# The Lilian day number is similar to the Julian day number, except that Lilian day number 1 started at midnight on the first day of the Gregorian calendar, that is, 15 October 1582.
	

# https://github.com/derickr/timelib
## this is PHP library for detecting ... lots of REGEX
## https://github.com/derickr/timelib/blob/master/parse_date.re
## 


## in.sol package
		# declination(year,month,day,hour=12,minute=0,sec=0)
		# 	valid 1901 to 2099
		# hour = hour + minute/60 + sec/3600
		# jd = 367*year - (7*(year+(month+9)%/%12))%/%4 + (275*month)%/%9+day+1721013.5 + hour/24
		# if (inverse){ return(as.POSIXct((x-2440587.5)*86400,origin=ISOdate(1970,01,01,0,0,0),format="%Y-%m-%d %H:%M:%S" ))
		# else { return(as.numeric(x)/86400 + 2440587.5)








