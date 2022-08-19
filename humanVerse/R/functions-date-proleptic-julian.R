
# https://www.postgresql.org/docs/current/datetime-julian-dates.html


date.computeProlepticOffsetJulian = function(NUM, dir=-1, 
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
	
	if(off == "cus")
		{
		if(is.null(offset)) { offset = dir*1931383; }
		NUM = NUM + as.numeric(offset);
		return(NUM);
		}
		
	warning("invalid offset.for, returning NUM for offset = 0");
	NUM;	
	}
	
	
date.computeOffset = date.computeProlepticOffsetJulian


	
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
	
	return(date.computeProlepticOffsetJulian(JPN, -1, offset.for, offset));	
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
	
	a = date.computeProlepticOffsetJulian(JPN, 1, offset.for, offset) - 1; 
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
	


# DEFAULT anchors to JULIAN CALENDAR DATE: ... proleptic
#				August 5, 1600 (Tuesday) ==> RUTHVEN EPOCH 
# Historical Date Found in Documents (saying Tuesday)
# cdoy (Current Day of Year) ... important for first year calculations 
# 		## FROM https://www.timeanddate.com/date/weekday.html
# Above link also has Aug 5, 1600 as a Tuesday 
# date.generateProlepticJulian(999); date.generateProlepticJulian(999, "BACKWARD");
date.generateProlepticJulian = function() {}
date.generateProlepticJulian = function(n, dir="FORWARD", 
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
	if(is.negative(n)) 
		{ 
		n = -1* n;  # n is POSITIVE
		# REVERSE FORWARD/BACKWARD ...
		dir = if(dir == "FORWARD") { "BACKWARD"; } else { "FORWARD"; }
		}  
	DIRE = toupper(functions.cleanKey(dir, 4));
		filename = str.replace( "{n}", n, filename );
		filename = str.replace( "{dir}", dir, filename );
		filename = paste0(path, "/", filename);  # is trailing slash required, will it break?
	## HEADER 
	# recovery = FALSE;  # if partially complete, restart??? NICE TO HAVE, not now
	
	
		row = c("IDX", "YYYY", "MM", "DD", "DOW", "DOY");
		cat( paste0(row, collapse="|"), "\n", sep="", 
				file=filename, append=FALSE);
		
			
	if(DIRE == "FORW") # FORWARD in TIME, ASCENDING
		{
		
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
				percent = str.pad( round( 100* abs(i/n), 4 ), 5);
				cat("\n =====   ", cyear, "   ::   ", percent, "% ===== \n"); 
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
				percent = str.pad( round( 100* abs(i/n), 4 ), 5);
				cat("\n =====   ", cyear, "   ::   ", percent, "% ===== \n"); 
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

