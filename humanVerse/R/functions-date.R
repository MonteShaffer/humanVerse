
PI = 3.1415926535897932384626433;
PHI = (1 + sqrt(5) ) / 2 ;
E = exp(1);
# m = matrix(c(1,0,0,1), nrow=2);
# exp(m); # works 
# SI defintions: meter, kg, etc. 
# https://www.npl.co.uk/si-units/metre
# The metre is defined by taking the fixed numerical value of the speed of light in vacuum, 𝒸, to be 299 792 458 when expressed in the unit m s−1, where the second is defined in terms of the caesium frequency ∆ν. The wording of the definition was updated in 2019. (https://www.npl.co.uk/si-units/metre)
# The metre was initially defined as one ten-millionth of the distance on the Earth's surface from the north pole to the equator, on a line passing through Paris. Expeditions from 1792 to 1799 determined this length by measuring the distance from Dunkirk to Barcelona, with an accuracy of about 0.02%
# In 1848–49, physicist Hippolyte Fizeau measured the speed of light using a rotating cogwheel. In 2009, NPL and the BBC used a kitchen blender to recreate Fizeau’s experiment
# The exact coordinates of the Great Pyramid of Giza are 29.9792458°N which by itself means very little. However, some enthusiasts have noticed the number is exactly the same as the speed of light which travels at 299,792,458 metres per second. (https://www.express.co.uk/news/science/960740/ancient-egypt-great-pyramid-giza-speed-of-light) ... SNOPES involved, so likely TRUE (https://www.snopes.com/fact-check/pyramid-location-speed-light/):  It is true that the latitude 29.9792458 N intersects the Great Pyramid of Giza (at the selected longitude of around 31.134667 E). However:
# A prime meridian is an arbitrary meridian (a line of longitude) in a geographic coordinate system at which longitude is defined to be 0°. 
# In October 1884 the Greenwich Meridian was selected by delegates (forty-one delegates representing twenty-five nations) to the International Meridian Conference held in Washington, D.C., United States to be the common zero of longitude and standard of time reckoning throughout the world.
# https://en.wikipedia.org/wiki/Royal_Observatory,_Greenwich
# 51.4778, -0.0014
# GM = c(51.4778, 0, 44); 
# SM = c( 39.026423, -83.431059, 232);  # center of OVAL of Serpent Mound 
# GIZA = c(29.979086,  31.134166, 180);  # lat,long, alt-meters (APEX)
# Aconcagua
# Mount LamLam (marianas trench) ... Haleakalā
# 13.338611, 144.662778, 200
# Haleak ... (20.709722, -156.253333, 3055) ... google earth says 3050 (wiki 3055)
# https://www.npr.org/2007/04/07/9428163/the-highest-spot-on-earth
# Chimborazo (lightning?) ... (-1.469167, -78.8175, 6263.47) ... google earth says 6260 ... SIGN FIG?
# Chimborazo's summit is the farthest point on the Earth's surface from the Earth's center given that it is located along the planet's equatorial bulge. Based on average global sea level (mean sea level), Chimborazo's summit is lower than Mount Everest; see elevation as measured from sea level.
# And if you think that is wild, how about this: Which is higher, the Dead Sea (lowest point on the terrestrial Earth) or the top of Mount McKinley? Hear Spireguy's answer. [https://www.npr.org/2007/04/07/9428163/the-highest-spot-on-earth ... javascript:getStaticMedia('/npr/wesat/2007/04/20070407_wesat_deadsea','RM,WM')]
# https://en.wikipedia.org/wiki/List_of_highest_mountains_on_Earth#Stem_and_leaf_plot



date.constants = function(envir=parent.frame(1))
	{
	# https://www.nist.gov/pml/time-and-frequency-division
	# https://www.nist.gov/physical-measurement-laboratory/nist-guide-si-appendix-b9#TIME
	SECS_PER_MIN 	= 60;
	SECS_PER_HOUR 	= 60*60;
	SECS_PER_DAY 	= 60*60*24;  	# 86000
	
	assign("SECS_PER_MIN", 	SECS_PER_MIN, 	envir=envir);
	assign("SECS_PER_HOUR", SECS_PER_HOUR,	envir=envir);
	assign("SECS_PER_DAY",	SECS_PER_DAY,	envir=envir);
	
	# SIDEREAL
	SECS_PER_SSEC	= 0.9972696;
	SECS_PER_SMIN	= 59.83617;  	# 0.9972696 * 60 = 59.83618
	SECS_PER_SHOUR	= 3590.170;
	SECS_PER_SDAY	= 86164.09;
	SECS_PER_SYEAR	= 31558150;  	# is this true or 365 days?  365.2536?
	# TROPICAL
	SECS_PER_TYEAR	= 31556930   	# This looks true
									# 31556930 / 60 / 60 / 24 = 365.2422
	
	assign("SECS_PER_SSEC", 	SECS_PER_SSEC, 	envir=envir);
	assign("SECS_PER_SMIN", 	SECS_PER_SMIN,	envir=envir);
	assign("SECS_PER_SHOUR",	SECS_PER_SHOUR,	envir=envir);
	assign("SECS_PER_SDAY", 	SECS_PER_SDAY, 	envir=envir);
	assign("SECS_PER_SYEAR", 	SECS_PER_SYEAR,	envir=envir);
	assign("SECS_PER_TYEAR",	SECS_PER_TYEAR,	envir=envir);	
	}

num.round = function(n, by=5, method="round")
	{
	byidx = (n %% by == 0); # these already are indexed well 
    new = by * as.integer((n + by) / by);
    res = n;
    res[!byidx] = new[!byidx];
    res;
	# round(n/by) * by;
	#ceiling(n/by) * by;
	#floor(n/by) * by;
	# maybe if negative do ceiling, positive do floor ...
	# user wants basically an UP/DOWN feature ... 
	}

# https://stackoverflow.com/a/15272920/184614
console.readkey = function(msg="Press [ENTER] to continue, [ESC] to exit")
	{
	cat (msg);
	line = readline();
	# maybe do something like onkeypress (see stackoverflow METHOD 3)
	# if [-] slow down the auto.play ... [+] speed up 
	}

# http://rfunction.com/archives/1302
# par mar
snails.pace = function() {} 
# Symbola
# https://fontlibrary.org/en/font/symbola
snails.pace = function(snails = 6, finish.line = 8, moves = 200,
							auto.play = NULL, ymax = 2 + snails,
							s.family = NULL,
							s.par = FALSE, s.pch=(10+snails), s.cex=snails,
							snail.col = c("orange", "blue", "pink", "green", "yellow", "red")
						)
	{
	old.par = par(no.readonly = TRUE);	
	on.exit({par(new = FALSE); par(old.par);}); # add=TRUE to on.exit ... always, maybe like dput(all)
	snail.x = 0*(1:snails); 
	if(ymax < snails) { ymax = snails; }
	y.d = ymax - snails; 
	scale.y = ymax / snails;
	snail.y = (scale.y*(1:snails)) - (1/(snails/2) * y.d);
	snail.y = snail.y - 1; # moved the rect to top, so adjust 
	
	# RANDOM colors ... `sample(colors(), snails);`
	n.col = length(snail.col);
	n.missing = snails - n.col;
	### collision is possible, highly improbable
	if(n.missing > 0) { snail.col = c(snail.col, sample(colors(), n.missing)); }
		
	snail.rank = 0*snail.x; 
	crank = 1; # current rank 	
	move.number = 0;
	n = 0; # current number randomized (color)
	
	snails.plot = function() 
		{ 
		xmax = max(finish.line, max(snail.x) );
		# define reasonable xmax if overpainting 
		if(s.par) { xmax = 1.3* moves/snails;  }
		
		
		
		
		
		plot(snail.x, snail.y, 
				col=snail.col, 
				pch=s.pch, cex=s.cex, 
				xlim=c(-1, num.round(xmax, 5) ), 
				ylim=c(-1, ymax ), 
				axes=FALSE, 
				frame.plot=FALSE, 
				xlab="", ylab="",
				main=""
				); 
			
		#axis(gr.side("bottom")); 
		axis(1);
			has.rank = (snail.rank != 0);
			snail.lab = paste0(snail.x, "*", snail.rank);
			snail.lab[!has.rank] = snail.x[!has.rank];
		text(snail.x, y=snail.y, labels=snail.lab, col="black"); 
		abline(v = finish.line, col="gray", lty="dashed");
		
		# main in plot is updating, so place a textbox (white) to overwrite?
		status=paste0("Move #", move.number, " of ", moves);
			r.col = "white"; if(n != 0) { r.col = snail.col[n]; }
		# berryFunctions, roundedRectangle 
		# just draw inner rectangles with missing corners and add 1/4 radius circles ... gr.circle (should be a function)
		# https://stackoverflow.com/questions/34522732/changing-fonts-in-ggplot2#
		# Also, windowsFonts disappeared from grDevices after 3.4.1. The code here needs updating. – smci  Aug 16, 2018 at 22:19
		# windowsFonts()
		# library(extrafont); font_import(); loadfonts(device = "win");
		# Another option is to use showtext package which supports more types of fonts (TrueType, OpenType, Type 1, web fonts, etc.) and more graphics devices, and avoids using external software such as Ghostscript.
		# library(showtext); font_add_google("Montserrat", "Montserrat")
		# font_paths()  ; font_files()  ; font_add("Palatino", "pala.ttf")
		# http://www.cookbook-r.com/Graphs/Fonts/
		# https://rdrr.io/cran/berryFunctions/man/roundedRect.html
		# http://www.statisticstoproveanything.com/2013/09/using-custom-images-as-pch-values-in-r.html
		# https://cran.r-project.org/web/packages/utf8/vignettes/utf8.html
		# U+1F40C [snail]
		# font_add("Symbola", "Symbola.ttf")
		# https://www.fileformat.info/info/unicode/char/1f40c/fontsupport.htm
		# windowsFonts()
		# font_add_google("Cousine", "Cousine")
		# font_families()
		# https://statr.me/
		# https://yixuan.blog/cleveland-r-meetup/pretty.html#1
		# showtext_auto()
		#  par(family = "Cousine")
		# plot(mtcars$wt, mtcars$mpg, pch="ȯ", col = "red", cex = 2, family="Cousine")
		# points(mtcars$wt, mtcars$mpg, pch="\u25D3", col = "blue", cex = 2, family="Symbola")
		# points(mtcars$wt, mtcars$mpg, pch="\u1F40C", col = "orange", cex = 2, family="Symbola")
		rect(0,ymax, finish.line, (ymax-1), border=NA, col=r.col);
		text(0, y=(ymax-1/2), labels=status, col="black", pos=4); # to the right 
		text(finish.line, y=(ymax-1/2), labels=status, col="white", pos=2); # to the left 
		
		if(s.par) { par(new = TRUE); }
		}
	snails.update = function() 
		{
		if(is.null(auto.play))
			{
			x = readline(prompt="Press [enter] to continue, [ESC] to quit");
			} else {
					if(move.number < 2) 
						{
						x = readline(prompt="Press [enter] to continue, [ESC] to quit");
						}
					}
		n = sample(1:snails, 1);
		assign("n", n, envir=parent.env(environment()) );
		
		snail.x[n] = 1 + snail.x[n];
		if( (snail.rank[n] == 0) && (snail.x[n] >= finish.line) )
			{ 			
			snail.rank[n] = crank;
			crank = 1 + crank; 			
			# update to MAIN environment
			assign("snail.rank", snail.rank, envir=parent.env(environment()) );
			assign("crank", crank, envir=parent.env(environment()) );
			}		
		snail.x;
		}

	par(new = FALSE); 	
	# c(bottom, left, top, right)
	par(mar=c(2,1,1,1));
	snails.plot(); 
	while(move.number < moves)
		{
		move.number = 1 + move.number;
		snail.x = snails.update();
		snails.plot();	
		if(!is.null(auto.play)) { Sys.sleep(auto.play); }
		}
	}




date.formatter = function(input, from, to)
	{
	# YYYYg or j or x 
	# short and seconds ... bad idea ... 
	# MM or MMn [short-name] ("Jun") or MMnn ("June") [name]
	# DD
	# WWi (iso, four other calculations)
	# don't use "s" or "n" as they may be expanded for DD names / WW names 
	# "s" is seconds, so "n" or "nn"
	# DOWi (iso, when the week starts, Sun/Mon)
	# DOWn [short-name] ("Mon") or DOWnn ("Monday");
	# DOYj or (g as papal) or british ... JAN/FEB were END of YEAR in julian/gregorian ... changed with british adoption 
	# hh (24-hour), hham (12-hour with AM/PM)
	# mm 
	# ss.sss 
	# shhX (X-divisions in one solar day) ... e.g., divide daytime by X = 18, nightime also divided by X=18 ... length of daytime = nighttime is equal on equinox ... regardless, everyday has 18 hours of daytime and 18 hours of nighttime, the length of the hours change 
	# https://www.adavidsingh.com/time-ancient-rome/
	# in summer, roman hour (daylight) was 90 minutes ... 
	# 18 / 3, not by 8 ... or 5 ...   5 * 8 * 3 = 120 ... so maybe 12 like rome?
	# 18 is from Hebrew? 36 decans (Egyptian)?
	# The Egyptians divided the clock into 12 hours of daytime and 12 hours of night-time (or alternatively 10 hours between sunrise and sunset, an hour for each twilight period and 12 hours of darkness). (https://bit.ly/3cnCxrM)
	# bit.ly is databse lookup, not good data analytics 
	# https://leetcode.com/problems/encode-and-decode-tinyurl/
	# https://www.geeksforgeeks.org/how-to-design-a-tiny-url-or-url-shortener/
	# can't I do base64 ... hex ... 
	# don't do solar mm or ss.sss just make 
	#     shh18d.ddddddd as decimal ... 0 - 17.99999  # day
	# 		shh18p as p's and q's ... opposites (day / night)
	#       shh18q
	#     shh18n.nnnnnnn as decimal ... 0 - 17.99999  # night, not [n]?, dark 
	# length(one shh18d) = length(one ssh18n) on EQUINOX 
	# length is function( lat, lon, alt for YYYY-MM-DD )
	# https://www.w3schools.com/sql/func_mysql_date_format.asp
	# Upon successful completion, strptime() shall return a pointer to the character following the last character parsed. Otherwise, a null pointer shall be returned.
	# https://pubs.opengroup.org/onlinepubs/9699919799/functions/strptime.html
	# convert to myISO similar to temp.convert logic 
	# YYYYMMDD or YYYY-MM-DD ... I add YYYYg for g, j, x as year types
	# [D] is the weekday number, from 1 through 7, beginning with Monday and ending with Sunday.
	# [Www] is the week number prefixed by the letter W, from W01 through W53.
	# the week with the starting year's first Thursday in it (the formal ISO definition), ... one of five different definitions 
	#  [DDD] is the "day of year", from 001 through 365 (366 in leap years)
	# Thh:mm:ss.sss ... 00:00:00.000 NOT 24:00:00.000
	# Z as UTC, no offset ... 
	# "−05:00" for New York on standard time (UTC-05:00)
	# "−04:00" for New York on daylight saving time (UTC-04:00)
	#  then the hyphen-minus should be used. ASCII does not have a minus sign, so its hyphen-minus character
	# the "T" seems unnecessary 
	
	
	
	
	}

date.setCalendar = function() {}

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

	
	
	
	