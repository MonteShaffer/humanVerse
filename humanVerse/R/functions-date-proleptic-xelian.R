
# 10 VOWELS; 20 CONSONANTS (maybe 30) ... unique sound, unique letter 
# phoible.org/parameters ... ranking of soundex usages globally.
# w  	r  	t  		y  	p  ... what of th (theta)
# s 	sh 	ch  	d  	z
# f  	g  	h  		j  	k  (k or s, NO c)
# v  	b  	x  		m  	n (~n, nj); r(r) ... rolling is optional  

# a  	e  	i  		o  	u   # spanish sounds 
# -y (boy, say); A (cat, sat, mat); eye (OIC, I, eye); UH (schwa); OH (glottal)
# -w (bow, ow) ... separate "-y" from "y" and same with "-w" / "w" 
# acqua => akwa ... agua => agwa (g^w doesn't need it own letter)
# accents like spanish ... /right leaning EMPHASIS on current syLLAble
# http://easypronunciation.com/en/english/word/{word}


# SUN & MOON:  aku & uka [aku in Egyptian as great light, endowed with spirit)
# phases of sun(year), moon(th), sun(day) ... 
# EQU, growing, MAX, shrinking, EQU, shrinking, MIN, growing (8 phases)
# FRACTALS of scale 
# SPRING EQUINOX = "ala" ... (to spring forth, HEBREW)
# ANEP = 3/4 moon, 20th day of moon cycle (egyptian)
# AGEP (means darkness) ... Uru was gods who lightened darkness; Khenu gave lesser light
# AZIS (means light, arabic)
# qaaf = growing moon (egyptian) ... 12th day ?
# petsch 9 (new moon)
# qai, ket(u) = renewal ...  khat = tip of sword/beginning ...  7 brothers/sister? ... Arcturi/Pleidia (celestial N/S ?)
# ala, 
# AKU (sun year), UKA (moon phases 'month')
# AKUtri = sun month (sonth?) ... what is name for a day ... 
# Abrashit ... name of new year ... 
# half-moon (Egyptian) = uniting of the bulls
# http://www.touregypt.net/featurestories/moon.htm 


# N/S E/W as up/down stream, birth/sleep of sun ... see pg 406 EGYPT = pdf pg 561
# ALSO https://seshmedewnetcher.com/egyptian-orientation-and-geography/
# "DECANS" as "WEEKS"
# left side [abi] first, strong side spiritual (feminine) ... woman 
# right side [amen] => [ami]? or [iba] second, strong side physically (masculine) ... man (out of wo)
# facing you [abi-iba] where b are middle three fingers [a] is thumb, [i] is pinkie
# facing away from you (piano) [iba-abi] shows reflection by rotation (viewpoint)
# each decan has two pentads ... F/M as in 
##											kAt/ka
##											sAt/sa
##											mAt/ma 			... cat/sat/mat (wow)!!

## count open palms facing you, F[abi]: 1,2,3,4,5 (thumb, ... pinkie)
##                              M[ami]: 5,4,3,2,1 (pinkie, ... thumb)

## identification is 1 kAt, ... 5 kAt, 5 ka, ... 1 ka 
##                   1 sAt, ... 5 sAt, 5 sa, ... 1 sa
##                   1 mAt, ... 5 mAt, 5 ma, ... 1 ma 

## civil identification 1-10 as  1-10 kAtka (one word)
##								 1-10 sAtsa 
##								 1-10 mAtma 

## three decans define a solar month .. a sonth? 
## there are three seasons of four sonths in a year 

## fa na ha sha  (1) M21-ish is start of year (Abrashit) ... growing ... 1 kAt-fa or 1 kAtka-fa
## pa ra ba la   (2) turning of solstice [parabola] ... 1 kAt-pa is about AUG 1
## wa da ya za   (3) returning ... 1 kAt-wa is about DEC 1

## any confusion of language, season can be prouncounced
## fa-na-ha-sha , pa-ra-ba-la , wa-da-ya-za 
## vertical pronunciation are four divisiosn of the season 
## fa-pa-wa , na-ra-da, ha-ba-ya, sha-la-za 

## epogemonalj days are at END (4) restoring/preparing 
## OUT of TIME days are "ta-ta (1-9)" ... say number AFTER for out of time
## say number before for IN TIME ... 5 sAt-fa ... or 5 sAtsa-fa 
## tells you everything ... sAt is spiritual ... sAtsa is civil (numerical calculations)



## dawn to dusk, (sunrise to sunset) ... dawn is NEW day early morning 
## dawn, sunrise
##                early-morning, mid-morning, morning, 
##                early noon,    solar noon, after noon,
##                early-evening, mid-evening, evening,
## sunset, dusk 
## 13, midpoint is "solar noon"
## Maybe drop early/mid and get to 9 overall ... how to get to 9? 5 is noon ... 
## NIGHT watch hours, solar midnight is opposite "solar noon"
## days on average are 24 hours, 15 minute difference from April/September
## base days on the actual local sun ... not the average ... 
## back to sundials 


## day/night has 9 phases + 9 = 18 total {chiasmus}
## maybe 10, so matches hands?  hands down (pinkies are dawn/dusk),
##                          noon is touchdown jesus W shape (thumbs touching)
## should we reverse left/right based on NILE (left is sunrise, I think so)
## counting numbers, palms up ... counting time, palms down ... 
## left-to-right either system ... 1-10 is numbers, 1-9 is time of day 
## 
## dawn, sunrise, morning, early-noon [12:00], 
##                                            solar noon, 
## dusk, sunset,  evening, after-noon,  
## moon/sun has 8 phases ?


## if day as 9+9 = 18 phases, should SUN/MOON have 8? or 18?
## solar noon = summer solstice = full moon 
## solar midnight = winter solstice = no/new moon 
## what is the mirror of dawn/sunrise ... at night ?
## EQU1, growing, MAX, returning, EQU2, returning, MIN, growing, ...
## EQU1 is exact, others are calculated in MOD 30 space ... 
## observed solstice can be exact but not part of CALENDAR ...





education = function()
	{
	
	
	}
	
edumacate = education;



sine.forYear = function(YYYY)
	{
	
	leap = date.calculateLeapDays(YYYY, "xelian", "integer");

	fn.sine = function(x, freq) { sin(2 * pi * freq * x); }
	# off by phases ... 
	fn.sine.inv = function(y, freq) { asin(y)/2/pi/freq } 
		
	x = 0:360; # start and return to zero 
	y = fn.sine(x, 1/360);
	
	# maybe do.plot() ... with do.plot.defaults to save parameters,
	#  ... for par(new=TRUE), only override the new values ... 
	# https://r-coder.com/plot-r
	# https://www.stat.auckland.ac.nz/~ihaka/120/Notes/ch03.pdf
	plot(x, y, pch=".", bty="n", col="black", 
						main="",
						xlim = c(-10, 370), xlab="", xaxt="n",
						ylim = c(-1,1), ylab="", yaxt="n"
		);
		# 5's 
		idx = 1+(1:72 * 5);
		points(x[idx], y[idx], pch="-", col="blue");  # color="blue" ... color not a graphical parameter
		# 10's 
		idx = 1+(1:36 * 10);
		points(x[idx], y[idx], pch="+", col="green");
		# 30's
		idx = 1+(1:12 * 30);
		points(x[idx], y[idx], pch=5, col="red");
		## ta-ta
		idx = (360:(365+leap));
		points(idx, 0*idx, pch="*", col="purple");
		## to compute differences
		y_ = c(y, rep(0, (length(idx)-1)) );
	par(new=TRUE);
	ndays = 365 + leap;
	x2 = 0:ndays;
	y2 = fn.sine(x2, 1/ndays);
	plot(x2, y2, type="l", bty="n", col="gray",  
						main="",
						xlim = c(-10, 370), xlab="", xaxt="n",
						ylim = c(-1,1), ylab="", yaxt="n"
		);
						
	dy = y2 - y_;  # this is phase-shift difference 
	
	# get x-diff (days) for a given y (12 month markers) [TODO]
	# inverse sine?
	## x2_ = fn.sine.inv(y, 1/ndays); # or 1/360 or 1/ndays ? 
	## get y value on 1/360 ... find x2 of y2 ~= y on 1/365 ...
	## 
	idx = 1+(1:12 * 30);
					add = c(0,0,0, 60,120,180, 180,180,180, 240,300,360);
	my = y[idx]; mx = abs(fn.sine.inv(my, 1/ndays)) + add;  # shifting > 90
				
	
	my2 = 0*my;
	for(i in 1:12)
		{
		# limit x2 to neighborhood 
		which( (y2 < my[i] - 0.01)
		}
	
	dx = x2_ - x;  # this is a day difference at a given month marker 
	
	}



GOD = function(x)
	{
	# x = 1:9999;
	x.sorted = sort(x, decreasing=TRUE);
	x.sum = stats.sum(x);
	
	
	## ANCHOR:  Mar 21, 1972 - "1980" as - 723179
	## THIS is DAY ORIGIN (YEAR = 0, MONTH = 0, DAY = 0.  Call it ABRASHET PRIME.
	## YEAR 0 has no leap days; 
	## YEARS (not zero) DIVISIBLE by 5 have +1 leap day 
	##                               25 have +1 leap day (+2 total)
	##                               500 have +1 leap day (+3 total)
	##                              5000 have +1 leap day (+4 total)
	## AVG YEAR is = 365 + 1/5 + 1/25 + 1/500 + 1/5000 = 365.2422
	# 1980 * 365.24219 
	# 1980 * 365.2422 ... 723179.6 ... 
	# March 21, 1972 - 723179 ... YEAR 0 (leap +5 year) ... (.6) gets worked in ...
	# 1980 * 365 + as.integer(1980/5) + as.integer(1980/25) + as.integer(1980/500) + as.integer(1980/5000)
	# 723178
	# 1980 / 60 = 33 
	# 1980 * 365 + as.integer(1980/4) - as.integer(1980/100) + as.integer(1980/400)
	# 723180
	#  M21,1972 ... 723179 days ... MXX as DAY 0 (MONTH 1, DAY 1, YEAR 0)
	# YEAR 0 would have 4 "leap days" ... would that center the error term ...
	# MXX as EQUINOX M22/M23 on Julian/Gregorian for ~10BC
	# julian ( as.Date("1972-03-21"), -2440588 ) = 2441398
	# 2441398 - 723179 = 1718219 JDN .. -1 ... 1718218 (2.718281828459045090796)
	
	# https://en.wikipedia.org/wiki/Phase_(waves)
	# tides / complex ... https://beckmw.wordpress.com/tag/plot/
	
	
	# weirdness on "-MAGNITUDE-" in list ...
	### res = list("-ONE-" = 1, "-RANK-" = 1, "-MAGNITUDE-", " > SUM(ALL) ");
	res = list("-ONE-" = 1, "-RANK-" = 1); res$`-MAGNITUDE-` = " > SUM(ALL) ";	
	res = property.set("sum", res, x.sum);
	res = property.set("max", res, x.sorted[1]);
	res;
	}
	
	