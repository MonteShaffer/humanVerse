
# 10 VOWELS; 20 CONSONANTS
# phoible.org/parameters ... ranking of soundex usages globally.
# w  	r  	t  		y  	p 
# s 	sh 	ch  	d  	z
# f  	g  	h  		j  	k  (k or s, NO c)
# v  	b  	x  		m  	n (~n, nj); r(r) ... rolling is optional  

# a  	e  	i  		o  	u   # spanish sounds 
# -y (boy, say); A (cat, sat, mat); eye (OIC, I, eye); UH (schwa); OH (glottal)
# acqua => akwa ... agua => agwa (g^w doesn't need it own letter)
# accents like spanish ... /right leaning EMPHASIS on current syLLAble


# SUN & MOON:  aku & uka [aku in Egyptian as great light, endowed with spirit)
# phases of sun(year), moon(th), sun(day) ... 
# EQU, growing, MAX, shrinking, EQU, shrinking, MIN, growing (8 phases)
# FRACTALS of scale 
# SPRING EQUINOX = "ala" ... (to spring forth, HEBREW)
# ANEP = 3/4 moon, 20th day of moon cycle (egyptian)
# qaaf = growing moon (egyptian) ... 12th day ?
# petsch 9 (new moon)
# qai, ket(u) = renewal ...
# ala, 

# Abrashit ... name of new year ... 
# half-moon (Egyptian) = uniting of the bulls


# N/S E/W as up/down stream, birth/sleep of sun ... see pg 406 EGYPT = pdf pg 561
# ALSO https://seshmedewnetcher.com/egyptian-orientation-and-geography/
# "DECANS" as "WEEKS"
# left side [abi] first, strong side spiritual (feminine) ... woman 
# right side [amen] second, strong side physically (masculine) ... man (out of wo)
# each decan has two pentads ... F/M as in 
##											kAt/ka
##											sAt/sa
##											mAt/ma 			... cat/sat/mat (wow)!!



education = function()
	{
	
	
	}
	
edumacate = education;





GOD = function(x)
	{
	# x = 1:9999;
	x.sorted = sort(x, decreasing=TRUE);
	x.sum = stats.sum(x);
	
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
	
	
	# weirdness on "-MAGNITUDE-" in list ...
	### res = list("-ONE-" = 1, "-RANK-" = 1, "-MAGNITUDE-", " > SUM(ALL) ");
	res = list("-ONE-" = 1, "-RANK-" = 1); res$`-MAGNITUDE-` = " > SUM(ALL) ";	
	res = property.set("sum", res, x.sum);
	res = property.set("max", res, x.sorted[1]);
	res;
	}
	
	