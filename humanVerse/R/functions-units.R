
units.convert = function(x, ..., from="in", to="ft", type="distance")
	{
	x = dots.addTo(x, ...);
	# convert everthing to a STANDARD on first pass
	FROM	= prep.arg(from, n=3, keep="-", case="lower");  
	TO 		= prep.arg(to, 	 n=3, keep="-", case="lower");
	TYPE	= prep.arg(type, n=4, case="lower");
	
	if(TYPE == "angl")
		{
		msg = prep.msg("There is a function ", "<i>[angle.convert]</i>", " with helper functions in the humanVerse.  To convert degrees to radians and so on.");
		cat.stop(msg);		
		}
	if(TYPE == "base")
		{
		msg = prep.msg("There is a function ", "<i>[base.convert]</i>", " with helper functions in the humanVerse.  To convert numbers to string forms of other bases (hexadecimal, octal, binary).");
		cat.stop(msg);		
		}
	if(TYPE == "colo")
		{
		msg = prep.msg("There is a function ", "<i>[color.convert]</i>", " with helper functions in the humanVerse.  To convert colors from different formats (hex to rgb to cmyk to hsl).");
		cat.stop(msg);		
		}
	if(TYPE == "temp")
		{
		msg = prep.msg("There is a function ", "<i>[temp.convert]</i>", " with helper functions in the humanVerse.  To convert temperature from different formats (C to F to K).");
		cat.stop(msg);		
		}
		
	std = NULL;
	out = NULL;
	## num.toEng can convert scales (as strings) ... 
	## primarily not an SI unit conversion ... scale conversion ...	
	## maybe comment about that ... 
	## https://www.nist.gov/pml/special-publication-811/nist-guide-si-appendix-b-conversion-factors/nist-guide-si-appendix-b9#LENGTH
	conv.dist = function() {}
	if(TYPE %in% c("dist", "len", "leng"))  # distance or length ... 
		{
		
		# meter 
		if(is.null(std) && FROM %in% c("met", "m")) 
			{ std = 1 * x; }
			
		# astronomical unit (ua)
		if(is.null(std) && FROM %in% c("ua", "au", "aus", "ast", "uni-ast", "ast-uni")) 
			{ std = 149597900000 * x; }
		# chain (based on U.S. survey foot) (ch)
		if(is.null(std) && FROM %in% c("cha", "ch")) 
			{ std = 20.11684 * x; }
		# fathom (based on U.S. survey foot)
		if(is.null(std) && FROM %in% c("fat")) 
			{ std = 1.828804 * x; }
		# feet ft foot 
		if(is.null(std) && FROM %in% c("ft", "foo", "fee")) 
			{ std = 12 * 0.0254 * x; }
		# survey-feet  us-feet (based on U.S. survey foot)
		if(is.null(std) && FROM %in% c("us-ft", "us-foo", "us-fee", "sur-ft", "sur-foo", "sur-fee")) 
			{ std = 0.3048006 * x; }
		# inches
		if(is.null(std) && FROM %in% c("in", "inc")) 
			{ std = 0.0254 * x; }
		# kayser 
		if(is.null(std) && FROM %in% c("kai", "kay")) 
			{ std = 100 * x; }
		# light-year (l. y.)
		if(is.null(std) && FROM %in% c("ly", "lyr", "lig", "l-y", "lig-yr", "lig-yea")) 
			{ std = 1.828804 * x; }
		# mile (mi)
		if(is.null(std) && FROM %in% c("mi", "mil")) 
			{ std = 1609.344 * x; }
		# survey-mile  us-mile (based on U.S. survey foot)
		if(is.null(std) && FROM %in% c("us-mi", "us-mil", "sur-mi", "sur-mil")) 
			{ std = 1609.347 * x; }
		# nautical-mile  ... The value of this unit, 1 nautical mile = 1852 m, was adopted by the First International Extraordinary Hydrographic Conference, Monaco, 1929, under the name "International nautical mile."
		if(is.null(std) && FROM %in% c("n-mi", "n-mil", "nau-mi", "nau-mil")) 
			{ std = 1852 * x; }
		# parsec 
		if(is.null(std) && FROM %in% c("par")) 
			{ std = 30856780000000000 * x; }
		# pica-computer 
		if(is.null(std) && FROM %in% c("pic-com")) 
			{ std = 0.004233333 * x; }
		# pica-printer 
		if(is.null(std) && FROM %in% c("pic-pri")) 
			{ std = 0.004217518 * x; }
		# point-computer 
		if(is.null(std) && FROM %in% c("pt-com", "poi-com")) 
			{ std = 0.004233333 * x; }
		# point-printer 
		if(is.null(std) && FROM %in% c("pt-pri", "poi-pri")) 
			{ std = 0.004217518 * x; }
		# rod 
		if(is.null(std) && FROM %in% c("rd", "rod")) 
			{ std = 5.029210 * x; }	
		# yard 
		if(is.null(std) && FROM %in% c("yd", "yar")) 
			{ std = 0.9144 * x; }
			
		if(is.null(std)) { stop("what std show METHOD and FROM "); }
dput(FROM);
dput(std);
		################### REVERSE ############################
		
		# meter 
		if(is.null(out) && TO %in% c("met", "m")) 
			{ out = std/1; }
			
		# astronomical unit (ua)
		if(is.null(out) && TO %in% c("ua", "au", "aus", "ast", "uni-ast", "ast-uni")) 
			{ out = std/149597900000; }
		# chain (based on U.S. survey foot) (ch)
		if(is.null(out) && TO %in% c("cha", "ch")) 
			{ out = std/20.11684; }
		# fathom (based on U.S. survey foot)
		if(is.null(out) && TO %in% c("fat")) 
			{ out = std/1.828804; }
		# feet ft foot 
		if(is.null(out) && TO %in% c("ft", "foo", "fee")) 
			{ out = std/12 * 0.0254; }
		# survey-feet  us-feet (based on U.S. survey foot)
		if(is.null(out) && TO %in% c("us-ft", "us-foo", "us-fee", "sur-ft", "sur-foo", "sur-fee")) 
			{ out = std/0.3048006; }
		# inches
		if(is.null(out) && TO %in% c("in", "inc")) 
			{ out = std/0.0254; }
		# kayser 
		if(is.null(out) && TO %in% c("kai", "kay")) 
			{ out = std/100; }
		# light-year (l. y.)
		if(is.null(out) && TO %in% c("ly", "lyr", "lig", "l-y", "lig-yr", "lig-yea")) 
			{ out = std/1.828804; }
		# mile (mi)
		if(is.null(out) && TO %in% c("mi", "mil")) 
			{ out = std/1609.344; }
		# survey-mile  us-mile (based on U.S. survey foot)
		if(is.null(out) && TO %in% c("us-mi", "us-mil", "sur-mi", "sur-mil")) 
			{ out = std/1609.347; }
		# nautical-mile  ... The value of this unit, 1 nautical mile = std/1852 m, was adopted by the First International Extraordinary Hydrographic Conference, Monaco, 1929, under the name "International nautical mile."
		if(is.null(out) && TO %in% c("n-mi", "n-mil", "nau-mi", "nau-mil")) 
			{ out = std/1852; }
		# parsec 
		if(is.null(out) && TO %in% c("par")) 
			{ out = std/30856780000000000; }
		# pica-computer 
		if(is.null(out) && TO %in% c("pic-com")) 
			{ out = std/0.004233333; }
		# pica-printer 
		if(is.null(out) && TO %in% c("pic-pri")) 
			{ out = std/0.004217518; }
		# point-computer 
		if(is.null(out) && TO %in% c("pt-com", "poi-com")) 
			{ out = std/0.004233333; }
		# point-printer 
		if(is.null(out) && TO %in% c("pt-pri", "poi-pri")) 
			{ out = std/0.004217518; }
		# rod 
		if(is.null(out) && TO %in% c("rd", "rod")) 
			{ out = std/5.029210; }	
		# yard 
		if(is.null(out) && TO %in% c("yd", "yar")) 
			{ out = std/0.9144; }

dput(TO);
dput(out);		
		if(is.null(out)) { stop("what out show METHOD and FROM "); }
		
		return(out);
		}

	# https://www.nist.gov/physical-measurement-laboratory/nist-guide-si-appendix-b9#ACCELERATION
	conv.acce = function() {}
	if(TYPE %in% c("acce", "grav", "g"))  # acceleration ... 
		{
		# meter-per-sec-squared  m/s^2 m/s2 ms^2 ms2 
		if(is.null(std) && FROM %in% c("m/s", "ms2", "ms^", "mps", "met-per-sec", "met-per-sec-squ", "m-p-s-s")) 
			{ std = 1 * x; }
		# g 
		if(is.null(std) && FROM %in% c("g", "gra", "gn")) 
			{ std = 9.80665 * x; }
		# foot-per-second-squared  ft/s2 ft/s^2 
		if(is.null(std) && FROM %in% c("foo-per-sec-squ", "fee-per-sec-squ", "ft/", "ftp", "foo", "fee")) 
			{ std = 0.3048 * x; }
		# gal (Gal)
		if(is.null(std) && FROM %in% c("gal", "ga")) 
			{ std = 0.01 * x; }
		# inch-per-second-squared  in/s2  in/s^2
		if(is.null(std) && FROM %in% c("in-per-sec-squ", "inc-per-sec-squ", "in/", "inp")) 
			{ std = 0.01 * x; }
			
		if(is.null(std)) { stop("what show METHOD and FROM "); }
		
		################### REVERSE ############################
		
		
		# meter-per-sec-squared  m/s^2 m/s2 ms^2 ms2 
		if(is.null(out) && TO %in% c("m/s", "ms2", "ms^", "mps", "met-per-sec", "met-per-sec-squ", "m-p-s-s")) 
			{ out = std/1; }
		# g 
		if(is.null(out) && TO %in% c("g", "gra", "gn")) 
			{ out = std/9.80665; }
		# foot-per-second-squared  ft/s2 ft/s^2 
		if(is.null(out) && TO %in% c("foo-per-sec-squ", "fee-per-sec-squ", "ft/", "ftp", "foo", "fee")) 
			{ out = std/0.3048; }
		# gal (Gal)
		if(is.null(out) && TO %in% c("gal", "ga")) 
			{ out = std/0.01; }
		# inch-per-second-squared  in/s2  in/s^2
		if(is.null(out) && TO %in% c("in-per-sec-squ", "inc-per-sec-squ", "in/", "inp")) 
			{ out = std/0.01; }
		
		if(is.null(out)) { stop("what out show METHOD and FROM "); }
		
		return(out);
		}

	# TIME
	# https://www.nist.gov/physical-measurement-laboratory/nist-guide-si-appendix-b9#TIME
	conv.time = function() {}
	if(TYPE %in% c("time", "tim"))  # time ... 
		{
		# second 
		if(is.null(std) && FROM %in% c("sec", "s")) 
			{ std = 1 * x; }
		# day 
		if(is.null(std) && FROM %in% c("day", "d")) 
			{ std = 86400 * x; }
		# day-siderial
		if(is.null(std) && FROM %in% c("day-sid", "day-s", "d-s")) 
			{ std = 86164.09 * x; }
		# hour 
		if(is.null(std) && FROM %in% c("hou", "hr", "h")) 
			{ std = 3600 * x; }
		# hour-siderail
		if(is.null(std) && FROM %in% c("hou-sid", "hr-sid", "hr-s", "h-sid", "h-s")) 
			{ std = 3590.170 * x; }
		# minute 
		if(is.null(std) && FROM %in% c("min", "m")) 
			{ std = 60 * x; }
		# minute-siderail
		if(is.null(std) && FROM %in% c("min-sid", "m-sid", "min-s", "m-s")) 
			{ std = 59.83617 * x; }
		# second-siderail
		if(is.null(std) && FROM %in% c("sec-sid", "s-sid", "sec-s", "s-s")) 
			{ std = 0.9972696 * x; }
		# shake
		if(is.null(std) && FROM %in% c("sha")) 
			{ std = 0.00000001 * x; }
		# year-365
		if(is.null(std) && FROM %in% c("yea-365","yr-365", "y-365", "yea-3", "yr-3", "y-3")) 
			{ std = 31536000 * x; }
		# year-sidereal
		if(is.null(std) && FROM %in% c("yea-sid","yr-sid", "y-sid", "yea-s", "yr-s", "y-s")) 
			{ std = 31558150 * x; }
		# year-tropical
		if(is.null(std) && FROM %in% c("yea-tro","yr-tro", "y-tro", "yea-t", "yr-t", "y-t")) 
			{ std = 31556930 * x; }
		
		if(is.null(std)) { stop("what show METHOD and FROM "); }
		
		################### REVERSE ############################
		
				# second 
		if(is.null(out) && TO %in% c("sec", "s")) 
			{ out = std/1; }
		# day 
		if(is.null(out) && TO %in% c("day", "d")) 
			{ out = std/86400; }
		# day-siderial
		if(is.null(out) && TO %in% c("day-sid", "day-s", "d-s")) 
			{ out = std/86164.09; }
		# hour 
		if(is.null(out) && TO %in% c("hou", "hr", "h")) 
			{ out = std/3600; }
		# hour-siderail
		if(is.null(out) && TO %in% c("hou-sid", "hr-sid", "hr-s", "h-sid", "h-s")) 
			{ out = std/3590.170; }
		# minute 
		if(is.null(out) && TO %in% c("min", "m")) 
			{ out = std/60; }
		# minute-siderail
		if(is.null(out) && TO %in% c("min-sid", "m-sid", "min-s", "m-s")) 
			{ out = std/59.83617; }
		# second-siderail
		if(is.null(out) && TO %in% c("sec-sid", "s-sid", "sec-s", "s-s")) 
			{ out = std/0.9972696; }
		# shake
		if(is.null(out) && TO %in% c("sha")) 
			{ out = std/0.00000001; }
		# year-365
		if(is.null(out) && TO %in% c("yea-365","yr-365", "y-365", "yea-3", "yr-3", "y-3")) 
			{ out = std/31536000; }
		# year-sidereal
		if(is.null(out) && TO %in% c("yea-sid","yr-sid", "y-sid", "yea-s", "yr-s", "y-s")) 
			{ out = std/31558150; }
		# year-tropical
		if(is.null(out) && TO %in% c("yea-tro","yr-tro", "y-tro", "yea-t", "yr-t", "y-t")) 
			{ out = std/31556930; }
		



	
		if(is.null(out)) { stop("what out show METHOD and FROM "); }
		
		return(out);
		}

# maybe velo / speed is just dist/time ... 
	
	
	# AREA
	https://www.nist.gov/physical-measurement-laboratory/nist-guide-si-appendix-b9#AREA
	conv.area = function() {}
	if(TYPE %in% c("area", "are", "ar"))  # area ... 
		{
		# square-meter ... m2 ... m^2 ...  
		if(is.null(std) && FROM %in% c("squ-met", "met-squ", "m2", "m^2")) 
			{ std = 1 * x; }
		# acre (based on U.S. survey foot) 
		if(is.null(std) && FROM %in% c("acr", "us-acr", "sur-acr")) 
			{ std = 4046.873 * x; }
		# are (a)
		if(is.null(std) && FROM %in% c("are", "a")) 
			{ std = 100 * x; }
		# barn (b)
		if(is.null(std) && FROM %in% c("bar", "b")) 
			{ std = 0.0000000000000000000000000001 * x; }
		# circular mil
		if(is.null(std) && FROM %in% c("cir-mil", "c-m", "cm")) 
			{ std = 0.0000000005067075 * x; }
		# hectare (ha)
		if(is.null(std) && FROM %in% c("hec", "ha")) 
			{ std = 10000 * x; }
		# square foot (ft2)
		if(is.null(std) && FROM %in% c("ft2", "ft^", "sq-ft","squ-ft", "sq-foo", "sq-fee", "squ-foo", "squ-foo", "ft-sq", "ft-squ", "foo-sq", "fee-sq", "foo-squ", "fee-squ")) 
			{ std = 0.09290304 * x; }
		# square inch (in2)
		if(is.null(std) && FROM %in% c("in2", "in^", "sq-in","squ-in", "sq-inc", "squ-inc", "squ-inc", "in-sq", "in-squ", "inc-sq", "inc-squ")) 
			{ std = 0.00064516 * x; }
		# square mile (mi2)
		if(is.null(std) && FROM %in% c("mi2", "mi^", "sq-mi","squ-mi", "sq-mil", "squ-mil", "squ-mil", "mi-sq", "mi-squ", "mil-sq", "mil-squ")) 
			{ std = 2589988 * x; }
		# square mile (based on U.S. survey foot) (mi2) 
		if(is.null(std) && FROM %in% c("us-mi2", "us-mi^", "us-sq-mi","us-squ-mi", "us-sq-mil", "us-squ-mil", "us-squ-mil", "us-mi-sq", "us-mi-squ", "us-mil-sq", "us-mil-squ")) 
			{ std = 2589998 * x; }
		# square yard (yd2)
		if(is.null(std) && FROM %in% c("yd2", "yd^", "sq-yd","squ-yd", "sq-yar", "squ-yar", "squ-yar", "yd-sq", "yd-squ", "yar-sq", "yar-squ")) 
			{ std = 0.8361274 * x; }
			
			
			
			
		
		if(is.null(std)) { stop("what show METHOD and FROM "); }
		
		################### REVERSE ############################
		
		# square-meter ... m2 ... m^2 ...  
		if(is.null(out) && TO %in% c("squ-met", "met-squ", "m2", "m^2")) 
			{ out = std/1; }
		# acre (based on U.S. survey foot) 
		if(is.null(out) && TO %in% c("acr", "us-acr", "sur-acr")) 
			{ out = std/4046.873; }
		# are (a)
		if(is.null(out) && TO %in% c("are", "a")) 
			{ out = std/100; }
		# barn (b)
		if(is.null(out) && TO %in% c("bar", "b")) 
			{ out = std/0.0000000000000000000000000001; }
		# circular mil
		if(is.null(out) && TO %in% c("cir-mil", "c-m", "cm")) 
			{ out = std/0.0000000005067075; }
		# hectare (ha)
		if(is.null(out) && TO %in% c("hec", "ha")) 
			{ out = std/10000; }
		# square foot (ft2)
		if(is.null(out) && TO %in% c("ft2", "ft^", "sq-ft","squ-ft", "sq-foo", "sq-fee", "squ-foo", "squ-foo", "ft-sq", "ft-squ", "foo-sq", "fee-sq", "foo-squ", "fee-squ")) 
			{ out = std/0.09290304; }
		# square inch (in2)
		if(is.null(out) && TO %in% c("in2", "in^", "sq-in","squ-in", "sq-inc", "squ-inc", "squ-inc", "in-sq", "in-squ", "inc-sq", "inc-squ")) 
			{ out = std/0.00064516; }
		# square mile (mi2)
		if(is.null(out) && TO %in% c("mi2", "mi^", "sq-mi","squ-mi", "sq-mil", "squ-mil", "squ-mil", "mi-sq", "mi-squ", "mil-sq", "mil-squ")) 
			{ out = std/2589988; }
		# square mile (based on U.S. survey foot) (mi2) 
		if(is.null(out) && TO %in% c("us-mi2", "us-mi^", "us-sq-mi","us-squ-mi", "us-sq-mil", "us-squ-mil", "us-squ-mil", "us-mi-sq", "us-mi-squ", "us-mil-sq", "us-mil-squ")) 
			{ out = std/2589998; }
		# square yard (yd2)
		if(is.null(out) && TO %in% c("yd2", "yd^", "sq-yd","squ-yd", "sq-yar", "squ-yar", "squ-yar", "yd-sq", "yd-squ", "yar-sq", "yar-squ")) 
			{ out = std/0.8361274; }
			
			
		



	
		if(is.null(out)) { stop("what out show METHOD and FROM "); }
		
		return(out);
		}

## VOLUME is LARGE ... 
	# https://www.nist.gov/pml/special-publication-811/nist-guide-si-appendix-b-conversion-factors/nist-guide-si-appendix-b9#VOLUME
	# VOLUME 
	# VOLUME is a chore ... 
	
	
	# https://www.nist.gov/system/files/documents/2021/03/18/ansi-nist_archived_2010_geographic.pdf
	# COORDINATES ... https://en.wikipedia.org/wiki/ISO_6709
	# lat,lon,alt ... dec, dec, meters 
	# ... have lon offset (anchor) ... xela is Khufur ... Pyramid 
	# DEG,DMS,GRT
	# GMT type format CCYYMMDDHHMMSSz
	# DEG ; Long Name: DegreeValue | Optionally populated, the format shall be ±xxx.xxxx±yyy.yyyy, where x refers to latitude and y refers to longitude. Can be auto captured/converted using GPS signals when available. For example, +039.1455- 077.2057. 
	# DMS ; Long Name : DegreeMinuteSecondValue | Optionally populated, the format shall be ±xxxDxxMxxS±yyyDyyMyyS, where x refers to latitude and y refers to longitude. Can be auto captured/converted from GPS signals when available. For example, +039D08M44S-077D12M20S.
	

	}


# avogadro_constant # 6.02214179e23 ... udunits2-common.xml ... units::
	# assay_ton
	# troy_ounce / apothecary_ounce ... aliases ... 
	# The definition is exact.  From 1901 to 1964, however, 1 liter was 1.000028 dm^3 (volume of 1 kg of water under standard conditions).
	# meter/speed INTERMIXED in definitions 
	# The meter is the length of the path travelled by light in vacuum during a time interval of 1/299 792 458 of a second.
# units.convert = function() {}
# we have color.convert, temp.convert, base.convert, SI units in num.toEng 
# in => foot => nautical mile => regular mile (british vs french, OLD MAPS)
# gravity = m.s2 ... m/s^2 (default)
# c ... speed of light 182, mph 
# speed ... m/s is default 
# area ... m2 (m^2)... acre 
# lat/long ... DMS ... DD.ddd ... 
# moles is just SI units ...
# energy (Joule)
# filesize, SI_units_base2 ... TODO 
# flow ... liter per sec ... with [per] we have to embedded units to convert
#
#  maybe unit.convertTime ... unit.convertDistance 
# interactions 
# fathom, yard, naut_mi, naut2_mi, au, light_year, parsec, 
# point/pixel/pica ???
# mass ... g, Da?, carat, metric_ton, oz, pound, short_ton, long_ton, stone 
# power ... "W" ... erg/sec ... cal/sec ... BTU/sec (or /hour) ... hp (horsepower)
# conv_unit.R ....
# pressure "atm" ... 
# volume "liter" ... m3 ... us_cup, gallon, british tsp 
# 

