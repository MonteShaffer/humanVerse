
# x could be a list of long/lati/alt  ... from="list" 
# x could be strings in a certain format ... most likely
# allow geocaching format, any format ... fixed-width?
geo.convert = function(x, ..., from="", to="")
	{
	
	
	
	}




SI.prepKey = function(key="micro")
	{
	# multivariate ? 
	search = NULL;
	num.constants();
	KEY	= prep.arg(key, n=3, case="lower");
	
	# search on number 
	if(is.null(search) && is.numeric(key))
		{
		nums =  SI_PREFIX$SI.idx;
		# index in search ... 
		idx = v.nearestIDX(nums, key);
		# if numeric, a nearest should be found
		if(!is.null(idx)) {  search = idx; } 
		}
		
		
	# search on name ... 
	if(is.null(search))
		{
		choices =  substring(SI_PREFIX$SI.name, 1, 3);
		idx = v.which(choices, KEY);
		if(!is.null(idx)) {  search = idx; }
		}
	# search on name ... 
	if(is.null(search))
		{
		Ke	= prep.arg(key, n=2, case="unchanged");
		choices_ =  substring(SI_PREFIX$SI.symbol, 1, 2);
		idx = v.which(choices_, Ke);
		if(!is.null(idx)) {  search = idx; }
		}
	if(is.null(search))
		{
		if(KEY %in% c("mu", "µ")) 
			{  # may change, so lookup 
			idx = v.which(SI_PREFIX$SI.idx, -6);
			if(!is.null(idx)) {  search = idx; }
			}
		}
	if(is.null(search))
		{
		### how to reference the BASE element ... 
		if(KEY %in% c("bas", "ba", "b", ".ba", ".b", ".", "gra", "met", "m/s")) 
			{  # may change, so lookup 
			idx = v.which(SI_PREFIX$SI.idx, 0);
			if(!is.null(idx)) {  search = idx; }
			}
		}
	## maybe remove trailing units ... cm => c 
	# cm ... I remove m[illi] and c[enti]
	# nkey = str.replace(SI_PREFIX$SI.symbol, "", key);
	## h391ow to subtract to strings ... 
	
	# return actual key with row as attribute 
	res = SI_PREFIX[idx, ];
	res;
	}
	
# SI.convert(1:10, from="kilo", to="BASE", force.from=TRUE)
# SI.convert(1:10, from=7, to="nano", force.from=TRUE)
SI.convert = function(x, ..., from="micro", to="nano", force.from=FALSE, force.to=FALSE)
	{
	x = dots.addTo(x, ...);
	
	info.f = SI.prepKey(from);  FROM.exp = info.f$SI.idx;  # -6
		rownames(info.f) = "";
	if(force.from) { FROM.exp = from; }
	info.t = SI.prepKey(to); 	TO.exp = info.t$SI.idx;    # -9
		rownames(info.t) = "";
	if(force.to) { TO.exp = to; }
		
	de = TO.exp - FROM.exp;
	
	res = x * 10^de;	
	# may have slightly changed?
	res = property.set("SI.from", res, info.f);
	res = property.set("SI.to", res, info.t);
	res = property.set("from", res, FROM.exp);
	res = property.set("to", res, TO.exp);
	minvisible(res);
	}


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
		cat.warning(msg);
		angle.convert(x, from=from, to=to);
		}
	if(TYPE == "base")
		{
		msg = prep.msg("There is a function ", "<i>[base.convert]</i>", " with helper functions in the humanVerse.  To convert numbers to string forms of other bases (hexadecimal, octal, binary).");
		cat.warning(msg);	
		base.convert(x, from=from, to=to);		
		}
	if(TYPE == "colo")
		{
		msg = prep.msg("There is a function ", "<i>[color.convert]</i>", " with helper functions in the humanVerse.  To convert colors from different formats (hex to rgb to cmyk to hsl).");
		cat.warning(msg);
		color.convert(x, from=from, to=to);		
		}
	if(TYPE == "temp")
		{
		msg = prep.msg("There is a function ", "<i>[temp.convert]</i>", " with helper functions in the humanVerse.  To convert temperature from different formats (C to F to K).");
		cat.warning(msg);	
		temp.convert(x, from=from, to=to);
		}
	
	std = NULL;
	out = NULL;
	## num.toEng can convert scales (as strings) ... 
	## primarily not an SI unit conversion ... scale conversion ...	
	## maybe comment about that ... 
	## https://www.nist.gov/pml/special-publication-811/nist-guide-si-appendix-b-conversion-factors/nist-guide-si-appendix-b9#LENGTH
	conv.dist = function() {}
	
	if(TYPE %IN% c("dist", "len", "leng"))  # distance or length ... 
		{
		IN.init();
		
		# meter 
		if(is.null(std) && FROM %IN% c("Meter", "meter", "met", "m")) 
			{ std = 1 * x; }
			
		# astronomical unit (ua)
		if(is.null(std) && FROM %IN% c("Astronomical Unit", "ua", "au", "aus", "ast", "uni-ast", "ast-uni")) 
			{ std = 149597900000 * x; }
		# chain (based on U.S. survey foot) (ch)
		if(is.null(std) && FROM %IN% c("Chain", "ch", "cha")) 
			{ std = 20.11684 * x; }
		# fathom (based on U.S. survey foot)
		if(is.null(std) && FROM %IN% c("Fathom", "fat")) 
			{ std = 1.828804 * x; }
		# feet ft foot 
		if(is.null(std) && FROM %IN% c("Foot", "ft", "foo", "fee")) 
			{ std = 12 * 0.0254 * x; }
		# survey-feet  us-feet (based on U.S. survey foot)
		if(is.null(std) && FROM %IN% c("Survey-Foot", "sur-ft", "us-ft", "us-foo", "us-fee", "sur-foo", "sur-fee")) 
			{ std = 0.3048006 * x; }
		# inches
		if(is.null(std) && FROM %IN% c("Inches", "in", "inc")) 
			{ std = 0.0254 * x; }
		# kayser 
		if(is.null(std) && FROM %IN% c("Kayser", "kai", "kay")) 
			{ std = 100 * x; }
		# light-year (l. y.)
		if(is.null(std) && FROM %IN% c("Light-Year", "lyr", "ly", "lig", "l-y", "li-yr", "l-yr", "lig-yr", "lig-yea")) 
			{ std = 1.828804 * x; }
		# mile (mi)
		if(is.null(std) && FROM %IN% c("Mile", "mi", "mil")) 
			{ std = 1609.344 * x; }
		# survey-mile  us-mile (based on U.S. survey foot)
		if(is.null(std) && FROM %IN% c("Survey-Mile", "sur-mi", "us-mi", "us-mil", "sur-mil")) 
			{ std = 1609.347 * x; }
		# nautical-mile  ... The value of this unit, 1 nautical mile = 1852 m, was adopted by the First International Extraordinary Hydrographic Conference, Monaco, 1929, under the name "International nautical mile."
		if(is.null(std) && FROM %IN% c("Nautical-Mile",  "nau-mi", "n-mi", "n-mil", "nau-mil")) 
			{ std = 1852 * x; }
		# parsec 
		if(is.null(std) && FROM %IN% c("Parsec", "par")) 
			{ std = 30856780000000000 * x; }
		# pica-computer 
		if(is.null(std) && FROM %IN% c("Pica-Computer", "pic-com")) 
			{ std = 0.004233333 * x; }
		# pica-printer 
		if(is.null(std) && FROM %IN% c("Pica-Printer", "pic-pri")) 
			{ std = 0.004217518 * x; }
		# point-computer 
		if(is.null(std) && FROM %IN% c("Point-Computer", "pt-com", "poi-com")) 
			{ std = 0.004233333 * x; }
		# point-printer 
		if(is.null(std) && FROM %IN% c("Point-Printer", "pt-pri", "poi-pri")) 
			{ std = 0.004217518 * x; }
		# rod 
		if(is.null(std) && FROM %IN% c("Rod", "rd", "rod")) 
			{ std = 5.029210 * x; }	
		# yard 
		if(is.null(std) && FROM %IN% c("Yard", "yd", "yar")) 
			{ std = 0.9144 * x; }
			
			
			
		if(is.null(std)) 
			{ 
			msg = msg.badOption("from", from, FROM);			
			cat("\n\n"); minvisible( IN.df(), print=TRUE ); cat("\n\n"); 
			IN.clear();
			cat.stop(msg);
			}
		IN.clear();
		
dput(FROM);
dput(std);
		################### REVERSE ############################
		# since I alreayd have the IN.df() ... could I not just use it here?
		# Pass a key? ... we may not have scanned all of the above ... 
		IN.init();
		
		# meter 
		if(is.null(out) && TO %IN% c("Meter", "meter", "met", "m")) 
			{ out = std/1; }
			
		# astronomical unit (ua)
		if(is.null(out) && TO %IN% c("Astronomical Unit", "ua", "au", "aus", "ast", "uni-ast", "ast-uni")) 
			{ out = std/149597900000; }
		# chain (based on U.S. survey foot) (ch)
		if(is.null(out) && TO %IN% c("Chain", "ch", "cha")) 
			{ out = std/20.11684; }
		# fathom (based on U.S. survey foot)
		if(is.null(out) && TO %IN% c("Fathom", "fat")) 
			{ out = std/1.828804; }
		# feet ft foot 
		if(is.null(out) && TO %IN% c("Foot", "ft", "foo", "fee")) 
			{ out = std/12 * 0.0254; }
		# survey-feet  us-feet (based on U.S. survey foot)
		if(is.null(out) && TO %IN% c("Survey-Foot", "sur-ft", "us-ft", "us-foo", "us-fee", "sur-foo", "sur-fee")) 
			{ out = std/0.3048006; }
		# inches
		if(is.null(out) && TO %IN% c("Inches", "in", "inc")) 
			{ out = std/0.0254; }
		# kayser 
		if(is.null(out) && TO %IN% c("Kayser", "kai", "kay")) 
			{ out = std/100; }
		# light-year (l. y.)
		if(is.null(out) && TO %IN% c("Light-Year", "lyr", "ly", "lig", "l-y", "li-yr", "l-yr", "lig-yr", "lig-yea")) 
			{ out = std/1.828804; }
		# mile (mi)
		if(is.null(out) && TO %IN% c("Mile", "mi", "mil")) 
			{ out = std/1609.344; }
		# survey-mile  us-mile (based on U.S. survey foot)
		if(is.null(out) && TO %IN% c("Survey-Mile", "sur-mi", "us-mi", "us-mil", "sur-mil")) 
			{ out = std/1609.347; }
		# nautical-mile  ... The value of this unit, 1 nautical mile = std/1852 m, was adopted by the First International Extraordinary Hydrographic Conference, Monaco, 1929, under the name "International nautical mile."
		if(is.null(out) && TO %IN% c("Nautical-Mile",  "nau-mi", "n-mi", "n-mil", "nau-mil")) 
			{ out = std/1852; }
		# parsec 
		if(is.null(out) && TO %IN% c("Parsec", "par")) 
			{ out = std/30856780000000000; }
		# pica-computer 
		if(is.null(out) && TO %IN% c("Pica-Computer", "pic-com")) 
			{ out = std/0.004233333; }
		# pica-printer 
		if(is.null(out) && TO %IN% c("Pica-Printer", "pic-pri")) 
			{ out = std/0.004217518; }
		# point-computer 
		if(is.null(out) && TO %IN% c("Point-Computer", "pt-com", "poi-com")) 
			{ out = std/0.004233333; }
		# point-printer 
		if(is.null(out) && TO %IN% c("Point-Printer", "pt-pri", "poi-pri")) 
			{ out = std/0.004217518; }
		# rod 
		if(is.null(out) && TO %IN% c("Rod", "rd", "rod")) 
			{ out = std/5.029210; }	
		# yard 
		if(is.null(out) && TO %IN% c("Yard", "yd", "yar")) 
			{ out = std/0.9144; }
		
dput(TO);
dput(out);
		if(is.null(out)) 
			{ 
			msg = msg.badOption("to", to, TO);			
			cat("\n\n"); minvisible( IN.df(), print=TRUE ); cat("\n\n"); 
			IN.clear();	
			cat.stop(msg);
			}	
		IN.clear();
		
		return(out);
		}

	# https://www.nist.gov/physical-measurement-laboratory/nist-guide-si-appendix-b9#ACCELERATION
	conv.acce = function() {}
	if(TYPE %IN% c("acce", "grav", "g"))  # acceleration ... 
		{
		IN.init();
		
		# meter-per-sec-squared  m/s^2 m/s2 ms^2 ms2 
		if(is.null(std) && FROM %IN% c("Meter Per Second Squared", "m/s^2", "m/s", "ms2", "ms^", "mps", "met-per-sec", "met-per-sec-squ", "m-p-s-s", "met")) 
			{ std = 1 * x; }
		# g 
		if(is.null(std) && FROM %IN% c("Gravity", "gn", "g", "gra")) 
			{ std = 9.80665 * x; }
			
		# foot-per-second-squared  ft/s2 ft/s^2 
		if(is.null(std) && FROM %IN% c("Foot per Second Squared", "ft/s^2", "foo-per-sec-squ", "fee-per-sec-squ", "ft/", "ftp", "foo", "fee")) 
			{ std = 0.3048 * x; }
			
		# gal (Gal)
		if(is.null(std) && FROM %IN% c("Galileo Galilei", "galileo", "gal", "ga")) 
			{ std = 0.01 * x; }
		# inch-per-second-squared  in/s2  in/s^2
		if(is.null(std) && FROM %IN% c("Inch Per Second Squared", "in/s^2", "in-per-sec-squ", "inc-per-sec-squ", "in/", "inc")) 
			{ std = 0.01 * x; }
		
		
		if(is.null(std)) 
			{ 
			msg = msg.badOption("from", from, FROM);			
			cat("\n\n"); minvisible( IN.df(), print=TRUE ); cat("\n\n"); 
			IN.clear();
			cat.stop(msg);
			}
		IN.clear();
		
		
		################### REVERSE ############################
		IN.init();
		
		# meter-per-sec-squared  m/s^2 m/s2 ms^2 ms2 
		if(is.null(out) && TO %IN% c("Meter Per Second Squared", "m/s^2", "m/s", "ms2", "ms^", "mps", "met-per-sec", "met-per-sec-squ", "m-p-s-s", "met")) 
			{ out = std/1; }
		# g 
		if(is.null(out) && TO %IN% c("Gravity", "gn", "g", "gra")) 
			{ out = std/9.80665; }
			
		# foot-per-second-squared  ft/s2 ft/s^2 
		if(is.null(out) && TO %IN% c("Foot per Second Squared", "ft/s^2", "foo-per-sec-squ", "fee-per-sec-squ", "ft/", "ftp", "foo", "fee")) 
			{ out = std/0.3048; }
			
		# gal (Gal)
		if(is.null(out) && TO %IN% c("Galileo Galilei", "galileo", "gal", "ga")) 
			{ out = std/0.01; }
		# inch-per-second-squared  in/s2  in/s^2
		if(is.null(out) && TO %IN% c("Inch Per Second Squared", "in/s^2", "in-per-sec-squ", "inc-per-sec-squ", "in/", "inc")) 
			{ out = std/0.01; }
		
		if(is.null(out)) 
			{ 
			msg = msg.badOption("to", to, TO);			
			cat("\n\n"); minvisible( IN.df(), print=TRUE ); cat("\n\n"); 
			IN.clear();	
			cat.stop(msg);
			}	
		IN.clear();
		
		return(out);
		}

	# TIME
	# https://www.nist.gov/physical-measurement-laboratory/nist-guide-si-appendix-b9#TIME
	conv.time = function() {}
	if(TYPE %IN% c("time", "tim"))  # time ... 
		{
		# second 
		if(is.null(std) && FROM %IN% c("sec", "s")) 
			{ std = 1 * x; }
		# day 
		if(is.null(std) && FROM %IN% c("day", "d")) 
			{ std = 86400 * x; }
		# day-siderial
		if(is.null(std) && FROM %IN% c("day-sid", "day-s", "d-s")) 
			{ std = 86164.09 * x; }
		# hour 
		if(is.null(std) && FROM %IN% c("hou", "hr", "h")) 
			{ std = 3600 * x; }
		# hour-siderail
		if(is.null(std) && FROM %IN% c("hou-sid", "hr-sid", "hr-s", "h-sid", "h-s")) 
			{ std = 3590.170 * x; }
		# minute 
		if(is.null(std) && FROM %IN% c("min", "m")) 
			{ std = 60 * x; }
		# minute-siderail
		if(is.null(std) && FROM %IN% c("min-sid", "m-sid", "min-s", "m-s")) 
			{ std = 59.83617 * x; }
		# second-siderail
		if(is.null(std) && FROM %IN% c("sec-sid", "s-sid", "sec-s", "s-s")) 
			{ std = 0.9972696 * x; }
		# shake
		if(is.null(std) && FROM %IN% c("sha")) 
			{ std = 0.00000001 * x; }
		# year-365
		if(is.null(std) && FROM %IN% c("yea-365","yr-365", "y-365", "yea-3", "yr-3", "y-3")) 
			{ std = 31536000 * x; }
		# year-sidereal
		if(is.null(std) && FROM %IN% c("yea-sid","yr-sid", "y-sid", "yea-s", "yr-s", "y-s")) 
			{ std = 31558150 * x; }
		# year-tropical
		if(is.null(std) && FROM %IN% c("yea-tro","yr-tro", "y-tro", "yea-t", "yr-t", "y-t")) 
			{ std = 31556930 * x; }
		
		if(is.null(std)) { stop("what show METHOD and FROM "); }
		
		################### REVERSE ############################
		
				# second 
		if(is.null(out) && TO %IN% c("sec", "s")) 
			{ out = std/1; }
		# day 
		if(is.null(out) && TO %IN% c("day", "d")) 
			{ out = std/86400; }
		# day-siderial
		if(is.null(out) && TO %IN% c("day-sid", "day-s", "d-s")) 
			{ out = std/86164.09; }
		# hour 
		if(is.null(out) && TO %IN% c("hou", "hr", "h")) 
			{ out = std/3600; }
		# hour-siderail
		if(is.null(out) && TO %IN% c("hou-sid", "hr-sid", "hr-s", "h-sid", "h-s")) 
			{ out = std/3590.170; }
		# minute 
		if(is.null(out) && TO %IN% c("min", "m")) 
			{ out = std/60; }
		# minute-siderail
		if(is.null(out) && TO %IN% c("min-sid", "m-sid", "min-s", "m-s")) 
			{ out = std/59.83617; }
		# second-siderail
		if(is.null(out) && TO %IN% c("sec-sid", "s-sid", "sec-s", "s-s")) 
			{ out = std/0.9972696; }
		# shake
		if(is.null(out) && TO %IN% c("sha")) 
			{ out = std/0.00000001; }
		# year-365
		if(is.null(out) && TO %IN% c("yea-365","yr-365", "y-365", "yea-3", "yr-3", "y-3")) 
			{ out = std/31536000; }
		# year-sidereal
		if(is.null(out) && TO %IN% c("yea-sid","yr-sid", "y-sid", "yea-s", "yr-s", "y-s")) 
			{ out = std/31558150; }
		# year-tropical
		if(is.null(out) && TO %IN% c("yea-tro","yr-tro", "y-tro", "yea-t", "yr-t", "y-t")) 
			{ out = std/31556930; }
		



	
		if(is.null(out)) { stop("what out show METHOD and FROM "); }
		
		return(out);
		}

# maybe velo / speed is just dist/time ... 
	
	
	# AREA
# 	https://www.nist.gov/physical-measurement-laboratory/nist-guide-si-appendix-b9#AREA
	conv.area = function() {}
	if(TYPE %IN% c("area", "are", "ar"))  # area ... 
		{
		# square-meter ... m2 ... m^2 ...  
		if(is.null(std) && FROM %IN% c("squ-met", "met-squ", "m2", "m^2")) 
			{ std = 1 * x; }
		# acre (based on U.S. survey foot) 
		if(is.null(std) && FROM %IN% c("acr", "us-acr", "sur-acr")) 
			{ std = 4046.873 * x; }
		# are (a)
		if(is.null(std) && FROM %IN% c("are", "a")) 
			{ std = 100 * x; }
		# barn (b)
		if(is.null(std) && FROM %IN% c("bar", "b")) 
			{ std = 0.0000000000000000000000000001 * x; }
		# circular mil
		if(is.null(std) && FROM %IN% c("cir-mil", "c-m", "cm")) 
			{ std = 0.0000000005067075 * x; }
		# hectare (ha)
		if(is.null(std) && FROM %IN% c("hec", "ha")) 
			{ std = 10000 * x; }
		# square foot (ft2)
		if(is.null(std) && FROM %IN% c("ft2", "ft^", "sq-ft","squ-ft", "sq-foo", "sq-fee", "squ-foo", "squ-foo", "ft-sq", "ft-squ", "foo-sq", "fee-sq", "foo-squ", "fee-squ")) 
			{ std = 0.09290304 * x; }
		# square inch (in2)
		if(is.null(std) && FROM %IN% c("in2", "in^", "sq-in","squ-in", "sq-inc", "squ-inc", "squ-inc", "in-sq", "in-squ", "inc-sq", "inc-squ")) 
			{ std = 0.00064516 * x; }
		# square mile (mi2)
		if(is.null(std) && FROM %IN% c("mi2", "mi^", "sq-mi","squ-mi", "sq-mil", "squ-mil", "squ-mil", "mi-sq", "mi-squ", "mil-sq", "mil-squ")) 
			{ std = 2589988 * x; }
		# square mile (based on U.S. survey foot) (mi2) 
		if(is.null(std) && FROM %IN% c("us-mi2", "us-mi^", "us-sq-mi","us-squ-mi", "us-sq-mil", "us-squ-mil", "us-squ-mil", "us-mi-sq", "us-mi-squ", "us-mil-sq", "us-mil-squ")) 
			{ std = 2589998 * x; }
		# square yard (yd2)
		if(is.null(std) && FROM %IN% c("yd2", "yd^", "sq-yd","squ-yd", "sq-yar", "squ-yar", "squ-yar", "yd-sq", "yd-squ", "yar-sq", "yar-squ")) 
			{ std = 0.8361274 * x; }
			
			
			
			
		
		if(is.null(std)) { stop("what show METHOD and FROM "); }
		
		################### REVERSE ############################
		
		# square-meter ... m2 ... m^2 ...  
		if(is.null(out) && TO %IN% c("squ-met", "met-squ", "m2", "m^2")) 
			{ out = std/1; }
		# acre (based on U.S. survey foot) 
		if(is.null(out) && TO %IN% c("acr", "us-acr", "sur-acr")) 
			{ out = std/4046.873; }
		# are (a)
		if(is.null(out) && TO %IN% c("are", "a")) 
			{ out = std/100; }
		# barn (b)
		if(is.null(out) && TO %IN% c("bar", "b")) 
			{ out = std/0.0000000000000000000000000001; }
		# circular mil
		if(is.null(out) && TO %IN% c("cir-mil", "c-m", "cm")) 
			{ out = std/0.0000000005067075; }
		# hectare (ha)
		if(is.null(out) && TO %IN% c("hec", "ha")) 
			{ out = std/10000; }
		# square foot (ft2)
		if(is.null(out) && TO %IN% c("ft2", "ft^", "sq-ft","squ-ft", "sq-foo", "sq-fee", "squ-foo", "squ-foo", "ft-sq", "ft-squ", "foo-sq", "fee-sq", "foo-squ", "fee-squ")) 
			{ out = std/0.09290304; }
		# square inch (in2)
		if(is.null(out) && TO %IN% c("in2", "in^", "sq-in","squ-in", "sq-inc", "squ-inc", "squ-inc", "in-sq", "in-squ", "inc-sq", "inc-squ")) 
			{ out = std/0.00064516; }
		# square mile (mi2)
		if(is.null(out) && TO %IN% c("mi2", "mi^", "sq-mi","squ-mi", "sq-mil", "squ-mil", "squ-mil", "mi-sq", "mi-squ", "mil-sq", "mil-squ")) 
			{ out = std/2589988; }
		# square mile (based on U.S. survey foot) (mi2) 
		if(is.null(out) && TO %IN% c("us-mi2", "us-mi^", "us-sq-mi","us-squ-mi", "us-sq-mil", "us-squ-mil", "us-squ-mil", "us-mi-sq", "us-mi-squ", "us-mil-sq", "us-mil-squ")) 
			{ out = std/2589998; }
		# square yard (yd2)
		if(is.null(out) && TO %IN% c("yd2", "yd^", "sq-yd","squ-yd", "sq-yar", "squ-yar", "squ-yar", "yd-sq", "yd-squ", "yar-sq", "yar-squ")) 
			{ out = std/0.8361274; }
			
			
		



	
		if(is.null(out)) { stop("what out show METHOD and FROM "); }
		
		return(out);
		}

## VOLUME is LARGE ... 
	# https://www.nist.gov/pml/special-publication-811/nist-guide-si-appendix-b-conversion-factors/nist-guide-si-appendix-b9#VOLUME
	# VOLUME 

	conv.vol = function() {}
	if(TYPE %IN% c("volu", "vol", "vo"))  # volume ... 
		{
		# cubic-meter ... m3 ... m^3 ...  
		if(is.null(std) && FROM %IN% c("cub-met", "met-cub", "m3", "m^3")) 
			{ std = 1 * x; }
			
		# acre-foot ... acre-foot (based on U.S. survey foot)  
		if(is.null(std) && FROM %IN% c("acr-foo", "acr", "acr-ft", "acr-fee")) 
			{ std = 1233.489 * x; }
		# barrel [for petroleum, 42 gallons (U.S.)]  
		if(is.null(std) && FROM %IN% c("bar", "us-bar", "bar-us")) 
			{ std = 0.1589873 * x; }
		# bushel (U.S.) (bu)  
		if(is.null(std) && FROM %IN% c("bus", "us-bus", "bus-us", "bu", "us-bu", "bu-us")) 
			{ std = 0.03523907 * x; }
		# cord (128 ft3) 
		if(is.null(std) && FROM %IN% c("cor", "co")) 
			{ std = 3.624556 * x; }
		# cubic foot (ft3) 
		if(is.null(std) && FROM %IN% c("cub-ft", "cub-foo", "cub-fee", "ft-cub", "fee-cub", "foo-cub", "ft3", "ft^")) 
			{ std = 0.02831685 * x; }
		# cubic inch (in3) 
		if(is.null(std) && FROM %IN% c("cub-in", "cub-inc", "in-cub", "inc-cub", "in3", "in^")) 
			{ std = 0.00001638706 * x; }
		# cubic mile (mi3) 
		if(is.null(std) && FROM %IN% c("cub-mi", "cub-mil", "mi-cub", "mil-cub", "mi3", "mi^")) 
			{ std = 4168182000 * x; }
		# cubic yard (yd3)
		if(is.null(std) && FROM %IN% c("cub-yd", "cub-yar", "yd-cub", "yar-cub", "yd3", "yd^")) 
			{ std = 0.7645549 * x; }
			
		# cup (U.S.)
		if(is.null(std) && FROM %IN% c("cup-us", "us-cup", "cup-usa", "usa-cup")) 
			{ std = 0.0002365882 * x; }
			
		# us-gallon... gallon (U.S.) (gal)
		if(is.null(std) && FROM %IN% c("us-gal", "usa-gal", "gal-us", "gal-usa")) 
			{ std = 0.003785412 * x; }
			
		# imperial-gallon... gallon [Canadian and U.K. (Imperial)] (gal)
		if(is.null(std) && FROM %IN% c("imp-gal", "uk-gal", "gal-uk", "gal-imp")) 
			{ std = 0.00454609 * x; }
			
		# us-gill... gill (U.S.) (gi)
		if(is.null(std) && FROM %IN% c("usa-gil", "us-gil", "gil-us", "gil-usa", "usa-gi", "us-gi", "gi-us", "gi-usa")) 
			{ std = 0.0001420653 * x; }


		# imperial-gill... gill [Canadian and U.K. (Imperial)] (gi)
		if(is.null(std) && FROM %IN% c("imp-gil", "uk-gil", "gil-uk", "gil-imp", "imp-gi", "uk-gi", "gi-uk", "gi-imp")) 
			{ std = 0.0001420653 * x; }
			
		# us-ounce ... fluid ounce (U.S.) (fl oz)
		if(is.null(std) && FROM %IN% c("usa-oun", "us-oun", "oun-us", "oun-usa", "usa-oz", "us-oz", "oz-us", "oz-usa")) 
			{ std = 0.00002957353 * x; }
			
		# imperial-ounce ... ounce [Canadian and U.K. fluid (Imperial)] (fl oz)
		if(is.null(std) && FROM %IN% c("imp-oun", "uk-oun", "oun-uk", "oun-imp", "imp-oz", "uk-oz", "oz-uk", "oz-imp")) 
			{ std = 0.00002841306 * x; }
			
			
		# liter ... liter (L) 19
		if(is.null(std) && FROM %IN% c("lit", "li", "l")) 
			{ std = 0.001 * x; }
			
		
		# peck ... peck (U.S.) (pk)
		if(is.null(std) && FROM %IN% c("usa-pec", "us-pec", "pec-us", "pec-usa", "pec")) 
			{ std = 0.008809768 * x; }
			
		# dry-pint ... pint (U.S. dry) (dry pt)
		if(is.null(std) && FROM %IN% c("pt-dry", "dry-pt", "pin-dry", "dry-pin")) 
			{ std = 0.0005506105 * x; }
		# liquid-pint ... pint (U.S. liquid) (liq pt)
		if(is.null(std) && FROM %IN% c("pt-liq", "liq-pt", "pin-liq", "liq-pin")) 
			{ std = 0.0004731765 * x; }
			
		# dry-quart ... quart (U.S. dry) (dry qt)
		if(is.null(std) && FROM %IN% c("qt-dry", "dry-qt", "qua-dry", "dry-qua")) 
			{ std = 0.001101221 * x; }
		# liquid-quart ... quart (U.S. liquid) (liq qt)
		if(is.null(std) && FROM %IN% c("qt-liq", "liq-qt", "qua-liq", "liq-qua")) 
			{ std = 0.0009463529 * x; }
			
		# stere (st)
		if(is.null(std) && FROM %IN% c("st", "ste")) 
			{ std = 1 * x; }
		
		# tablespoon
		if(is.null(std) && FROM %IN% c("tab", "tbl", "tas")) 
			{ std = 0.00001478676 * x; }
		# teaspoon
		if(is.null(std) && FROM %IN% c("tea", "tsp", "tes")) 
			{ std = 0.000004928922 * x; }
			
		# ton, register
		if(is.null(std) && FROM %IN% c("ton", "reg")) 
			{ std = 2.831685 * x; }
		
		

		
		
		if(is.null(std)) { stop("what show METHOD and FROM "); }
		
		################### REVERSE ############################
		
		# cubic-meter ... m3 ... m^3 ...  
		if(is.null(out) && TO %IN% c("cub-met", "met-cub", "m3", "m^3")) 
			{ out = std/1 ; }
			
		# acre-foot ... acre-foot (based on U.S. survey foot)  
		if(is.null(out) && TO %IN% c("acr-foo", "acr", "acr-ft", "acr-fee")) 
			{ out = std/1233.489 ; }
		# barrel [for petroleum, 42 gallons (U.S.)]  
		if(is.null(out) && TO %IN% c("bar", "us-bar", "bar-us")) 
			{ out = std/0.1589873 ; }
		# bushel (U.S.) (bu)  
		if(is.null(out) && TO %IN% c("bus", "us-bus", "bus-us", "bu", "us-bu", "bu-us")) 
			{ out = std/0.03523907 ; }
		# cord (128 ft3) 
		if(is.null(out) && TO %IN% c("cor", "co")) 
			{ out = std/3.624556 ; }
		# cubic foot (ft3) 
		if(is.null(out) && TO %IN% c("cub-ft", "cub-foo", "cub-fee", "ft-cub", "fee-cub", "foo-cub", "ft3", "ft^")) 
			{ out = std/0.02831685 ; }
		# cubic inch (in3) 
		if(is.null(out) && TO %IN% c("cub-in", "cub-inc", "in-cub", "inc-cub", "in3", "in^")) 
			{ out = std/0.00001638706 ; }
		# cubic mile (mi3) 
		if(is.null(out) && TO %IN% c("cub-mi", "cub-mil", "mi-cub", "mil-cub", "mi3", "mi^")) 
			{ out = std/4168182000 ; }
		# cubic yard (yd3)
		if(is.null(out) && TO %IN% c("cub-yd", "cub-yar", "yd-cub", "yar-cub", "yd3", "yd^")) 
			{ out = std/0.7645549 ; }
			
		# cup (U.S.)
		if(is.null(out) && TO %IN% c("cup-us", "us-cup", "cup-usa", "usa-cup")) 
			{ out = std/0.0002365882 ; }
			
		# us-gallon... gallon (U.S.) (gal)
		if(is.null(out) && TO %IN% c("us-gal", "usa-gal", "gal-us", "gal-usa")) 
			{ out = std/0.003785412 ; }
			
		# imperial-gallon... gallon [Canadian and U.K. (Imperial)] (gal)
		if(is.null(out) && TO %IN% c("imp-gal", "uk-gal", "gal-uk", "gal-imp")) 
			{ out = std/0.00454609 ; }
			
		# us-gill... gill (U.S.) (gi)
		if(is.null(out) && TO %IN% c("usa-gil", "us-gil", "gil-us", "gil-usa", "usa-gi", "us-gi", "gi-us", "gi-usa")) 
			{ out = std/0.0001420653 ; }


		# imperial-gill... gill [Canadian and U.K. (Imperial)] (gi)
		if(is.null(out) && TO %IN% c("imp-gil", "uk-gil", "gil-uk", "gil-imp", "imp-gi", "uk-gi", "gi-uk", "gi-imp")) 
			{ out = std/0.0001420653 ; }
			
		# us-ounce ... fluid ounce (U.S.) (fl oz)
		if(is.null(out) && TO %IN% c("usa-oun", "us-oun", "oun-us", "oun-usa", "usa-oz", "us-oz", "oz-us", "oz-usa")) 
			{ out = std/0.00002957353 ; }
			
		# imperial-ounce ... ounce [Canadian and U.K. fluid (Imperial)] (fl oz)
		if(is.null(out) && TO %IN% c("imp-oun", "uk-oun", "oun-uk", "oun-imp", "imp-oz", "uk-oz", "oz-uk", "oz-imp")) 
			{ out = std/0.00002841306 ; }
			
			
		# liter ... liter (L) 19
		if(is.null(out) && TO %IN% c("lit", "li", "l")) 
			{ out = std/0.001 ; }
			
		
		# peck ... peck (U.S.) (pk)
		if(is.null(out) && TO %IN% c("usa-pec", "us-pec", "pec-us", "pec-usa", "pec")) 
			{ out = std/0.008809768 ; }
			
		# dry-pint ... pint (U.S. dry) (dry pt)
		if(is.null(out) && TO %IN% c("pt-dry", "dry-pt", "pin-dry", "dry-pin")) 
			{ out = std/0.0005506105 ; }
		# liquid-pint ... pint (U.S. liquid) (liq pt)
		if(is.null(out) && TO %IN% c("pt-liq", "liq-pt", "pin-liq", "liq-pin")) 
			{ out = std/0.0004731765 ; }
			
		# dry-quart ... quart (U.S. dry) (dry qt)
		if(is.null(out) && TO %IN% c("qt-dry", "dry-qt", "qua-dry", "dry-qua")) 
			{ out = std/0.001101221 ; }
		# liquid-quart ... quart (U.S. liquid) (liq qt)
		if(is.null(out) && TO %IN% c("qt-liq", "liq-qt", "qua-liq", "liq-qua")) 
			{ out = std/0.0009463529 ; }
			
		# stere (st)
		if(is.null(out) && TO %IN% c("st", "ste")) 
			{ out = std/1 ; }
		
		# tablespoon
		if(is.null(out) && TO %IN% c("tab", "tbl", "tas")) 
			{ out = std/0.00001478676 ; }
		# teaspoon
		if(is.null(out) && TO %IN% c("tea", "tsp", "tes")) 
			{ out = std/0.000004928922 ; }
			
		# ton, register
		if(is.null(out) && TO %IN% c("ton", "reg")) 
			{ out = std/2.831685 ; }
		
		



	
		if(is.null(out)) { stop("what out show METHOD and FROM "); }
		
		return(out);
		}

	
	
	# NO to fractions (acceleration is unique)
	# what about FORCE / WORK ( Newton / Joule )???
	# not now ... 
	# unit.tests with other libraries???
	
	
	# https://www.nist.gov/system/files/documents/2021/03/18/ansi-nist_archived_2010_geographic.pdf
	# COORDINATES ... https://en.wikipedia.org/wiki/ISO_6709
	# lat,lon,alt ... dec, dec, meters 
	# ... have lon offset (anchor) ... xela is Khufur ... Pyramid 
	# DEG,DMS,GRT
	# GMT type format CCYYMMDDHHMMSSz
	# DEG ; Long Name: DegreeValue | Optionally populated, the format shall be ±xxx.xxxx±yyy.yyyy, where x refers to latitude and y refers to longitude. Can be auto captured/converted using GPS signals when available. For example, +039.1455- 077.2057. 
	# DMS ; Long Name : DegreeMinuteSecondValue | Optionally populated, the format shall be ±xxxDxxMxxS±yyyDyyMyyS, where x refers to latitude and y refers to longitude. Can be auto captured/converted from GPS signals when available. For example, +039D08M44S-077D12M20S.
	# 48°24′42″N 114°20′24″W # https://en.wikipedia.org/wiki/Whitefish,_Montana
	# https://geohack.toolforge.org/geohack.php?pagename=Whitefish,_Montana&params=48_24_42_N_114_20_24_W_region:US-MT_type:city
	# DMS		48° 24′ 42″ N, 114° 20′ 24″ W
	# Decimal	48.411667, -114.34
	# Geo URI	geo:48.411667,-114.34
	# https://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system
	# https://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system#Simplified_formulae
	# These grid zones are uniform over the globe, except in two areas. On the southwest coast of Norway, grid zone 32V (9° of longitude in width) is extended further west, and grid zone 31V (3° of longitude in width) is correspondingly shrunk to cover only open water. Also, in the region around Svalbard, the four grid zones 31X (9° of longitude in width), 33X (12° of longitude in width), 35X (12° of longitude in width), and 37X (9° of longitude in width) are extended to cover what would otherwise have been covered by the seven grid zones 31X to 37X. The three grid zones 32X, 34X and 36X are not used.
	# UTM	11U 696832 5365475
	# always an acception ...
	# maybe make XTM from GIZA ... 
	# https://cran.r-project.org/web/packages/geosphere/geosphere.pdf
	#02° 31’ 21" North by 32° 5’ 39" East
	# 02 31 21 N, 32 5 39 E
	# 023121N, 320539E
	# 023121, 320539

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

