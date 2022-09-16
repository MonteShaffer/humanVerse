
units.convert = function(x, ..., from="in", to="ft", type="distance")
	{
	x = dots.addTo(x, ...);
	# convert everthing to a STANDARD on first pass
	FROM = prep.arg(from, n=3, keep="-", case="lower");  
	TO = prep.arg(to, n=3, keep="-", case="lower");
	TYPE = prep.arg(type, n=4, case="lower");
	
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
	## primarily not an SI unit conversion ... scale conversion ...	
	## maybe comment about that ... 
	## https://www.nist.gov/pml/special-publication-811/nist-guide-si-appendix-b-conversion-factors/nist-guide-si-appendix-b9#LENGTH
	if(TYPE %in% c("dist", "len", "leng"))  # distance or length ... 
		{
		# astronomical unit (ua)
		if(FROM %in% c("ua", "au", "aus", "ast", "uni-ast", "ast-uni")) 
			{ std = 149597900000 * x; }
		# chain (based on U.S. survey foot) (ch)
		if(FROM %in% c("cha", "ch")) 
			{ std = 20.11684 * x; }
		# fathom (based on U.S. survey foot)
		if(FROM %in% c("fat")) 
			{ std = 1.828804 * x; }
		# feet ft foot 
		if(FROM %in% c("ft", "foo", "fee")) 
			{ std = 12 * 0.0254 * x; }
		# survey-feet  us-feet (based on U.S. survey foot)
		if(FROM %in% c("us-ft", "us-foo", "us-fee", "sur-ft", "sur-foo", "sur-fee")) 
			{ std = 0.3048006 * x; }
		# inches
		if(FROM %in% c("in", "inc")) 
			{ std = 0.0254 * x; }
		# kayser 
		if(FROM %in% c("kai", "kay")) 
			{ std = 100 * x; }
		# light-year (l. y.)
		if(FROM %in% c("ly", "lyr", "lig", "l-y", "lig-yr", "lig-yea")) 
			{ std = 1.828804 * x; }
		# mile (mi)
		if(FROM %in% c("mi", "mil")) 
			{ std = 1609.344 * x; }
		# survey-mile  us-mile (based on U.S. survey foot)
		if(FROM %in% c("us-mi", "us-mil", "sur-mi", "sur-mil")) 
			{ std = 1609.347 * x; }
		# nautical-mile  ... The value of this unit, 1 nautical mile = 1852 m, was adopted by the First International Extraordinary Hydrographic Conference, Monaco, 1929, under the name "International nautical mile."
		if(FROM %in% c("n-mi", "n-mil", "nau-mi", "nau-mil")) 
			{ std = 1852 * x; }
		# parsec 
		if(FROM %in% c("par")) 
			{ std = 30856780000000000 * x; }
		# pica-computer 
		if(FROM %in% c("pic-com")) 
			{ std = 0.004233333 * x; }
		# pica-printer 
		if(FROM %in% c("pic-pri")) 
			{ std = 0.004217518 * x; }
		# point-computer 
		if(FROM %in% c("pt-com", "poi-com")) 
			{ std = 0.004233333 * x; }
		# point-printer 
		if(FROM %in% c("pt-pri", "poi-pri")) 
			{ std = 0.004217518 * x; }
		# rod 
		if(FROM %in% c("rd", "rod")) 
			{ std = 5.029210 * x; }	
		# yard 
		if(FROM %in% c("yd", "yar")) 
			{ std = 0.9144 * x; }
			
		if(is.null(std)) { stop("what"); }
		
		################### REVERSE ############################
		
		# astronomical unit (ua)
		if(FROM %in% c("ua", "au", "aus", "ast", "uni-ast", "ast-uni")) 
			{ out = x/149597900000; }
		# chain (based on U.S. survey foot) (ch)
		if(FROM %in% c("cha", "ch")) 
			{ out = x/20.11684; }
		# fathom (based on U.S. survey foot)
		if(FROM %in% c("fat")) 
			{ out = x/1.828804; }
		# feet ft foot 
		if(FROM %in% c("ft", "foo", "fee")) 
			{ out = x/12 * 0.0254; }
		# survey-feet  us-feet (based on U.S. survey foot)
		if(FROM %in% c("us-ft", "us-foo", "us-fee", "sur-ft", "sur-foo", "sur-fee")) 
			{ out = x/0.3048006; }
		# inches
		if(FROM %in% c("in", "inc")) 
			{ out = x/0.0254; }
		# kayser 
		if(FROM %in% c("kai", "kay")) 
			{ out = x/100; }
		# light-year (l. y.)
		if(FROM %in% c("ly", "lyr", "lig", "l-y", "lig-yr", "lig-yea")) 
			{ out = x/1.828804; }
		# mile (mi)
		if(FROM %in% c("mi", "mil")) 
			{ out = x/1609.344; }
		# survey-mile  us-mile (based on U.S. survey foot)
		if(FROM %in% c("us-mi", "us-mil", "sur-mi", "sur-mil")) 
			{ out = x/1609.347; }
		# nautical-mile  ... The value of this unit, 1 nautical mile = x/1852 m, was adopted by the First International Extraordinary Hydrographic Conference, Monaco, 1929, under the name "International nautical mile."
		if(FROM %in% c("n-mi", "n-mil", "nau-mi", "nau-mil")) 
			{ out = x/1852; }
		# parsec 
		if(FROM %in% c("par")) 
			{ out = x/30856780000000000; }
		# pica-computer 
		if(FROM %in% c("pic-com")) 
			{ out = x/0.004233333; }
		# pica-printer 
		if(FROM %in% c("pic-pri")) 
			{ out = x/0.004217518; }
		# point-computer 
		if(FROM %in% c("pt-com", "poi-com")) 
			{ out = x/0.004233333; }
		# point-printer 
		if(FROM %in% c("pt-pri", "poi-pri")) 
			{ out = x/0.004217518; }
		# rod 
		if(FROM %in% c("rd", "rod")) 
			{ out = x/5.029210; }	
		# yard 
		if(FROM %in% c("yd", "yar")) 
			{ out = x/0.9144; }
		
		}

	# https://www.nist.gov/physical-measurement-laboratory/nist-guide-si-appendix-b9#ACCELERATION
	if(TYPE %in% c("acce", "grav", "g"))  # acceleration ... 
		{
		# astronomical unit (ua)
		if(FROM %in% c("ua", "au", "aus", "ast", "uni-ast", "ast-uni")) 
			{ std = 149597900000 * x; }
		
		
		}


		# astronomical unit (ua)
		if(FROM %in% c("ua", "au", "aus", "ast", "uni-ast", "ast-uni")) 
			{ std = 149597900000 * x; }
		
		# astronomical unit (ua)
		if(FROM %in% c("ua", "au", "aus", "ast", "uni-ast", "ast-uni")) 
			{ out = x/149597900000; }


		
		std = switch(FROM,					  			
						  "inc" = 0.0254 * x,
						  "ft" 	= 12 * 0.0254 * x,
						  "foo" = 12 * 0.0254 * x,
						  "fee" = 12 * 0.0254 * x,
						  "C"	= degX,	
						  "K"  	= degX + ABS_ZERO_C,				
						  "R"  	= (degX + ABS_ZERO_R - 32) * (5/9),				
					x											# DEFAULT
					);
		}
	
	out = switch(TO,					  			
					  "F" 	= 9/5 * degC + 32,
					  "C"	= degC,	
					  "K"  	= degC - ABS_ZERO_C,				
					  "R"  	= (9/5 * degC) - ABS_ZERO_R + 32,				
				in											# DEFAULT
				);

	out;
	}


# avogadro_constant # 6.02214179e23 ... udunits2-common.xml ... units::
	# assay_ton
	# troy_ounce / apothecary_ounce ... aliases ... 
	# The definition is exact.  From 1901 to 1964, however, 1 liter was 1.000028 dm^3 (volume of 1 kg of water under standard conditions).
	# meter/speed INTERMIXED in definitions 
	# The meter is the length of the path travelled by light in vacuum during a time interval of 1/299 792 458 of a second.
units.convert = function() {}
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



	
temp.constants = function() {}
temp.constants = function(envir=parent.env(environment()))
	{
	ABS_ZERO_F = -459.67;
	ABS_ZERO_C = -273.15; 
	ABS_ZERO_K = 0;
	ABS_ZERO_R = -459.67; 
	
	C_FREEZING = 0.01;
	C_BOILING = 99.9839;
	C_MAGIC = 4;  # 4 degrees C, 40 degrees F ... water magic (ice floats)
	
	assign("ABS_ZERO_F", ABS_ZERO_F, envir=envir);
	assign("ABS_ZERO_C", ABS_ZERO_C, envir=envir);
	assign("ABS_ZERO_K", ABS_ZERO_K, envir=envir);
	assign("ABS_ZERO_R", ABS_ZERO_R, envir=envir);	
	}

	
temp.isNA = function(degX, Xunits="celsius")
	{
	X = prep.arg(Xunits, n=1, case="upper");
	temp.constants();
		Xconstant.str = paste0("ABS_ZERO_",X);
		Xconstant = eval(parse(text = Xconstant.str));
#		dput(Xconstant);
	is.Z = (degX < Xconstant);
	if(any(is.Z)) { warning("one or more values below absolute zero"); }
	degX[is.Z] = NA;
	degX;	
	}


temp.convert = function(degX, ..., from="fahrenheit", to="celsius")
	{
	degX = dots.addTo(degX, ...);
	temp.constants();
	# convert everthing to "celsius" on first pass
	FROM = prep.arg(from, n=1, case="upper");  # good thing F doesn't mean FALSE anymore!?!
	TO = prep.arg(to, n=1, case="upper");
# dput(degX); dput(F); dput(T); dput(ABS_ZERO_R);
cat("\n START degX ... ", degX, "\n");
	degC = switch(FROM,					  			
					  "F" 	= 5/9 * (degX - 32),
					  "C"	= degX,	
					  "K"  	= degX + ABS_ZERO_C,				
					  "R"  	= (degX + ABS_ZERO_R - 32) * (5/9),				
				degX											# DEFAULT
				);
cat("\n in degC ... ", degC, "\n");			
	# convert everything from "celsius" on second pass 	
	degN = switch(TO,					  			
					  "F" 	= 9/5 * degC + 32,
					  "C"	= degC,	
					  "K"  	= degC - ABS_ZERO_C,				
					  "R"  	= (9/5 * degC) - ABS_ZERO_R + 32,				
				degC											# DEFAULT
				);
cat("\n in degN ... ", degN, "\n");					
	temp.isNA(degN, to);
	}
	


# 12 unique, meaningful conversions 


# F, C, K, R ... LOL
# x = c("F", "C", "K", "R");
# m = e1071::permutations(4);
# m2 =  matrix(x[m], ncol=2);
# m3 = m2[!duplicated(m2), ];
# m4 = m3[ c( which(m3[,1] == x[1]), which(m3[,1] == x[2]), which(m3[,1] == x[3]), which(m3[,1] == x[4]) ),  ];
# m4 = m3[ c(4,1,8,  2,7,3,  9,6,5, 11,10,12), ];

# for(i in 1:12) 
	# {
	# mm = m4[i, ];
	temp.c2f = 	function(degC) { temp.convert(degC, "C", "F"); }
	# row = paste0("temp.", tolower(mm[1]), "2", tolower(mm[2]), " = function(deg", toupper(mm[1]), ", ...) { temp.convert(deg", toupper(mm[1]), ", ...,  from=\"", toupper(mm[1]), "\", to=\"", toupper(mm[2]), "\"); } ");
	# print.noquote(row);
	# cat(row, "\n\n");
	# }


temp.f2c = function(degF, ...) { temp.convert(degF, ...,  from="F", to="C"); }   

temp.f2k = function(degF, ...) { temp.convert(degF, ...,  from="F", to="K"); }  

temp.f2r = function(degF, ...) { temp.convert(degF, ...,  from="F", to="R"); }  

temp.c2k = function(degC, ...) { temp.convert(degC, ...,  from="C", to="K"); }  

temp.c2r = function(degC, ...) { temp.convert(degC, ...,  from="C", to="R"); }  

temp.c2f = function(degC, ...) { temp.convert(degC, ...,  from="C", to="F"); }  

temp.k2r = function(degK, ...) { temp.convert(degK, ...,  from="K", to="R"); }  

temp.k2f = function(degK, ...) { temp.convert(degK, ...,  from="K", to="F"); }  

temp.k2c = function(degK, ...) { temp.convert(degK, ...,  from="K", to="C"); }  

temp.r2f = function(degR, ...) { temp.convert(degR, ...,  from="R", to="F"); }  

temp.r2c = function(degR, ...) { temp.convert(degR, ...,  from="R", to="C"); }  

temp.r2k = function(degR, ...) { temp.convert(degR, ...,  from="R", to="K"); }  

















