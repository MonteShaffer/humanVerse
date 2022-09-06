 

check.hex = function(hexstr, ..., prepend="0x", case="lower")
	{
	hexstr = dots.addTo(hexstr, ...)
	hexstr = str.trim(str.replace(c("0x","#"), "", hexstr));
	# rather than checking, let's remove and add leading "0x"
	hexstr = paste0(prepend, str.trim(tolower(hexstr)));
	hexstr;	
	}
	
# dechex() - Decimal to hexadecimal
# bindec() - Binary to decimal
# octdec() - Octal to decimal
# base_convert() -  
# base_convert(string $num, int $from_base, int $to_base): string
# https://stackoverflow.com/questions/64378066/how-can-i-convert-between-numeral-systems-in-r
# https://stackoverflow.com/a/64380219/184614

	

num.convert = function(x, ..., from="binary", to="octal")
	{
	x = dots.addTo(x, ...);
	# first to decimal (integer) ... maybe lists ...
	F = prep.arg(from, 1, case="upper");
	T = prep.arg(to, 1, case="upper");
	xINT = switch(F,					  			
					  "B" 	= 5/9 * (degX - 32), 	# BINARY
					  "H"	= degX,					# HEX
					  "O"  	= degX + ABS_ZERO_C,	# OCT				
					  "D"  	= dsfjkl,				# DECIMAL (INT, BASE 10)
				xINT											# DEFAULT
				);
cat("\n in degC ... ", degC, "\n");			
	# convert everything from "celsius" on second pass 	
	degN = switch(T,					  			
					  "F" 	= 9/5 * degC + 32,
					  "C"	= degC,	
					  "K"  	= degC - ABS_ZERO_C,				
					  "R"  	= (9/5 * degC) - ABS_ZERO_R + 32,				
				degC											# DEFAULT
				);
cat("\n in degN ... ", degN, "\n");					
	temp.isNA(degN, to);
	
	# now back to string forms ... 
	}	

# maybe go back and README on colors and do color.convert ... to/fro on RGB

# library BMS? 

	
# bin2hex, hex2bin, and so on ...


 
