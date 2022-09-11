





# we need SMART ... if #11223344 ... then alpha = TRUE ... 


check.hex = function(hexstr, ..., prepend="0x", case="upper")
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
# BASE MATHS with above ... I just want simple "character" to number ...
	
	
	
# dec2hex = function(x, ..., to.length=to.length) { base.convert(x, ..., from="DEC", to="HEX", to.length=to.length); }







# hex2dec
# dec2oct
# oct2dec
# dec2bin
# bin2dec

# bin2hex ... do all COMBOS ...
	
	

# # maybe go back and README on colors and do color.convert ... to/fro on RGB

# # library BMS? 

	
# # bin2hex, hex2bin, and so on ...
 




# imagemagick has get nearest color function ...


# color.id("#cc00cc") ... uses min SS^2 in plotrix
# not cosine

 