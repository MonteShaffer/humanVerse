
u.num2str = function(..., to.length=4, pre="U+")
	{
	num = prep.dots(..., default=59912);
	hex = toBase(num, base=16, to.length=to.length);
	paste0(pre,hex, collapse="");
	}


u.toNum = function(...)
	{
	str = prep.dots(..., default="U+22EF");
	uinfo = list.pair(str.explode("U+", str));
	utf   = list.getElements(uinfo, 2);
	as.integer(as.hexmode(utf));
	}
	


u.fromNum = function(..., collapse=FALSE)
	{
	num = prep.dots(..., default=128012);
	intToUtf8(num, multiple=!collapse);
	}
	




u.toSymbol = function(..., collapse=FALSE)
	{ 
	str = prep.dots(..., default="U+22EF");
	num = u.toNum(str);
	u.fromNum(num, collapse=collapse);
	}


u.getSymbol = u.toSymbol;
















	
	
		# maybe cast \x ... OTHER formats ...
	# utf8ToInt(utf);




	
	# if(str.contains("U+", str))




# utf8ToInt("U+1F40C")
		# utf8ToInt("\U1F40C"); # 128012; # intToUtf8(128012)
		# U+1F40C [snail]
		# plot(1, pch= intToUtf8(128024) )






	# res = intToUtf8(num);  # not keeping separate elements ... collapsed
	# str.explode("",res);
	


# cdot ... U+22EF

# u.getSymbol(c("U+1F40C","U+22EF"));
# uu = u.getSymbol(c("U+22EF","U+1F40C","U+22EF"), collapse=TRUE);
#  "⋯🐌⋯" ... > length(uu) ... [1] 1 ..... > str.len(uu) ... [1] 3
## FIXED, something weird about intToUtf8(num); [collapsing]?
## MORE weirdness
# > uu = u.getSymbol(c("U+22EF","U+1F40C","U+22EF"), collapse=FAlSE);
# > uu
# [1] "⋯"  "🐌" "⋯" 
# > char.more = uu[1]
# > char.more
# [1] "⋯"
# > 


# MAYBE ALLOW a key ... 'EGYPTIAN HIEROGLYPH C020' or EGYPTIAN_HIEROGLYPH_C020
# U+13071

# THIS DOES SOMETHING ??? utf8ToInt("U+1F40C")











	
	## codes = c("e","E", "+", "-", "*", "10", 0:9);
	## utfs = c("ᵉ","ᴱ", "⁺", "⁻", "×", "10", "U+2070", "U+00B9", "U+00B2", "U+00B3", "U+2074", "U+2075", "U+2076", "U+2077", "U+2078", "U+2079"
	 
	 ## digits = 0:9;
	 ## digits.super = c("U+2070", "U+00B9", "U+00B2", "U+00B3", "U+2074", "U+2075", "U+2076", "U+2077", "U+2078", "U+2079");
	 
	 ## nums = u.toNum(digits);
	 ## symbols = u.toSymbol(digits);
	 
	 # sqrt: U+221A , cuberoot: U+221B, fourthroot: U+221C
	 # prop: U+221D, infty: U+221E
	 ## maybe parse FILE.INFO and build a comprehensive MAP 
	 ## U+2245	APPROXIMATELY EQUAL TO
	 ## U+06CA	ARABIC LETTER WAW WITH TWO DOTS ABOVE
	 
	 # https://unicode.org/charts/charindex.html
	 # NOT ALL BUT SOME 
	 # https://unicode.org/ucd/
	 # https://www.unicode.org/Public/14.0.0/ucdxml/
	 # THIS HAS EVERYTHING
	 
	 
	 ## math     = c("+", "-", "*", "/");
	 ## math.utf = c("U+002B", "U+2212", "U+00D7", "U+00F7");

	 
	 # c(8304L, 185L, 178L, 179L, 8308L, 8309L, 8310L, 8311L, 8312L, 8313L)
	 
	 
	 
	 
	 # U+00D7
	 # https://www.fileformat.info/info/unicode/category/Sm/list.htm
	 
# num.toEng(x, show.what="exp", e=" ᴱ", e.pos="+",e.zero="0");
# https://unicode-table.com/en/sets/superscript-and-subscript-letters/
# str.replace(NUMS, with UNICODE small ... also hyphen and plus)

# https://en.wikipedia.org/wiki/Unicode_subscripts_and_superscripts
# U+00D7 ==> 215 ? 
# ord("×");
# chr(215);
	
	# assign("EXP_UTF", utf, envir=envir);
