

u.parse = function(str = "Fa\xe7ade")
	{
	s = enc2utf8(str);
	
	
	}

# js.b64("Fa\xe7ade")

# > d = "Fa\xe7ade"
# > enc2utf8(d)
# [1] "Fa<e7>ade"


# > s_b64_enc_str("Façade")
# [1] "RmHDp2FkZQ=="
# > s_b64_dec_str( s_b64_enc_str("Façade") )
# [1] "Façade"
# > s_b64_dec_str ('RmHnYWRl')
# [1] "Fa\xe7ade"

# the \xe is from my online javascript b-encode ...
# a u.parse("Fa\xe7ade") should work ...

# RmHnYWRl
# RmHDp2FkZQ==
# one is shorter, just CHARS ... javascript ...
### JAVASCRIPT is doing something under the hood to auto-parse the string
# 'Façade'
# base64_encode(str)
# 'RmHnYWRl'
# base64_decode(base64_encode(str))
# 'Façade'


int.u8 = intToUtf8;
u8.int = utf8ToInt;


# int.u8(rev(u8.int("monte jasen shaffer")))

# U+0000 ==> U+007F  (0,127)	... 1 byte, 8 bits         
#                    	... 0xxxxxxx
# U+0080 ==> U+07FF  (128,2047)	... 2 bytes, 16 bits		
#                    	... 110xxxxx	10xxxxxx
# U+0800 ==> U+FFFF  (2048,65535) 	... 3 bytes, 24 bits 	
#                    	... 1110xxxx	10xxxxxx	10xxxxxx
# U+10000 ==> U+10FFFF (65536,1114111)	... 4 bytes, 32 bits 
# 						... 11110xxx	10xxxxxx	10xxxxxx	10xxxxxx

u.int2bin16 = function() {}
u.int2bin32 = function() {}


u.pad8 = function(...)
	{
	x = prep.dots(... , default=c(36, 163, 2361, 8364, 54620, 66376) );
	
	# https://en.wikipedia.org/wiki/UTF-8#Encoding
	b = int2base(x, base=2);
	
	pads = c(7, 11, 16, 21);
	idxs = list(
				(x <= 127),
				(x >= 128 	& x <= 2047),
				(x >= 2048 	& x <= 65535),
				(x >= 65536 & x <= 1114111)
				);
				
	fns = list();
	fns[[1]] = function(y) { paste0("0", y); }
	fns[[2]] = function(y) 
					{
					y = check.list(str.splitN(y, n=6, from="end"));
					ny = length(y);
					res = character(ny);
					for(i in 1:ny)
						{
						y_ = y[[i]];
						res[i] = paste0("110",y_[1], "10",y_[2]);
						}
					res;					
					}
	fns[[3]] = function(y) 
					{
					y = check.list(str.splitN(y, n=6, from="end"));
					ny = length(y);
					res = character(ny);
					for(i in 1:ny)
						{
						y_ = y[[i]];
						res[i] = paste0("1110",y_[1], "10",y_[2], "10",y_[3]);
						}
					res;					
					}
	fns[[4]] = function(y) 
					{
					y = check.list(str.splitN(y, n=6, from="end"));
					ny = length(y);
					res = character(ny);
					for(i in 1:ny)
						{
						y_ = y[[i]];
						res[i] = paste0("11110",y_[1], "10",y_[2], "10",y_[3], "10",y_[4]);
						}
					res;
					}
	
	bu = b;	
	for(i in 1:4)
		{
		if(anyTRUE(idxs[[i]]))
			{
			b[ idxs[[i]] ] = str.pad( b[ idxs[[i]] ], 
								to.length = pads[i], side="LEFT");
			bu[ idxs[[i]] ] = fns[[i]]( b[ idxs[[i]] ] );
			}
		}
		
	hex = bin2hex(bu);	
	list("binary.codepoint" = b, "binary.utf8" = bu, "hex.utf8" = hex);
	}
	
# work on converters ... COMPARE to wikipedia below 
# https://en.wikipedia.org/wiki/UTF-8#Encoding
#  x = u.pad8()
# str.splitN(x$binary.codepoint, n=4)


u.plus2int = function(...)
	{
	x = prep.dots(... , default=c("U+0024", "U+00A3", "U+0939", "U+20AC", "U+D55C", "U+10348") );
	uinfo = list.pair(str.explode("U+", toupper(x))); 
	utf   = list.getElements(uinfo, 2); 
	base2int(utf, base=16);
	}
	
# u = c("U+0024", "U+00A3", "U+0939", "U+20AC", "U+D55C", "U+10348");
# i = u.plus2int();
# b = int2base(i, base=2);



u.convert = function(..., from="U+", to="\\u")
	{
	x = prep.dots(...);
dput(x);
	# convert everthing to INTEGER first pass
	FROM = prep.arg(from, n=2, case="upper", keep="-");
	TO = prep.arg(to, n=2, case="upper", keep="-");
	uint = switch(FROM,					  			
					  "U+"	= {uinfo = list.pair(str.explode("U+", x)); utf   = list.getElements(uinfo, 2); base2int(utf, base=16);},
					  "\\U"	= utf8ToInt(x),	
					  "\\X"	= utf8ToInt(x),	  # charCode (iconv) ?
					  
				x											# DEFAULT
				);
				
	# https://en.wikipedia.org/wiki/UTF-8#Encoding
# > euro = u.toSymbol("U+20AC")
# > strlen(euro)  # 1 
# 010 0100 ... # 8364
# €	U+20AC	0010 0000 1010 1100	    11100010 10000010 10101100	E2 82 AC
# > int2base(8364, base=2, to.length=16);  # lenght depends ...
# [1] "10000010101100" ... "0010000010101100"
# bin = "0010000010101100";  bin-code vs bin-utf ...  
# tmp = str.splitN(bin, n=6); # from end ...


	# convert everything from INTEGER on second pass 
	
	out = switch(TO,					  			
					  "D" 	= deg,
					  "R"	= (pi/180) * deg,	
					  "G"  	= deg * 10/9,
					  "M"	= deg/60, 	# arc-min
					  "S"	= deg/3600,	# arc-sec
					  "T"	= deg/360,	# turns 					  
				deg											# DEFAULT
				);
	math.cleanup( out );
	}
	


u.fromEscape = function(...)
	{
	u = prep.dots(..., default="\u22ef");
	utf8ToInt(u);
	}
	


u.num2str = function(..., to.length=4, pre="U+")
	{
	num = prep.dots(..., default=59912);
	hex = int2base(num, base=16, to.length=to.length);
	paste0(pre,hex, collapse=""); 
	}
	
u.toEscape = function()
	{
	a.int = charCodeAt(a, 1);
		a.hex = int2base(a.int, base=16, to.length=4);
		res = paste0("\"", "\\u" , a.hex , "\"");	
		
	
	
	}


u.toNum = function(..., pre="U+")
	{ # 1F925
	str = prep.dots(..., default="U+22EF");
	uinfo = list.pair(str.explode(pre, str));
	utf   = list.getElements(uinfo, 2);
	# as.integer(as.hexmode(utf));  # should I just do int2base ?
	base2int(utf, base=16);
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
