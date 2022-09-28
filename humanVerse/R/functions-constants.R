
## JAVASCRIPT 
#  y = [Math.PI, Math.E, Math.LN10, Math.LN2, Math.LOG10E, Math.LOG2E, Math.SQRT2, Math.SQRT1_2]

# https://stackoverflow.com/questions/12498347/why-does-ls-in-r-not-show-global-variables
# https://stackoverflow.com/questions/49056642/how-to-make-variable-available-to-namespace-at-loading-time
##  f <- sys.function(-1);     ns <- environment(f)
##  ?assignInMyNamespace
# where is pi ... can I store constants like that, not in ls() but global?
# Do note that these are not global, just exported from the base namespace and hence accessible from the global workspace.
# https://oeis.org/A002392
# https://oeis.org/ ... useful to lookup constant with URL 
# http://numbers.computation.free.fr/Constants/Sqrt2/sqrt2.html
# https://apod.nasa.gov/htmltest/gifcity/sqrt2.1mil
# // https://www.wolframalpha.com/input/?i=sin%2845*PI%2F180%29



constants.default = function(default.numbers = TRUE, namespace="humanVerse", which="ALL")
	{
	# TODO which ... ALL, NUMBERS, STRING, TIME, TEMP 
	
	NUMBERS = list(
					PI			= "3.1415926535897932384626",
					PHI			= "1.6180339887498948482045",
					E			= "2.718281828459045",
					π			= "3.1415926535897932384626433832795028841971693993751058209749445",
					LN10		= "2.30258509299404568401799145",
					LN2  		= "0.69314718055994530941723212145",
					LOG10E		= "0.43429448190325182765112891891660508229439700580366656611445",
					LOG2E		= "1.442695040888963407359924681001892137426645",
					SQRT2		= "1.4142135623730950488016887242096980785696718753769480731766797379907324784621070388503875343276415727350138462309122970249248360558507372126441214970999358314132226659275055927557999505011527820605714701095599716059702745",
					invSQRT2	= "0.70710678118654752440084436210484903928483593768847403658833986899536623923105351942519376716382078636750692311545",
					SQRT3		= "1.73205080756887729352744634150587236694280525381038062805580697945",
					invSQRT3	= "0.5773502691896257645",
					SQRT3_2 	= "0.86602540378443864676372317075293618347140262690519031402790348972596650845",
					SQRT5		= "2.236067977499789696409173668731276235440618359611525724270897245",
					invSQRT5	= "0.44721359549995793928183473374625524708812367192230514485417944908210418512756097988288288167575645",
					ANSWER.is	= "5"
					);

	n = length(NUMBERS);
	NUM.names = names(NUMBERS);
	for(i in 1:n)
		{
		KEY = NUM.names[i];
		VAL = NUMBERS[[i]];
		if(default.numbers) { VAL = as.numeric(VAL); }
		##assignInNamespace(KEY, VAL, ns=namespace); # need library
		gggassign(KEY,VAL);
		}


	B64 		= "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";
	BXXv 		= c(as.character(0:9), LETTERS[1:22]);
	

	STRINGS = list(
					B64			= B64,
					B64v 		= str.explode("", B64), 

					BXX 		= paste0(BXXv, collapse=""),  # bounded for 2,32 
					BXXv 		= BXXv,

					Bits64		= int2base(0:63, base=2),
					
					SI_PREFIX 	= num.SIunits("regular")
					);
				
	s = length(STRINGS);
	STR.names = names(STRINGS);
	for(i in 1:s)
		{
		KEY = STR.names[i];
		VAL = STRINGS[[i]];
		##assignInNamespace(KEY, VAL, ns=namespace);
		gggassign(KEY,VAL);
		}
	
	
	# create unicode library ... make searchable with tag adds 
	# have grid-search using MNIST tech to find similar items...
	# allow non-standard elements into search space (e.g., old proto-sinaitci or egyptian that wasn't approved)
	# #https://en.wikipedia.org/wiki/Proto-Sinaitic_script
	# why we like lower-case omega
	# https://en.wikipedia.org/wiki/Proto-Sinaitic_script#/media/File:Proto-semiticS-01.svg
	# https://en.wikipedia.org/wiki/Proto-Sinaitic_script#/media/File:Proto-semiticT-01.svg
	U = list(
					U_TIMES		= "U+00D7",
					U_CHI 		= "U+1D6D8", 
					U_BETA		= "U+03B2",
					U_GAMMA		= "U+0393",
					U_THETA		= "U+1D6C9",
					U_ALPHA		= "U+03B1",
					U_EPSILON	= "U+03B5",
					U_SQRT		= "U+221A",
					U_ROOT2		= "U+221A",
					U_ROOT3		= "U+221B",
					U_ROOT4		= "U+221C",
				U_PROPORTIONAL	= "U+221D",
					U_INFINITY	= "U+221E",
				# https://www.compart.com/en/unicode/U+2245
				U_APPROXIMATELY	= "U+2245",
					
					U_MATH		= c("U+002B", "U+2212", "U+00D7", "U+00F7"),
					U_SNAIL		= "U+1F40C",
					U_CDOTS		= "U+22EF",
					U_LYING		= "U+1F925", 
					U_ARROW_UP	= "U+2191",
				U_ARROW_LEFT	= "U+27FD",
					U_EURO		= "U+20AC",
					U_POUND		= "U+00A3",
					# https://en.wikipedia.org/wiki/Astrological_symbols#Signs_of_the_zodiac
					U_ZODIAC = list("Aries" = "U+2648", "Taurus" = "U+2649", "Gemini" = "U+264A", "Cancer" = "U+264B", "Leo" = "U+264C", "Virgo" = "U+264D", "Libra" = "U+264E", "Scorpio" = "U+264F", "Sagittarius" = "U+2650", "Capricorn" = "U+2651", "Aquarius" = "U+2652", "Pisces" = "U+2653"),
				U_ZODIAC_CHINA = list(""), # 12 signs, 5 elements (yin/yang)
					# https://en.wikipedia.org/wiki/Lichun
					# DEMO "wiki" parser on that page, grab other planets 
					# https://pages.ucsd.edu/~dkjordan/resources/unicodemaker.html
					# https://hanzicraft.com/character/%E7%89%9B%E4%B8%91
					U_STAIR		= "U+1328D", # Egyptian  ... sort by ASC/DESC
					U_EGY_N		= "U+13216",  # water (not m?)
					# https://en.wikipedia.org/wiki/Mem
					U_EGY_FIVE1 = "U+132B0",
					U_EGY_FIVE2 = "U+132B1",
					U_EGY_ONE	= "U+133FA",
			U_EGY_NUMBER_LINE 	= "U+13416",
					U_EGY_CROSS = "U+133F6",
					U_EGY_TAV	= "U+13384",  # intersect/union member of in this group 
				U_EGY_THIRDS 	= "U+13279", # third part
				U_PERSIAN_RUG 	= "U+13254", 
					U_STAR		= "U+131FD", # http://www.alanwood.net/unicode/egyptian-hieroglyphs.html
					U_EYE		= "U+13080", # Egyptian Fractions (how to reflect ... U+XX to mirror reflect?)
					U_PHALLUS	= "U+130B8",		# Egyptian 
					U_OX		= "U+130FE",		# OLD HEBREW 
					U_CROSS		= "U+5341",		# CHINESE "10"
					# different? https://www.fileformat.info/info/unicode/char/571f/browsertest.htm
					U_SCHOLAR	= "U+58EB",
					U_EARTH		= "U+571F",
					
					# https://www.compart.com/en/unicode/block/U+0080
					U_N_QUESTION = "U+00BF", 		# upside-down ?
					U_N_EXCLAMATION = "U+00A1", 	# upside-down !
					
					U_ARAB_NO	= "U+10A7F",
				U_CHINA_SUNDAY 	= "U+65E5", 
					
					
					# https://en.wikipedia.org/wiki/Proto-Sinaitic_script
					# https://codepoints.net/U+10A67?lang=en
					# OX ... 
					U_SUP_09	= c("U+2070", "U+00B9", "U+00B2", "U+00B3", "U+2074", "U+2075", "U+2076", "U+2077", "U+2078", "U+2079"),
					# maybe do letters/LETTERS in SUPer and SUBscripts
					U_POWER		= c("ᵉ","ᴱ", "⁺", "⁻"),  # what are codes? no SUPERSCRIPT TIMES ???
					
					# https://www.w3schools.com/charsets/ref_utf_cyrillic.asp
					U_RU_R_ 	= "U+042F",  # cyrillic omeaga, olala
					
					
					# # https://www.unicode.org/faq/private_use.html
					# R doesn't allow null \x00 in a string. 
					# challenge to do b64 with that limitation	
					# # PUA range is U+E000...U+F8FF
					.U_NULL		= "U+EA08",   # 59912
					.U_INF_		= "U+E007",   # 57351	# NEGATIVE INFINITY
					.U_INF		= "U+E008"    # 57352  
					
					
					
					
					);
				
	u = length(U);
	U.names = names(U);
	for(i in 1:u)
		{
		KEY = U.names[i];
		VAL = U[[i]];
		##assignInNamespace(KEY, VAL, ns=namespace);
		gggassign(KEY,VAL);
		}
	
	SYSTEM = list(  
						SEEK_END = "end",
						SEEK_START = "start",
						SEEK_CURRENT = "current",
						BUFFER = 1024,
						
						OPTIMUS_PRIME = 1,
						CIPHER_IN_SNOW= 0,
						ZERO=0,
						ONE = 1,
						COMMENT = "#",
						MULTILINE_START = "/*",
						MULTILINE_END = "*/",
						VSEP = "\\./",
						COMMA = ",",	
						FAKE_COMMA = "`[comma]`",
						PIPE = "|",
						FAKE_PIPE = "`[pipe]`",
						SINGLE_QUOTE = "'",
						SQ = "'",
						ESC_SQ = '\'',
						FAKE_SQ = "`[sq]`",
						DOUBLE_QUOTE = '"',
						DQ = '"',
						ESC_DQ = "\"",
						FAKE_DQ = "`[dq]`",
						BACKSLASH = "\\",
						IN_STRING = FALSE,		# these will change often in parser
						STRING_TYPE = NULL,		# pseudo-constants, as in C++ 'const' definition?
						TAB = "\t",
						FAKE_TAB = "`[t]`",
						NEWLINE = "\n",
						FAKE_NEWLINE = "`[n]`",
						EOL = "\r\n",
						FAKE_EOL = "`[rn]`",
						
						OP = "(", 			# OPEN_PARENTHESSDFJlkd
						CP = ")"
					);
					
					
				
				
				
				
	s = length(SYSTEM);
	SYS.names = names(SYSTEM);
	for(i in 1:s)
		{
		KEY = SYS.names[i];
		VAL = SYSTEM[[i]];
		##assignInNamespace(KEY, VAL, ns=namespace);
		gggassign(KEY,VAL);
		}
	

	
	TIME = list(
				SECS_PER_SEC	= 1,
				SECS_PER_MIN 	= 60,
				SECS_PER_HOUR 	= 60*60,
				SECS_PER_DAY 	= 60*60*24,  	# 86000
		SECS_PER_SIDEREAL_SEC	= 0.9972696,
		SECS_PER_SIDEREAL_MIN	= 59.83617,  	# 0.9972696 * 60 = 59.83618
		SECS_PER_SIDEREAL_HOUR	= 3590.170,		# SIDEREAL
		SECS_PER_SIDEREAL_DAY	= 86164.09,
		SECS_PER_SIDEREAL_YEAR	= 31558150,  	# is this true or 365 days?  365.2536?
	
		SECS_PER_TROPICAL_YEAR	= 31556930   	# TROPICAL
									# This looks true-ish to MEAN TROP YEAR 
									# 31556930 / 60 / 60 / 24 = 365.2422
	
				);
	
		
		
	t = length(TIME);
	TIM.names = names(TIME);
	for(i in 1:t)
		{
		KEY = TIM.names[i];
		VAL = TIME[[i]];
		##assignInNamespace(KEY, VAL, ns=namespace);
		gggassign(KEY,VAL); 
		}

	
	TEMP = list(
				ABS_ZERO_F 		= -459.67,
				ABS_ZERO_C 		= -273.15, 
				ABS_ZERO_K 		= 0,
				ABS_ZERO_R 		= -459.67, 
				
				C_FREEZING 		= 0.01,
				C_BOILING 		= 99.9839,
				C_MAGIC 		= 4  # 4 degrees C, 40 degrees F ... water magic (ice floats)
				
				);
	
	t = length(TEMP);
	TEM.names = names(TEMP);
	for(i in 1:t)
		{
		KEY = TEM.names[i];
		VAL = TEMP[[i]];
		##assignInNamespace(KEY, VAL, ns=namespace);
		gggassign(KEY,VAL);
		}
	
	
		CIRCLE = list(
				TURNS_PER_TURN	= 1,
			DEGREES_PER_TURN 	= 360,
			RADIANS_PER_TURN 	= 2*pi, 
			GONS_PER_TURN 		= 400,  # 'gons' by creators, others GRADS?
			MINUTES_PER_TURN 	= 360*60, 
			SECONDS_PER_TURN 	= 360*60*60
				);
	
		
	c = length(CIRCLE);
	CIR.names = names(CIRCLE);
	for(i in 1:c)
		{
		KEY = CIR.names[i];
		VAL = CIRCLE[[i]];
		##assignInNamespace(KEY, VAL, ns=namespace);
		gggassign(KEY,VAL);
		}
		
	


	
	minvisible(list(
					"NUMBERS" 	= NUMBERS, 
					"STRINGS" 	= STRINGS,
					"U"			= U,
					"SYSTEM" 	= SYSTEM,
					"TIME" 		= TIME,
					"TEMP" 		= TEMP,
					"CIRCLE" 	= CIRCLE
					), display=none);
	}



constants.init = function(globalize.ALL = TRUE)
	{
	
	
	}
	
	
constants.get = function(KEY)
	{
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.KEY = check.type(KEY);
	if(!ct.KEY || !is.character(KEY))	
		{ key = deparse(substitute(key)); } else { key = KEY; }
##########################################################

	memory.get(key, "-CONSTANTS-", VAL);	
	}
	
	
constants.define = function(KEY, VAL, namespace="humanVerse")
	{
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.KEY = check.type(KEY);
	if(!ct.KEY || !is.character(KEY))	
		{ key = deparse(substitute(key)); } else { key = KEY; }
##########################################################
	
	# a constant, if SET doesn't change ... DEFINE_CONSTANT not const 
	val = constants.get(key);
	# obviously in R, they can just change the value .... FAKE_CONSTANTS
	if(!is.null(val)) { warning("Constant has already been defined as "); return(NULL); }
	
	# if(global) { gggassign(key,VAL); }
	# if(humanVerse) { }
	memory.set(key, "-CONSTANTS-", VAL);
	invisible(VAL);
	}
	

# works 
# π = 3.1415926535897932384626
# .onLoad = function (libname, pkgname) {
 

	