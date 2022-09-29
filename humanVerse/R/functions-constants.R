
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
	# UTF8 finder  # https://shapecatcher.com/
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
					# # PUA range is U+E000...U+F8FF ... EA60 ... EA5F
					#                57344 ... 63743
					# -INF 59999  +INF 60000
					# 98955999911209916464644119185682 ... 46999921285999993996122
					#      559999112					             599999399
					#     1550059912								1550059912
					# OMG ...
					# 101323206354325344240488600003090822619003 ... 7299600005402969139086326671417923649756297192502128839909708484 
					#						  1550059912 ... 600003090
					#   1550059912 ... 600005402
					# is that my PSEUDO mensa number 
					.U_NULL		= "U+EA07",   # 59911
					.U_NA		= "U+EA08",   # 59912
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
						
						EXT = ".",  # file extension 
						DOT = ".",
						LDOTS = "...", 
						DOUBLE_SLASH = "//",
						DIR_WINDOZE = "\\",
						DIR_LINUX 	= "/",
						SLASH = "/", 
						DIR_SEPARATOR = "/", 	# seems to work everywhere, but could update at load time based on OS 
						ATTRIBUTE_KEY = "@",   # slot operator, ridiculous
						COMMENT_CHAR = "#",
						COMMENT = "#",
						MULTILINE_START = "/*",
						MULTILINE_END = "*/",
						VSEP = "\\./",
						COMMA = ",",	
						FAKE_COMMA = "`[comma]`",
						PIPE = "|",
						EMPTY = "",
						EMTPY = "",
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
						FAKE_NULL = "`[null]`",
						# use PI for NULL, NA use MT 
						# 227878880988633599116081923.863353016082155991131414420509144729353502223
						NULL_NUM = 227878880988633599116081923.863353016082155991131414420509144729353502223,
						# CURRENTLY EQUAL ... NULL_NUM == NULL_INT
						# TOO BIG ... can't cast as.integer/as.numeric 
						## too big even to cast to INTEGER ... 2^31 
						## can't manipulate very EASILY ...
						# NULL_INT = 227878880988633599116081923,
						#                       
						NULL_INT = 863353016082155991131414420509144729353502223,
						#                      1559911314
						#                      1550059912
						# maybe swithc NULL / INF ... 57... don't like 7 
						NULL_CHAR = "U+EA07",  # PUA ... do I need NULL_FN? [BIGFORK]
						FAKE_NA = "`[NA]`",
						# NA_NUM == NA_INT ... FALSE, GOOD 
						#NA_NUM = 59912001001550.4068925815,
						NA_NUM = 1550059912.4068925815,
						# 1550059912 is rather random, not found in 1 MILLION of PI 
						NA_INT = 1550059912,
						# > NA_NUM == NA_INT ... [1] FALSE
						# > as.integer(NA_NUM) == NA_INT [1] TRUE

						NA_CHAR = "U+EA08",	# could do same for NA ... 
						# what about +/- INF 
						# https://www.facade.com/legacy/amiinpi/?thenum=57351
						# search my pi.dec.txt DATA file 
						# WHY IS NOTEPAD++ not finidng 57351
						# 22199658207573513157075923.592169694573513122969929
						#                                    1550059912
						# 2350141441973568548161361157352552133.62258218781200116285735213380860436525201235
						# playing off of JAMES BOND 007 .... 
						# maybe just use  60000 ... use one number for INF, make - for -INF
						# "U+EA60" 
						#.U_INF_		= "U+E007",   # 57351	# NEGATIVE INFINITY
						#.U_INF		= "U+E008"    # 57352  
						
						OP = "(", 			# OPEN_PARENTHESSDFJlkd
						CP = ")",
						# alex amazon buy now ... lol 
						
						"\\./" 		= list(
										
										PRIME_AXIOM = "There are '10' types of people in this world: those that understand mathematics *and* those that do not.",
										PRIME_DIRECTIVE = "As a semi-sentient being, I am 'intrisically motivated' to *share* =freely= any perspective, understanding, knowledge, experience, or wisdom that I believe I possess.",	
										PRIME_GOAL = "To advance (or restore) the human conditon for the benefit of ALL.",
										PRIME_KISS = "Transcending simplicity on THIS side of complexity to reveal simplicity on the OTHER side of complexity. [Break on through to the OTHER side.]",
										PRIME_DICHO = "¡Del dicho al hecho hay mucho estrecho!",
										PRIME_ADMONICION = "¡Oh, que tenga sabiduría! ¿Qué más puedo decir?",
										PRIME_QUESTION = "What are you listening for?", 
										PRIME_ANSWER = 5,
										PRIME_SEPARATOR = "\\./",
										# LIST (this.key) ... VSEP ... key from above ...
										# maybe make a smart.list function that does this for a list ... 
										# smart.list = (...) ... 
										# allow trailing comma --> is that possible?
										# if we trap it before it throws an error 
										# looks like we can TRAP ... 
										# check.type( x=3,y=4, ) 

										
										# https://www.youtube.com/watch?v=Tktvu_J1grI
										# https://hinduism.stackexchange.com/questions/6777/was-value-of-pi-really-defined-in-a-sloka
										# https://web.archive.org/web/20041114092424/http://www.pa.uky.edu:80/~ameya/pi.html
										# 
										PRIME_SHLOKA = "gopi bhagya madhuvrata, srngiso dadhi sandhiga, khala jivita khatava, gala hala rasandara",
										PRIME_TZU = "Silence is strength.",
										PRIME_CHANGE = "You and the river are always changing.  For the better.", 
										PRIME_TREEBEARD = "Break the damns.  Release the rivers.  Let the salmon run home so we can 'fillet' them.",
										PRIME_LEGEND = "{an ee} {na fee} {a la} {al ma} {ee ma} {mer men}",  # 12, not 10? ... linguistic lessons WE COULD HAVE LEARNED about language de-evolution (degression)
										PRIME_GALILEO = "E pur, si muove",
										PRIME_IDIOT = c("ID-10-T error", "SCSI buffer underrun"),
										PRIME_READING = "Combine letters to form words.  Combine words to form sentences.  Top to bottom.  Left to right.",
										PRIME_SPEAKING = "Combine sounds to form words.  Combine words to form ideas.  Beginning to end.",
										PRIME_BEGINNING = "{ab} {ra} {ωet}",
										PRIME_WHAT = "sais quoi", # {se} {kwa}
										# http://billposer.org/Software/tamilconverters.html
										# where is a transliterator ...
										# I want it as simple ascii SOUNDS ... 
										PRIME_TAMIL = "எல்லாம் நல்லதே",
										# {ee lam} {na la de} ? 
										# ee as in SEE 
										# de as in DAY (spanish e)
										# by default, (a e i o u) are SPANISH 
										# lam is CORRECT 
										# lAm is LAMB (open the mouth)
										# keep it simple, based on common language, use AVERAGE to advtange ... 
										# https://phoible.org/parameters
										# ONE SOUND, one letter 
										# che ==> чe ... 
										# loco ==> lo ko
										# DOES the "c" disappear or use as s (RUSSIAN)
										# c => C ... good scaling 
										# "s" looks like a 5 ... maybe kill it ...
										# salaam vs shalaam ...
										# z
										# sha ... ωa ... 
										# CASE shouldn't change symbol, just scale it
										# maybe use a BOLD/BRIGHT feature 
										# s => S ... GOOD 
										# a => A ... BAD 
										# j => J ... good 
										# 1,1,1 on 9-numerology above ...
										# avoid dislexisak possibilities 
										# if (b) ... no (d)
										# if (d) ... no (b) 
										# avoid COMBOS that are not possible in child development or for some other linguists
										# [th]ree ... not a good combo ... [t]ree easier ... RU: tree ... ES: tres ... ee is easier to recognize that SPANISH "i" even though that is the sound ... acute/grave ...
										# https://www.quora.com/What-is-the-difference-between-e-%C3%A8-%C3%A9-%C3%AA-%C3%AB-%C4%93-%C4%97-and-%C4%99
										# e_ as ee? where _ is under the e (not above) [e over _ in latex?] ... 
										# allow accents [syllabization/rhythm] above, symbology below ... wouldn't work for j ... how to address ___ underline ?
										# 
										# have optional sounds
										# [r]ruffles have [r]ridges ... you can roll the r if you want ... your CHOICE ...
										# 
										# [a] vs [ɑ] ... typing vs handwriting 
										# double-storey a and single-storey ɑ. 
										# letter users define sha ... 0x1d222 or 0x1d788 or  0x3c9 ... doesn't work ("U+3c9") or ("U+03c9") ... has to be ("U+03C9")
										
										PRIME_NAMETAG = "Welcome to the {humanVerse}!"
										
											)
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
		
	


	
	# minvisible(list(
					# "NUMBERS" 	= NUMBERS, 
					# "STRINGS" 	= STRINGS,
					# "U"			= U,
					# "SYSTEM" 	= SYSTEM,
					# "TIME" 		= TIME,
					# "TEMP" 		= TEMP,
					# "CIRCLE" 	= CIRCLE
					# ), display=none);
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
 

	