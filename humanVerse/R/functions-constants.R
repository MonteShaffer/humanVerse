
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
	
	
	SYSTEM = list(  
						SEEK_END = "end",
						SEEK_START = "start",
						SEEK_CURRENT = "current"
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
	if(!is.null(val)) { warning("Constant has already been defined as "); return(NULL); }
	
	# if(global) { gggassign(key,VAL); }
	# if(humanVerse) { }
	memory.set(key, "-CONSTANTS-", VAL);
	invisible(VAL);
	}
	

# works 
# π = 3.1415926535897932384626
# .onLoad = function (libname, pkgname) {
 

	