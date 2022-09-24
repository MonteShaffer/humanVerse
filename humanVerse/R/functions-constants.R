
constants.default = function()
	{
	
	
	
	
	}

constants.init = function(globalize.ALL = TRUE)
	{
	
	
	}
	
	
constants.get = function(KEY)
	{
	key = check.string(KEY);
	memory.get(key, "-CONSTANTS-", VAL);	
	}
	
	
constants.define = function(KEY, VAL, global=FALSE)
	{
	key = check.string(KEY);
	
	# a constant, if SET doesn't change ... DEFINE_CONSTANT not const 
	val = constants.get(key);
	if(!is.null(val)) { warning("Constant has already been defined as "); return(NULL); }
	
	if(global) { gggassign(key,VAL); }
	memory.set(key, "-CONSTANTS-", VAL);
	invisible(VAL);
	}
	

#  y = [Math.PI, Math.E, Math.LN10, Math.LN2, Math.LOG10E, Math.LOG2E, Math.SQRT2, Math.SQRT1_2]

# [3.141592653589793, 2.718281828459045, 2.302585092994046, 0.6931471805599453, 0.4342944819032518, 1.4426950408889634, 1.4142135623730951, 0.7071067811865476]
 
 
PI		= 3.1415926535897932384626
PHI		= 1.6180339887498948482045
E		= 2.718281828459045
π		= 3.1415926535897932384626433832795028841971693993751058209749445


# https://oeis.org/A002392
# https://oeis.org/ ... useful to lookup constant with URL 
LN10	= 2.30258509299404568401799145
LN2  	= 0.69314718055994530941723212145
LOG10E	= 0.43429448190325182765112891891660508229439700580366656611445
LOG2E	= 1.442695040888963407359924681001892137426645
SQRT2	= "NO 45" ??? 
SQRT3	= ""
SQRT3_2 = "";

Math.LOG10E, Math.LOG2E, Math.SQRT2, Math.SQRT1_2]
 1.4426950408889634, 1.4142135623730951, 0.7071067811865476]
 
 
# works 
# π = 3.1415926535897932384626

PHI = (1+sqrt(5))/2;
 
B64 	= "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";
B64v 	= str.explode("", B64);

BXX 	= c(as.character(0:9), LETTERS[1:22]);  # bounded for 2,32 
BXXv 	= str.explode("", BXX);	

Bits64	= int2base(0:63, base=2);
	