
constants.default = function()
	{
	
	
	
	
	}

constants.init = function(globalize.ALL = TRUE)
	{
	
	
	}
	
	
constants.define = function(KEY, VAL)
	{
	key = check.string(KEY);
	
	}
	
	
# identical(0xFL, as.integer(15))
# 0xA0 ... 0xAFL
base.chars = c(as.character(0:9), LETTERS[1:22]);

B64 	= "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";
B64v 	= str.explode("", B64);

BXX 	= c(as.character(0:9), LETTERS[1:22]);  # bounded for 2,32 
BXXv 	= str.explode("", BXX);	

Bits64	= int2base(0:63, base=2);
	