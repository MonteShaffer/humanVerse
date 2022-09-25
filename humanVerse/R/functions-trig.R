


trig.setMode = function(mode="radians")
	{
	memory.set("-RADIANS_MODE-", "SYSTEM", mode);	
	}
	
	
trig.toMode = function(x, mode="radians", direction="from")
	{
	mo = prep.arg(mode, n=2);	
	DIR = prep.arg(direction, n=1, case="upper");
	
	mem = memory.get("-RADIANS_MODE-", "SYSTEM");
	
	MODE = switch(mo,					  			
					  "de"	= "degrees",
					  "ra"	= "radians",	
					  "go"	= "gon",	
					  "mi"	= "min-arc", 	# arc-min
					  "se"	= "sec-arc",	# arc-sec
					  "tu"	= "turns",		# turns 
				"memory"					# DEFAULT
				);
	if(MODE == "memory")
		{		
		if(is.null(mem)) { MODE = "radians"; } else { MODE = mem; }
		}

	if(mem != MODE) { trig.setMode(MODE); }
	if(MODE == "radians") { return(x); }
		# sin needs "f" ... asin needs "t"
	if(DIR == "f") { angle.convert(x, from=MODE, to="radians"); } 
		else { angle.convert(x, from="radians", to=MODE); } 
	}

####################### REGULAR TRIG ###########################
math.sin = function(..., mode="memory")
	{
	# maybe do better with fractional components
	x = prep.dots(..., default=pi);
	x = trig.toMode(x, mode);  	# by default the mode is "radians" 
								# if I enter angles in degrees, 
								# this will auto-convert
	math.cleanup( sin(x) );
	}
	
math.cos = function(..., mode="memory")
	{
	x = prep.dots(..., default=pi);
	x = trig.toMode(x, mode);
	math.cleanup( cos(x) );
	}
	
math.tan = function(..., mode="memory")
	{
	x = prep.dots(..., default=pi);
	x = trig.toMode(x, mode);
	math.cleanup( tan(x) );
	}

cotan 		= function(...) { 1/math.tan(...); }
cosecant 	= function(...) { 1/math.sin(...); } 	
secant 		= function(...) { 1/math.cos(...); } 
	
	
math.asin = function(..., mode="memory")
	{
	x = prep.dots(..., default=1);
	x = trig.toMode(x, mode, "to");
	math.cleanup( asin(x) );
	}

arcsin = function() {}
arcsin = math.asin;	

	
math.acos = function(..., mode="memory")
	{
	x = prep.dots(..., default=1);
	x = trig.toMode(x, mode, "to");
	math.cleanup( acos(x) );
	}

arccos = function() {}
arccos = math.acos;

	
	
math.atan = function(..., mode="memory")
	{
	x = prep.dots(..., default=1);
	x = trig.toMode(x, mode, "to");
	math.cleanup( atan(x) );
	}

arctan = function() {}
arctan = math.atan;


# no atan2h?
math.atan2 = function(y, x, mode="memory")
	{
	math.cleanup( atan2(y, x) );
	}

arctan2 = function() {}
arctan2 = math.atan2;


####################### HYPERBOLIC TRIG ###########################


math.sinh = function(..., mode="memory")
	{
	x = prep.dots(..., default=1);
	x = trig.toMode(x, mode);
	math.cleanup( sinh(x) );
	}
	
math.cosh = function(..., mode="memory")
	{
	x = prep.dots(..., default=1);
	x = trig.toMode(x, mode);
	math.cleanup( cosh(x) );
	}
	
math.tanh = function(..., mode="memory")
	{
	x = prep.dots(..., default=1);
	x = trig.toMode(x, mode);
	math.cleanup( tanh(x) );
	}

cotanh 		= function(...) { 1/math.tanh(...); }
cosecanth 	= function(...) { 1/math.sinh(...); } 	
secanth 	= function(...) { 1/math.cosh(...); } 
	
	
math.asinh = function(..., mode="memory")
	{
	x = prep.dots(..., default=1);
	x = trig.toMode(x, mode, "to");
	math.cleanup( asinh(x) );
	}

arcsinh = function() {}
arcsinh = math.asinh;	

	
math.acosh = function(..., mode="memory")
	{
	x = prep.dots(..., default=1);
	x = trig.toMode(x, mode, "to");
	math.cleanup( acosh(x) );
	}

arccosh = function() {}
arccosh = math.acosh;

	
	
math.atanh = function(..., mode="memory")
	{
	x = prep.dots(..., default=1);
	x = trig.toMode(x, mode, "to");
	math.cleanup( atanh(x) );
	}

arctanh = function() {}
arctanh = math.atanh;

