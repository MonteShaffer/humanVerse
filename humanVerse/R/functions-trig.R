
####################### REGULAR TRIG ###########################
math.sin = function(...)
	{
	# maybe do better with fractional components
	x = prep.dots(..., default=pi);
	math.cleanup( sin(x) ); 
	}
	
math.cos = function(...)
	{
	x = prep.dots(..., default=pi);
	math.cleanup( cos(x) );
	}
	
math.tan = function(...)
	{
	x = prep.dots(..., default=pi);
	math.cleanup( tan(x) );
	}

cotan 		= function(...) { 1/math.tan(...); }
cosecant 	= function(...) { 1/math.sin(...); } 	
secant 		= function(...) { 1/math.cos(...); } 
	
	
math.asin = function(...)
	{
	x = prep.dots(..., default=pi);
	math.cleanup( asin(x) );
	}

arcsin = function() {}
arcsin = math.asin;	

	
math.acos = function(...)
	{
	# maybe do better with fractional components
	x = prep.dots(...);
	math.cleanup( acos(x) );
	}

arccos = function() {}
arccos = math.acos;

	
	
math.atan = function(...)
	{
	# maybe do better with fractional components
	x = prep.dots(...);
	math.cleanup( atan(x) );
	}

arctan = function() {}
arctan = math.atan;


# no atan2h?
math.atan2 = function(y, x)
	{
	math.cleanup( atan(y, x) );
	}

arctan2 = function() {}
arctan2 = math.atan2;


####################### HYPERBOLIC TRIG ###########################


math.sinh = function(...)
	{
	x = prep.dots(...);
	math.cleanup( sinh(x) );
	}
	
math.cosh = function(...)
	{
	x = prep.dots(...);
	math.cleanup( cosh(x) );
	}
	
math.tanh = function(...)
	{
	x = prep.dots(...);
	math.cleanup( tanh(x) );
	}

cotanh 		= function(...) { 1/math.tanh(...); }
cosecanth 	= function(...) { 1/math.sinh(...); } 	
secanth 	= function(...) { 1/math.cosh(...); } 
	
	
math.asinh = function(...)
	{
	x = prep.dots(...);
	math.cleanup( asinh(x) );
	}

arcsinh = function() {}
arcsinh = math.asinh;	

	
math.acosh = function(...)
	{
	x = prep.dots(...);
	math.cleanup( acosh(x) );
	}

arccosh = function() {}
arccosh = math.acosh;

	
	
math.atanh = function(...)
	{
	x = prep.dots(...);
	math.cleanup( atanh(x) );
	}

arctanh = function() {}
arctanh = math.atanh;

