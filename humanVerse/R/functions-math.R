
check.base = function(base = 10)
	{
	base = as.integer(base);
	if(base > 36 || base < 2) { stop("'base' must be between 2 and 36."); }
	base; 
	}
	
cleanup.base = function(xstr)
	{
	xstr = as.character(xstr);
	# left trim any non-elements ... maybe REGEX replace 0-9A-V
	# HEX 
	xstr = str.trim(str.replace(c("#","0x","0X"), "", xstr) );
	xstr;
	}

# fromBase to an INTEGER 
fromBase = function(..., base=10)
	{
	xstr = prep.dots(...);
#dput(xstr);
	xstr = cleanup.base(xstr);
# dput(xstr);
	b = check.base(base);
	# lower case, I guess 
	base.chars = c(as.character(0:9), LETTERS[1:22]);
	
	N = length(xstr);
	res = integer(N);
	for(i in 1:N)
		{
		xstri = toupper(xstr[i]);
		n = str.len(xstri);
		re = 0; power = 0;
		for(j in n:1)
			{
			xj = charAt(xstri,j);
			xn = which(base.chars == xj) - 1;			
			add = xn * b^power;
			re = re + add;
			power = 1 + power;
			}
		res[i] = re;
		}
	res;	
	}
	
base.from = fromBase;	 
	

# an INTEGER to a base as a string 
# cpp_int2base(cpp_base2int(c("abc", "def"))) ... in primes.cpp for now ... 
toBase = function(..., base=10, to.length=NULL)
	{
	x = prep.dots(...);
#dput(x);
	b = check.base(base);
	base.chars = c(as.character(0:9), LETTERS[1:22]);
	N = length(x);
	res = character(N);
	for(i in 1:N)
		{
		xi 	= x[i];
		n	= ceiling(log(xi, b));
		vec = NULL;
		val = xi;
		
		## C++ 
		# while(num > 0)
		# {
		# res = d[num % base] + res;
		# num /= base;
		# }
      
		while(n >= 0)
			{
			rem = val %/% b^n;
			val = val - rem * b^n;
			vec = c(vec, rem);
			n   = n - 1;
			}
		
		if(!is.null(vec))
			{
			# truncate leading zeros ...
			while(vec[1] == 0 & length(vec) > 1)
				{
				vec = vec[-1];
				}
			} else { vec = 0; }
		# zero offset
		res[i] = paste0(base.chars[vec+1], collapse="");		
		}
	# str.pad("LEFT");
	if(!is.null(to.length)) { res = str.pad(res, to.length, "0", "LEFT"); }
	res;
	}

base.to = toBase;

base.convert = function(..., from="binary", to="octal", to.length=NULL)
	{
	x = prep.dots(...);
	# first to decimal (integer) 
	FROM = prep.arg(from, n=1, case="upper");
	TO = prep.arg(to, n=1, case="upper");
	 
	xINT = switch(FROM,					  			
					  "B" 	= fromBase(x, base=2), 	# BINARY
					  "O"  	= fromBase(x, base=8),	# OCT
					  "H"	= fromBase(x, base=16),	# HEX					  
				as.integer(x)	# DEFAULT # DECIMAL (INT, BASE 10)
				);
	xOUT = switch(TO,					  			
					  "B" 	= toBase(xINT, base=2, to.length=to.length), 	# BINARY
					  "O"  	= toBase(xINT, base=8, to.length=to.length),		# OCT
					  "H"	= toBase(xINT, base=16, to.length=to.length),	# HEX					  
				xINT		# DEFAULT DECIMAL (INT, BASE 10)
				);
#dput(xOUT);
	xOUT;
	}	

convertBase = base.convert;




# choices = c("dec", "hex", "bin", "oct");
# n = length(choices);
# for(i in 1:n)
	# {
	# for(j in 1:n)
		# {
		# f = tolower(choices[i]); F = toupper(f);
		# s = tolower(choices[j]); S = toupper(s);
		# if(f != s)
			# {
			# row = '{f}2{s} = function(..., to.length=NULL) { base.convert(..., from="{F}", to="{S}", to.length=to.length); }';
			
			# row = str.replace(c("{f}", "{s}", "{F}", "{S}"), c(f,s,F,S), row);
			# cat(row, "\n\n");	
			# }
		
		# }
	# }























dec2hex = function(..., to.length=NULL) { base.convert(..., from="DEC", to="HEX", to.length=to.length); } 

dec2bin = function(..., to.length=NULL) { base.convert(..., from="DEC", to="BIN", to.length=to.length); } 

dec2oct = function(..., to.length=NULL) { base.convert(..., from="DEC", to="OCT", to.length=to.length); } 

hex2dec = function(..., to.length=NULL) { base.convert(..., from="HEX", to="DEC", to.length=to.length); } 

hex2bin = function(..., to.length=NULL) { base.convert(..., from="HEX", to="BIN", to.length=to.length); } 

hex2oct = function(..., to.length=NULL) { base.convert(..., from="HEX", to="OCT", to.length=to.length); } 

bin2dec = function(..., to.length=NULL) { base.convert(..., from="BIN", to="DEC", to.length=to.length); } 

bin2hex = function(..., to.length=NULL) { base.convert(..., from="BIN", to="HEX", to.length=to.length); } 

bin2oct = function(..., to.length=NULL) { base.convert(..., from="BIN", to="OCT", to.length=to.length); } 

oct2dec = function(..., to.length=NULL) { base.convert(..., from="OCT", to="DEC", to.length=to.length); } 

oct2hex = function(..., to.length=NULL) { base.convert(..., from="OCT", to="HEX", to.length=to.length); } 

oct2bin = function(..., to.length=NULL) { base.convert(..., from="OCT", to="BIN", to.length=to.length); } 

