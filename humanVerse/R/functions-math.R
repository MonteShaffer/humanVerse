
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


.base2int = function(xstri, base=16)
	{
	# univariate, no checks 
	base.chars = c(as.character(0:9), LETTERS[1:22]);
	
	xstri = toupper(xstri);
	xv = str.explode("",xstri);
	idx = set.match(xv, base.chars) - 1;
	n = length(xv);
	p = base^((n-1):0);
	
	sum( idx * p ); 	
	}
	
	
# fromBase 
# base2int to an INTEGER 
base2int = function(..., base=16, method="first")
	{
	xstr = prep.dots(..., default=c("0", "abc", "EA08", "c8008c", "abbacdc", "7FFFFFFF") );
#dput(xstr);
	xstr = cleanup.base(xstr);
	b = check.base(base);
	
	METHOD = prep.arg(method, n=1);
			keys = c("c","b"); vals = c("cpp", "base");	default = "first";	
	METHOD = prep.switch(METHOD, keys, vals, default);
		
		
	if((METHOD == "cpp" || METHOD == "first") && exists("cpp_base2int")) 
			{ 
			res = cpp_base2int(xstr,b);
			return( v.return(res) );
			}
	
	N = length(xstr);
	res = integer(N);
	
	for(i in 1:N)
		{ 
		res[i] = .base2int(xstr[i], b);
		}
	res;	
	}
	
base.from = base2int;	 
	




.int2base = function(num, base=16)
	{
	# univariate, no checks 
	base.chars = c(as.character(0:9), LETTERS[1:22]);	
	r = ""; if(num == 0) { return("0"); }
	while(num > 0) 
		{
		m = num %% base;
		r = paste0(base.chars[m + 1], r);
		num =  as.integer(num/base);
		}
	r;
	}


# toBase
# an INTEGER to a base as a string 
# cpp_int2base(cpp_base2int(c("abc", "def"))) ... in primes.cpp for now ...  
int2base = function(..., base=16, to.length=NULL, method="first")
	{
	x = prep.dots(..., default = c(0, 2748, 59912, 13107340, 180071644,  2^31 - 1) );
#dput(x);
	b = check.base(base);
	
	METHOD = prep.arg(method, n=1);
			keys = c("c","b"); vals = c("cpp", "base");	default = "first";	
	METHOD = prep.switch(METHOD, keys, vals, default);
		
	if((METHOD == "cpp" || METHOD == "first") && exists("cpp_int2base")) 
			{ 
			res = cpp_int2base(x,b);
			return( v.return(v.toNA(res, (res==""))) );
			}
			
	N = length(x);
	res = character(N);
	for(i in 1:N)
		{			
		res[i] = .int2base(x[i], base);	
		}
	# str.pad("LEFT");
	if(!is.null(to.length)) { res = str.pad(res, to.length, "0", "LEFT"); }
	res;
	}

base.to = int2base;

base.convert = function(..., from="binary", to="octal", to.length=NULL)
	{
	x = prep.dots(...);
	# first to decimal (integer) 
	FROM = prep.arg(from, n=1, case="upper");
	TO = prep.arg(to, n=1, case="upper");
	 
	xINT = switch(FROM,					  			
					  "B" 	= base2int(x, base=2), 	# BINARY
					  "O"  	= base2int(x, base=8),	# OCT
					  "H"	= base2int(x, base=16),	# HEX					  
				as.integer(x)	# DEFAULT # DECIMAL (INT, BASE 10)
				);
	xOUT = switch(TO,					  			
					  "B" 	= int2base(xINT, base=2, to.length=to.length), 	# BINARY
					  "O"  	= int2base(xINT, base=8, to.length=to.length),		# OCT
					  "H"	= int2base(xINT, base=16, to.length=to.length),	# HEX					  
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

