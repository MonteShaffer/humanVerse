
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

# base2int to an INTEGER 
base2int = function(..., base=16)
	{
	xstr = prep.dots(...);
#dput(xstr);
	xstr = cleanup.base(xstr);
# dput(xstr);
	b = check.base(base);
	# lower case, I guess 
	base.chars = c(as.character(0:9), LETTERS[1:22]);
	# base.keys = 1:length(base.chars);
	# base.list = list.create(base.chars, base.keys);
	
	N = length(xstr);
	res = integer(N);
	
	for(i in 1:N)
		{
		xstri = toupper(xstr[i]);
		xv = str.explode("",xstri);
		idx = set.match(xv, base.chars);
		n = length(xv);
		res[i] = sum( idx * b^((n-1):0) );
		}
	res;	
	}
	
base.from = base2int;	 
	

# an INTEGER to a base as a string 
# cpp_int2base(cpp_base2int(c("abc", "def"))) ... in primes.cpp for now ...  
int2base = function(..., base=16, to.length=NULL)
	{
	x = prep.dots(...);
#dput(x);
	b = check.base(base);
	base.chars = c(as.character(0:9), LETTERS[1:22]);
	N = length(x);
	res = character(N);
	for(i in 1:N)
		{
		num = x[i];	
		r = "";
		
		while(num > 0)
			{
			m = num %% base;
			r = paste0(base.chars[m + 1], r);
			num =  as.integer(num/base);
			}
			
		res[i] = r;		
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

