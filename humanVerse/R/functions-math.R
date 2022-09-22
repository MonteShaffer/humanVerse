


		
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
	

# I have a large number, convert to long integer string 
strint = function(num = 5*10^222)
	{
	if(is.infinite(num)) { stop("we have reached Infinity ... and beyond!"); }
	# univariate 
	# truncate floating-point noise ...
	x.sci = format(num, scientific=TRUE, digits=22);
	x.exp = str.split("e+", x.sci);
		wh = as.numeric( x.exp[1] );
		ex = as.integer( x.exp[2] );
	nu = str.split(".", wh);
	nu[2] = v.TO(nu[2], NA, "");

	# noise after 16
	nlen = str.len(nu);
	r = paste0(nu[1], substring(nu[2], 1, 16));
	ex = ex - nlen[2];
	r = paste0(r, str.rep("0",ex) );
	r;
	}

.base2base = function(numstr, from=16, to=10)
	{
	#  BASE to DECIMAL 
	base.chars = c(as.character(0:9), LETTERS[1:22]);
	
	
	numstr = toupper(numstr);
	sn = str.explode("",numstr);	
	idx = rev( set.match(sn, base.chars) - 1);
	n = length(idx);
	sum = character(n);
	for(i in n:1)
		{
		sum[i] = strint( idx[i] * from^(i-1));		
		}
	
	
	# option.set("scipen", 999);
		# ss = as.character(5*10^222)
	# option.set("scipen", 0);
	
	slen = str.len(sum);
	vsum = check.list(str.explode("", str.pad(sum, max(slen), side = "LEFT")) );
	ms = max(slen);
	
	rsum = character(ms);
	idx = 0;
	carry = 0;
	for(j in ms:1)
		{
		idx %++%.
		row = list.getElements(vsum, j);
		rs = sum(as.numeric(row), carry );
		if(rs > 9 ) 
			{ 
			carry = as.integer(rs/10); 
			rs = rs - 10*carry; 
			} else { carry = 0; }
		rsum[j] = as.character(rs);		
		}
	if(carry > 0) { rsum = c(as.character(carry), rsum); }
	rsum = paste0( rsum,  collapse="");
	
	# we have a decimal number (any length) ??? 
	if(TO == "dec") { return(rsum); }
	
	
	
	
	
	
	
	
	n = length(xv);
	p = base^((n-1):0);
	
	sum( idx * p );   # this can get very big ... maybe GO BACK and read in REVERSE, slow but I deal with one digits at a time ...
					  # I may have overflow (remainder) into the next 
					  # so maybe I just do the calculation at each digit,
					  # store as a character vec ... str.pad 
					  # and do an old-school SUM with carryover ...
	
	#  DECIMAL to BASE 
	r = ""; if(num == 0) { return("0"); }
	while(num > 0) 
		{
		m = num %% base;
		r = paste0(base.chars[m + 1], r);
		num =  as.integer(num/base);
		}
	r;
	
	}


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

# must be in range of allowed integers 2^31 - 1:
int.convert = function(..., from="binary", to="octal", to.length=NULL)
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



















# what about string to string conversion (no limitations on number)
# str(table.bin2hex)
# str(list.mapInvert(table.bin2hex))

bin = function(..., n=2)
	{
	str = prep.dots(..., default="c8008c");
	bins = check.list(str.splitN(str, n=n));
	first = list.getElements(bins, 1);
	first = str.pad(first, to.length=n, side="LEFT");
	list.return( list.setElements(bins, 1, first) );	
	}
	
	

# this works for "bin" and "hex" only ... 
# bin(base.convert())
base.convert = function(..., from="binary", to="hex")
	{
	FROM = prep.arg(from, n=3);
	TO   = prep.arg(to,   n=3);
	
	maps = list(
			"hex" = list("hex" = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F"),
						"n" = 4,
						"bin" = c("0000", "0001", "0010", "0011", "0100", "0101", "0110", "0111", "1000", "1001", "1010", "1011", "1100", "1101", "1110", "1111")
						),
			"oct" = list("oct" = c("0", "1", "2", "3", "4", "5", "6", "7"),
						"n" = 3,
						"bin" = c("000", "001", "010", "011", "100", "101", "110", "111")
						)
			);
			
	key = FROM;
	if(FROM == "bin") { key = TO; }		
	
			
	x = prep.dots(..., default=paste0(maps[[key]][[FROM]],collapse="") );
	x = as.character(x);
dput(x);
	if(FROM == "bin") 
		{ 
		x = bin(x, n=maps[[key]]$n);
		idx = set.match(x, maps[[key]][[FROM]]);
		res = paste0(maps[[key]][[TO]][idx], collapse="");
		return(res);
		} else { 
				x = str.explode("", x); 
				# map to binary ... paste and re-bin to new "n"
				idx = set.match(x, maps[[key]][[FROM]]);
				b = paste0(maps[[key]][["bin"]][idx], collapse="");
				if(TO == "bin") { return(b); }
				
				y = bin(b, n=maps[[TO]]$n);
				idx = set.match(y, maps[[TO]][["bin"]]);
				res = paste0(maps[[TO]][[TO]][idx], collapse="");
				return(res);				
				}
	}	



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
			# row = '{f}2{s} = function(...) { base.convert(..., from="{f}", to="{f}"); }';
			
			# row = str.replace(c("{f}", "{s}", "{F}", "{S}"), c(f,s,F,S), row);
			# cat(row, "\n\n");	
			# }
		
		# }
	# }























dec2hex = function(...) { base.convert(..., from="dec", to="hex"); } 

dec2bin = function(...) { base.convert(..., from="dec", to="bin"); } 

dec2oct = function(...) { base.convert(..., from="dec", to="oct"); } 

hex2dec = function(...) { base.convert(..., from="hex", to="dec"); } 

hex2bin = function(...) { base.convert(..., from="hex", to="bin"); } 

hex2oct = function(...) { base.convert(..., from="hex", to="oct"); } 

bin2dec = function(...) { base.convert(..., from="bin", to="dec"); } 

bin2hex = function(...) { base.convert(..., from="bin", to="hex"); } 

bin2oct = function(...) { base.convert(..., from="bin", to="oct"); } 

oct2dec = function(...) { base.convert(..., from="oct", to="dec"); } 

oct2hex = function(...) { base.convert(..., from="oct", to="hex"); } 

oct2bin = function(...) { base.convert(..., from="oct", to="bin"); } 

