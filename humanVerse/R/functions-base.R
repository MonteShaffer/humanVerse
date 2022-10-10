 

cleanup.base = function(xstr)
	{
	xstr = as.character(xstr);
	# res = gsub(REGEX_BASE32_NOT, "", xstr);
	
	# gsub("[^[0-9a-vA-V]]", "", xstr);  # doesn't work 
	# gsub("[^[0-9A-Va-v]", "", xstr);   # works, notice unbalanced brackets???
	
	# DOESN'T carefully account for "0x" ... could do after ...
	
	# return(res);
	
	# left trim any non-elements ... maybe REGEX replace 0-9A-V
	# HEX 
	xstr = str.trim(str.replace(c("#","0x","0X"), "", xstr) );
	
	# xstr = gsub("[^[0-9A-Va-v]", "", xstr);
	
	xstr;
	}



# use base.convert for "unnamed" conversion 
# strings only , passes through INTEGER MAX 2^31
base.convert = function(..., from=10, to=7)
	{
	x = prep.dots(...);
	x = as.character(x);
	
	xfrom 	= base2int(x, 		base=from);
	xto 	= int2base(xfrom, 	base=to);
	xto;
	}








.base2int = function(xstri, base=16)
	{
	# univariate, no checks 
	xstri = as.character(xstri[1]);
	
	MAP = BXXv[1:base];	
	
	# update to allow base64 
	if(base == 64) { MAP = B64v; }
	
		# bad news on B64 
	if(base != 64)
		{ xstri = toupper(xstri); }
	xv = str.explode("", xstri);
	idx = set.match(xv, MAP) - 1;
	n = length(xv);
	p = base^((n-1):0);
	 
	sum( idx * p ); 	
	}
	 
	
base2int = function(..., base=16, method="first")
	{
	xstr = prep.dots(..., default=c("0", "abc", "EA08", 
								"c8008c", "abbacdc", "7FFFFFFF") 
					);

	xstr = cleanup.base(xstr);
	b = check.base(base);
	if(is.null(b)) { stop("base issues"); }
	
		# NOTICE 'default' doesn't have to be in key/val map ...
	METHOD = prep.switch( prep.arg(method, n=1),
							keys = c("c","b"),
							vals = c("cpp", "base"),
							default = "first");

	if((METHOD == "cpp" || METHOD == "first") 
			&& exists("cpp_int2base") && b != 64) 
			{
			res = cpp_base2int(xstr, b);
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
	
	




 
.int2base = function(num, base=16)
	{
	# univariate, no checks 
	num = as.integer(num[1]);
	MAP = BXXv[1:base];
	# update to allow base64 
	if(base == 64) { MAP = B64v; }
	
	if(num == 0) { return(MAP[1]); }
	r = ""; 
	while(num > 0) 
		{
		m = num %% base;
		r = paste0(MAP[m + 1], r);
		num =  as.integer(num/base);
		}
	r;
	}



int2base = function(..., base=16, to.length=NULL, method="first")
	{
		x = prep.dots(..., 	default = c(0, 2748, 59912, 
								13107340, 180071644,  2^31 - 1) 
				);

	b = check.base(base);
	if(is.null(b)) { stop("base issues"); }
	
			# NOTICE 'default' doesn't have to be in key/val map 
	METHOD = prep.switch( prep.arg(method, n=1),
							keys = c("c","b"),
							vals = c("cpp", "base"),
							default = "first");
	
		
	if((METHOD == "cpp" || METHOD == "first") 
			&& exists("cpp_int2base") && b != 64) 
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
	if(!is.null(to.length)) 
		{ res = str.pad(res, to.length, "0", "LEFT"); }
	res;
	}




# these are the "named" CONVENTIONAL conversions ... bin = 2^1, oct = 2^3, hex = 2^4 ... 
# must be in range of allowed integers 2^31 - 1:
int.convert = function(..., from="binary", to="octal", to.length=NULL)
	{
	x = prep.dots(...);
	# first to decimal (integer) 
	FROM 	= prep.arg(from, n=1, case="upper");
	TO 		= prep.arg(to, n=1, case="upper");
	
	# default is "integer" or "decimal" (bad label)
	 
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




### make dec2hex ... also int2hex ... 

# these have to be defined INTEGERS (2^31 - 1)


############################################################
############################### HELPER FUNCTIONS ###########
############################################################


int.buildHelperFunctions = function()
	{	
	choices = c("int", "dec", "hex", "bin", "oct");
	n = length(choices);
	for(i in 1:n)
		{
		for(j in 1:n)
			{
			f = tolower(choices[i]); F = toupper(f);
			s = tolower(choices[j]); S = toupper(s);
			if((f == "dec" && s == "int") || (f == "int" && s == "dec")) { next; } 
			if(f != s)
				{
				row = '{f}2{s} = function(...) { int.convert(..., from="{f}", to="{s}"); }';
				
				row = str.replace(c("{f}", "{s}", "{F}", "{S}"), c(f,s,F,S), row);	
				
				.cat(row);	 
				}
			
			}
		}
	}
	
	
############## OUTPUT HERE (COPY/PASTE CURRENTLY) ###########


int2hex = function(...) { int.convert(..., from="int", to="hex"); }



int2bin = function(...) { int.convert(..., from="int", to="bin"); }



int2oct = function(...) { int.convert(..., from="int", to="oct"); }



dec2hex = function(...) { int.convert(..., from="dec", to="hex"); }



dec2bin = function(...) { int.convert(..., from="dec", to="bin"); }



dec2oct = function(...) { int.convert(..., from="dec", to="oct"); }



hex2int = function(...) { int.convert(..., from="hex", to="int"); }



hex2dec = function(...) { int.convert(..., from="hex", to="dec"); }



hex2bin = function(...) { int.convert(..., from="hex", to="bin"); }



hex2oct = function(...) { int.convert(..., from="hex", to="oct"); }



bin2int = function(...) { int.convert(..., from="bin", to="int"); }



bin2dec = function(...) { int.convert(..., from="bin", to="dec"); }



bin2hex = function(...) { int.convert(..., from="bin", to="hex"); }



bin2oct = function(...) { int.convert(..., from="bin", to="oct"); }



oct2int = function(...) { int.convert(..., from="oct", to="int"); }



oct2dec = function(...) { int.convert(..., from="oct", to="dec"); }



oct2hex = function(...) { int.convert(..., from="oct", to="hex"); }



oct2bin = function(...) { int.convert(..., from="oct", to="bin"); }


