

base.init = function()
	{
	if(!is.defined(BXXv)) { constants.default(); }
	# can we get this check to happen here, so we don't have to constantly check?
	
	
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
	xstri = as.character(xstri[1]);
	base.init();
	MAP = BXXv[1:base];	
	# update to allow base64 
	# update to allow base64 
	if(base == 64) { MAP = B64v; }
	
	# xstri = toupper(xstri);
	xv = str.explode("",xstri);
	idx = set.match(xv, MAP) - 1;
	n = length(xv);
	p = base^((n-1):0);
	
	sum( idx * p ); 	
	}
	
	
# fromBase 
# base2int to an INTEGER 
base2int = function(..., base=16, method="first")
	{
	xstr = prep.dots(..., default=c("0", "abc", "EA08", 
								"c8008c", "abbacdc", "7FFFFFFF") 
					);
	
		
#dput(xstr);
	xstr = cleanup.base(xstr);
	b = check.base(base);
	if(is.null(b)) { stop("base issues"); }
	
	METHOD = prep.arg(method, n=1);
		keys = c("c","b"); 
		vals = c("cpp", "base");
		# NOTICE 'default' doesn't have to be in key/val map ...
		default = "first";	
	METHOD = prep.switch(METHOD, keys, vals, default);
		
		
	if((METHOD == "cpp" || METHOD == "first") && exists("cpp_int2base") && b != 64) 
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
	
	





.int2base = function(num, base=16)
	{
	# univariate, no checks 
	num = as.integer(num[1]);
	base.init();
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


# toBase
# an [POSITIVE] INTEGER to a base as a string 
# cpp_int2base(cpp_base2int(c("abc", "def"))) ... in primes.cpp for now ...  
int2base = function(..., base=16, to.length=NULL, method="first")
	{
		x = prep.dots(..., 	default = c(0, 2748, 59912, 
								13107340, 180071644,  2^31 - 1) 
				);
#dput(x);
	b = check.base(base);
	if(is.null(b)) { stop("base issues"); }
	
	METHOD = prep.arg(method, n=1);
			keys = c("c","b"); 
			vals = c("cpp", "base");
			# NOTICE 'default' doesn't have to be in key/val map ...
			default = "first";	
	METHOD = prep.switch(METHOD, keys, vals, default);
		
	if((METHOD == "cpp" || METHOD == "first") && exists("cpp_int2base") && b != 64) 
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
	if(!is.null(to.length)) 
		{ res = str.pad(res, to.length, "0", "LEFT"); }
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

# these have to be defined INTEGERS (2^31 - 1)
dec2hex = function(...) { int.convert(..., from="dec", to="hex"); } 

dec2bin = function(...) { int.convert(..., from="dec", to="bin"); } 

dec2oct = function(...) { int.convert(..., from="dec", to="oct"); } 

hex2dec = function(...) { int.convert(..., from="hex", to="dec"); } 

hex2bin = function(...) { int.convert(..., from="hex", to="bin"); } 

hex2oct = function(...) { int.convert(..., from="hex", to="oct"); } 

bin2dec = function(...) { int.convert(..., from="bin", to="dec"); } 

bin2hex = function(...) { int.convert(..., from="bin", to="hex"); } 

bin2oct = function(...) { int.convert(..., from="bin", to="oct"); } 

oct2dec = function(...) { int.convert(..., from="oct", to="dec"); } 

oct2hex = function(...) { int.convert(..., from="oct", to="hex"); } 

oct2bin = function(...) { int.convert(..., from="oct", to="bin"); } 




















# what about string to string conversion (no limitations on number)
# str(table.bin2hex)
# str(list.mapInvert(table.bin2hex))

# this buckets or 'bins' hexstr in 2 or 3 ... 
#  to do converion 16 / 24 (base 64)
# buckets any string of numbers in a base to buckets 
# useful with binary strings ... n=4, n=8, n=16
bin = function(..., n=2, pad="0")
	{
	str = prep.dots(..., default="c8008c");
	bins = check.list(str.splitN(str, n=n)); 
	first = list.getElements(bins, 1);
	first = str.pad(first, n, pad, "LEFT"); 
		# defaults of function 
		# str.pad(first, n);  # equivalent 
	list.return( list.setElements(bins, 1, first) );	
	}
	
	
	






























# > .hex_b64("abcdef")
# [1] "q83v"
# > .b64_hex("q83v")
# [1] "abcdef"
	
.b64_hex = function(b64str, collapse="")
	{
	# b64str is one long string, no breaks ...
	if(length(b64str) > 1) {  b64str = paste(b64str, collapse=""); }
	b = bin(b64str, n=2, pad="A");   # B64v[1];
	nb = length(b);
	MAP = .map_hexb64();
	idx = set.match(b, MAP$b64); # there shouldn't be NA/NULL
	r = tolower(MAP$hex[idx]);  # to match 'raw' format 
	hexstr = paste0(r, collapse=collapse);
	hexstr;
	}



 

.hex_b64 = function(hexstr, collapse="")
	{
	MAP = .map_hexb64();
	# hexstr is one long string, no breaks ...
	if(length(hexstr) > 1) {  hexstr = paste(hexstr, collapse=""); }
	# 'raw' seems to be lower case, TRUE hex is upper?
	b = toupper( bin(hexstr, n=3, pad="0") );   # BXXv[1];
	nb = length(b);
	
	idx = set.match(b, MAP$hex);	# there shouldn't be NA/NULL
	r = MAP$b64[idx];
	b64str = paste0(r, collapse=collapse);
	b64str;
	}
	

.map_hexb64 = function()
	{
	res = memory.get("map", "-B64_HEX-");
	if(is.null(res))
		{
		n = .lcm.bits(64, 16);
		w64 = .lcm.width(64, n);  # 2 wide
		wH = .lcm.width(16, n);	  # 3 wide
		
		raw64 = memory.get("raw64", "-B64_HEX-");
		if(is.null(raw64))
			{
			info = int2base(0:(n-1), base=64);
			raw64 = str.pad(info, w64, "A", "LEFT");
			memory.set("raw64", "-B64_HEX-", raw64);
			}
		rawH = memory.get("rawH", "-B64_HEX-");
		if(is.null(rawH))
			{
			info = int2base(0:(n-1), base=16);
			rawH = str.pad(info, wH, "0", "LEFT");
			memory.set("rawH", "-B64_HEX-", rawH);
			}
		### FOR SETS this is rather meaningless
		# if(keys == "hex")
			# {
			# res = list.create(rawH, raw64);
			# memory.set(keys, "-B64_HEX-", res);
			# } else {
					# res = list.create(raw64, rawH);
					# memory.set("b64", "-B64_HEX-", res);
					# }		
		res = list("b64" = raw64, "hex" = rawH);
		memory.set("map", "-B64_HEX-", res);
		}
	res;	
	}
	
.lcm.width = function(a=64, n=4096)
	{
	log2(n)/log2(a);	
	}
	
.lcm.bits = function(a=64, b=16)
	{
	# if a = 16, b=64 ... both 2^n form 
	# 2^( gcd.lcm( log2(16), log2(64) )$lcm );
	# a=5; b=16;
	# 2^( gcd.lcm( ceiling(log2(5)), ceiling(log2(16)) )$lcm );
	# lcm = gcd.lcm(5,16)$lcm;  # 80 
	# set.match( 16^(1:80), 5^(1:80) )  # will they match?
	# > set.match( 16^(1:20), 64^(1:10) )
	# [1] NA NA  2 NA NA  4 NA NA  6 NA NA  8 NA NA 10 NA NA NA NA NA
	# getting to floating.point issue on set.match 
	# how to wrap it in is.equal ... 
	2^( gcd.lcm( ceiling(log2(a)), ceiling(log2(b)) )$lcm );
	}
	
	
# notice the gcd/lcm of 16, 64 would get me to 4096 somehow 
# how to apply to any base 5, 17 ... just build maps ...
#  gcd.lcm( log2(16), log2(64) ) ... 12 
# gcd.lcm( log2(5), log2(16) ) ,,, 2.32
# how to find ... 16*16*16 = 64*64 
#  5*5*



	
	






	
	

# these are string-to-string conversions ... no INTEGER checks 
# see int.convert for those 
# this works for "bin" and "hex" only ... 
# bin(base.convert())
# no matter what, let's cast to "binary" strings of a length appropriate to do the lookup ...

# base.convert = function(..., from="binary", to="hex", decimal.numeric=TRUE)


.baseTrim = function(x, pad="0")
	{
	# 
	# maybe do something smart with base ... bin ( repeat myself ?)
	# convolutins is BAD ... 
	y = str.trimFromAny(x, pad, "LEFT");
	y[y==""] = "0";
	y;	
	}


# FROM = 16; TO = 32; a = int2base(0:(FROM*FROM), base=FROM);
# b = .base2bin(a, FROM);	c = .bin2base(b, TO);
# d = .base2bin(c, TO); 	e = .bin2base(d, FROM);
# identical(a,e);


# FROM = 16; TO = 64; a = int2base(0:(FROM*FROM), base=FROM);
# b = .base2bin(a, FROM);	c = .bin2base(b, TO);
# d = .base2bin(c, TO); 	e = .bin2base(d, FROM);
# identical(a,e); 

	# this is limited by the ^power operator ... 
	# let's do it by singletons ...
# x = int2base(0:(FROM*FROM), base=FROM)
## STOPPING POINT, MAYBE WORKING 
.base2bin = function(x, FROM=5, left.trim=TRUE) 
	{
	num.init();  # verify CONSTANTS are available ...
	# assume I have a vec x 
	n = length(x);
	res = character(n);
	

	mapFROM = BXXv[1:FROM];
	if(FROM == 64) { mapFROM = B64v; }
	
	b = log(FROM, 2);  # are we in a base-2 subset 
	b_ = as.integer(b);
	bn = NULL;
	if(b != b_) { bn = 1 + b_; }
	
	if(is.null(bn))
		{
		# we are living in a base-2 world ... easy as PIE ... PROTO-INDO-EURO
		mapTO = bin(Bits64[1:FROM], n=b_);
		for(i in 1:n)
			{
			xi = x[i];
			xiv = str.explode("", xi);  #  xiv			
			idx = set.match(xiv, mapFROM);
			res[i] = paste0(mapTO[idx], collapse="");
			}
		if(left.trim) { res = .baseTrim(res); }
		return(res);			
		} else {
				# THIS is NOT WORKING, carryover issues TODO
				mapTO.lower = bin(Bits64[1:FROM], n=b_);
				mapTO.upper = bin(Bits64[1:FROM], n=bn);
				slen = list.getLengths(mapTO.lower);
				# should exist here 
				# may be a few ... 
				# logic = v.test(slen, 1, invert=TRUE); 
				idx = v.which(slen, 1, invert=TRUE); 
				
				map.was = list.getByIDX(mapTO.lower, idx);
				map.is  = list.getByIDX(mapTO.upper, idx); # maybe a list    
				mapTO = list.setByIDX(mapTO.lower, idx, vals);
				# mapTO[[idx]] = mapTO.upper[[idx]]
				# if idx is BOOLEAN, above would work, NOT!
				# mapTO[[logic]] = mapTO.upper[[logic]];
				for(i in 1:n)
					{
					xi = x[i];
					xiv = str.explode("", xi);  #  xiv			
					idx = set.match(xiv, mapFROM);
					res[i] = paste0(mapTO[idx], collapse="");
					}
				if(left.trim) { res = .baseTrim(res); }
		
				return(res);
				}
	}


.bin2base = function(y, TO=16, left.trim=TRUE) 
	{
	# in general, we want to trim 
	# should be simple enough if 2^n form 
	num.init();  # verify CONSTANTS are available ...
	# assume I have a vec x 
	n = length(y);
	res = character(n);

	mapTO = BXXv[1:TO];
	if(TO == 64) { mapTO = B64v; }
	 
	b = log(TO, 2);  # are we in a base-2 subset 
	b_ = as.integer(b);
	bn = NULL;
	if(b != b_) { bn = 1 + b_; }
	
	if(is.null(bn))
		{
		# we are living in a base-2 world ... easy as PIE ... PROTO-INDO-EURO
		mapFROM = unlist( bin(Bits64[1:TO], n=b_) );
		
		for(i in 1:n)
			{
			yi = y[i];
				# this needs to be another b_ ... 
				# log(FROM, 2)
			yiv = bin(yi, n=b_);		
			idx = set.match(yiv, mapFROM);
			res[i] = paste0(mapTO[idx], collapse="");
			}
		if(left.trim) { res = .baseTrim(res); }
		
		return(res);			
		} else {
				# THIS is NOT WORKING, carryover issues TODO
				stop("non trivial with partial bits, TODO");
				if(left.trim) { res = .baseTrim(res); }
		
				}
	
	} 
 
base.convert = function(..., from=5, to=16, decimal.numeric=TRUE)
	{
	FROM = check.base(from);
		if(is.null(FROM)) { stop("base issues"); }
	TO = check.base(to);
		if(is.null(TO)) { stop("base issues"); }
	x = prep.dots(..., default=int2base(0:(FROM*FROM), base=FROM) );
	x = as.character(x);

	y = .base2bin(x, FROM=FROM);
	res = .base2bin(y, TO=TO);
	
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


























