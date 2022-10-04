

# gsub("[a-zA-Z0-9/+=]", "", str, invert=TRUE)
# Error in gsub("[a-zA-Z0-9/+=]", "", str, invert = TRUE) : 
# unused argument (invert = TRUE)


## u.parse("Fa\xe7ade") ... 

# unicode problem 
# https://developer.mozilla.org/en-US/docs/Glossary/Base64
# this code is the same as btoa and atob 
# http://c.mshaffer.com/js/monte/base64.js
# c2003? /* BASE 64 encode / decode */
# a = js.b64("Façade", "encode");	bb = js.b64(a, "decode");
# b = js.b64("RmHnYWRl", "decode");	aa = js.b64(b, "encode");
# function utf8_to_b64(str) {
  # return window.btoa(unescape(encodeURIComponent(str)));
# }

# function b64_to_utf8(str) {
  # return decodeURIComponent(escape(window.atob(str)));
# }
# http://127.0.0.1:10983/library/utils/html/URLencode.html
# THE ABOVE is what my C++ code is doing?

# encodeURI(value)

# encodeURIComponent(value)

# decodeURI(value)

# decodeURIComponent(value)


# > cpp_b64_enc(b)
# [1] "RmHDp2FkZQ=="
# utf8_to_b64(str);
# 'RmHDp2FkZQ=='



base64.decode = function(b64str)
	{
	n = length(b64str);
	res = character(n);
	for(i in 1:n)
		{
		res[i] = js.b64(b64str[i], "decode");
		}		
	res;
	}
	
base64.encode = function(str)
	{   
	n = length(str);
	res = character(n);
	for(i in 1:n) 
		{
		res[i] = js.b64(str[i], "encode");
		}		
	res;
	}












## this is too complicated, make it SIMPLE
## // maybe port http://c.mshaffer.com/js/monte/base64.js
## originally it was STRING <==> B64STRING
## we need JSON for STRING <==> OBJECT 
## keep them separate ....


# > js.b64("monte")
# [1] "bW9udGNA="
# > js.b64("Monte")
# [1] "TW9udGNA="

# univariate 
js.b64 = function(input, method="encode") 
	{
	output = "";
	chr1 = chr2 = chr3 = "";
	enc1 = enc2 = enc3 = enc4 = "";
	i = 0;	
	 
.__encode__. = function() {}  	 
	encode = function(humanVerse=c("welcomeTo")) 
		{	
		# input.charCodes	 = charCode(input); 
		icc = charCode(input);  
		n = str.len(input);  
		
		while (i < n)  # 3 characters at a time ... 
			{		# this worked without zero index ... increment THEN
			chr1 = icc[ i %++%. ];
			chr2 = icc[ i %++%. ];
			chr3 = icc[ i %++%. ];	
			### javascript works with NaN
			### R throws errors with NA ...
				NA2 = is.na(chr2);
			chr2 = v.TO(chr2, NA, 0);
				NA3 = is.na(chr3);
			chr3 = v.TO(chr3, NA, 0);
					
			enc1 = chr1 %>>% 2;
			enc2 = ((chr1 %&% 3) %<<% 4) %|% (chr2 %>>% 4);
			enc3 = ((chr2 %&% 15) %<<% 2) %|% (chr3 %>>% 6);
			enc4 = chr3 %&% 63;

			
			if (NA2) { enc3 = 	enc4 = 64; }
			if (NA3) {			enc4 = 64; }

			output = paste0(output, 
							B64v[enc1+1],	# zero indexed 
							B64v[enc2+1],
							B64v[enc3+1],
							B64v[enc4+1]
							);
			}
		return(output);
		}
		
.__decode__. = function() {}  
	decode = function(humanVerse=c("welcomeTo")) 
		{
		#// remove all characters that are not A-Z, a-z, 0-9, +, /, or =
		#input = input.replace(/[^A-Za-z0-9\+\/\=]/g, "");
		# maybe throw down warning (trim whitespace first, then warn)

		input 	= gsub("[^a-zA-Z0-9/+=]", "", input);	
		n 		= str.len(input); 
		
		iv		= str.explode("", input);
		
		while (i < n)
			{
			enc1 = v.which(B64v, iv[ i %++%. ])-1; # zero-indexed
			enc2 = v.which(B64v, iv[ i %++%. ])-1;
			enc3 = v.which(B64v, iv[ i %++%. ])-1;
			enc4 = v.which(B64v, iv[ i %++%. ])-1;
 

			chr1 = (enc1 %<<% 2) %|% (enc2 %>>% 4);	
			chr2 = ((enc2 %&% 15) %<<% 4) %|% (enc3 %>>% 2);
			chr3 = ((enc3 %&% 3) %<<% 6) %|% enc4;
					
			# can I trap String.fromCharCode 0 to AA==	... \x00 NULL ???	
			output = paste0(output, String.fromCharCode(chr1));
			if (enc3 != 64) 
				{
				output = paste0(output, String.fromCharCode(chr2));
				}
			if (enc4 != 64) 
				{
				output = paste0(output, String.fromCharCode(chr3));
				}
			}
		return(output);
		}

	### main 
.__main__. = function() {}  
	METHOD = prep.arg(method, n=1);
	if(METHOD == "e") 
		{ return( encode() ); } else { return( decode() ); }
	}




# a = .serialize(iris); b = 

# x = as.raw(1:10); y = raw.toString(x); 
# z = raw.fromString(y); identical(x,z);

# x = as.raw(1:10); y = raw.toString(x, collapse=NULL);
# z = raw.fromString(y, splitN=FALSE); identical(x,z);

# a = .serialize(iris); b = raw.toString(a);  strlen(b);
# c = cpp_base64_enc(b); strlen(c);
# d = cpp_base64_dec(c); e = raw.fromString(d); f = .unserialize(e);
# identical(f, iris);






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
	


# can I genericize this function?
# fermat primes > .lcm.bits(5, 17); [1] 32768
# res = NULL; for(i in 2:32) { for(j in 2:32) { x = .lcm.bits(i,j); row = c(i,j,x); res = rbind(res, row); } }
# res = as.dataframe(res); colnames(res) = c("i", "j", "map.size"); res = df.sortBy(res, "map.size", "DESC"); res;
# max ... one example ...
# # .lcm.bits(9, 17); [1] 1,048,576
## convert to common and back again 
## .lcm.bits(9, 2:32)

### map everything to/from "2" ... smallest but changes 
## .lcm.bits(2, c(2:32, 64))
## x = c(`-2-` = 2, `-3-` = 4, `-4-` = 4, `-5-` = 8, `-6-` = 8, `-7-` = 8, `-8-` = 8, `-9-` = 16, `-10-` = 16, `-11-` = 16, `-12-` = 16, `-13-` = 16, `-14-` = 16, `-15-` = 16, `-16-` = 16, `-17-` = 32, `-18-` = 32, `-19-` = 32, `-20-` = 32, `-21-` = 32, `-22-` = 32, `-23-` = 32, `-24-` = 32, `-25-` = 32, `-26-` = 32, `-27-` = 32, `-28-` = 32, `-29-` = 32, `-30-` = 32, `-31-` = 32, `-32-` = 32, `-64-` = 64)




.bXX_bin = function(bXXstr, x=7, collapse="")
	{
	MAP = .map_bin2xx(x);
		bins = property.get("bins", MAP);
	# bXXstr is one long string, no breaks ...
	if(length(bXXstr) > 1) 
		{  bXXstr = paste(bXXstr, collapse=""); }
	# 'raw' seems to be lower case, TRUE hex is upper?
	b = toupper( bin(bXXstr, n=bins[[2]], pad="0") );   # BXXv[1];
	nb = length(b);
	
	idx = set.match(b, MAP[[2]]);
	r = MAP[[1]][idx];
	binstr = paste0(r, collapse=collapse);
	binstr;
	}


.map_bin2xx = function(x=7)
	{
	mkey = paste0("-BIN_", str.pad(x, 2, "0", "LEFT"), "-");
	res = memory.get(mkey, "-BASE_CONVERSION_MAPS-");
	if(is.null(res))
		{
		n 	= .lcm.bits(x, 2);	# 8 in map (for 7)
				# fraction is problem ... 
		wX  = as.integer(ceil(.lcm.width(x, n))); 
				# 3 wide (for 7)
		wB 	= as.integer(.lcm.width(2, n));	
						# 000, 001, 010, 011, 100, 101, 110, 111
						#  0,   1,   2,   3,   4,   5,   6,   10
						# theres the problem ... floor => ceil 
						#  00   01   02   03   04   05   06   10 
		binfo = int2base(0:(n-1), base=2);
		rawBIN = str.pad(binfo, wB, "0", "LEFT");

		xinfo = int2base(0:(n-1), base=x);
		rawXX = str.pad(xinfo, wX, "0", "LEFT");
		
		# parallel vectors, easiest for set.match 
		res = list("bin" = rawBIN, x = rawXX);
		res = property.set("bins", res, list("bin" = wB, x = wX) )
		memory.set(mkey, "-BASE_CONVERSION_MAPS-", res);
		}
	res;	
	}
		
.map_hexb64 = function()
	{
	res = memory.get("-HEX_B64-", "-BASE_CONVERSION_MAPS-");
	if(is.null(res))
		{
		n 	= .lcm.bits(64, 16);	# 4096 in map 
		w64 = .lcm.width(64, n);  	# 2 wide
		wH 	= .lcm.width(16, n);	# 3 wide
		
		binfo = int2base(0:(n-1), base=64);
		raw64 = str.pad(binfo, w64, "A", "LEFT");

		hinfo = int2base(0:(n-1), base=16);
		rawH = str.pad(hinfo, wH, "0", "LEFT");
		
		# parallel vectors, easiest for set.match 
		res = list("b64" = raw64, "hex" = rawH);
		memory.set("-HEX_B64-", "-BASE_CONVERSION_MAPS-", res);
		}
	res;	
	}
	
	
.lcm.width = function(a=64, n=4096)
	{
	log2(n)/log2(a);	
	}
	
.lcm.bits = function(a=64, b=16)
	{
	# let b be multivariate 
	n = length(b);
	res = numeric(n);
	for(i in 1:n)
		{
		res[i] = 2^( gcd.lcm( ceiling(log2(a)), 
								ceiling(log2(b[i])) )$lcm );
		}
	names(res) = paste0("-",b,"-");
	res;
	}




