

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



