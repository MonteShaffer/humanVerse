

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

# univariate 
js.b64 = function(input, method="encode") 
	{
	output = "";
	chr1 = chr2 = chr3 = "";
	enc1 = enc2 = enc3 = enc4 = "";
	i = 0;	
	 
	encode = function(tol=c()) 
		{	
		input.charCodes	 = charCode(input); 
		n = str.len(input); 
		
		while (i < n)
			{		# this worked without zero index ... increment THEN
			chr1 = input.charCodes[ i %++%. ];	
			chr2 = input.charCodes[ i %++%. ];	
			chr3 = input.charCodes[ i %++%. ];	
				 
					# chr1 >> 2;
					# CREATE "%>>%" function ... "%+=" ... "%|%" .. "%&%"
			enc1 = bitShiftR(chr1,2); 
					# ((chr1 & 3) << 4) | (chr2 >> 4);
			enc2 = bitwOr( bitShiftL(bitwAnd(chr1,3),4), bitShiftR(chr2,4) );			
					# ((chr2 & 15) << 2) | (chr3 >> 6);
			enc3 = bitwOr( bitShiftL(bitwAnd(chr2,15),2), bitShiftR(chr3,6) );
					# chr3 & 63;
			enc4 = bitwAnd(chr3,63);

			# is.na or is.nan?
			if (is.na(chr2) || is.nan(chr2)) 
				{
				enc3 = enc4 = 64;
				} else if (is.na(chr3) || is.nan(chr3)) 
					{
					enc4 = 64;
					}

			output = paste0(output, 
							B64v[enc1+1],	# zero indexed 
							B64v[enc2+1],
							B64v[enc3+1],
							B64v[enc4+1]
							);
			}
		return(output);
		}

	decode = function(tol=c()) 
		{
		#// remove all characters that are not A-Z, a-z, 0-9, +, /, or =
		#input = input.replace(/[^A-Za-z0-9\+\/\=]/g, "");
		# maybe throw down warning (trim whitespace first, then warn)

		input = gsub("[^a-zA-Z0-9/+=]", "", input);	
		n = str.len(input); 
		
		input.vec		 = str.explode("", input);
		
		while (i < n)
			{
			enc1 = v.which(B64v, input.vec[ i %++%. ])-1; # zero-indexed
			enc2 = v.which(B64v, input.vec[ i %++%. ])-1;
			enc3 = v.which(B64v, input.vec[ i %++%. ])-1;
			enc4 = v.which(B64v, input.vec[ i %++%. ])-1;

					# (enc1 << 2) | (enc2 >> 4);
			chr1 = bitwOr( bitShiftL(enc1,2), bitShiftR(enc2,4) );				
					# ((enc2 & 15) << 4) | (enc3 >> 2);
			chr2 = bitwOr( bitShiftL(bitwAnd(enc2,15),4), bitShiftR(enc3,2) );
					# ((enc3 & 3) << 6) | enc4;
			chr3 = bitwOr( bitShiftL(bitwAnd(enc3,3),6), (enc4) );
					
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
	METHOD = prep.arg(method, n=1);
	if(METHOD == "e") { return( encode() ); } else { return( decode() ); }
	}



