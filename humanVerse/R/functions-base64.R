

# gsub("[a-zA-Z0-9/+=]", "", str, invert=TRUE)
# Error in gsub("[a-zA-Z0-9/+=]", "", str, invert = TRUE) : 
# unused argument (invert = TRUE)






## this is too complicated, make it SIMPLE
## // maybe port http://c.mshaffer.com/js/monte/base64.js
## originally it was STRING <==> B64STRING
## we need JSON for STRING <==> OBJECT 
## keep them separate ....

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

js.b64 = function(input, method="encode") 
	{
	output = "";
	chr1 = chr2 = chr3 = "";
	enc1 = enc2 = enc3 = enc4 = "";
	i = 0;	
	 
	# this is univariate function	
	if(!is.defined(B64)) { constants.default(); }
	
	encode = function() 
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

	decode = function() 
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




# obj convert is another THING ...


	
base.toB64 = function(basestring, from=16)
	{
	
	}
	
base.fromB64 = function(B64string, to=16)
	{
	
	}
	

























































### THIS IS MULTIVARIATE ... user has to KNOW which function to call
base64.encode = function(objlist, method="JSON", ...)
	{
	objlist = check.list(objlist);
	# seems to convert to LIST automatically if input is vector ...
	n = length(objlist); 
	res = character(n);
	for(i in 1:n)
		{
		res[i] = base64.enc(objlist[[i]], method=method, ...);
		}
	res;
	}

### THIS IS MULTIVARIATE ... user has to KNOW which function to call
base64.decode = function(b64.vec, method="JSON", ...)
	{
	n = length(b64.vec);
	res = list();
	for(i in 1:n)
		{
		res[[i]] = base64.dec(b64.vec[i], method=method, ...);
		}
	list.return(res);	# collapse on n=1
	}


## THIS IS UNIVARIATE
base64.enc = function(obj, method="serialize")
	{	
	# necessary overhead
	METHOD = prep.arg(method, 1); 
	obj.raw = NULL;
	if(is.null(obj.raw) && METHOD == "j") 
		{ 
		obj.str = JSON.stringify(obj); 
		obj.raw = charToRaw(obj.str); 
		}
	if(is.null(obj.raw)) 
		{ 
		obj.raw = .serialize(obj); 
		}	
	b64.enc(obj.raw);
	}

 
## THIS IS UNIVARIATE
base64.dec = function(b64.str, method="serialize", ...)
	{
	# necessary overhead
	METHOD = prep.arg(method, 1);
	obj.raw = b64.dec(b64.str);
	
	obj = NULL;
	
	if(is.null(obj.raw) && METHOD == "j") 
		{	
		obj.str = rawToChar(obj.raw);
		obj = JSON.parse(obj.str); 
		} ## no ... on this function
	if(is.null(obj)) 
		{ 
		obj = .unserialize(obj.raw); 
		}	
	obj;
	}



































# https://coolbutuseless.github.io/2021/12/04/base64-encoding/decoding-in-plain-r/
## b64.init, b64.enc, b64.dec are in RAW FORM
## we will wrap into a STRING IN / OUT form


# https://coolbutuseless.github.io/2021/12/04/base64-encoding/decoding-in-plain-r/
b64.init = function() 
	{
	chars 	= c(LETTERS, letters, 0:9, '+', '/', '='	);
	values 	= c( charCode(LETTERS) - charCode('A'),
					charCode(letters) - charCode('a') + 26L,
					charCode(0:9)		 - charCode('0') + 52L,
										 62L, 63L, 0L		);
	# lookup = values; names(lookup) = chars;
	lookup = setNames(values, chars); 
	lookup;
	}

# https://coolbutuseless.github.io/2021/12/04/base64-encoding/decoding-in-plain-r/
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Decode a base64 string to a vector of raw bytes
#'
#' @param b64 Single character string containing base64 encoded values
#'
#' @return raw vector
#'
#' @example
#' b64 = 'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNk+A8AAQUBAScY42YAAAAASUVORK5CYII='
#' b64.dec(b64)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
b64.dec = function(b64) {
lookup = b64.init();
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# Get a, integer 6-bit value for each of the characters in the string
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	chars		<- strsplit(b64, '')[[1]]
	six_bits <- lookup[chars]

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# Explode these integers into their individual bit values (32 bits per int)
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	bits <- intToBits(six_bits)

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# Convert to 32 row matrix
	# Truncate to 6-row matrix (ignoring bits 7-32).
	# Then reshape to 8-row matrix.
	# Note that 'intToBits()' output is little-endian, so switch it here to
	# big endian for easier logic
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	mat <- matrix(as.integer(bits), nrow = 32)[6:1,]
	N <- length(mat)
	stopifnot(N %% 8 == 0)
	dim(mat) <- c(8, N/8)

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# Convert bits to bytes by multiplying out rows by 2^N and summing
	# along columns (i.e. each column is a bit-pattern for an 8-bit number)
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	raw_vec <- as.raw(colSums(mat * c(128L, 64L, 32L, 16L, 8L, 4L, 2L, 1L)))

	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# Trim padded characters
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	if (endsWith(b64, "==")) {
		length(raw_vec) <- length(raw_vec) - 2L
	} else if (endsWith(b64, "=")) {
		length(raw_vec) <- length(raw_vec) - 1L
	}

	raw_vec
}


# https://coolbutuseless.github.io/2021/12/04/base64-encoding/decoding-in-plain-r/
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Encode a raw vector to a base64 encoded character string
#'
#' @param raw_vec raw vector
#'
#' @return single character string containing base64 encoded values
#'
#' @example
#' b64.enc(as.raw(1:20))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
b64.enc = function(raw_vec) {
	lookup = b64.init();
	char_ = names(lookup);
	
	stopifnot(is.raw(raw_vec))

	# work out if we need to pad the result to an 8-bit boundary
	npad <- 3L - (length(raw_vec) %% 3L)
	if (npad %in% 1:2) {
		length(raw_vec) <- length(raw_vec) + npad
	}

	# Create an 8 row matrix.	Each column is the bit-vector for an 8-bit number
	int <- as.integer(raw_vec)
	res <- as.integer(bitwAnd(rep(int, each = 8),	c(128L, 64L, 32L, 16L, 8L, 4L, 2L, 1L)) > 0)
	mat <- matrix(res, nrow = 8)

	# Reshape to a 6-row matrix (i.e. 6-bit numbers)
	N <- length(mat)
	stopifnot(N %% 6 == 0)
	dim(mat) <- c(6, N/6)

	# Calcualte the 6-bit numbers
	mat <- mat * c(32L, 16L, 8L, 4L, 2L, 1L)
	values <- colSums(mat)

	# Find the letter which is associated with each 6-bit number
	# and paste together into a string
	chars <- char_[values + 1L]
	b64 <- paste(chars, collapse = "")

	# Replace padding bits with '=' signs
	if (npad == 1) {
		b64 <- gsub(".$", "=", b64)
	} else if (npad == 2) {
		b64 <- gsub("..$", "==", b64)
	}

	b64
}

	




b64.test = function()
	{
	for (i in seq(100)) 
			{
				data		<- as.raw(sample(i))
				b64_me	<- b64.enc(data)
				b64_ref <- openssl::base64_encode(data)
				
				# Does my base64 string agree with `openssl`?
				stopifnot(identical(b64_me, b64_ref))
				
				# Does the decoded value match the original data?
				decoded <- b64.dec(b64_ref)
				stopifnot(identical(data, decoded))
			}

			print("All good!");
		}


