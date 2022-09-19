

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' str.toInteger
#'
#'
#------------------------------------------------#
str.toInteger = function(str, isHEX=FALSE, base=0L)
	{
	# FALSE means it is just normal numbers we want to convert 
	if(!isHEX) { return (as.integer(str)); }  # e.g, strtoi(str, base=10)?
	return( strtoi(str, base=base) );
	}

#++++++++++++++++++++++++#
#'
#' @rdname stringToInteger
#' @export
stringToInteger = str.toInteger;
#^^^^^^^^^^^^^^^^^^^^^^^^#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' str.fromInteger
#'
#'
#------------------------------------------------#
str.fromInteger = function(intvec)
	{
	as.character(as.integer(intvec));
	}

# str = c("U+22EF"), 
str.toUTF = function(..., collapse=FALSE)
	{
	str = prep.dots(..., default=c("U+22EF"));
	# maybe jsut stringi 
	# str = c("U+22EF","0x03B2L", "\x22EF", "\u22ef")
	# intToUtf8(0x03B2L)
	# text(1, 1.2, intToUtf8(c(21315, 31179, 19975, 36733)), cex = 5)
	u.toSymbol(str, collapse=collapse);
	}
	
	
str.fromUTF = function(utf, format="U+")
	{
	
	}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' str.toHEX
#'
#'
#------------------------------------------------#
str.toHEX = function(...)
	{
	str = prep.dots(...);
	n = length(str);
	res = character(n);
	for(i in 1:n)
		{
		res[i] = paste0( as.character(charToRaw(str[i])), collapse="");
		}
	res;
	}
	
	
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' str.fromHEX
#'
#'
#------------------------------------------------#
str.fromHEX = function(...)
	{
	hstr = prep.dots(...);
	n = length(hstr);
	res = character(n);
	for(i in 1:n)
		{
		tt  = str.splitN(hstr[i], n=2);
		ttx = paste0("0x",tt);
		ttr = as.raw( hexdec(ttx) );		
		res[i] = rawToChar(ttr);
		}
	res;
	}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' str.toCharacterVector
#' 
#'
#------------------------------------------------#
str.toCharacterVector = function(str, sep="")
	{
	# add both options (cpp or not?)
	# strsplit(str, sep, fixed=TRUE)[[1]];
	res = str.explode(sep, str);
	list.return(res);
	}

#++++++++++++++++++++++++#
#'
#' @rdname charVector
#' @export
charVector = str.toCharacterVector;

#++++++++++++++++++++++++#
#'
#' @rdname str.toCharVector
#' @export
str.toCharVector = str.toCharacterVector;


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' str.fromCharacterVector
#'
#'
#------------------------------------------------#
str.fromCharacterVector = function(charslist, sep="")
	{
	res = check.list(charslist);
	str.implode(sep, res);
	}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' str.toBASE64
#'
#'
#------------------------------------------------#
str.toBASE64 = function(...)
	{
	str = prep.dots(...);
	n = length(str);
	res = character(n);
	for(i in 1:n)
		{
		res[i] = b64.enc( charToRaw(str[i]) );
		}
	res;
	}
	
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' str.fromBASE64
#'
#'
#------------------------------------------------#
str.fromBASE64 = function(...)
	{
	bstr = prep.dots(...);
	n = length(bstr);
	res = character(n);
	for(i in 1:n)
		{
		res[i] = rawToChar( b64.dec(bstr[i]) );
		}
	res;
	}




str.toLanguage = function(str)
	{
	# univariate?
	base::str2lang(str)
	}
str.fromLanguage = function(lang.obj)
	{
	# univariate?
	lang2str(lang.obj);  # I wrote this inverse 
	}



str.toSymbol = function(str)
	{
	# univariate?
	str2symb(str);  # I wrote this 
	}
str.fromSymbol = function(symb.obj)
	{
	# univariate?
	symb2str(symb.obj);  # I wrote this 
	}
	
	
	
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' str.toMD5
#'
#'
#------------------------------------------------#
str.toMD5 = function(str, times=1, method="digest", ...)
	{
	# necessary overhead
	m = prep.arg(method, 1);
	times = as.integer(times);  # make certain it's an integer
	str = as.character(str);	# make certain it's a string (character)
	if(times < 1) 
		{ 
		warning("what are you doing setting the times < 1, RETURNING NULL "); 
		return(NULL); 
		}

	if(m == "c" && exists("cpp_md5"))
		{
		# cpp_md5( (str = c("monte","shaffer") ), 9 );
		res = ( cpp_md5(str, times) );
		return(res);
		}

	is.digest = (m == "d" && is.library("digest") );
	is.openssl = (m == "o" && is.library("openssl") );
	# we have to use the SLOW base function
	is.hack = (!is.digest && !is.openssl); 

	if(is.hack)
		{
		## first look in system /inst/R/functions-md5_.R 
		if(!is.function(md5_))
			{
			source("https://raw.githubusercontent.com/MonteShaffer/humanVerse/main/humanVerse/inst/R/functions-md5_.R");
			Sys.sleep(1); # let it settle?
			}
		
		if(!is.function(md5_))
			{
			stop("UNABLE to load backup MD5 function");
			}
		}

	
	
	n = length(str); # how many
	res = character(n);
	for(i in 1:n)
		{
		res[i] = str[i];
		for(j in 1:times)
			{
			if(is.digest)
				{
				# vdigest ... # Dirk, I don't see it!
				# digest::digest(str, algo="md5", serialize=FALSE);
				### THIS is locally scoped?
				if( !exists("algo", inherits = FALSE ) )			
						{ 
						algo 		= "md5"; 
						}
				if( !exists("serialize", inherits = FALSE ) )	
						{ 
						serialize 	= FALSE; 
						}
				
				res[i] = digest::digest(res[i], 
											algo=algo, 
											serialize=serialize, ...);
				}
			if(is.openssl)
				{
				# openssl::md5(strvec);
				# could speed this up in vector form
				res[i] = openssl::md5(res[i]);
				}
			if(is.hack)
				{
				## I could vector this if digest also is, 
				## then I could remove one 'FOR' loop
				res[i] = md5_(res[i]);
				}
			}
		}
	return(res);
	}


str.HASH = function()
	{
	str = NULL;
	str = c(str, rand() );
	str = c(str, sample(letters, rand(1, rand(1,100)), replace=TRUE) );
	str = c(str, rnorm( rand(1, rand(1,55) )) );
	str = c(str, primes.get( rand(1, rand(1,33) ) ) );
	colls = sample(LETTERS, rand(1, rand(1,5)));
	for(i in 1: rand(1, rand(1, 22)))
		{
		str = sample(str);
		colls = sample(colls);
		}
	coll = paste0(coll, collapse="");
	res = str.toMD5( paste0(str, collapse=coll), times=rand(1, rand(1,5)) );
	minvisible(res, "LAST-HASH");
	} 
 
 
 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' str.fromMD5
#'
#'
#------------------------------------------------#
str.fromMD5 = function(str, times=1, method="digest", ...)
	{
	#  body(str.fromMD5);
	msg = "\n\n tldr; \n\n\t MD5 is a hashing algorithm that digests an input into a 32-bit hexstr. \n\t\t The [times=1] feature will allow iterative MD5 hashing:  e.g., n=2 is MD5(MD5(str)) \n\t\t It would be cool if it had an inverse, but alas, it does not!  There are \n\t\t HACKERS that have rainbow tables of results, a dictionary-based INVERSION.  \n\t\t Who knows, maybe I will implement that lookup in R 'one day' \n\n\t -->  MD5 is **SAFE** to hash most things (except passwords, encryption keys, etc.)  But don't worry, in the early days FACEBOOK stored your password in the database in PLAIN TEXT form.  At least an MD5 storage requires some work. [This about how much LIFE ENERGY we waste dealing with password-type events.  Is that MKULTRA programming?!?]  The optimal storage, IMHO, is using the SJCL algorithm as it has easy-to-use client side (Javascript, n1=12346 times) and server side (PHP [or Javascript], n2=12345678 times) interfacing.  Like MD5, you don't ever know what the password is, but you can perform a handshake. \n\t\t Good coding STANDARDS requires me to include the inverse function here to send an important \n\t\t VERBOSE message to you, dear reader.  \n\t\t  THIS my friends is how choice architecture works!  Cheers and Aloha! \n\n\n\t\t\t MD5: https://en.wikipedia.org/wiki/MD5 \n\n\n\t\t\t HASHING:  https://en.wikipedia.org/wiki/Hash_function \n\n\n\t\t\t RAINBOWS: https://crackstation.net/ [Great, except NEW captch] \n\n\t\t\t\t https://md5decrypt.net/en/ \n\n\t\t\t http://md5.mshaffer.com  \n\n\n\t\t\t SJCL: https://bitwiseshiftleft.github.io/sjcl/ ";
	
	warning(msg, call. = TRUE, immediate. = TRUE);
	
	}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' str.toRaw
#'
#' 
#------------------------------------------------#
str.toRaw = function(str)
	{
	n = length(str);
	res = list();
	for(i in 1:n)
		{
		res[[i]] = charToRaw(str[i]);
		}
	list.return(res);
	}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' str.fromRaw
#'
#'
#------------------------------------------------#
str.fromRaw = function(raw)
	{
	if(is.list(raw))
		{
		n = length(raw);
		res = character(n);
		for(i in 1:n)
			{
			res[i] = rawToChar(raw[[i]]);
			}
		res;
		} else { rawToChar(raw); }
	}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' str.toObjectName
#'
#'
# eval ... parse 
#------------------------------------------------#
str.toObjectName = function(obj.str) 
	{ 
	eval(parse(text = obj.str));  # as-is, no checks?
	}  
	
	
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' str.fromObjectName
#'
#'
#------------------------------------------------#
str.fromObjectName = function(objname) 
	{ 
	res = deparse(substitute(objname));
	res = str.replace('"', "", res);
	return(res);
	}











