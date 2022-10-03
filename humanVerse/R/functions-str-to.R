

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
		ttr = as.raw( int.convert(ttx, from="hex", to="dec") );		
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
#' str.int2base64
#'
#'
#------------------------------------------------#
str.int2base64 = function(...) {}
#		res[i] = b64.enc( charToRaw(str[i]) );
		
#		res[i] = rawToChar( b64.dec(bstr[i]) );
		




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

 
.now = function() 
	{ 
	# RdH file?  
	# RhV ... source(R with multiline comments)
	# Rhelp 
	
	if(exists("cpp_nano"))
		{
		cpp_nano()/1000000000;
		} else { as.numeric(Sys.time()); }	
	}

# POSIXlt seems to work on timezone stuff, ct ... NOPE
# POSIXct doesn't use timezone info...
.timestamp = function(type="", tz=DEFAULT_TIMEZONE) 
	{  
	now = as.POSIXlt(Sys.time(), tz);
	if(type == "lt") { return( now );  }
	if(type == "ct") {	return( as.POSIXct(now, tz) ); }
	# formatted string ... # https://stackoverflow.com/a/29518651/184614
	# humanVerse 
	if(type == "hv" || type=="humanVerse") { return( strftime(now , "%Y-%m-%d\\./%H:%M:%S GMT%z") ); }
	if(type == "full") { return( strftime(now , "%Y-%m-%d %H:%M:%S %z") ); }
	
	if(type == "YYYY-MM-DD") { return( strftime(now , "%Y-%m-%d") ); }
	
	# custom formatter (todo) 
	# datef = .dateFormatter(type);
	datef = "%Y-%m-%d";
	
	
	
	#strftime(now , "%Y-%m-%dT%H:%M:%S%z")
	strftime(now , datef)
	}


time.now = function(method="first")
	{
	METHOD = prep.arg(method, n=1);
	# cpp_milli()/1000
	# cpp_micro()/1000000
	# cpp_nano()/1000000000
	FNS = list(
			"cpp" 		= function() { cpp_nano()/1000000000; } , 
			"base" 		= function() { as.numeric(Sys.time()); }
			);
			
	if(METHOD == "c") { return( FNS[["cpp"]]() ); }
	if(METHOD == "b") { return( FNS[["base"]]() ); }
	
	# first, ...
	# CASCADING, first-one to meet criteria 
	hasResult = FALSE;
	if(!hasResult && exists("cpp_nano"))
		{
		hasResult = TRUE;
		res = FNS[["cpp"]]();
		}
		
	if(!hasResult)
		{
		res = FNS[["base"]]();
		}
	
	res;
	}


.uniqid = function(n = 1, prefix = "", usep = ".", 
								more.entropy = FALSE)
	{
	res = character(n);
	for(i in 1:n)
		{
		when = time.now();
		
		fwhen = floor(when);
		fdiff = floor((when - fwhen)*1000000);
		
		if(more.entropy)
			{
			# singleton univariate is slower, but more entropy
			u = str.replace("-", "", .uuid(n=1, mode="basic-v4"));
			b = str.replace(c("+","/","="), "", .hex_b64(u) );
			s = str.letterShuffle(b);
			o = substring(b, 1, 10); 
			}
		if(!more.entropy)
			{
			# singleton univariate is slower, but more entropy
			r = rand(0,580085);
			o = tolower(int2base(r, base=16, to.length=5));
			}
		
		res[i] = paste0(	prefix,
							fwhen,
							usep,
							str.pad(fdiff, 6),
							usep,
							o,
							sep=""
							);	
		}
	res;
	}
	
	
	
	
 
 	 
.uuid = function(n=5, mode="basic-v4") 
	{
	MODE = prep.switch( prep.arg(mode, n=2, keep="-"), 
						c("ba","ba-v4"), 
						c("basic", "basic-v4"), "basic-v4");
	
	CPP = list("basic-v4" = 
				list("fn.name" = "cpp_uuid_basic_v4",
					"FN" = function() { cpp_uuid_basic_v4(n); } 
					),
				"basic" = 
				list("fn.name" = "cpp_uuid_basic",
					"FN" = function() { cpp_uuid_basic(n); } 
					)
				);
				
	cpp = CPP[[MODE]];
	if(exists(cpp$fn.name))
		{
		return( cpp$FN() );
		}
	
	if(MODE == "basic")
		{
		V 	= str.explode("", "0123456789abcdef");
		S 	= c(0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0);
		D 	= v.test(S, 1);
		RNG	= function(humanVerse=c("welcomeTo")) { rand(1, 16); }
		
		res = character(n);
		for(j in 1:n)
			{
			one = "";
			for(i in 1:16)
				{
				if (D[i]) { one %.=% "-"; }
				one %.=% V[RNG()];
				one %.=% V[RNG()];
				}
			
			res[j] = one;
			}
		return(res);
		}
		
	if(MODE == "basic-v4")
		{
		V  = str.explode("", "0123456789abcdef");
		D  = function(humanVerse=c("welcomeTo"))  { rand(1, 16); }
		D2 = function(humanVerse=c("welcomeTo"))  { rand(9, 12); }
		
		res = character(n);
		for(j in 1:n)
			{
			one = "";
			for(i in 1:8)
				{
				one %.=% V[D()];
				}
			one %.=% "-";
			for(i in 1:4)
				{
				one %.=% V[D()];
				}
			one %.=% "-4";
			for(i in 1:3)
				{
				one %.=% V[D()];
				}
			one %.=% "-";
			one %.=% V[D2()];
			for(i in 1:3)
				{
				one %.=% V[D()];
				}
			one %.=% "-";
			for(i in 1:12)
				{
				one %.=% V[D()];
				}
			res[j] = one;
			}
		return(res);
		}
	}


is.uuid = function(str=c("abcdef", "3b53a5c6-fe8a-44eb-9192-f51e80c370a2"))
	{
	grepl(REGEX_UUID, str);	
	}

 
str.fromB64 = function(b64str)
	{
	base64.decode(b64str);
	}
 

str.toB64 = function(str)
	{
	base64.encode(str);
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

	is.digest = (m == "d" && is.library_("digest") );
	is.openssl = (m == "o" && is.library_("openssl") );
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
			res[i] = rawToChar(raw[[i]]);  # this will fail on obj with NULL 
			}		# as.character(as.raw(1:10)) fives me something, but how to revere that ?
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











