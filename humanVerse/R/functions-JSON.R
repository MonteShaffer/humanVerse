
# obj = 1:10 

RHON.isKey = function(obj, key="dataframe")
	{
	# if typeof picks it up, **NOT** a key 
	if(key == "vector")
		{
		if(is.vector(obj)) { return("vector"); } else { return(NULL); }		
		}
	if(key == "atomic")
		{
		if(is.atomic(obj)) { return("atomic"); } else { return(NULL); }		
		}
	if(key == "dataframe")
		{
		if(is.dataframe(obj)) { return("dataframe"); } else { return(NULL); }		
		}
	if(key == "matrix")
		{
		if(is.matrix(obj)) { return("matrix"); } else { return(NULL); }		
		}
	if(key == "Inf" || key == "infinite")
		{
		if(is.infinite(obj)) { return("infinite"); } else { return(NULL); }		
		}
	if(key == "NA")
		{
		if(is.na(obj)) { return("NA"); } else { return(NULL); }		
		}
	if(key %in% c("NAN", "NaN", "nan"))
		{
		if(is.nan(obj)) { return("NaN"); } else { return(NULL); }		
		}
	
	
	}

RHON.is = function(obj, keys=c("atomic", "vector", "dataframe", "matrix", "na", "NaN", "Inf", "function", "primitive")
	{
	# THESE ARE not typeof?
	# keys 
	res = NULL;
	for(key in keys)
		{
		res = c(res, RHON.isKey(obj, key);
		}
	res;	
	}



RHON.fn = function(obj)
	{
	# cleanup the function for transport 
	# stringify, one line ... R2_lang on it ...
	
	
	}

RHON.describe = function(obj, name=NULL)
	{
	# name is the name of the obj in its recursive context 
	# if(is.null(name))) { name = deparse(substitute(obj)); }
	ct.OBJ = check.type(obj);
	
	# nested lists are a bitch, recursive 
	omode = mode(obj);
	osource = NULL;			# WHERE did it come from (pkg, fn, line no, WEB)
							# if NOT NULL 'name' it came from parent (so skip)
	osymbol = NULL;			# pass in dots, deparse to get key ... CHECK.TYPE
	otype = typeof(obj);  	# integer, double, 
							# function (is closure)
							# environment 
							# symbol 
							# dataframe (is list)
							# raw is typeof ... 
							# NA (is logical)
	olen  = length(obj);	# iris is length 5 (columsn), 5 vectors in list
	oclasses = class(obj);	# multivariate 
	oises   = RHON.is(obj);
	onames = names(obj);  # this is colnames   # if NULL, exclude downstream
	rnames = rownames(obj); 
	rdim = dim(obj);  # usefully for multidimensinally arrays
	oattr = attributes(obj);  # less names?
	
	# attributes(iris) ... names, row.names, class="data.frame"
	omethods = methods(obj);  # could be a function 
							# THROWS error, have to trap .getMethods (functions already)
	
	# attributes maybe recursive ... 
	# listnames = names(list);
	# listkeys = 1:length(list);
	# merge these ... that's how it works ... no name is still an index ... named can still be referenced by index ....
	
	
	
	# trap NULL in list, or assign to NA 
	
	
	}


# let's compare rjson to jsonlite 
# do they comply with ECMA/javascript standards?
# # https://www.unicode.org/faq/private_use.html
# PUA 


# U+E000..U+F8FF

# u.toNum("U+E000");  # 57344
# u.toNum("U+F8FF");  # 63743

# .NULL = u.toSymbol("U+EA08");

# 59912 ... "U+EA08"

JSON.init = function()
	{
	set.seed('123')
myobject <- list(
mynull = NULL,
mycomplex = lapply(eigen(matrix(-rnorm(9),3)), round, 3),
mymatrix = round(matrix(rnorm(9), 3),3),
myint = as.integer(c(1,2,3)),
mydf = cars,
mylist = list(foo='bar', 123, NA, NULL, list('test')),
mylogical = c(TRUE,FALSE,NA),
mychar = c('foo', NA, 'bar'),
somemissings = c(1,2,NA,NaN,5, Inf, 7 -Inf, 9, NA),
myrawvec = charToRaw('This is a test')
);

# https://github.com/douglascrockford/JSON-js
# // JSON numbers must be finite. Encode non-finite numbers as null.
# // Indeterminate form (e.g. 0 * Infinity, 1 ** Infinity, Infinity / Infinity, Infinity - Infinity)
# https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/NaN

# RHON is not JSON ...

# https://stackoverflow.com/a/16530895/184614
# http://mathjs.org
# math.i;                         // i
# math.sqrt(-4) 
# [1, 2, "NA", "NaN", 5, "Inf", "-Inf", 9, "NA"]
# x = [1, 2, null, NaN, 5, Infinity, -Infinity, 9, null]
# typeof(x); # object 
# x = myobject$somemissings
# [1]    1    2   NA  NaN    5  Inf -Inf    9   NA
# > y = as.integer(x)
# [1]  1  2 NA NA  5 NA NA  9 NA





j1 = jsonlite::serializeJSON(myobject);
j2 = rjson::toJSON(myobject); # j2 fails


test = "[ 1, 2, 3, 4]";
j1 = jsonlite::unserializeJSON(test);	# j1 returns something, not correct
j2 = rjson::fromJSON(test);				# j2 passes 

# javascript 
# y = JSON.parse('[1, 2, 3, 4]')
# (4) [1, 2, 3, 4]

test = "{ 1, 2, 3, 4}"; # fails in javascript 
# fails everywhere, good ...

# parallel arrays or arrays of assoc ... CHOICE for dataframe ...
# 'properties'
# typeof
# what about functions ... remove (srcref) as option .... 
# y = JSON.parse('["alex", "bob", "cindi"]')
# typeof(y)  # 'object'
# typeof(y[1]) # 'string'

# complex ... parallel arrays / parallel matrices ... 
#					Re = 
#					Im = 


# y = JSON.parse('["alex", 1,2, "bob", "cindi", "U+EA08"]')
# typeof(y[2])
# a javascript array may become a list if the types are variable ...
# 


test = '["alex", 1,2, "bob", "cindi", "U+EA08"]';
j1 = jsonlite::unserializeJSON(test);	# j1 fails
j2 = rjson::fromJSON(test);				# j2 got this correct, how did it fail on easier one ... 
 

# rsjon is the template 
# separate VECTOR logic from other object 
# typeof on the VECTOR ... Inf ==> Infinity not "Inf" 
# is.na ... is.nan ... is.infinite   !!!!!=  is.character ... one is vec, one is element ...
# javascript number ... 1e+28 
# Math.PI ... check constants ...
# https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/E
#  y = [Math.PI, Math.E, Math.LN10, Math.LN2, Math.LOG10E, Math.LOG2E, Math.SQRT2, Math.SQRT1_2]

# [3.141592653589793, 2.718281828459045, 2.302585092994046, 0.6931471805599453, 0.4342944819032518, 1.4426950408889634, 1.4142135623730951, 0.7071067811865476]
 
test = '[1,2,3,4,5]';
jsonlite::unserializeJSON(test);
rjson::fromJSON(test);

test = '[Math.PI, Math.E, Math.LN10, Math.LN2, Math.LOG10E, Math.LOG2E, Math.SQRT2, Math.SQRT1_2]';
jsonlite::unserializeJSON(test);
rjson::fromJSON(test);	

# check.num as INTEGER ... math.cleanup (integer) 
# if possible, coerce to integer on vector ... allTRUE

obj.info = function(...)
	{
	
	# properties 
	# symbolname ... x 
	# typeof 
	# names
	# colnames
	# rownames 
	# attributes {nested}
	
	
	# vector is fundamental unit (in R)
	# if length is 1, I can drop the [] around the vector ... 
	
	# parallel is better for scrolling purposes (left-to-right ignored), up/down 
	
	# THiS is RHON not JSON ... RVON .. R-humanVerse-Object-Notation
	#  RHON seems best ... 
	# function, stringify, keeps "\n\t", store in one line 
	

	
	}



identical(jsonlite::unserializeJSON(jsonlite::serializeJSON(myobject)), myobject);
	return (myobject);
	}



JSON.pretty = function(jsonstr, indent=4)
	{
	# maybe requires two passes ...
	
	
	
	}


JSON.parse = function(jsonstr)
	{
	jsonlite::unserializeJSON(jsonstr);
	}



JSON.stringify = function(obj, digits=16, prettify=FALSE, indent=5)
	{
	j1 = jsonlite::serializeJSON(obj, digits=digits, pretty=FALSE);
	if(prettify) 
		{ 
		j2 = jsonlite::prettify(j1, indent=indent);
		return (j2);
		}
cat("\n\n");
	return (j1);
	}





JSON = function(x)
	{
	.Inf_ = "U+E007";  # PUA (57351)
	.Inf  = "U+E008";  # PUA (57352)
	.NULL = "U+EA08";  # PUA (59912)
	
	# var rx_one = /^[\],:{}\s]*$/;
	# var rx_two = /\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4})/g;
	# var rx_three = /"[^"\\\n\r]*"|true|false|null|-?\d+(?:\.\d*)?(?:[eE][+\-]?\d+)?/g;
	# var rx_four = /(?:^|:|,)(?:\s*\[)+/g;
	# var rx_escapable = /[\\"\u0000-\u001f\u007f-\u009f\u00ad\u0600-\u0604\u070f\u17b4\u17b5\u200c-\u200f\u2028-\u202f\u2060-\u206f\ufeff\ufff0-\uffff]/g;
	# var rx_dangerous = /[\u0000\u00ad\u0600-\u0604\u070f\u17b4\u17b5\u200c-\u200f\u2028-\u202f\u2060-\u206f\ufeff\ufff0-\uffff]/g;
	

	f = function(n) 
		{
		# this is for dates 
        # // Format integers to have at least two digits.
		n = as.character(n);
		if(strlen(n) < 2) { paste0("0",n); }
        n;
		}
		
	date.toJSON = function(el)
		{
		if(!is.finite(el)) { return(.NULL.); }  # NULL 
		# format  2000-12-31T22:03:44Z ... UTC 
		# as.Date or as.posix
		}
	
	boolean.toJSON = function(el)
		{
		# these could be vectors in R?
		if(el) { return("true"); }
		"false";
		}
		
	number.toJSON = function(el)
		{
		el = check.math(el, method = "integer");
		}
	
	string.toJSON = function(el)
		{
		as.character(el);  # nothing to do here ...
		}
		
	gap = NULL;
    indent = NULL;

	# // table of character substitutions
	meta = list(
				 "\b" = "\\b",
				 "\t" = "\\t",
				 "\n" = "\\n",
				 "\f" = "\\f",
				 "\r" = "\\r",
				 "\"" = "\\\"",
				 "\\" = "\\\\"
				);
    rep = NULL;

 

	quote.utf = function(a) 
		{
		# "\\u" + ("0000" + a.charCodeAt(0).toString(16)).slice(-4)
		# "\u001F" ... '\\u001f'
		a.int = charCodeAt(a, 1);
		a.hex = int2base(a.int, base=16, to.length=4);
		res = paste0("\"", "\\u" , a.hex , "\"");	
cat(res);
		res;
		}
		
		
	quote = function(string) 
	{

# // If the string contains no control characters, no quote characters, and no
# // backslash characters, then we can safely slap some quotes around it.
# // Otherwise we must also replace the offending characters with safe escape
# // sequences.
# https://community.adobe.com/t5/illustrator-discussions/strange-amp-annoying-json-behavior-in-extendscript/td-p/11964686

		# /[\\"\u0000-\u001f\u007f-\u009f\u00ad\u0600-\u0604\u070f\u17b4\u17b5\u200c-\u200f\u2028-\u202f\u2060-\u206f\ufeff\ufff0-\uffff]/g;
        # rx_escapable.lastIndex = 0;
        # return rx_escapable.test(string)
            # ? "\"" + string.replace(rx_escapable, function (a) {
                # var c = meta[a];
                # return typeof c === "string"
                    # ? c
                    # : "\\u" + ("0000" + a.charCodeAt(0).toString(16)).slice(-4);
            # }) + "\""
            # : "\"" + string + "\"";
    }

	
	}