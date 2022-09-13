
# wrap.lang could READ (PARSE) itself, find the needed key and display the updated TEXT ... easier to parse because EVAL'D inline ... loop over elements and MATCH ... 


wrap.parse = function(file, 
							base.locale = "en-us", 
							search=c("wrap.lang")
					)
	{
	# build LOCALE["en-us"]["filename"]["fn.name"]["line.no in FUNCTION body] = value with %s everywhere ... NOT whitespace "\n\t\t\t", skip those on the comma search ... ... eval(INSIDE of wrap.lang) to get a char vector ... ?  what if it is a value ...
##################### NOT ENCAPSULATED in QUOTES, skip to next comma and put %s .... #############################
 	
	## cat.warning("\n\n\t\t\t For TRENDLINE type [",type,"] you cannot have any data (currently [",n,"] rows) \n\t\t\t\t in [y] that is <= 0 (less than or equal to zero) ... \n\t\t\t\t REMOVING ROWS and trying to COMPUTE \n\n");
	
	# readLines ... do this one line at a time 
	# str.split, find function ... start ... keep reading lines until function has closed ... count opening ( = 1 ... increment ( = ++ as they appear ... decrement when encounter ) ... when counter == 0, we have finished parsing the function ...
	tmp = str.split("wrap.lang", filestr);
	# foreach ... find ( .. then start reading, looking for strings and commas ... 
	
	
	}







str = '
if( ( TYPE == "EXP" || TYPE == "POW") && any(df$y<=0) )
		{
		msg = wrap.lang("\n\n\t\t\t", "For TRENDLINE type [",type,"] you cannot have any data", "\n\t\t\t\t",  "(currently [",n,"] rows) in [y] that is <= 0 (less than or equal to zero) ...", "\n\t\t\t\t", "*SYSTEM* is REMOVING ROWS and trying to COMPUTE", "\n\n");
		cat.warning(msg);
		# cat.warning("\n\n\t\t\t For TRENDLINE type [",type,"] you cannot have any data (currently [",n,"] rows) \n\t\t\t\t in [y] that is <= 0 (less than or equal to zero) ... \n\t\t\t\t REMOVING ROWS and trying to COMPUTE \n\n");
		df = subset(df, y > 0);
		n = nrow(df);
	';

# not true explosion as \n is INFLATED, maybe readLines or fopen/fread will do better at this ...



lines = str.explode("\n", str)
n = length(lines);
out = NULL;
status = "searching";
in.string = FALSE;
in.string.type = NULL;
within.string = NULL;
p.count = 0; # number of paranthesis

DQ = '"';
SQ = "'";
OP = "(";
CP = ")";
COMMA = ",";
OBJ = TRUE;

for(i in 1:n)
	{
	line = lines[i];
	if(str.contains("wrap.lang", line))
		{
		res = NULL; idx = 1; cres = NULL;  # comma res 
		status = "scanning";
		tmp = str.explode("wrap.lang", line);
		# simple line, so we are vectors not lists of vectors 
		r = tmp[2]; # to be read, one character at a time ... updating chracter count (nested functions)
		scan = str.explode("", r);
		slen = length(scan);
		for(j in 1:slen)
			{
			char = scan[j];
			
			cval = NULL;
			
			# in.string = FALSE or TRUE toggle ...
			# do I really care about commas 
			if(is.null(cval)) && char==COMMA)
				{
				cval = TRUE;
				ctype = NULL;
				if(is.null(ctype) && OBJ) { what = "OBJECT"; ctype=TRUE;}
				if(is.null(ctype) && str.trim(cres) == "") { what = "EMPTY"; ctype=TRUE; }
				if(is.null(ctype))
					{
					what = cres;  # this is going to be language-ified
					}
				what = property.set("line.no", what, i);
				# eventually set other properties (file, fn)
				# idx is the element of the "", "", "", n, "", 
				res[[idx]] = what;  # cres = strvec or OBJ
									# strvec could just be whitespace, skip ...
							
				idx = 1 + idx;
				# resetting
				OBJ = TRUE; 
				}
			
			if(is.null(cval)) && char == DQ)
				{
				cval = TRUE;
				#in.string = FALSE; in.string.type = NULL;
				if(!in.string)
					{
					in.string = TRUE;
					OBJ = FALSE;
					in.string.type = "DQ"; double quote
					} else {
							if(in.string.type == "DQ")
								{
								# eval within.str ... is it white space, if so ignore 
								# otherwise store to the language list ... at the line number ... in res...
								}
							}
				}
							
			if(is.null(cval)) && char == SQ)
				{
				cval = TRUE;
				#in.string = FALSE; in.string.type = NULL;
				if(!in.string)
					{
					in.string = TRUE;
					OBJ = FALSE;
					in.string.type = "SQ"; single quote
					} else {
							if(in.string.type == "SQ")
								{
								# eval within.str ... is it white space, if so ignore 
								# otherwise store to the language list ... at the line number ... in res...
								}
							}
				
				}
			
			if(is.null(cval)) && char == OP) 
				{ 
				cval = TRUE;
				p.count = p.count + 1; 
				}
			if(is.null(cval)) && char == CP) 
				{ 
				cval = TRUE;
				p.count = p.count - 1; 
				if(p.count == 0)
					{
					# finished at FUNCTION LEVEL ... wrap it up ...
dput(cres); stop("finished function");  
					}
				}
			if(is.null(cval))
				{
				# end of the road, we are "scanning" so append the char to the res
				
				cres = c(cres, char);
				}
			
			}
dput(line);
stop("monte");
		
		}
	if(status == "scanning")
		{
		# until we are at the end, then return to searching 
		
		}
	}



language.set = function(locale="en-us")
	{
	# this would search for "en-us"
	# if not found, then "en"
	# if not found then "DEFAULT"
	
	}
	
	
check.ansi = function(str)
	{
	# TODO 
	# does the system support ansi?
	# does the user have it in PREFERENCES as YES
	has.ansi = FALSE;
	if(!has.ansi)
		{
		str = strip.tags(str);
		}
	str;	
	}
	
check.lang = function(str)
	{
	# we can override the LANG LOCALE here in real time if the translation.json object exists 
	
	str;
	}
	
	
# was wrap.lang 
prep.msg = function(...,  type="msg", out="paste0", sep=" ")
	{
	
	str = dots.addTo(NULL, ...); 
	str = check.ansi(str);
	str = check.lang(str);
		# append res = property.set("msg.type", res, "message or notice or warning 3 or error");

	if(is.null(out)) { return(str); } # do nothing ...
	# out is a string ... one level deep
	fn.str = as.character(substitute(out));
	fn.obj = function.find(fn.str, character.only=TRUE);
	if(is.null(fn.obj)) { return(str); }
# dput( as.character(substitute(out)) );
	# find.function(out) ... 
	# res = paste0(str, collapse=sep);
	# do.call("paste0", list(str, collapse=""))
	# other functions may have different parameters ... 
	do.call(fn.str, list(str, collapse=sep));
	}

#           italics  underline bold   bright (not <BR /> already taken)	
# ansi tags:  <i></i>  <u></u> <b></b> <bb></bb> 
#             <fg #abcdef></fg>  <bg "red"></bg>
#             <color fg="F33" bg="wsu:crimson"></color>
# good practice, separate "\n\t" as BLANK
# also good practice, separate "<i>", "CONTENT", "</i>"
# parser will recognize as TAGS and *NOT* show them to translator
# %s (variable) %w (whitespace) %t (tags) ... 
# if an element is TAGGED, I don't send to translator 
# "Welcome to the ", "<i>human<fg green>V</fg>erse!"
# DESIGN RULES ... translator and color console in one swoop.
# also allows for color once we move up to the WEB API ...
# can also allow HELP to use color ...

	
# msg = wrap.lang("\n\n", "tldr;", "\n\n\n\t", "R-DEV believes this is poor programming practice to allow you to", "\n\t\t", "`suppressError()` so they have not included it in base R.", "\n\t\t", "It is probably true, but 'git-r-done' first, and then", "\n\t\t", "figure out the minutia such as why this function is", "\n\t\t", "throwing an error.  That is why I have past such a ", "\n\t\t",  "VERBOSE message to you, dear reader.", "\n\n\t", "By altering this function [set msg to something else, not empty ''],", "\n\t\t",  "you can reduce the length of this message.", "\n\n\t", "Or you can set the flag show.notice=FALSE to prevent it from printing.", "\n\t\t", "THIS my friends is how choice architecture works!  Cheers and Aloha!", "\n\n\n");
	
	
	
language = function(str, locale="en-us")
	{
	# this is a marker to understand that this element is LANGUAGE-SPECIFIC ... 
	# language.wrap 
	# wrap.lang(str)
	# needs to occur INPLACE, so I can parse it ... 
	# library:filename:fnname:line-no ... line-no of FN parsed, not the file? 
	# cat(I can get the text, n, more text , x, more text ... )
	# store the statement by line # with elements on the line ...
	## cat.warning("\n\n\t\t\t For TRENDLINE type [",type,"] you cannot have any data (currently [",n,"] rows) \n\t\t\t\t in [y] that is <= 0 (less than or equal to zero) ... \n\t\t\t\t REMOVING ROWS and trying to COMPUTE \n\n");
	
	# should cleanup and put multiline, with "\n" separate 
	# the "\n" formatting maybe discarded in WEB API ...
	
	# maybe a list of functions I am scanning
	# warning/stop/notice ... no notice, called message
	# cat.warning/warning.cat 
	# str.wrapper 
	# silent would be wrap.lang(str) ... e.g., within PIP ... part of cat, but only what I want to make translatable ...
	# str.commentWrapper
	#  str.wordWrap  ... str.wrap ... 
	# see suppressError, I build the string then pass to warning()
	# <bytecode: 0x000001369cc55030>
	# so wrap msg=wrap.lang() ... where which performs like paste0, so segmented   
	# wrap.lang(str, out="paste0", collapse="")
	# wrap.lang(str, out="cat", collapse="\n")
	# wrap.lang(str, out="warning(msg, call. = FALSE, immediate. = TRUE");
	
	# cat.warning requires ONE LAYER from parent ... this is TWO layers
	# KISS ... just do "paste0", collapse=""

	
	}
	
	

