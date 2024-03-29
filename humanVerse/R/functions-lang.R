
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
		msg = prep.msg("\n\n\t\t\t", "Welcome to the", "<i>humanVerse</i>", "\n\n\t\t\t", "For TRENDLINE type [",type,"] you cannot have any data", "\n\t\t\t\t",  "(currently [",n,"] rows) in [y] that is <= 0 (less than or equal to zero) ...", "\n\t\t\t\t", "*SYSTEM* is REMOVING ROWS and trying to COMPUTE", "\n\n");
		cat.warning(msg);
		# cat.warning("\n\n\t\t\t For TRENDLINE type [",type,"] you cannot have any data (currently [",n,"] rows) \n\t\t\t\t in [y] that is <= 0 (less than or equal to zero) ... \n\t\t\t\t REMOVING ROWS and trying to COMPUTE \n\n");
		df = subset(df, y > 0);
		n = nrow(df);
	';
	
	
	
	
	# envir=parent.env(environment())

# not true explosion as \n is INFLATED, maybe readLines or fopen/fread will do better at this ...


## back/translate YANDEX or some freq standalone python web app WITH OWN API



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
		str = strip.Htags(str);
		}
	str;	
	}
	
check.lang = function(str)
	{
	# we can override the LANG LOCALE here in real time if the translation.json object exists 
	
	str;
	}
	
	
count.ansi = function(str)
	{
	v  = str.count("<v>", str);		# this is a "garbage" tag used to 
	vc = str.count("</v>", str);	# ignore the element for prep.msg (TRANSLATE)
	i  = str.count("<i>", str);
	ic = str.count("</i>", str);
	u  = str.count("<u>", str);
	uc = str.count("</u>", str);
	b  = str.count("<b>", str);
	bc = str.count("</b>", str);
	l  = str.count("<bb>", str); 
	lc = str.count("</bb>", str);
	f  = str.count("<fg", str);
	fc = str.count("</fg>, str");
	g  = str.count("<bg", str);
	gc = str.count("</bg>", str);
	c  = str.count("<color", str);
	cc = str.count("</color>", str);
	sum(i, ic, u, uc, b, bc, l, lc, f, fc, g, gc, c, cc);
	}
	

#           italics  underline bold   bright (not <BR /> already taken)	
# ansi tags:  <i></i>  <u></u> <b></b> <bb></bb> 
#             <fg #abcdef></fg>  <bg "red"></bg>
#             <color fg="F33" bg="wsu:crimson"></color>
# ansi16-windows:black ... easier to just say 0c0c0c ?
# if hex ... hex => nearest neighbor 
# <br>ight will just BUMP the current color + 85 on RGB[1,3]
# I need to know what <br> is inside for current color ...
# MAYBE ix-ney br ... Next arguments are 5;n or 2;r;g;b
## <blink>  <so> strikethourhg
# https://www.w3schools.com/tags/tag_strike.asp
## <del>  <ins>   ... delete/ins ... <s> means EDIT: no longer correct ...
## <code> ... or <sample>
## <var>  ... formula 
## <strong> ... <b> with meaning 
## https://www.w3schools.in/html/blink-tag
# if ":", look in library ...
# if(not ":"), look in base ... 
# if comma "," ... 0,1,2 ... determine range => 0/255 
# if comman of length 4, assume c,m,y,k ... 
# good practice, separate "\n\t" as BLANK
# also good practice, separate "<i>", "CONTENT", "</i>"
# parser will recognize as TAGS and *NOT* show them to translator
# %s (variable) %w (whitespace) %t (tags) ... 
# if an element is TAGGED, I don't send to translator 
# "Welcome to the ", "<i>human<fg green>V</fg>erse!</i>"
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
	# cat.warning/cat.warning 
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
	
	

