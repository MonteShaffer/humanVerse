
# THIS is *beautiful* ... will it work?

# if I am _IN_OBJECT ... it can get messy ...
# is it recursive, have to count () ... how to not which COMMA to stop 
# I can enter into a string mode as IN_OBJECT 
# basically ignor stuff in () ... STREAM through it ...
# DESIGN RULES ... simple element ? 
# c("str", n, "st", fun(dkk,n=3,str="kd,"), " str");
# c("str", n, "st", ifelse({},{},{fun(dkk,n=3,str="kd,")}), " str");
# c("str", n, "st", x == 3, " str");

# we parse.lang on a filename, so a larger 
# OBJECT appends the filename ... 
# we have idx , line.no , char.no , WHAT ... dataframe
parse.lang = function(lines, debug=FALSE, from.readLines=TRUE)
	{
	envir = environment();
print(environment());
print(parent.frame(1));
print(parent.env(environment()));

	# inside strings, R parser !Q?FMDS
	if(from.readLines)
		{
		lines = str_replace("\\n", "\n", lines);  
		lines = str_replace("\\t", "\t", lines);
		# # I don't want to remove TRUTH
		
		### lines = str_replace("\"", '"', lines); 
		
		##	# what it does to \\ inside "" is what?
		## "the \\\"*SYSTEM*\\\" is" ==> "the \\\\\\"*SYSTEM*\\\\\\" is"
		## the internal parser is NOT SMART
	
		# maybe just go to file/scan ... my own readlines, binary?
		# EOL is \r\n not \n ... 
		}


	fn.search = "prep.msg";  	# univariate for now 
								# we need to make certain we are not inside a string? wrap.lang("hi wrap.lang"
								# should be fine... first occurence ... 
								# exploding on line level, and reading ... status ... 
	# TAG is new type ... has ansi/sgml tags ... SKIP %t
	# OBJ ...................................... SKIP %s
	# BLANK .................................... SKIP %w (whitespace)
	# GOOD-TO-GO ............................... Marked for translation
	
	
	out = NULL;  # this will be a nested list ... JSON-like to replace PO/POT syntax ... portable to WEB API as well ... HELP can be INTL
	df = NULL;  # out will be a list of dataframes for each fn found 
				# across the system, we can find unique WHAT 
				# so the translator has less work to do ...
	
	IN_OBJECT = FALSE;
	IN_STRING = FALSE;
	IN_STRING.type = "";
	quote = IN_STRING.type;   # this memory lives passed IN_STRING.type 
	p.count = 0; # number of paranthesis

	i = j = 1;  # i is line number ... 
				# j is char in line ...
	j.start = 1;
	
	slen = 0;
	scan = NULL;
	
	count.finds = 0;

				###	"Fake for substitution"
	PIPE = "|";		FPIPE = "[p]";  # we could make unique "`[.p.]`"
	TAB = "\t";   	FTAB = "[t]";
	NEWLINE = "\n";	FNEWLINE = "[n]";
	
	
	WHAT_OBJECT = "-.OBJECT.-";
	WHAT_EMPTY = "-.EMPTY.-";
	WHAT_TAG = "-.TAG.-";
	WHAT_LANG = "-.TRANSLATE.-";
	
	COMMENT = "#";
	BACKSLASH = "\\";
	DQ = '"';		FDQ = "[dq]";
	EDQ = "\"";  # escaped dq ... 
	SQ = "'";		FSQ = "[sq]";
	ESQ = '\'';  # escaped sq ...
	OP = "(";
	CP = ")";
	COMMA = ",";
	OBJ = TRUE;	  # we can have cat("\n", "The answer is [", ans, "] to ");
					# how to identify , ans , isOBJ ==> %s eventually
	cval = NULL;  # cc has been evaluated? TRUE or FALSE
	ctype = NULL; # when we finish a COMMA block, what do we have?
					# OBJ , like `, ans ,` ABOVE ... mapped to %s (sprintf)
					# STR ... which is what **lang** will STORE
					# EMPTY ... `,"\n\t   \n",` white space, ignored 
								# maybe map to %w as in white space ...
	cc = "";	  	# current char ... single value 
	pc = ""; 		# previous character (for escape codes, 
										# like \" inside DQ
	
	
	line.eval = NULL;		# line has been evaluated? TRUE or FALSE 
	line = NULL;	# just putting all GLOBAL VARS at top ...
	
	idx = 1; 	  # how many (COMMAS-1) "in" are we ?
	cres = NULL;  # comma res 
	status = "searching";  # 'searching' for function or 'scanning' it 
	
	
	pause = function()
		{
if(debug)
	{
		x = readline(prompt="Press [enter] to continue, [ESC] to quit");
cat("\n", "[",i,"]" , lines[i], "\n");
	}
		}
		
		
	whitespace = function(ws)
		{
		paste0(
			str.replace( 	
				c(NEWLINE, TAB, PIPE, DQ, SQ, COMMA, BACKSLASH),
				c(FNEWLINE,FTAB,FPIPE,FDQ,FSQ, "[COMMA]", "[BACKSLASH]"),
						ws), 
			collapse="");
		}
		
	show.status = function(level, nnn=1)
		{
if(debug)
	{
			
		if(level == "line")
			{ # searching / scanning mode (mostly searching)
			cat("\n", status, ": \t line [",i,"] ... [", slen, "] ", whitespace(line), "\n");	
			pause();		
			} 
		if(level == "char")
			{

if(nnn == 1)
	{
cat("\n\t [",idx,"] => line [",i,":",j,"] ... ", 
				whitespace(pc), " => ",  whitespace(cc),
				"\t p: ", p.count, 
				"\t io: ", IN_OBJECT, 
				"\t is: ", IN_STRING, 
				"\t ist: ", IN_STRING.type, 
				"\t cv: ", cval, 
				"\t cres: ", whitespace(cres), 
	"\n");
	} else {
	
cat("\n\t\t\t\t",						" ... ", 
				whitespace(pc), " => ", whitespace(cc),
				"\t p: ", p.count, 
				"\t io: ", IN_OBJECT, 
				"\t is: ", IN_STRING, 
				"\t ist: ", IN_STRING.type, 
				"\t cv: ", cval, 
				"\t cres: ", whitespace(cres), 
	"\n");
	}
			}
		# where are we ... i, j loop 
		# current char 
		# state of variables OBJ etc 
		# state of IN_STRING ... 
		# level == "line" ... searching mode 
		# level == "char" ... scanning mode (in the weeds)
		flush.console();
	}
		}
	
	add.to = function() {}
	add.to = function()
		{
		cres = c(cres, cc);		cres 		%TO% envir ;
		}
	
	do.OBJ = function() {}
	do.OBJ = function()
		{
if(debug)
	{
cat("\n do.OBJ \n");
	}
		cval 		= TRUE; 	cval 		%TO% envir ;
		IN_OBJECT 	= TRUE; 	IN_OBJECT 	%TO% envir ;
		
		add.to();
		return(TRUE);		
		}
	
	do.DQ = function() {}
	do.DQ = function()
		{
if(debug)
	{
cat("\n do.DQ \n");
	}

		cval 		= TRUE; 	cval 		%TO% envir ;
		if(IN_OBJECT)
			{
			# we encountered a DQ inside the OBJ envir 
			add.to();
			return(TRUE);
			}
		if(!IN_STRING)
			{
			# start a string 
			cres = NULL;
			IN_STRING = TRUE;		IN_STRING 		%TO% envir ;
			IN_OBJECT = FALSE;		IN_OBJECT 		%TO% envir ;
			IN_STRING.type = "DQ";	IN_STRING.type 	%TO% envir ;
			j.start = j+1;			j.start 		%TO% envir ;			
			return(TRUE);
			} 
			
		if(IN_STRING.type == "DQ")
			{	
			# we have a escaped DQ 
			# what if ppc is also a backslash and I am in a string ... I think we are fine ...
			if(pc != BACKSLASH)
				{
				# we are at the end of IN_STRING ... TIE it OFF 
				IN_STRING 		= FALSE;			IN_STRING 		%TO% envir ;
				quote 			= IN_STRING.type;		quote 		%TO% envir ;
				IN_STRING.type 	= "";				IN_STRING.type 	%TO% envir ;			
				} else {
if(debug)
	{
cat("\n DQ inside DQ \n");
	}
						# we encountered a DQ inside the DQ envir 
						add.to();
						}
			return(TRUE);
			} 
		if(IN_STRING.type == "SQ")
			{	
if(debug)
	{
cat("\n DQ inside SQ \n");
	}
			# we encountered a DQ inside the SQ envir 
			add.to();
			return(TRUE);
			}
		stop("how did we get here");
		}

		
	do.SQ = function() {}
	do.SQ = function()
		{
if(debug)
	{
cat("\n do.SQ \n");
	}
		# all the way up or just the CALLER?		
		cval 		= TRUE; 	cval 		%TO% envir ;
		if(IN_OBJECT)
			{
			# we encountered a SQ inside the OBJ envir 
			add.to();
			return(TRUE);
			}
		if(!IN_STRING)
			{
			# start a string 
			cres = NULL;			cres			%TO% envir ;
			IN_STRING = TRUE;		IN_STRING 		%TO% envir ;
			IN_OBJECT = FALSE;		IN_OBJECT 		%TO% envir ;
			IN_STRING.type = "SQ";	IN_STRING.type 	%TO% envir ;
			j.start = j+1;			j.start 		%TO% envir ;	
			return(TRUE);
			} 
			
		if(IN_STRING.type == "SQ")
			{	
			# we have a escaped SQ 
			# what if ppc is also a backslash and I am in a string ... I think we are fine ...
			if(pc != BACKSLASH)
				{
				# we are at the end of IN_STRING ... TIE it OFF 
				IN_STRING 		= FALSE;			IN_STRING 		%TO% envir ;
				quote 			= IN_STRING.type;		quote 		%TO% envir ;
				IN_STRING.type 	= "";				IN_STRING.type 	%TO% envir ;	
				} else {
if(debug)
	{
cat("\n SQ inside SQ \n");
	}
						# we encountered a SQ inside the SQ envir 
						add.to();
						}
			return(TRUE);
			} 
		if(IN_STRING.type == "DQ")
			{	
if(debug)
	{
cat("\n SQ inside DQ \n");
	}
			# we encountered a SQ inside the DQ envir 
			add.to();
			return(TRUE);
			}
		stop("how did we get here");
		}
		
	do.OP = function() {}
	do.OP = function()
		{
if(debug)
	{
cat("\n do.OP \n");
	}
		cval 		= TRUE; 	cval 	%TO% envir ;
		p.count = p.count + 1;	p.count %TO% envir ;
		if(p.count > 1) { add.to(); } else { cres = NULL; } 
								cres 	%TO% envir ;
		}
		
	do.CP = function() {}
	do.CP = function()
		{
if(debug)
	{ 
cat("\n do.CP \n");
	}
		cval 		= TRUE; 	cval 		%TO% envir ;
		p.count = p.count - 1; 	p.count 	%TO% envir ;
		if(p.count >= 1) { add.to(); }
		if(p.count == 0)
			{
if(debug)
	{
cat("\n CLOSING FUNCTION ... p.count = 0 \n");
	}
			# finished at FUNCTION LEVEL ... wrap it up ...
			# finish();
			process.one(); # this will call finish() because CP
			}
		}
 

	do.COMMA = function() {}
	do.COMMA = function()
		{
if(debug)
	{
cat("\n do.COMMA \n");
	}
		# what if comma is just in the text ... 
		cval 		= TRUE; 	cval 		%TO% envir ;
		if(IN_STRING)
			{
			add.to();
			} else {		
					process.one();
					}
		}
	
	process.one = function() {}
	process.one = function() 
		{
		# assign("ctype", NULL, envir=envir );
		# I think this is LOCAL to this function only 
		ctype = NULL;
		cres = paste0(cres, collapse="");
		what = ""; 
		more = NULL;
		
if(debug)
	{	
cat("\n process.one \n");		
dput(cres);
	}
# BECAUSE we added IN_STRING to "add.to" ... we lose the OBJECT NAME, 
		
		
		# IN_OBJECT becomes %s 
		if(is.null(ctype) && IN_OBJECT) 	
			{ 
			# what should NOW be populated
			if(what == "") { what = "%s"; }
			more 	= WHAT_OBJECT;	
			ctype	= TRUE; 
			}
		# EMPTY becomes %w (or skipped)
							
		if(is.null(ctype))
			{ 
			EMPTY = (str.trim(cres) == "");
			if(EMPTY) 
				{ 
				what 	= cres;
				more 	= WHAT_EMPTY; 	
				ctype	= TRUE; 
				}
			}
						# str.count("<", ex) == str.count(">", ex)
						# HAS_TAGs = (!is.na(str.between(cres, c("<","/>"))));
						# HAS_TAGS = (count.ansi(cres) > 1);
		if(is.null(ctype))
			{
			ca = count.ansi(cres);
if(debug)
	{	
cat("\n ANSI: ", ca, "\n");
	}
			HAS_TAGS = (ca > 0);
			if(HAS_TAGS) 
				{ 
				what 	= cres;
				more 	= WHAT_TAG;	
				ctype	= TRUE; 
				}
			}
		
		# # this is going to be language-ified
		if(is.null(ctype)) 			
			{ 
			what 	= cres;
			more 	= WHAT_LANG; 		
			ctype	= TRUE; 
			}
			
		#assign("ctype", ctype, envir=envir );
		# local to this function environ only 
		
		# what = property.set("line.no", what, i);
		# what = property.set("char.no", what, j);  # of the COMMA or CP
		# eventually set other properties (file, fn)
		# separate NUMERIC ...... from CHAR
			# quote = IN_STRING.type;
			
# saveState			
		j.start_ = j.start;
		j_ = j;
		if(IN_OBJECT)
			{
			# don't have to deal with SQ/DQ surrounding it 
			j.start = j.start + 1;
			j = j - 1;
			what = substring(lines[i], j.start, j);
			} else { 
					# SQ/DQ and COMMA/CP 
					j = j - 2; 
					}
		# df.row is using names to append df ... 
		row = df.row(idx, i, j.start, j, quote, more, what)
	
# restoreState	
		j = j_;
		j.start = j.start_;
		
		df = rbind(df, row);	df 			%TO% envir ;
							
		idx = 1 + idx; 			idx 		%TO% envir ;
		j.start = j; 			j.start 	%TO% envir ;
		
		# resetting ... if we got this far, we shouldn't have PARSE errors
		IN_OBJECT = FALSE; 		IN_OBJECT 	%TO% envir ;
		cres = NULL;			cres 		%TO% envir ;

		
if(debug)
	{
		.%GLOBAL% cres; 
		.%GLOBAL% line;
		.%GLOBAL% what;
		.%GLOBAL% res;
		.%GLOBAL% idx;
	}
		
		## TRUNCATE line for DEBUGGING PURPOSES ... scan already has REAL LINE 
		line = substring(line, j+1, slen);	line %TO% envir ;
		
if(debug)
	{	
cat("\n more: ", whitespace(more), "\n");
cat("\n what: ", whitespace(what), "\n");
pause();
show.status("char", 2);
	}
		
		if(cc == COMMA) { return(TRUE); }
		if(cc == CP) 	{ finish(); return(TRUE); }

show.status("char", 3);
pause();
traceback();
		stop("how did you get here");
		}

	finish = function() {}	
	finish = function() 
		{
		# we collected everything between wrap.lang(EVERYTHING)
		# it is possible that the line has two of them, so we 
		# process one already truncated the line by j 
		# truncate where we were (j), and update that as the line 
		# and go back to status scanning ...
		# store 'df' to 'out' 
		# STATUS = searching ... have line updated 
		# searching();
		colnames(df) = c("idx", "line.no", "char.s", "char.e", "quote", "what", "content");		
		# we don't need nested SQ ESCAPED in this ... 
		### this then gets rid of actual backslashes as well ... 
		df$content = str.replace('\\"','"',df$content); 
		df$content = str.replace("\\'","'",df$content); 
		out[[count.finds]] = df;
# df will *also* nullify on init()?
		status = "searching";
		df = NULL;					df 		%TO% envir ;
									out 	%TO% envir ;
									status 	%TO% envir ;
		
if(debug)
	{			
print(str(df));
pause();
# stop("finished");
	}
		}	
				
	scan.char = function() {}
	scan.char = function()
		{				
		show.status("char", 1);
		# cc as "current char"
		cval = NULL; 		cval %TO% envir ;
		
		
		if(is.null(cval) && cc == OP) { do.OP(); }
		
		if(p.count > 1)
			{
			if(is.null(cval) && cc == DQ) { do.DQ(); }
			if(is.null(cval) && cc == SQ) { do.SQ(); }
			
			if(is.null(cval) && cc == CP) { do.CP(); }
			if(is.null(cval) && cc == COMMA) { do.COMMA(); }
			}
		
		if(is.null(cval)) { add.to(); }
		
		show.status("char", 2);
		
		pc = cc;		pc %TO% envir ;		
		}
		
	scan.init = function() {}
	scan.init = function() 
		{
		scan = str.explode("", line); 	scan %TO% envir ; 
		
if(debug)
	{	
cat("\n SCAN: ", scan, "\n");
	}
		slen = length(scan);			slen %TO% envir ;
		scanning();
		}
	
	
	scanning = function() {}
	scanning = function()
		{		
		if(slen > 0)
			{
			for(j in 1:slen)
				{
				cc = scan[j]; 
									j %TO% envir ;
									cc %TO% envir ;
				scan.char();
				}
			}		
		}
	

	
	
	searching = function() {}
	searching = function()
		{
#print(parent.frame(1));
#print(parent.env(environment()));

		line.eval = TRUE;		line.eval %TO% envir ;
		
			# make certain "FN.NAME(" are attached in search
		tline = str.removeWhiteSpace(line, replace="", n=1);
		if(str.contains( paste0(fn.search,"("), tline))
			{
			df 	= NULL; 			df 		%TO% envir ;
			cres 	= NULL; 		cres 	%TO% envir ;
			idx 	= 1;			idx 	%TO% envir ;
			status 	= "scanning"; 	status 	%TO% envir ;
			
			count.finds = 1 + count.finds;
							count.finds 	%TO% envir ;
							
if(debug)
	{
cat("\n SEARCHING FOUND MATCH: ", tline, "\n\n");
	}
	
	# use pos here to do the first.paranthesis LOGIC ... 
	# then we skip whitespace NOT in a STRING, until we get to STRING/OBJECT ...
	# WE DON'T HAVE SKIP_WHITESPACE fully developed ... 
	# 'we' ... the mouse in my pocket ... me/gods
			tmp = str.explode(fn.search, line);
			# simple line, so we are vectors not lists of vectors 
			tlen = length(tmp);
			# just in case line says wrap.lang("wrap.lang" ... we put the second one back ...
			r = str.implode(fn.search, tmp[2:tlen]); # to be read, one character at a time ... updating chracter count (nested functions)
			b = tmp[1]; # this is before, scan for comments tags
						# this parser works on final R code 
						# can we assume it is parsed CORRECTLY, let's say yes
						# a comment tag could be inside of a string
						# would I even know .... 
						# a COMMENT in my rendering would suggest BAD parsed CODE 
			if(!str.contains(COMMENT,b))
				{
				# for now, hopefully the COMMENT isn't inside a string
				# from a previous line ... EDGE CASE
				# hard to imagine since I am reading the file from 
				# beginning search for first element ...
				line = r;		line %TO% envir ;
				scan.init();
				}				
			}
		}
	
	main = function() {}
	
# this is not what I will be doing... I will have an input of lines 
	# # # lines = str.explode("\r\n", str);
	# # # COMMA, not new lines  ??? 
	# # lines = str.explode(COMMA, str);
	# # # let's put it back now we have chunks ... 
	# # lines = paste0(lines,COMMA); 
print(lines);
	n = length(lines);
dput(n);
	for(i in 1:n)
		{
		line = lines[i];
if(debug)
	{	
cat("\n ################# BACK to MAIN ############# \n");
	}
		show.status("line");
		line.eval = FALSE;		
		j.start = 1;
		if(!line.eval && status == "searching") { searching(); }
		# don't call scanning TWICE 
		if(!line.eval && status == "scanning" ) { scan.init();  }		
		}	
	
	out = list.return(out);
	minvisible(out, print=FALSE);
	out;
	}







# ex = parse("parse/parse-lang-simple.txt");
# print(ex);

lines = readLines("parse/parse-lang-simple.txt");
		lines = str_replace("\\n", "\n", lines);  
		lines = str_replace("\\t", "\t", lines);
print(lines);

# fp = fopen("parse/parse-lang.txt");
	
	