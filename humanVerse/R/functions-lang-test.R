
# THIS is *beautiful* ... will it work?

# we parse.lang on a filename, so a larger 
# OBJECT appends the filename ... 
# we have idx , line.no , char.no , WHAT ... dataframe
parse.lang = function(lines, debug=FALSE)
	{
	# inside strings, R parser !Q?FMDS
	lines = str_replace("\\n", "\n", lines);  
	lines = str_replace("\\t", "\t", lines);


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
	
	IN_STRING = FALSE;
	IN_STRING.type = NULL;
	p.count = 0; # number of paranthesis

	i = j = 1;  # i is line number ... 
				# j is char in line ...
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
			cat("\n", "searching: \t line [",i,"] ... [", slen, "] ", whitespace(line), "\n");	
			pause();		
			} 
		if(level == "char")
			{

if(nnn == 1)
	{
cat("\n\t [",idx,"] => line [",i,":",j,"] ... ", 
				whitespace(pc), " => ",  whitespace(cc),
				"\t p: ", p.count, 
				"\t io: ", OBJ, 
				"\t is: ", IN_STRING, 
				"\t ist: ", IN_STRING.type, 
				"\t cv: ", cval, 
				"\t cres: ", whitespace(cres), 
	"\n");
	} else {
	
cat("\n\t\t\t\t",						" ... ", 
				whitespace(pc), " => ", whitespace(cc),
				"\t p: ", p.count, 
				"\t io: ", OBJ, 
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
	add.to = function(envir=parent.env(environment()))
		{
		if(IN_STRING)
			{
			cres = c(cres, cc);	
			assign("cres", cres, envir=envir );
			}
		}
	
	do.DQ = function() {}
	do.DQ = function(envir=parent.env(environment()))
		{
if(debug)
	{
cat("\n do.DQ \n");
	}
		# all the way up or just the CALLER?		
		assign("cval", TRUE, envir=envir );
		if(!IN_STRING)
			{
			IN_STRING = TRUE;
			assign("IN_STRING", TRUE, envir=envir );
			assign("OBJ", FALSE, envir=envir );
			assign("IN_STRING.type", "DQ", envir=envir );
			} else {
					if(IN_STRING.type == "DQ")
						{
						# we have a escaped DQ 
						# what if ppc is also a backslash and I am in a string ... I think we are fine ...
						if(pc != BACKSLASH)
							{
							# we are at the end of IN_STRING ... TIE it OFF 
							assign("IN_STRING.type", NULL, envir=envir );
							assign("IN_STRING", FALSE, envir=envir );
							} else {
if(debug)
	{
cat("\n DQ inside DQ \n");
	}
									# we encountered a DQ inside the DQ envir 
									add.to();
									}
						} else {
if(debug)
	{
cat("\n DQ inside SQ \n");
	}
								# we encountered a DQ inside the SQ envir 
								add.to();
								}
					}
		}
		
	do.SQ = function() {}
	do.SQ = function(envir=parent.env(environment()))
		{
if(debug)
	{
cat("\n do.SQ \n");
	}
		# all the way up or just the CALLER?		
		assign("cval", TRUE, envir=envir );
		if(!IN_STRING)
			{
			IN_STRING = TRUE;
			assign("IN_STRING", TRUE, envir=envir );
			assign("OBJ", FALSE, envir=envir );
			assign("IN_STRING.type", "SQ", envir=envir );
			} else {
					if(IN_STRING.type == "SQ")
						{
						# we have a escaped SQ 
						# what if ppc is also a backslash and I am in a string ... I think we are fine ...
						if(pc != BACKSLASH)
							{
							# we are at the end of IN_STRING ... TIE it OFF 
							assign("IN_STRING.type", NULL, envir=envir );
							assign("IN_STRING", FALSE, envir=envir );
							} else {
									# we encountered a SQ inside the SQ envir 
if(debug)
	{
cat("\n SQ inside SQ \n");
	}
									add.to();
									}								
						
						} else {
if(debug)
	{
cat("\n SQ inside DQ \n");
	}
								# we encountered a SQ inside the DQ envir 
								add.to();
								}
					}
		}
		
	do.OP = function() {}
	do.OP = function(envir=parent.env(environment()))
		{
if(debug)
	{
cat("\n do.OP \n");
	}
		assign("cval", TRUE, envir=envir );
		p.count = p.count + 1;
		assign("p.count", p.count, envir=envir );
		if(p.count > 1) { add.to(); }
		}
		
	do.CP = function() {}
	do.CP = function(envir=parent.env(environment()))
		{
if(debug)
	{ 
cat("\n do.CP \n");
	}
		assign("cval", TRUE, envir=envir );
		p.count = p.count - 1; 
		assign("p.count", p.count, envir=envir );
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
	do.COMMA = function(envir=parent.env(environment()))
		{
if(debug)
	{
cat("\n do.COMMA \n");
	}
		# what if comma is just in the text ... 
		assign("cval", TRUE, envir=envir );
		if(IN_STRING)
			{
			add.to();
			} else {		
					process.one();
					}
		}
	
	process.one = function() {}
	process.one = function(envir=parent.env(environment())) 
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
		
		
		# OBJ becomes %s 
		if(is.null(ctype) && OBJ) 	
			{ 
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
						# str.count(ex, "<") == str.count(ex, ">")
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
		row = df.row(idx, i, j); row$more = more; row$what = what;
		df = rbind(df, row);
		assign("df", df, envir=envir );
		
		# idx is the element of the "", "", "", n, "", 
		# res[[idx]] = what;  # cres = strvec or OBJ or EMPTY
		# assign("res", res, envir=envir );
							
		idx = 1 + idx; 
		assign("idx", idx, envir=envir );
		# resetting ... if we got this far, we shouldn't have PARSE errors
		# that is, we don't need to reset p.count or IN_STRINGX
		OBJ = TRUE; 
		assign("OBJ", OBJ, envir=envir );
		cres = NULL;
		assign("cres", cres, envir=envir );
		
		"cres" %GLOBAL% cres;
		"line" %GLOBAL% line;
		"what" %GLOBAL% what;
		"res" %GLOBAL% res;
		"idx" %GLOBAL% idx;
		
		## TODO 
		# compute end from slen and j + 1 
		### WHY TRUNCATE THE LINE, we have 'j' ... just keep going ...
		line = substring(line, j+1, slen);
# print(line); print(j); print(slen); stop("monte");
		assign("line", line, envir=envir );
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
	finish = function(envir=parent.env(environment())) 
		{
		# we collected everything between wrap.lang(EVERYTHING)
		# it is possible that the line has two of them, so we 
		# process one already truncated the line by j 
		# truncate where we were (j), and update that as the line 
		# and go back to status scanning ...
		# store 'df' to 'out' 
		# STATUS = searching ... have line updated 
		# searching();
		colnames(df) = c("idx", "line.no", "char.no", "what", "content");		
		# we don't need nested SQ ESCAPED in this ... 
		### this then gets rid of actual backslashes as well ... 
		df$content = str.replace('\\"','"',df$content); 
		df$content = str.replace("\\'","'",df$content); 
		out[[count.finds]] = df;
# df will *also* nullify on init()?
		df = NULL;
		assign("out", out, envir=envir );
		assign("df", df, envir=envir );
		assign("status", "searching", envir=envir );
		
if(debug)
	{			
print(str(df));
pause();
# stop("finished");
	}
		}	
				
	scan.char = function() {}
	scan.char = function(envir=parent.env(environment()))
		{				
		show.status("char", 1);
		# cc as "current char"
		assign("cval", NULL, envir=envir );
		if(is.null(cval) && cc == DQ) { do.DQ(); }
		if(is.null(cval) && cc == SQ) { do.SQ(); }
		if(is.null(cval) && cc == OP) { do.OP(); }
		if(is.null(cval) && cc == CP) { do.CP(); }
		if(is.null(cval) && cc == COMMA) { do.COMMA(); }
		
		# we don't add.to unless it is IN_STRING 
		if(is.null(cval)) { add.to(); }
		show.status("char", 2);
		
		pc = cc;
		assign("pc", pc, envir=envir );
		}
		
	scan.init = function() {}
	scan.init = function(envir=parent.env(environment())) 
		{
		scan = str.explode("", line); # truncated by r ... 
		assign("scan", scan, envir=envir );
if(debug)
	{	
cat("\n SCAN: ", scan, "\n");
	}
		slen = length(scan);
		assign("slen", slen, envir=envir );
		scanning();
		}
	
	
	scanning = function() {}
	scanning = function(envir=parent.env(environment()))
		{		
		if(slen > 0)
			{
			for(j in 1:slen)
				{
				cc = scan[j]; 
				assign("j", j, envir=envir );
				assign("cc", cc, envir=envir );
				scan.char();
				}
			}		
		}
	

	
	
	searching = function() {}
	searching = function(envir=parent.env(environment()))
		{
		assign("line.eval", TRUE, envir=envir );
			# make certain "FN.NAME(" are attached in search
		tline = str.removeWhiteSpace(line, replace="", n=1);
		if(str.contains( paste0(fn.search,"("), tline))
			{
			assign("df", NULL, envir=envir );
			assign("cres", NULL, envir=envir );
			assign("idx", 1, envir=envir );
			assign("status", "scanning", envir=envir );
			
			count.finds = 1 + count.finds;
			assign("count.finds", count.finds, envir=envir );
			
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
				assign("line", r, envir=envir );
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
		if(!line.eval && status == "searching") { searching(); }
		# don't call scanning TWICE 
		if(!line.eval && status == "scanning" ) { scan.init();  }		
		}	
	
	out = list.return(out);
	minvisible(out, print=FALSE);
	out;
	}









lines = readLines("parse/parse-lang.txt");
print(lines);
	
	