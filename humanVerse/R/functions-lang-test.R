
# THIS is *beautiful* ... will it work?

parse.lang = function(str)
	{
cat(str);
	fn.search = "prep.msg";  	# univariate for now 
								# we need to make certain we are not inside a string? wrap.lang("hi wrap.lang"
								# should be fine... first occurence ... 
								# exploding on line level, and reading ... status ... 
	# TAG is new type ... has ansi/sgml tags ... SKIP %t
	# OBJ ...................................... SKIP %s
	# BLANK .................................... SKIP %w (whitespace)
	# GOOD-TO-GO ............................... Marked for translation
	
	
	out = NULL;  # this will be a nested list ... JSON-like to replace PO/POT syntax ... portable to WEB API as well ... HELP can be INTL
	in.string = FALSE;
	in.string.type = NULL;
	p.count = 0; # number of paranthesis

	i = j = 1;  # i is line number ... 
				# j is char in line ...
	slen = 0;

	DQ = '"';
	SQ = "'";
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
	cc = "";	  # current char ... single value 
	
	line.eval = NULL;		# line has been evaluated? TRUE or FALSE 
	lines = line = NULL;	# just putting all GLOBAL VARS at top ...
	
	res = NULL;   # everything inside the FUNCTION 
	idx = 1; 	  # how many COMMAS "in" are we ?
	cres = NULL;  # comma res 
	status = "searching";  # 'searching' for function or 'scanning' it 
	
	
	pause = function()
		{
		x = readline(prompt="Press [enter] to continue, [ESC] to quit");
cat("\n", "[",i,"]" , lines[i], "\n");
		}
		
	show.status = function(level)
		{
		if(level == "line")
			{ # searching / scanning mode (mostly searching)
cat("\n", "level: ", level, "\t line [",i,"] ... [", slen, "] ", line, "\n");	
pause();		
			} 
		if(level == "char")
			{
cat("\n\t [",idx,"] => line [",i,"]", "char [",j,"] : ", cc, "\t p: ", p.count, "\t is: ", in.string, "\t ist: ", in.string.type, "\t cv: ", cval, "\t ct: ", ctype, "\t cres: ", paste0(str.replace("\t","[t]",cres), collapse=""), "\n");
			}
		if(level == "DQ")
			{
			
			}
		if(level == "process.one")
			{
			
			}
		if(level == "finish")
			{
			
			}
		# where are we ... i, j loop 
		# current char 
		# state of variables OBJ etc 
		# state of in.string ... 
		# level == "line" ... searching mode 
		# level == "char" ... scanning mode (in the weeds)
		flush.console();
		}
	
	add.to = function() {}
	add.to = function(envir=parent.env(environment()))
		{
		cres = c(cres, cc);	
		assign("cres", cres, envir=envir );
		}
	
	do.DQ = function() {}
	do.DQ = function(envir=parent.env(environment()))
		{
cat("\n do.DQ \n");
		# all the way up or just the CALLER?		
		assign("cval", TRUE, envir=envir );
		if(!in.string)
			{
			in.string = TRUE;
			assign("in.string", TRUE, envir=envir );
			assign("OBJ", FALSE, envir=envir );
			assign("in.string.type", "DQ", envir=envir );
			} else {
					if(in.string.type == "DQ")
						{
						# we are at the end of in.string ... TIE it OFF 
						assign("in.string.type", NULL, envir=envir );
						assign("in.string", FALSE, envir=envir );
						} else {
								# we encountered a DQ inside the SQ envir 
								add.to();
								}
					}
		}
		
	do.SQ = function() {}
	do.SQ = function(envir=parent.env(environment()))
		{
cat("\n do.SQ \n");
		# all the way up or just the CALLER?		
		assign("cval", TRUE, envir=envir );
		if(!in.string)
			{
			in.string = TRUE;
			assign("in.string", TRUE, envir=envir );
			assign("OBJ", FALSE, envir=envir );
			assign("in.string.type", "SQ", envir=envir );
			} else {
					if(in.string.type == "SQ")
						{
						# we are at the end of in.string ... TIE it OFF 
						assign("in.string.type", NULL, envir=envir );
						assign("in.string", FALSE, envir=envir );
						} else {
								# we encountered a SQ inside the DQ envir 
								add.to();
								}
					}
		}
		
	do.OP = function() {}
	do.OP = function(envir=parent.env(environment()))
		{
cat("\n do.OP \n");
		assign("cval", TRUE, envir=envir );
		p.count = p.count + 1;
		assign("p.count", p.count, envir=envir );
		if(p.count > 1) { add.to(); }
		}
		
	do.CP = function() {}
	do.CP = function(envir=parent.env(environment()))
		{
cat("\n do.CP \n");
		assign("cval", TRUE, envir=envir );
		p.count = p.count - 1; 
		assign("p.count", p.count, envir=envir );
		if(p.count > 1) { add.to(); }
		if(p.count == 0)
			{
			# finished at FUNCTION LEVEL ... wrap it up ...
			# finish();
			process.one(); # this will call finish() because CP
			}
		}


	do.COMMA = function() {}
	do.COMMA = function(envir=parent.env(environment()))
		{
cat("\n do.COMMA \n");
		# what if comma is just in the text ... 
		assign("cval", TRUE, envir=envir );
		if(in.string)
			{
			add.to();
			} else {		
					assign("ctype", NULL, envir=envir );
					process.one();
					}
		}
	
	process.one = function() {}
	process.one = function(envir=parent.env(environment())) 
		{
cat("\n process.one \n");
show.status("char");
pause();
		cres = paste0(cres, collapse="");
		# OBJ becomes %s 
		if(is.null(ctype) && OBJ) 	{ what = "OBJECT"; 	ctype=TRUE; }
		# EMPTY becomes %w (or skipped)
							 EMPTY = (str.trim(cres) == "");
		if(is.null(ctype) && EMPTY) { what = "EMPTY"; 	ctype=TRUE; }
						# str.count(ex, "<") == str.count(ex, ">")
						HAS_TAGs = (!is.na(str.between(cres, c("<","/>"))));
		if(is.null(ctype) && HAS_TAGs) { what = "TAG"; 	ctype=TRUE; }
		
		# # this is going to be language-ified
		if(is.null(ctype)) 			{ what = cres; 		ctype=TRUE; }
		assign("ctype", ctype, envir=envir );
		
		what = property.set("line.no", what, i);
		what = property.set("char.no", what, j);  # of the COMMA or CP
		# eventually set other properties (file, fn)
		
cat("\n what: ", what, "\n");
pause();
		
		# idx is the element of the "", "", "", n, "", 
		res[[idx]] = what;  # cres = strvec or OBJ or EMPTY
		assign("res", res, envir=envir );
							
		idx = 1 + idx;
		assign("idx", idx, envir=envir );
		# resetting ... if we got this far, we shouldn't have PARSE errors
		# that is, we don't need to reset p.count or in.stringX
		OBJ = TRUE; 
		assign("OBJ", OBJ, envir=envir );
		cres = NULL;
		assign("cres", cres, envir=envir );
		
		## TODO 
		# compute end from slen and j + 1 
		line = substring(line, j+1, slen);
# print(line); print(j); print(slen); stop("monte");
		assign("line", line, envir=envir );
		if(cc == COMMA) { scanning(); return(TRUE); }
		if(cc == CP) { finish(); return(TRUE); }
show.status("char");
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
		# store 'res' to 'out' 
		# STATUS = searching ... have line updated 
		# searching();
		}	
				
	scan.char = function() {}
	scan.char = function(envir=parent.env(environment()))
		{				
		show.status("char");
		# cc as "current char"
		assign("cval", NULL, envir=envir );
		if(is.null(cval) && cc == DQ) { do.DQ(); }
		if(is.null(cval) && cc == SQ) { do.SQ(); }
		if(is.null(cval) && cc == OP) { do.OP(); }
		if(is.null(cval) && cc == CP) { do.CP(); }
		if(is.null(cval) && cc == COMMA) { do.COMMA(); }
		
		if(is.null(cval)) { add.to(); }
		show.status("char");
		}
		
	scanning = function() {}
	scanning = function(envir=parent.env(environment()))
		{
		scan = str.explode("", line); # truncated by r ... 
		slen = length(scan);
		assign("slen", slen, envir=envir );
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
		# what happens here! how to get back to for(i in 1:n) loop ...
		
		
		}
	

	
	
	searching = function() {}
	searching = function(envir=parent.env(environment()))
		{
		assign("line.eval", TRUE, envir=envir );
		if(str.contains(fn.search, line))
			{
			assign("res", NULL, envir=envir );
			assign("cres", NULL, envir=envir );
			assign("idx", 1, envir=envir );
			assign("status", "scanning", envir=envir );
			
			tmp = str.explode(fn.search, line);
			# simple line, so we are vectors not lists of vectors 
			tlen = length(tmp);
			# just in case line says wrap.lang("wrap.lang" ... we put the second one back ...
			r = str.implode(fn.search, tmp[2:tlen]); # to be read, one character at a time ... updating chracter count (nested functions)
			b = tmp[1]; # this is before, scan for comments tags
						# this parser works on final R code 
						# can we assume it is parsed CORRECTLY, let's say yes
			
			assign("line", r, envir=envir );
			scanning();			
			}
		}
	
	main = function() {}
	
	lines = str.explode("\n", str);
print(lines);
	n = length(lines);
	for(i in 1:n)
		{
		line = lines[i];
		show.status("line");
		line.eval = FALSE;		
		if(!line.eval && status == "searching") { searching(); }
		# don't call scanning TWICE 
		if(!line.eval && status == "scanning" ) { scanning();  }		
		}	
		
	out;
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
	
	