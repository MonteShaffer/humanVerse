
# we parse.lang on a filename, so a larger 
# OBJECT appends the filename ... 
# we have idx , line.no , char.no , WHAT ... dataframe
parse.lang = function(lines, method="purge", debug=TRUE)
	{
	# method == "purge" ... just remove them 
	# method == "parse"/"format"/"replace" ... replace with COMMENT_R ...
	METHOD = prep.arg(method, n=3, case="upper");  # "PUR" is the KEY ....
	
	# inside strings, R parser !Q?FMDS
	lines = str_replace("\\n", "\n", lines);  
	lines = str_replace("\\t", "\t", lines);
	
	COMMENT_R = "#";
	COMMENT_Rh = "#'";  # hadley 
	COMMENT_RhV = "#.";  #OPPOSITE 
	

	SLASH_COMMENT  = c("//");  
	
	
	START_OF_LINE_COMMENTS = c("C", "c", "%"); 
	MULTILINE = list();
	MULTILINE$c = list("start" = "/*", "end" = "*/");  		# c-base
									# ACTUALLY this is // COMMENT 
	MULTILINE$h = list("start" = "<!--", "end" = "//-->");	# html-base
	MULTILINE$p = list("start" = '"""', "end" = '"""');		# eww PYTHON 
	
	# cat doesn't work on list ... print_r ... as nested cat?
	
	
	# --SAME LINE--
	# code; /* jdfslj */ moreCode; ===>   code; moreCode; # jdfslj 
	
	# I may have SEVERAL LINES in the STACK?
	# NO, if I have started multiline and get to a new line ...
	# I scan the new line to see if there is not anything worthwhile
	# if not, I prepend COMMENT_R and move to the next line ...
	
	# new lines   # garbage collection 
	out.lines = out.gc = NULL;
	
	IN_STRING = FALSE;
	IN_STRING.type = NULL;
	
	TAG_COUNT = 0;
	# WHAT AM I COUNTING +/- FOR ... back to zero and next element is "/"
	
	# increment
	# decrement ... if zero 
	
	
	# if found, and TAG_COUNT == 0
	# maybe some substring thing ...
	
	IN_MULTILINE = FALSE;
	IN_MULTILINE.type = NULL;
	
	
	i = j = 1;  # i is line number ... 
				# j is char in line ...
	slen = 0;
	scan = NULL;
	
					"Fake for substitution"
	PIPE = "|";		FPIPE = "[p]";  # we could make unique "`[.p.]`"
	TAB = "\t";   	FTAB = "[t]";
	NEWLINE = "\n";	FNEWLINE = "[n]";
	
	
	DQ = '"';		FDQ = "[dq]";
	SQ = "'";		FSQ = "[sq]";
	OP = "(";
	CP = ")";
	COMMA = ",";
	
	cval = NULL;  # cc has been evaluated? TRUE or FALSE
	# maybe create a STACK and push/pop ... 
	cc = ">";	  # current char ... single value 
	pc = "-";		# previous character  cc-1
	ppc = "-";		# previous character  cc-2
	pppc = "/"; 		# previous character  cc-3
	ppppc = "/"; 	# previous character  cc-4 ... for "//-->"
	
	# for this given TAG, you could define ...
	# p.end = paste0(rev(c(cc,pc,ppc,pppc,ppppc)), collapse="");
	# p.end == MULTILINE$h$end

	
	line.eval = NULL;		# line has been evaluated? TRUE or FALSE 
	line = NULL;	# just putting all GLOBAL VARS at top ...
	
	cline = NULL;  # updated current line 
	cres = NULL;  # command remaining (partial line?) 
	status = "searching";  # 'searching' for multiline or 'parsing' it 
	
	
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
			str.replace( 	c(NEWLINE, TAB, PIPE, DQ, SQ),
							c(FNEWLINE,FTAB,FPIPE,FDQ,FSQ),
						ws), 
			collapse="");
		}
		
	show.status = function(level)
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

cat("\n\t [",idx,"] => line [",i,":",j,"] ... ", whitespace(cc), 
				"\t p: ", TAG_COUNT, 
				"\t io: ", OBJ, 
				"\t is: ", IN_STRING, 
				"\t ist: ", IN_STRING.type, 
				"\t cv: ", cval, 
				"\t cres: ", whitespace(cres), 
	"\n");
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
		cline = c(cline, cc);	
		assign("cline", cline, envir=envir );
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
			assign("IN_STRING.type", "DQ", envir=envir );
			} else {
					if(IN_STRING.type == "DQ")
						{
						# we are at the end of IN_STRING ... TIE it OFF 
						assign("IN_STRING.type", NULL, envir=envir );
						assign("IN_STRING", FALSE, envir=envir );
						} else {
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
						# we are at the end of IN_STRING ... TIE it OFF 
						assign("IN_STRING.type", NULL, envir=envir );
						assign("IN_STRING", FALSE, envir=envir );
						} else {
								# we encountered a SQ inside the DQ envir 
								add.to();
								}
					}
		}
		
	do.OPEN = function() {}
	do.OPEN = function(envir=parent.env(environment()))
		{
if(debug)
	{
cat("\n do.OPEN \n");
	}
		assign("cval", TRUE, envir=envir );
		TAG_COUNT = TAG_COUNT + 1;
		assign("TAG_COUNT", TAG_COUNT, envir=envir );
		if(TAG_COUNT > 1) { add.to(); }
		}
		
		
	finish = function() {}
	finish = function(envir=parent.env(environment()))
		{
		# we are at the end of a multiline, tie it off ... 
		# I would JUST FORCE a line break at this point ... slightly
		# alters line-numbering, but prevents issues with rare EDGE cases 
		# like in the __FILE__
		# the remainder of the line is truncated and just called line ... 
		# back to searching ...
		
if(debug)
	{ 
cat("\n finish \n");
	}		
		
		}
		
		
	do.CLOSE = function() {}
	do.CLOSE = function(envir=parent.env(environment()))
		{
if(debug)
	{ 
cat("\n do.CLOSE \n");
	}
		assign("cval", TRUE, envir=envir );
		TAG_COUNT = TAG_COUNT - 1; 
		assign("TAG_COUNT", TAG_COUNT, envir=envir );
		if(TAG_COUNT >= 1) { add.to(); }
		if(TAG_COUNT == 0)
			{
if(debug)
	{
cat("\n CLOSING MULTILINE ... TAG_COUNT = 0 \n");
	}
			finish();
			}
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
		
		# we don't add.to unless it is IN_STRING 
		if(is.null(cval)) { add.to(); }
		show.status("char");
		}
		
	parse.init = function() {}
	parse.init = function(envir=parent.env(environment())) 
		{
		scan = str.explode("", line); # truncated by r ... 
		assign("scan", scan, envir=envir );
if(debug)
	{	
cat("\n SCAN: ", scan, "\n");
	}
		slen = length(scan);
		assign("slen", slen, envir=envir );
		parsing();
		}
	
	
	parsing = function() {}
	parsing = function(envir=parent.env(environment()))
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
			
		tline = str.removeWhiteSpace(line, replace="", n=1);
		# does the line start with a comment ... if so skip ...
		
		# if not, we have to scan.char in MODE_SEARCHING 
		# we are looking for /* */ inline 
		# we are looking for // inline 
		# we are looking for /* ... to START PARSING ... 
		
		
		
		# do some magic ... if IN_MULTILINE, let's go to 'parsing'


		}
	
	main = function() {}
	

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
		# don't call parsing TWICE 
		if(!line.eval && status == "parsing" ) { parse.init();  }		
		}	
	
	out = list.return(out);
	minvisible(out, print=FALSE);
	out;
	}









lines = readLines("parse/colors.txt");
print(lines);
	
	