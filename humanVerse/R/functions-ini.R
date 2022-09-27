

# write a new readString function that doesn't require a length ... all of it by default ... 

ini.file = "C:/_git_/github/MonteShaffer/humanVerse/humanVerse/inst/R/sample.ini";
# inistr = readChars(ini.file, 8888);
# inistr = readChars(ini.file, 9999);
# lines = str.explode("\r\n", inistr);




#lines = lines[1:33];



parse.walkTheLine = function(){}
parse.walkTheLine = function(str, COMMENT="#", continue=NULL)
	{
	# I am not dealing with = signs ... just simple parser 

	SINGLE_QUOTE = "'";  # make these constants?
	DOUBLE_QUOTE = '"';
	BACKSLASH = "\\";
	# get to COMMENT or EOL, I stop, have what I need ...
	IN_STRING 		= FALSE;
	STRING_TYPE 	= NULL;
	nval			= "";
	if(!is.null(continue)) 
		{
		IN_STRING 	= continue[["IN_STRING"]];
		STRING_TYPE = continue[["STRING_TYPE"]];
		nval 		= continue[["nval"]];
		}
	
#cat("\n\n HEAD nval: ", nval, "\n\n");
#cat("\n\n str: ", str, "\n\n");	

	str = str.trim(str);
	strV = str.explode("", str);
	ns = length(strV);
	
	# write a generic str.walk function ... limited to this line ...
		
	
	cchar = "";
	pchar = "";
	i = 1;
	while(i <= ns)
		{
		pchar = cchar;
		cchar = strV[i];
		if(cchar == SINGLE_QUOTE || cchar == DOUBLE_QUOTE)
			{
			if(IN_STRING)
				{
				# already IN_STRING ...
				if(cchar != STRING_TYPE)
					{
					# we have ' in "envir" or " in 'envir' OKEY
					nval = paste0(nval, cchar);
					i %++%. ;
					next;
					}
				if(cchar == STRING_TYPE)
					{
					if(pchar == BACKSLASH)
						{
						# we have \' in 'envir'  or \" in "envir" OKEY
						nval = paste0(nval, cchar);
						i %++%. ;
						next;
						} else {
								# can I recover or do I have to stop ...
								# stop("looks like you have a QUOTE issue on line.no");
								
								# wait, this means the string is OVER
								IN_STRING = FALSE;
								break;
								}
					
					
					}
				
				}
			
			## just starting the STRING 
			IN_STRING = TRUE;
			if(cchar == SINGLE_QUOTE) { STRING_TYPE = SINGLE_QUOTE; } else { STRING_TYPE = DOUBLE_QUOTE; }
			i %++%. ;
			next;
			}
		
		if(cchar == COMMENT && !IN_STRING)
			{
			break;
			}
		# possible to have a missing CLOSING_STRING ... my parser won't care ...
		
		nval = paste0(nval, cchar);
		i %++%. ;
		next;
		
		}

	nval = str.trim(nval);
	# is there a use case when this goes wrong?
	# YEAH, a string with a COMMA inside ... Fort Knox
	# if(str.contains(COMMA, nval)) { nval = str.explode(COMMA,nval); }
	nnum = check.number(nval);
	if(allTRUE(nnum)) { nval = as.numeric(nval); }
	
	# if we have a multiline, and didn't close the STRING ... 
	if(IN_STRING)
		{
		if(str.trim(str) == "") { nval = paste0(nval,"\n"); }
		extra = list("nval" = nval, 
					"IN_STRING" = IN_STRING, 
					"STRING_TYPE" = STRING_TYPE);
		nval = property.set("extra", nval, extra);
		}
#cat("\n\n FOOT nval: ", nval, "\n\n");
	nval;
	}





# multiline logic is messing it up... skip for now ... 

parse.ini = function(lines)
	{
verbose=FALSE;
	# KISS ... store keys as | = val ...

	CONTINUE_KEY = "";
	MEMORY = list();
	RES = list();
	line.no = 0;
	pkey = "";
	pval = "";
	pparent = cparent = "";
	continue = NULL;
	
	envir = environment();
	
	prepKeyVal = function()
		{
if(verbose)
	{
cat("\n\n prepKeyVal() \n\n");
	}
		if(CONTINUE_KEY == "")
			{
			info = str.explode("=", line);
			key = str.trim(info[1]);
			# in case they put doubles (R vs php)
			key = str.replace("[[","[", key);  
			key = str.replace("]]","]", key);
			key = str.replace(c(SINGLE_QUOTE,DOUBLE_QUOTE), "", key);
if(verbose)
	{		
cat("\n\n"); dput(continue); cat("\n\n");
	}
			val = str.implode("=", info[-1]);			
			} else { 
					key = CONTINUE_KEY;
					val = line;					
					}
		key %TO% envir;
		val %TO% envir;
		}
		
	doNew = function()
		{
if(verbose)
	{
cat("\n\n doNew() \n\n");
	}
		info = parse.walkTheLine(line_, COMMENT=";");
			
		pparent = cparent;				pparent %TO% envir;	
		cparent = info;					cparent %TO% envir;	
		RES[[cparent]] = list();		RES %TO% envir;	
		pkey = pval = "";				pkey %TO% envir;	
										pval %TO% envir;	

if(verbose)
	{
print(RES);
	}
		}
	
	
	checkContinue = function()
		{
if(verbose)
	{
cat("\n\n checkContinue() \n\n");
	}
		if(CONTINUE_KEY != "")
			{
			key = CONTINUE_KEY;
			nval = pval;
			if(str.contains("[",key))
				{
				RES[[cparent]][[key]] = list.append(RES[[cparent]][[key]], nval);
				} else { RES[[cparent]][[key]] = nval; } 
			
			RES %TO% envir;	
				
			CONTINUE_KEY == "";			CONTINUE_KEY %TO% envir;
			continue = NULL;			continue %TO% envir;
			}
		}
		
		
	updateMultiLine = function()
		{
if(verbose)
	{
cat("\n\n updateMultiLine() with CONTINUE_KEY: ", CONTINUE_KEY, "\n\n");
	}
		status = ""
		continue = property.get("extra", nval);
		
		truth = "0";
		if(!is.null(continue)) { truth = "1"; }
		if(CONTINUE_KEY != "") { truth = paste0(truth,"1"); } else {truth = paste0(truth,"0");}
		
		if(truth == "00")
			{
			# continue is null 
			# CONTINUE KEY is EMPTY 
			
			pkey = key;
			pval = nval;
			status = "normal";
			}
			
		if(truth == "01")
			{
			# continue is null 
			# CONTINUE KEY is NOT EMPTY 
			# we had a multiline that finished 
			
			pkey = key;
			pval = nval;					pval %TO% envir;
			status = "end-multiline";
			# key = CONTINUE_KEY gets referenced inside 
			# contine = NULL, CONTINUE_KEY = "" AFTER ....
			checkContinue();
			}
			
		if(truth == "10")
			{
			# continue is not null 
			# CONTINUE KEY is EMPTY 
			# we had a multiline that started 
			
			CONTINUE_KEY = key;				CONTINUE_KEY %TO% envir;
			pkey = key;
			pval = nval;
			status = "started-multiline";
			}
			
		if(truth == "10")
			{
			# continue is not null 
			# CONTINUE KEY is NOT EMPTY 
			# we are in the middle of a streaming multiline
			
			pkey = key;
			pval = nval;
			status = "streaming-multiline";
			}
			
		key %TO% envir;
		nval %TO% envir;
		pkey %TO% envir;
		pval %TO% envir;
		return(status);
		}
		
	storeKeyVal = function()
		{
if(verbose)
	{
cat("\n\n storeKeyVal() \n\n");
	}
		### MEMORY ELMENTS CAN ONLY BE ONE LINERS .... 
		# can only be one liners ...
		if(!str.contains("[",key) && str.contains("^",key))
			{
			key = str.trim(str.replace("^", "", key));
			MEMORY[[key]] = nval;				MEMORY %TO% envir;
			}
			
		## GRAB INFO from MEMORY with ^ operator 
		if(str.contains("[",key) && str.contains("^",key))
			{
			# which element has this in it ...
			keys = str.trim(str.replace("]","",str.explode("[", key)));
			logic = str.contains("^", keys);
			keys = str.replace("^", "", keys);
				subkey = keys[logic];
			if(!is.null(MEMORY[[subkey]]))
				{
				keys[logic] = MEMORY[[subkey]];
				}
			
			key = str.implode("", "[" %.% keys %.% "]");
			}
			
		## STORE 
		if(str.contains("[]",key))
			{
			RES[[cparent]][[key]] = list.append(RES[[cparent]][[key]], nval);
			} else { RES[[cparent]][[key]] = nval; } 
		
		# nval didn't change 
		key %TO% envir;
		RES %TO% envir;
			
		}
	
	main = function() {}
############################### MAIN #######################
	for(line in lines)
		{
		line.no %++%.;
		
if(verbose)
	{
cat("\n\n ", line.no, " --> ", line, "\n\n");
	}

if(verbose)
	{
xxx = readline(prompt="Press [enter] to continue, [ESC] to quit");
	}
		line_ = str.trim(line);
		first = charAt(line_,1);
		# SKIP LINES ...
		if(is.null(continue) && (first == COMMENT || first == EMPTY)) { next; }
		
		

# if(line.no > 20) { stop("testing"); }

	
		if(first == "[")  # not equal to anything ... 
			{
			doNew();
			next;
			}
		
		prepKeyVal();
		
		nval = parse.walkTheLine(val, COMMENT=";", continue=continue);
		
		status = updateMultiLine();
		
if(verbose)
	{
cat("\n\n status: ", status, " key : ", key, " nval : ", nval, "\n\n");
	}
		if(status != "normal") { next; }
		
		
		storeKeyVal();
			
		pkey = key;
		pval = nval;

if(verbose)
	{		
cat("\n\n\t\t\t ", pkey, " --> ", pval, "\n\n");
print(str(RES));		
print(str(MEMORY));
cat("\n\n MDLFjkdlsj \n\n");
	}
		
		}
	RES;
	}




