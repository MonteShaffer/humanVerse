

# write a new readString function that doesn't require a length ... all of it by default ... 

ini.file = "C:/_git_/github/MonteShaffer/humanVerse/humanVerse/inst/R/config/system/constants.ini";

# inistr = readChars(ini.file, 8888);
# inistr = readChars(ini.file, 9999);

inistr = readTextFile(ini.file);
lines = str.explode("\r\n", inistr);


#lines = lines[1:33];

ini.parse = function(lines, verbose=FALSE)
	{
	envir = environment();
	
	EMPTY			= "";
	COMMA 			= ","; 
	
	COMMENT			= "#";
	COMMENT_INI		= ";";
	COMMENTS		= c(COMMENT,COMMENT_INI,COMMA);
	
	MEMORY_STORE 	= "^=";
	EVAL_RCODE 		= "=R";
	BACKTICK		= "`";
 	
	SINGLE_QUOTE 	= "'";
	DOUBLE_QUOTE 	= '"';

	MEMORY 			= list();
	RES 			= list();
	fin 			= NULL;
	line.no 		= 0;
	CONTINUE_KEY	= ""; # if NOT empty, we are continuing on multiline 
	pkey 			= "";		
	pval 			= "";
	cparent 		= "";
	
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
gggassign("RES", RES);
	}
		
		line_ = str.trim(line);
		first = charAt(line_,1);
		# SKIP LINES ...
		if(first %in% COMMENTS) { next; }
		if((CONTINUE_KEY == EMPTY) && (first == EMPTY)) { next; }
		
		# stop here ... HARDSTOP =====>
		if(first == "=") { break;}
		
		# WE HAVE A HEADER 
		if(first == "[")  # not equal to anything ...  
			{
			info 			= ini.cleanKey(line);
			cparent 		= info;
			# unnecessary to do any assignment 
			# it won't appear in the STACK if nothing follows, so what?
			# RES				= ini.checkKey(cparent, RES);
			pkey 			= "";  # these are not parents ... 
			pval 			= "";
			
			next;
			}
		
		# WE HAVE A LINE ... LIKELY a KEY/VALUE PAIR 
		# THIS IS TOUGH???
		# maybe if COMMENT_KEY == ""
		#if(str.contains("=", line))  # a continued string may have an equal?
		if(COMMENT_KEY == "")	
			{ 
			info = str.explode("=", line);
			rkey = str.trim(info[1]);
			rval = str.trim(info[2]);
			
			hasMemory = hasRcode = FALSE;
			# allows for a bit of white space between ^ = R 
			if( str.contains(MEMORY_STORE, paste0(rkey,"=")) ) 
				{ 
				hasMemory = TRUE; 
				rkey = str.trim(str.end("^", rkey, trim=TRUE));
				}
			if( str.contains(EVAL_RCODE, paste0("=",rval)) ) 	 
				{ 
				hasRcode  = TRUE; 
				rval = str.trim(str.begin("R", rval, trim=TRUE));
				}
		
normal = function() {} 		

			key = ini.cleanKey(rkey);  # raw key 
			val = parse.walkTheLine(rval, COMMENTS); 
			fin = property.get("more", val); # are we finished?
			GTG = FALSE; # I am good to go ... multiline may stop this ... 
			if(is.null(fin)) { GTG = TRUE; }
			if(GTG)
				{
				if(hasRcode) 	{ val = eval(parse(text=val)); }
				if(hasMemory) 	{ MEMORY[[key]] = val; }
				
				RES = ini.assignVal(key, val, cparent, RES);
				}
			pkey = key;
			pval = val; 
			next;
			}
##################################
		# multiline?
		# earlier a partially assigned val was assigned ...
		# I have the CURRENT_KEY stored if partial 
		# I have the pval, which may stack over multiple lines ...
		# this will not work on eval?  Or it could if I waited on eval 
		# if CURRENT_KEY ...
		
		# we keep walking lines until the string TERMINATES
		# we set CURRENT_KEY to ""
		# we evaluate and move on ...
multiline = function() {} 		
		rval = line;  # grab everything (whitespace);
		
		# more = "pval";  # pval that grows ... 
		val = parse.walkTheLine(rval, COMMENTS, fin);
		fin = property.get("more", val); # are we finished?

			GTG = FALSE; # I am good to go ... multiline may stop this ... 
			if(is.null(fin)) { GTG = TRUE; }
			if(GTG)
				{
				# maybe do eval(parse in function enclosure
				# unlist the MEMORY_keys there ... 
				if(hasRcode) 	{ val = eval(parse(text=val)); }
				if(hasMemory) 	{ MEMORY[[key]] = val; }
				
				RES = ini.assignVal(CONTINUE_KEY, val, cparent, RES);
				
				CONTINUE_KEY = "";				
				pkey = "";
				pval = val; 
				next;
				}
		}

		
		
		
	RES;
	}


ini.evalMe = function(txt, MEMORY)
	{
	list.extract(MEMORY);
	
# dput(MEMORY);
# dput(salt);
	# "%nPr%" ... needs to be "`%nPr%`" into parser ... 
	#  eval(parse(text = "salt %in% MEMORY" ) ) ; # works fine 
	
	# so if %nPr% is an isolate ... we get it to correctly MAP to a function
	# a second stage of the parser will have to assign the KEYS to VALUES 
	# alias.add(key, val) ... INTERNAL function, easy-breasy, lemon-eays
	
	
	
	# eval(parse(text=val));
	
	
	}
	
	
	

# ini.assignVal(key, val, cparent, RES);
ini.assignVal = function(key, val, cparent, RES)
	{
	ckeys = str.explode("|", cparent);
	nc = length(ckeys);  # let's hardcode this to 5?
	
	keys = str.explode("|", key);
	nk = length(keys);
	
	all = c(ckeys, keys);  
	#	allparent = paste0(all, collapse="|");
	# RES = ini.checkKey(allparent, RES);
	
	# now here, we will walk and store results, very much like ini.checkKey ...
	list.smartAssign(RES, all, val);
	}

ini.checkKey = function(cparent, RES)
	{
	ckeys = str.explode("|", cparent);
	nc = length(ckeys);  # let's hardcode this to 5?
	for(i in 1:nc)
		{
		if(i == 1)
			{
			if(is.null(RES[[ ckeys[1] ]])) { RES[[ ckeys[1] ]] = list(); }
			}
		if(i == 2)
			{
			if(is.null(RES[[ ckeys[1] ]][[ ckeys[2] ]])) { RES[[ ckeys[1] ]][[ ckeys[2] ]] = list(); }
			}
		if(i == 3)
			{
			if(is.null(RES[[ ckeys[1] ]][[ ckeys[2] ]][[ ckeys[3] ]])) { RES[[ ckeys[1] ]][[ ckeys[2] ]][[ ckeys[3] ]] = list(); }
			}
		if(i == 4)
			{
			if(is.null(RES[[ ckeys[1] ]][[ ckeys[2] ]][[ ckeys[3] ]][[ ckeys[4] ]])) { RES[[ ckeys[1] ]][[ ckeys[2] ]][[ ckeys[3] ]][[ ckeys[4] ]] = list(); }
			}
		if(i == 5)
			{
			if(is.null(RES[[ ckeys[1] ]][[ ckeys[2] ]][[ ckeys[3] ]][[ ckeys[4] ]][[ ckeys[5] ]])) { RES[[ ckeys[1] ]][[ ckeys[2] ]][[ ckeys[3] ]][[ ckeys[4] ]][[ ckeys[5] ]] = list(); }
			}
			
		if(i == 6)
			{
			if(is.null(RES[[ ckeys[1] ]][[ ckeys[2] ]][[ ckeys[3] ]][[ ckeys[4] ]][[ ckeys[5] ]][[ ckeys[6] ]])) { RES[[ ckeys[1] ]][[ ckeys[2] ]][[ ckeys[3] ]][[ ckeys[4] ]][[ ckeys[5] ]][[ ckeys[6] ]] = list(); }
			}
			
		if(i == 7)
			{
			if(is.null(RES[[ ckeys[1] ]][[ ckeys[2] ]][[ ckeys[3] ]][[ ckeys[4] ]][[ ckeys[5] ]][[ ckeys[6] ]][[ ckeys[7] ]])) { RES[[ ckeys[1] ]][[ ckeys[2] ]][[ ckeys[3] ]][[ ckeys[4] ]][[ ckeys[5] ]][[ ckeys[6] ]][[ ckeys[7] ]] = list(); }
			}
			
		if(i == 8)
			{
			if(is.null(RES[[ ckeys[1] ]][[ ckeys[2] ]][[ ckeys[3] ]][[ ckeys[4] ]][[ ckeys[5] ]][[ ckeys[6] ]][[ ckeys[7] ]][[ ckeys[8] ]])) { RES[[ ckeys[1] ]][[ ckeys[2] ]][[ ckeys[3] ]][[ ckeys[4] ]][[ ckeys[5] ]][[ ckeys[6] ]][[ ckeys[7] ]][[ ckeys[8] ]] = list(); }
			}
			
		if(i == 9)
			{
			if(is.null(RES[[ ckeys[1] ]][[ ckeys[2] ]][[ ckeys[3] ]][[ ckeys[4] ]][[ ckeys[5] ]][[ ckeys[6] ]][[ ckeys[7] ]][[ ckeys[8] ]][[ ckeys[9] ]])) { RES[[ ckeys[1] ]][[ ckeys[2] ]][[ ckeys[3] ]][[ ckeys[4] ]][[ ckeys[5] ]][[ ckeys[6] ]][[ ckeys[7] ]][[ ckeys[8] ]][[ ckeys[9] ]] = list(); }
			}
			
		if(i == 10)
			{
			if(is.null(RES[[ ckeys[1] ]][[ ckeys[2] ]][[ ckeys[3] ]][[ ckeys[4] ]][[ ckeys[5] ]][[ ckeys[6] ]][[ ckeys[7] ]][[ ckeys[8] ]][[ ckeys[9] ]][[ ckeys[10] ]])) { RES[[ ckeys[1] ]][[ ckeys[2] ]][[ ckeys[3] ]][[ ckeys[4] ]][[ ckeys[5] ]][[ ckeys[6] ]][[ ckeys[7] ]][[ ckeys[8] ]][[ ckeys[9] ]][[ ckeys[10] ]] = list(); }
			}
			
			
			
		
		if(i > 10) { print(ckeys); stop("not implemented");}
		
		}
	
	RES;		
	}
	
	
	
ini.cleanKey = function(key)
	{
	# in case they put doubles (R vs php)
	key = str.replace("[[","[", key);  
	key = str.replace("]]","]", key);
	key = str.replace(c(SINGLE_QUOTE,DOUBLE_QUOTE), "", key);
	
	# str.contains("[]", key);  ??? 
	
	# this is header logic 
	kvec = str.explode("]", key);
	logic = str.contains("[", kvec);
	kvec = unlist(str.explode("[", kvec[logic]));
	if(!is.null(kvec))
		{
		logic = v.test(kvec, "");
		res = kvec[!logic];
		return( paste0(res, collapse="|") );
		}
	
	# normal key 
	key;
	}
	
	
	
	
	











# multiline logic is messing it up... skip for now ... 

parse.ini = function(lines, verbose=FALSE)
	{
# verbose=FALSE;
	# KISS ... store keys as | = val ...

	COMMA = ","; 
	COMMENT		= "#";
	COMMENT_INI	= ";";
	COMMENTS	= c(COMMENT,COMMENT_INI,COMMA);
	MEMORY_STORE = "^=";
	EVAL_RCODE = "=R";
	
	SINGLE_QUOTE 		= "'";
	DOUBLE_QUOTE 		= '"';

	
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
		info = parse.walkTheLine(line_, COMMENTS);
			
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
gggassign("RES", RES);
	}
		line_ = str.trim(line);
		first = charAt(line_,1);
		# SKIP LINES ...
		if(is.null(continue) && (first %in% COMMENTS || first == EMPTY)) { next; }
		
		

# if(line.no > 20) { stop("testing"); }

		if(first == "=")
			{
			break;  # stop here ... HARDSTOP =====>
			}
		
		if(first == "[")  # not equal to anything ... 
			{
			doNew();
			next;
			}
		
		prepKeyVal();
		
		nval = parse.walkTheLine(val, COMMENTS, continue=continue);
		
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




