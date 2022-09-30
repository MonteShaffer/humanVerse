

ini.file 	= "C:/_git_/github/MonteShaffer/humanVerse/humanVerse/inst/R/config/system/constants.ini";

ini.file 	= "C:/_git_/github/MonteShaffer/humanVerse/humanVerse/inst/R/config/system/humanVerse.ini";

inistr 		= readTextFile(ini.file);
lines 		= str.explode("\r\n", inistr);



ini.parse = function(lines, verbose=FALSE, ignore.eval = FALSE)
	{
	envir = environment();
	
	# if I get multiline string parsing working, adding 
	# multiline comments should not be difficult .... 
	# what about // comments ... two characters long , nice to have 
	
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
	
.__main__. = function() {}
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


.__skip = function() {} 		

		# SKIP LINES ...
		if(first %in% COMMENTS) { next; }
		if((CONTINUE_KEY == EMPTY) && (first == EMPTY)) { next; }


.__hardstop = function() {} 		
		
		# stop here ... HARDSTOP =====>
		# ===========> HARDSTOP 
		# <=========== HARDSTOP 
		if(first == "=" || first == "<") { break;}

.__header = function() {} 		
		
		# WE HAVE A HEADER 
		if(first == "[")  # not equal to anything ...  
			{
			info 			= ini.cleanKey(line);
			cparent 		= info;
			pkey 			= "";  # these are not parents ... 
			pval 			= "";
			
			next;
			}
		
		# WE HAVE A LINE ... LIKELY a KEY/VALUE PAIR 
.__normal = function() {} 		

		if(COMMENT_KEY == "")	
			{ 
			# "%~=%" = is.equal;  ;; this "=" is a problem 
			ne = str.count("=", line);
			# in this logic space, we should not have ne == 0 ... 
			if(ne > 1)
				{
				rkey = parse.walkTheLine(line, COMMENTS);
.__equal.sign.issue = function() {} 		
				
				info = str.remainder(rkey, line);
				rval = str.remainder("=", info);
				} else {	
						# ne == 1 ... 
						info = str.explode("=", line);
						rkey = str.trim(info[1]);
						rval = str.trim(info[2]);
						}
			
			hasMemory = hasRcode = FALSE;
.__memory.eval = function() {} 		

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
		 
	

			key = ini.cleanKey(rkey);  # raw key 
			val = parse.walkTheLine(rval, COMMENTS);

.__normal.GTG = function() {} 				
			fin = property.get("more", val); # are we finished?
			GTG = FALSE; # I am good to go ... multiline may stop this ... 
			if(is.null(fin)) { GTG = TRUE; } else { CONTINUE_KEY = key; }
			if(GTG)
				{
				if(hasRcode) 	{ val = ini.evalMe(val, MEMORY, ignore.eval = ignore.eval ); }
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
.__multiline = function() {} 		
		rval = line;  # grab everything (whitespace);
		
		# more = "pval";  # pval that grows ... 
		val = parse.walkTheLine(rval, COMMENTS, fin);
		fin = property.get("more", val); # are we finished?

.__multiline.GTG = function() {} 	
			GTG = FALSE; # I am good to go ... multiline may stop this ... 
			if(is.null(fin)) { GTG = TRUE; }
			if(GTG)
				{
				# maybe do eval(parse in function enclosure
				# unlist the MEMORY_keys there ... 
				# if(hasRcode) 	{ val = eval(parse(text=val)); }
				if(hasRcode) 	{ val = ini.evalMe(val, MEMORY, ignore.eval = ignore.eval ); }
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


ini.evalMe = function(txt, MEMORY, ignore.eval = FALSE)
	{
	if(ignore.eval) { return(txt); }
	list.extract(MEMORY);
	
# dput(MEMORY);
# dput(salt);
	# "%nPr%" ... needs to be "`%nPr%`" into parser ... 
	#  eval(parse(text = "salt %in% MEMORY" ) ) ; # works fine 
	
	# so if %nPr% is an isolate ... we get it to correctly MAP to a function
	# a second stage of the parser will have to assign the KEYS to VALUES 
	# alias.add(key, val) ... INTERNAL function, easy-breasy, lemon-eays
	# let's SKIP this for now ...
	# [ALIAS] should just store the keys ...
	
	
	
	eval(parse(text=val));	
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
	
	
	