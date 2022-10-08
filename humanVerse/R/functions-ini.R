


test = function(df)
	{
	.%THIS%.;
	
	info = paste0( .now(), "|", 
				paste0(capture.output(dput(THIS)),collapse="") );
		
	# info = list("now" = .now(), "THIS" = THIS);
	# info = paste0( .now(), "|", JSON.stringify(THIS) );
dput(info);
	}


	

	
	

path.build = function(partial)
	{
	# no leading dot ... part of the SYSTEM 
	# ./file ... could be anywhere in the cascade 
	# ./../../file ... could be anywhere in the cascade ...
	
	}


ini.parseFiles = function() {} 
ini.parseFiles = function(inifilesORDERmatters, 
							master = "cache/ini/master.rds", 
							use.cache = TRUE, smart.num = TRUE, ...) # other params to ini.parse 
	{
	.%THIS%.	 
	
	THIS %GLOBAL%.;
  
	NS = .now();
	uniqid = .uniqid();	 
	TIMESTAMP = .timestamp("YYYY-MM-DD");

	## full paths at this level 
	
	mf = master; 
	# mf 		= "C:/_R_/-humanVerse-/SYSTEM/cache/ini/master.rds";
	
	# mf = str.replace("cache/ini/master.rds", master, mf);
	
	if(use.cache && file.exists_(mf)) { return( readRDS(mf) ); }
	
	d 		= check.dir(mf);
	stem 	= mf %.-% d;
######################
# THIS NEEDS TO MOVE # 
######################
	log 	= paste0(d, "-logs-/", TIMESTAMP, ".log");
			check.file(log);
	

	cat.log( log, str.commentOneLine("START -INI- LOG ENTRY") );
	cat.log( log, .timestamp("humanVerse", tz="GMT") );	
	cat.log( log, NS );
	cat.log( log, TIMESTAMP );
	cat.log( log, uniqid );
	
	
	
######	openSesame(log);
#  dput(THIS); 

	cat.log( log, "--FUNCTION_INFO--" );
	cat.dput(THIS, log);
	
	
	# chmod(400) on original sources in inst/R/ after INSTALL?
	
	
	ofiles = inifilesORDERmatters;	
	
	# sp =  "C:/_git_/github/MonteShaffer/humanVerse/humanVerse/inst/R/config/";
	
	sp = ofiles[1]; 
	
	spd	= check.dir(sp);
	
	
	
	cat.log( log, "--FILES--" );
	cat.dput( ofiles, log);

	# insources = paste0(spd, ofiles);
	insources = ofiles; 
	
	oexts 	= check.ext(ofiles, dotless=FALSE);
	#ostem = (outs %.-% d) %-.% exts;
.cat( "MONTE ... ",  ofiles ,"\n\n", oexts );
	opaths = ofiles %-.% oexts; 
	
	opos = check.list(str.pos("/", opaths));
	ons = list.getLengths(opos);
	
	opartials 	= str.before("/", opaths, ons);
	ostems 		= str.after("/", opaths, ons);
	fstems 		= str.after("/", ofiles, ons); # with .ini 

opartials = "";

	outs = paste0(d, opartials, "/", ostems, ".rds");
	outs = prep.path(outs);  # cleans up at the same time 


	# checksums live here ...
	backups	= paste0(d, "-backups-/", TIMESTAMP , "/");
	backups = check.dir(backups); 
			
	outs.copy = paste0(backups, opartials, "/", uniqid, "_", ostems, ".rds");
	outs.copy = prep.path(outs.copy); 
	# if exists in the date ... append a .uniqid-md5 ...
	 
	ochecksums = paste0(d, "-backups-/", opartials, "/", ostems, "_"); 
	ochecksums = prep.path(ochecksums, trailing=FALSE);

	# this stem still has .rds ... FINE ... it's different 
	mfchecksum = paste0(d, "-backups-/", stem , "_");
	mfchecksum = prep.path(mfchecksum, trailing=FALSE);
	 
	mfcopy = paste0(backups, opartials, "/", uniqid, "_", stem);
	mfcopy = prep.path(mfcopy, trailing=FALSE);
		check.dir(mfcopy);
		
	# # mfchecksum = paste0(d, "-backups-/", stem , "_");
	# mfcopy = paste0(backups, uniqid, "_", stem);
	# mfcopy = prep.path(mfcopy, trailing=FALSE);
		# check.dir(mfcopy);
		
	
	MEMORY = list();# RES = list();
	# FINAL = list();  
	OUT = list();
	
	n = length(outs);	
	for(i in 1:n)
		{
		fstem 		= fstems[i];
		ostem		= ostems[i];
		insource 	= insources[i];
		out 		= outs[i];
		out.copy 	= outs.copy[i];
 
		# if we have to parse, we need the file anyway ...
		instring	= readTextFile(insource);
		checksum	= str.toMD5(instring);
		# checksum 	= checksum.textFile(insource);
		
		ochecksum 	= paste0(ochecksums[i], checksum, ".info");
		
		logic = file.exists_(ochecksum);
		
		ns = .now();
		msg = paste0("LOOP [i] :", i, "\n\t\t", "now.start : ", ns ,"\n\t\t", "stem : ", ostem, "\n\t\t", "source : ", insource,"\n\t\t", "out : ", out,"\n\t\t", "out.copy : ", out.copy, "\n\t\t", "checksum : ", checksum,  "\n\t\t", "checksum.file : ", ochecksum,   "\n\t\t", "checksum.exists : ", logic, "\n");
		
.cat(msg); flush.console();

		cat.log( log, msg );	
	
		
		
										# nonsensical ... 
										# checksum matches, so GOOD 
		if(!logic)    #  || !use.cache)
			{
			# FINAL[[i]] = res; 
			# detach(package:HVcpp,unload=TRUE)
					# instring	= readTextFile(insource);

._____main = function() {}
			# ostem is wrong ... 80-number.i ... subtraction?
			
			RES = ini.parse(instring, fname = fstem, MEMORY = MEMORY, smart.num=smart.num, ...);  
			
			# log it, backup, and so on ...
				check.dir(out.copy);
			writeRDS(RES, out.copy);
				check.dir(out);
			writeRDS(RES, out);	
				check.dir(ochecksum);
			cat.log( ochecksum, out.copy );					
					
			} else {
					RES = readRDS(out);
					# get MEMORY and pass on to NEXT ...					
					}

		ne = .now();
		nt = ne - ns;
		msg = paste0("\n\t\t\t", "now.end : ", ne , "\n\t\t\t", "parse.time [secs] : ", nt , "\n");
		
		.cat(msg);
		cat.log( log, msg );
		
		
		MEMORY 	= list.merge(MEMORY, property.get("MEMORY", RES) );
		OUT 	= list.merge(OUT, RES);
		# FINAL[[i]] = RES;		
		}
	
	
	# this stem still has .rds ... FINE ... it's different 
	#	ochecksum 	= paste0(ochecksums[i], checksum, ".info");
	
	
	
			writeRDS(OUT, mfcopy);
			writeRDS(OUT, mf);	


	
	cat.log ( log, mfcopy);
	cat.log ( log, mf);
	cat.log ( log, "\n\n");
	
		NE = .now();
		NT = NE - NS;
		msg = paste0("\n\t\t\t TOTAL TIME ::: ", "now.end : ", NE , "\n\t\t\t", "parse.time [secs] : ", NT , "\n");
		
		.cat(msg);
		cat.log( log, msg );
		
	
	cat.log( log, str.commentOneLine("END -INI- LOG ENTRY", brand="{R}", brand.dir="left") );
	cat.log ( log, "\n\n");
	
######	openSesame(log);

	OUT;
	}


# # # # # # ini.file 	= "C:/_git_/github/MonteShaffer/humanVerse/humanVerse/inst/R/config/system/constants.ini";

# # # # # # ini.file 	= "C:/_git_/github/MonteShaffer/humanVerse/humanVerse/inst/R/config/system/humanVerse.ini";
 
# # # # # # inistr 		= readTextFile(ini.file);
# # # # # # lines 		= str.explode("\r\n", inistr);
# # # # # # # lines		= lines[1:33];

 
  
# caching mechanism with md5sum(file);
# store as a rds file ... 
# tools::md5sum(ini.file) # "3fc3c980d825dec163e728b8d0217809" 
# str.toMD5(inistr); # "a7d8d29e8b427e8908d04be574904769"

ini.parse = function() {} 
ini.parse = function(inistr, fname="-file unknown-", 
							RES = list(), MEMORY = list(),
							smart.num = TRUE, ignore.eval = FALSE,verbose=TRUE, 
							test.mode=FALSE, test.skip = 0
					)
	{
	lines = str.explode("\r\n", inistr);
	
	envir = environment();
	
	# if I get multiline string parsing working, adding 
	# multiline comments should not be difficult .... 
	# what about // comments ... two characters long , nice to have 
	
	EMPTY			= "";
	COMMA 			= ","; 
		
	COMMENT			= "#";
	COMMENT_INI		= ";";
	COMMENT_CPP		= "//";
	COMMENT_MULTI	= c("/*", "*/");
	COMMENTS		= c(COMMENT,COMMENT_INI,COMMA);
	
	cmin			= NULL;  # this is multiline comments 
	
	
	
	MEMORY_STORE 	= "^=";
	EVAL_RCODE 		= "=R";
	BACKTICK		= "`";
 	
	SINGLE_QUOTE 	= "'";
	DOUBLE_QUOTE 	= '"';

	#MEMORY 			= list();
	#RES 			= list();
	fin 			= NULL;
	line.no 		= 0;
	CURRENT_KEY	= ""; # if NOT empty, we are continuing on multiline 
	pkey 			= "";		
	pval 			= ""; 
	cparent 		= "";
	
	KEY_ARRAY = ""; 
	kidx = 1;
	midx = 0;  # index of multiline when started ...
	 
.__main__. = function() {}
############################### MAIN #######################
if(test.mode)
	{
	gggassign("lines", lines);
	gggassign("COMMENTS", COMMENTS);
	
	
	}

	for(line in lines)
		{
		line.no %++%.;
		
		line_ = str.trim(line);
		first = charAt(line_,1);
		
		two = substring(line_, 1, 2);

		
if(test.mode)
	{
	gggassign("CURRENT_KEY", CURRENT_KEY);
	gggassign("line", line); 
	gggassign("line_", line_);
	gggassign("first", first);
	gggassign("cparent", cparent);
	gggassign("pkey", pkey);
	gggassign("pval", pval);
	
	gggassign("MEMORY", MEMORY);
	
	
	gggassign("fin", fin); 
	
	if(line.no < test.skip)
		{
		next;
		} 	
	}
		
if(verbose)
	{
cat("\n\n ", line.no, " --> ", line, "\n\n");
	}


if(test.mode)
	{
	if(verbose)
		{
		xxx = readline(prompt="Press [enter] to continue, [ESC] to quit");
		} 
gggassign("RES", RES);
	
	# tinfo = list("line.no" = line.no);
	
	}
	
	
		{
				
		if(CURRENT_KEY != "")	
			{ 
			# we jump to the front of the line ...
			
			
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
			val = ini.walkTheLine(rval, continue=fin, 
									smart.num = smart.num,
									test.mode = test.mode);
			
			
			fin = property.get("more", val); # are we finished?

.__multiline.GTG = function() {} 	
			GTG = FALSE; # I am good to go ... multiline may stop this ... 
			if(is.null(fin)) { GTG = TRUE; }
			if(GTG)
				{
if(verbose)
	{
.cat("MULTILINE ended on line: ", line.no);
	}
				# maybe do eval(parse in function enclosure
				# unlist the MEMORY_keys there ... 
				# if(hasRcode) 	{ val = eval(parse(text=val)); }
				if(hasRcode) 	{ val = ini.evalMe(val, MEMORY, ignore.eval = ignore.eval ); }
				if(hasMemory) 	{ MEMORY[[key]] = val; }
				
				# val = property.set("source", val, paste0(fname, ":", line.no));
				val = property.set("source", val, paste0(fname, ":", midx,"-",line.no));
				   
				RES = ini.assignVal(CURRENT_KEY, val, cparent, RES, hasRcode = hasRcode, test.mode=test.mode);
				
				CURRENT_KEY = "";				
				pkey = "";
				pval = val; 
				next;
				}
			# we are still in multiline, `fin` contains what we need ...
			pval = pkey = "";
			next; 		
			}


		
.__skip = function() {} 		

		# SKIP LINES ...
		if(first %in% COMMENTS) { next; }
		if(two == COMMENT_CPP) { next; }
			# we trimmed it ... and first is empty ... 
		if((CURRENT_KEY == EMPTY) && (first == EMPTY)) { next; }


.__hardstop = function() {} 		
		
		# stop here ... HARDSTOP =====>
		# ===========> HARDSTOP 
		# <=========== HARDSTOP 
		if(first == "=" || first == "<") { break;}

.__header = function() {} 		
		
		# WE HAVE A HEADER 
		if(first == "[")  # not equal to anything ...  
			{
			info 			= ini.cleanKey(line, "", kidx,
										KEY_ARRAY, envir, MEMORY,
										test.mode=test.mode);
			cparent 		= info;
			pkey 			= "";  # these are not parents ... 
			pval 			= "";
			
			next;
			}
		
		# WE HAVE A LINE ... LIKELY a KEY/VALUE PAIR 
.__normal = function() {} 		

		if(CURRENT_KEY == "")	
			{ 
.__multiline.COMMENT = function() {} 
			# line.no %++%.;   line = lines[ line.no ];
		 
		### TWICE as FAST on the SLOW one... asciii 
		### TAKES about 40 seconds ... 
		### need to grab ASCII art from WEB within 
		### maybe not add anymore ... 
		
			if(!is.null(cmin) 
					|| str.contains(COMMENT_MULTI[1], line) )
				{
				tmp = ini.walkTheLine(line, continue=cmin,
										smart.num = smart.num,
										test.mode=test.mode);		
				
				cmin = property.get("more", tmp);
				# if we have data in cmin ... ccount 
				# go to next, and we should keep looping right here 
				if(!is.null(cmin)) { next; }
				
				# we have an empty line ... 
				# a comment that walkTheLine caught, but this
				# parser did not ...
				if(tmp == EMPTY) { next; }
				}
				
			
			
			# "%~=%" = is.equal;  ;; this "=" is a problem 
			ne = str.count("=", line);
			# in this logic space, we should not have ne == 0 ... 
			# with multiline, maybe ... 
			if(ne == 0) { next; }
			if(ne > 1)
				{
		# # line = '"%~=%" = is.equal;  ;; this "=" is a problem ';	
				
		# # line = 'PRIME_CHOICE\t\t\t= "The greatest power members of the {humanVerse} possess is the ability to act as a ==free== agent.",';
		## line = 'x = 3;  /* this is why */  y = 4;'	
		## I append a ";" SEMICOLON where /*COMMENT*/ was ..
		## can't think of  a real USE case ... 
		## line = 'x = 3  /* this is why */  (y = 4);'	
				
				x = str.between('"%', line, '%"');
				if(!is.na(x))
					{
					rkey = paste0('"%', x, '%"');				
					} else {
							rkey = str.before("=", line, 1); 
							}
							
	
.__equal.sign.issue = function() {} 		
				
				info = str.after(rkey, line);
				rval = str.after("=", info);
				
				
				} else {	
						# ne == 1 ... 
						info = str.explode("=", line);
						rkey = str.trim(info[1]);
						rval = str.trim(info[2]);
						}
						
						
			
			hasMemory = hasRcode = FALSE;

.__memory.eval = function() {} 		

			# allows for a bit of white space between ^ = R 
			#if( str.contains(MEMORY_STORE, paste0(rkey,"=")) ) 
			if( str.ends(MEMORY_STORE, paste0(rkey,"=")) )
				{  
				hasMemory = TRUE; 
				rkey = str.trim(str.ends("^", rkey, trim=TRUE));
				}
				  
			#if( str.contains(EVAL_RCODE, paste0("=",rval)) )
			if( str.starts(EVAL_RCODE, paste0("=",rval)) )
				{ 
				hasRcode  = TRUE; 
				rval = str.trim(str.starts("R", rval, trim=TRUE));
				}
		 
			# GO FIRST 
			# in case we want to use in key?
			val = ini.walkTheLine(rval, continue=fin,
									smart.num = smart.num,
									test.mode=test.mode);  
			
			key = ini.cleanKey(rkey, val, kidx,
								KEY_ARRAY, envir, MEMORY,
								test.mode=test.mode);
								
			
.__normal.GTG = function() {} 				
			fin = property.get("more", val); # are we finished?
			GTG = FALSE; # I am good to go ... multiline may stop this ... 
			if(is.null(fin)) { GTG = TRUE; } 
				else { 
						CURRENT_KEY = key; midx = line.no; 
if(verbose)
	{
.cat("MULTILINE started on line: ", line.no);
	}
						}
			if(GTG)
				{
				if(hasRcode) 	{ val = ini.evalMe(val, MEMORY, ignore.eval = ignore.eval ); }
				if(hasMemory) 	{ MEMORY[[key]] = val; }
				
				val = property.set("source", val, paste0(fname, ":", line.no));
				
				RES = ini.assignVal(key, val, cparent, RES,  hasRcode = hasRcode, test.mode=test.mode);
				}
			pkey = key;
			pval = val; 
			next;
			}
		
		
		
		}
	


	
if(verbose)
	{
.cat("************ How did we get here!");
	}
		}
		
	# if we are looping through files	
	# allow memory carryover
	RES = property.set("MEMORY", RES, MEMORY);  
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
	
	
	
	eval( parse( text = txt ) );
	#eval( txt ) ;
	}
	
	
	  

# ini.assignVal(key, val, cparent, RES);
ini.assignVal = function(key, val, cparent, RES, hasRcode = hasRcode, test.mode=test.mode)
	{
	ckeys = str.explode("|", cparent);
	nc = length(ckeys);  # let's hardcode this to 5?
	
	keys = str.explode("|", key);
	nk = length(keys);
	  

	  
	all = c(ckeys, keys);

	# encoding issue ... taken care of in ini.cleanKey
	# all = c(cparent, key);
	# all = str.toHEX(all);
	
	# [1] "UNNAMED" "%=|%"   
# > all = str.toHEX(all);
# > all
# [1] "554e4e414d4544" "253d7c25"      
# > 
# Error in rawToChar(ttr) : embedded nul in string: 'U\0\0A\0ED'
# In addition: Warning message:
# In str.fromHEX(all) : out-of-range values treated as 0 in coercion to raw


	# all = str.wrap(wrap, all);
	
	#	allparent = paste0(all, collapse="|");
	# RES = ini.checkKey(allparent, RES);
	
	# now here, we will walk and store results, very much like ini.checkKey ...
	
	### encapsulate special ...
	# if(str.isWrapped("%", val)) 
		# { val = paste0(DOUBLE_QUOTE,val,DOUBLE_QUOTE); }

# already passed through EVAL() filter, so should be fine ...
	if(!hasRcode)
		{
		val = ini.wrapSpecial(val);
		}



if(test.mode)
	{
tkey = paste0(cparent, " ::: ", key, " .........       \t ", val);
.cat(tkey);		
	}
 	
	list.smartAssign(RES, all, val);
	}   

 
ini.unwrapSpecial = function(str)
	{
	# might be a non-string ... 
	# or a string of length > 1
	# SKIP checks for being WRAPPED .... 
	if(!is.character(str) || length(str) > 1) { return (str); }
	if(str.isWrapped("%", str))
		{ 
		# SPECIAL ... for transport "|" PIPE breaks ...		
		ke = str.unwrap("%", str);
		# str = str.wrap("%", base64.decode(ke) );	
		str = base64.decode(ke);
		}	
	str;
	}
	 
ini.wrapSpecial = function(str)
	{
# dput(str); 	
	 
	# if(str.isWrapped("%", str)) 
	# ^ is my memory device .... 
	if(str.contains("%", str) || str.contains("`", str) || str.contains("\\", str) || str.contains("|", str) ) 
		{ 
		# SPECIAL ... for transport "|" PIPE breaks ...		
		# ke = str.unwrap("%", str);
		# str = str.wrap("%", base64.encode(ke) );
		str = str.wrap("%", base64.encode(str) );
		}
	str;  
	}
  
ini.cleanKey = function(key, val="", kidx=1, KEY_ARRAY = "", envir=envir, MEMORY, test.mode=FALSE)
	{
	# in case they put doubles (R vs php)
	key = str.replace("[[","[", key);  
	key = str.replace("]]","]", key); 
	
	key = str.replace(c(SINGLE_QUOTE,DOUBLE_QUOTE), "", key);
	key = str.trim(key); 
	
	
	## memory key 
	# auto.save[^save.key][when]
	
	if(str.contains("^", key))
		{
		nkey = key;
		# multiple?
		tmp = str.between("[^", key, "]"); 
		rtmp = paste0("[^", tmp, "]"); 
	
if(test.mode)
	{	
dput(MEMORY);
	}
		if(!is.null(MEMORY[[tmp]]))
			{
			nkva = paste0("[", MEMORY[[tmp]], "]"); 
			nkey = str.replace(rtmp, nkva, key);
			} else { nkey = str.replace("^", "__^__", key); }
		# not found, let them know somehow ....
		key = nkey;		 
		} 
  
	######## %SPECIAL%
	
	key = ini.wrapSpecial(key);
	 
	
	
	# str.contains("[]", key);  ??? 
	isArray = (str.ends("[]", key));
	 
	
	# this is header logic && pkg[]
	kvec = str.explode("]", key);
	logic = str.contains("[", kvec);
	kvec = unlist(str.explode("[", kvec[logic]));
	if(!is.null(kvec))
		{
		## pkg[] # ... lives here ...
		logic = v.test(kvec, "");
		res = kvec[!logic];
		rlast = v.last(res);
		
		if(isArray && rlast == KEY_ARRAY) 
			{ res = c(res, kidx); kidx %++%.; }
		if(isArray && rlast != KEY_ARRAY) 
			{ res = c(res, 1); kidx = 2; KEY_ARRAY = rlast; }
		if(isArray)
			{ kidx %TO% envir; KEY_ARRAY %TO% envir; }
		return( paste0(res, collapse="|") );
		}
	
if(test.mode)
	{
dput(key);
	}
	
	# normal key 
	key;
	}
	
	
	

ini.walkTheLine = function(){}
ini.walkTheLine = function(str, continue=NULL, 
					smart.num = TRUE, test.mode=TRUE)
	{
.__declare.COMMENTS = function() {}
	EMPTY			= "";
	COMMA 			= ","; 
		
	COMMENT			= "#";
	COMMENT_INI		= ";";
	COMMENT_CPP		= "//";
	COMMENT_MULTI	= c("/*", "*/");
	COMMENTS		= c(COMMENT,COMMENT_INI,COMMA);
	
if(test.mode)
	{
dput(smart.num);
	}
	
	# MULTILINE comments ... pass flag, just looking for END 
	# allow for two-character comment "//" DOUBLE_SLASH 
	# IN_MULTILINE_COMMENT ... TYPE = "/*"  "*/"
	
	# I am not dealing with = signs ... just simple parser 
	# NO GLOBALS HERE, used for .... ini.parse(inistr)


.__declare.OTHER = function() {}
	SINGLE_QUOTE 	= "'";  # make these constants?
	DOUBLE_QUOTE 	= '"';
	
	BACKTICK		= '`';
		STRINGS = c(SINGLE_QUOTE, DOUBLE_QUOTE, BACKTICK);
	
	BACKSLASH 		= "\\";
	 
	IN_STRING 		= FALSE;
	STRING_TYPE 	= NULL;
	
	

.__continue = function() {}
	nval			= "";
	#oval 			= "";   # this is carryover part ...
	
	ccount = 0;  # how many /* are there ... when we get back to zero we STOP ...
	cval = "";  # GTG copy of something before a multiline comment ... 
	COMMENT_TYPE	= ""; # for now, only /* */ but maybe <!--
	

	if(!is.null(continue)) 
		{
# dput(continue); stop("monte");
		if(!is.null(continue[["IN_STRING"]]))
			{
			IN_STRING 	= continue[["IN_STRING"]];
			STRING_TYPE = continue[["STRING_TYPE"]];
			nval 		= paste0(continue[["nval"]], "\n");
			#oval 		= nval;
			}
		if(!is.null(continue[["ccount"]]))
			{
			COMMENT_TYPE	= continue[["COMMENT_TYPE"]];
	
			ccount 		= continue[["ccount"]];
			nval 		= continue[["nval"]];
			cval 		= continue[["cval"]]
			}
			
			
		# how did you get here?
		}

if(test.mode)
	{	
.cat("HEAD nval: ", nval);
.cat("\t\t str: ", str);
	}
	
	str = str.trim(str);
	strV = str.explode("", str);
	ns = length(strV);
		
	cchar = "";
	pchar = "";
	i = 1;
	
	
.__while = function() {}
	while(i <= ns)
		{
############################### one WHILE ##################
		pchar = cchar;
		cchar = strV[i];
		two = paste0(pchar, cchar);
		
		
.__multiline.FRONT = function() {}
		# jump to front of the line 
		if(COMMENT_TYPE != "" && (two == COMMENT_MULTI[1]) )
			{
			# allow nested 
			ccount %++%. ;
			i %++%. ;
			next;
			}			
		
		if(COMMENT_TYPE != "" && (two != COMMENT_MULTI[2]) )
			{
			# we don't need to update the "nval"
			i %++%. ;
			next;
			}
		


		
		if(COMMENT_TYPE != "" && (two == COMMENT_MULTI[2]))
			{
			ccount %--%. ;
			if(ccount == 0)
				{
				COMMENT_TYPE = "";	
				nval = cval; # could continue inline
				cval = "";	
				} 
			i %++%. ;
			next;
			}
			
		
.__STRING = function() {}		
		if(cchar %in% STRINGS)
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
								# this means the string is OVER
								IN_STRING = FALSE;
								break;
								}
					}				
				}
			
			## just starting the STRING 
			IN_STRING 	= TRUE;
			# any previous elements before string start are discarded			
			nval = "";
			STRING_TYPE = DOUBLE_QUOTE;
			if(cchar == SINGLE_QUOTE) 	
				{ STRING_TYPE = SINGLE_QUOTE; }
			if(cchar == BACKTICK) 		
				{ STRING_TYPE = BACKTICK; }
			i %++%. ;
			next;
			}

.__COMMENTS = function() {}
				
		if((cchar %in% COMMENTS) && !IN_STRING)
			{
			break;
			}
			
.__COMMENT_CPP = function() {}
		if((two == COMMENT_CPP) && !IN_STRING)
			{
			break;
			}
			
.__multiline.START = function() {}
		if( (two == COMMENT_MULTI[1]) && !IN_STRING )
			{
			# we are starting to read a MULTILINE comment 
			# any previous elements before  maybe be GTG 
			# don't discard ... copy 
			nval = paste0(nval, cchar);			
			nval = nval %-.% two;
			
			cval = nval;  
			# we may have more code to follow ... 
			if(cval != EMPTY) { cval = paste0(cval,";"); }
			nval = "";
			COMMENT_TYPE = "/**/";
			ccount = 1;
			i %++%. ;
			next;
			}
		
		# ... my parser won't care ...
		# possible to have a missing CLOSING_QUOTE 
		
		nval = paste0(nval, cchar);
		i %++%. ;
		next;		
############################### one WHILE ##################
		}

.__smart.type = function() {} 
	# nval = str.trim(nval);
	nval_ = str.trim(nval);
	if(smart.num) 
		{
		# allows for NUMBERS PI to be CHARS with FLAG
		nnum = check.number(nval_);
		if(.allTRUE(nnum)) { nval = as.numeric(nval_); }
		}
	
	nbool = check.boolean(nval_);
	if(.allTRUE(nbool)) { nval = as.logical(nval_); }
	
	# is this multivariate ... on NULL, not possible ...
	# if they wanted the string "NULL", they lost it ... 
	# just a single KEY=>VAL pair, not inside a LIST ...
	## creates HAVOC downstrem in the parser ... 
	######### if( v.test(nval_, "NULL") ) { nval = NULL; }
	
	
	# if we have a multiline, and didn't close the STRING ... 
	if(IN_STRING)
		{
		more = list("nval" = nval, 
					"IN_STRING" = IN_STRING, 
					"STRING_TYPE" = STRING_TYPE);
		nval = property.set("more", nval, more);
		}
		
	if(ccount > 0)
		{
		more = list("nval" = nval, COMMENT_TYPE = COMMENT_TYPE,
					"cval" = cval, "ccount" = ccount);
		nval = property.set("more", nval, more);
		}
		
		
if(test.mode)
	{
.cat("FOOT nval: ", nval);
	}
	
	nval;
	}







 
ini.test = function(f = "", skip=0, ...)
	{
	# usage ... find the line number of a [HEADER]
	# call ini.test("", 26);  # 26 is line number of a [HEADER]
	# hit [ESC] when you want to exit ... review
	# line ... lines ... RES ... MEMORY ... etc.
	if(f == "") 
		{
		# make this a system.path in library ... 
		f = "C:/_git_/github/MonteShaffer/humanVerse/humanVerse/inst/R/config/--.old.--/ZZ-test.ini";
		}
	fstr = readTextFile(f);
	
	RES = ini.parse(fstr, verbose = TRUE, test.mode=TRUE, 		
										test.skip=skip, ...);
 
	print(str(RES));
	
	invisible(RES);	
	}
	
	
	