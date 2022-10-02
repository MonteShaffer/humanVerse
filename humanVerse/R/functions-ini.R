


test = function(df)
	{
	.%THIS%.;
	
	info = paste0( .now(), "|", 
				paste0(capture.output(dput(THIS)),collapse="") );
		
	# info = list("now" = .now(), "THIS" = THIS);
	# info = paste0( .now(), "|", JSON.stringify(THIS) );
dput(info);
	}

# any filename that contains text ...
md5.textFile = function(fileTXT)
	{
	# tools::md5sum(fileTXT); # these are different ...
	str.toMD5( readTextFile(fileTXT) );	
	}
	

	
	

path.build = function(partial)
	{
	# no leading dot ... part of the SYSTEM 
	# ./file ... could be anywhere in the cascade 
	# ./../../file ... could be anywhere in the cascade ...
	
	}

# inifilesORDERmatters = c("system/10-constants.ini","system/20-humanVerse.ini","system/30-ascii.ini", "system/40-runtime.ini");

# ini.parseFiles(inifilesORDERmatters)

ini.parseFiles = function(inifilesORDERmatters, 
							master = "cache/ini/master.rds", 
							use.cache = TRUE, ...)
	{
	uniqid = str.uniqid();	
	TIMESTAMP = .timestamp("YYYY-MM-DD");
	
	mf 		= "C:/_R_/-humanVerse-/SYSTEM/cache/ini/master.rds";
	if(use.cache && file.exists_(mf)) { return( readRDS(mf) ); }
	
	d 		= check.dir(mf);
	stem 	= mf %.-% d;

	log 	= paste0(d, "-logs-/", TIMESTAMP, ".log");
			check.file(log);
	

	cat.log( log, str.commentOneLine("START -INI- LOG ENTRY") );
	cat.log( log, .timestamp("humanVerse", tz="GMT") );	
	cat.log( log, TIMESTAMP );
	cat.log( log, uniqid );
	
######	openSesame(log);
	
	.%THIS%.	
	cat.log( log, "--FUNCTION_INFO--" );
	cat.dput(THIS, log);
	
	
	# chmod(400) on original sources in inst/R/ after INSTALL?
	
	
	
	sp =  "C:/_git_/github/MonteShaffer/humanVerse/humanVerse/inst/R/config/";
	spd	= check.dir(sp);
	
	ofiles = inifilesORDERmatters;	
	
	cat.log( log, "--FILES--" );
	cat.dput( ofiles, log);

	insources = paste0(spd, ofiles);
	
	oexts 	= check.ext(ofiles, dotless=FALSE);
	#ostem = (outs %.-% d) %-.% exts;
	opaths = ofiles %-.% exts;
	
	opos = check.list(str.pos("/", opaths));
	ons = list.getLengths(opos);
	
	opartials 	= str.before("/", opaths, ons);
	ostems 		= str.after("/", opaths, ons);


	outs = paste0(d, opartials, "/", ostems, ".rds");
		check.dir(outs);

	# checksums live here ...
	backups	= paste0(d, "-backups-/", TIMESTAMP , "/");
		check.dir(backups);
			
	outs.copy = paste0(backups, opartials, "/", uniqid, "_", ostems, ".rds");
	# if exists in the date ... append a str.uniqid-md5 ...
	
	ochecksums = paste0(d, "-backups-/", opartials, "/", ostems, "_");

	# this stem still has .rds ... FINE ... it's different 
	mfchecksum = paste0(d, "-backups-/", stem , "_");
	mfcopy = paste0(backups, opartials, "/", uniqid, "_", stem);
	
	
	MEMORY = list();# RES = list();
	# FINAL = list();  
	OUT = list();
	
	n = length(outs);	
	for(i in 1:n)
		{
		
		ostem		= ostems[i];
		insource 	= insources[i];
		out 		= outs[i];
		out.copy 	= outs.copy[i];

		# if we have to parse, we need the file anyway ...
		instring	= readTextFile(insource);
		checksum		= str.toMD5(instring);
		# checksum 	= md5.textFile(insource);
		
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

			RES = ini.parse(instring, fname = ostem, MEMORY = MEMORY);  
			
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
		
		cat.log( log, msg );
		
		
		MEMORY 	= list.merge(MEMORY, property.get("MEMORY", RES) );
		OUT 	= list.merge(OUT, RES);
		# FINAL[[i]] = RES;		
		}
	
	
	# this stem still has .rds ... FINE ... it's different 
	#	ochecksum 	= paste0(ochecksums[i], checksum, ".info");
	
	# mfchecksum = paste0(d, "-backups-/", stem , "_");
	mfcopy = paste0(backups, uniqid, "_", stem);
		check.dir(mfcopy);
			writeRDS(OUT, mfcopy);
		check.dir(mf);
			writeRDS(OUT, mf);	
	
	cat.log ( log, mfcopy);
	cat.log ( log, mf);
	cat.log ( log, "\n\n");
	
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
				verbose=TRUE, ignore.eval = FALSE)
	{
	lines = str.explode("\r\n", inistr);
	
	#envir = environment();
	
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

	#MEMORY 			= list();
	#RES 			= list();
	fin 			= NULL;
	line.no 		= 0;
	CURRENT_KEY	= ""; # if NOT empty, we are continuing on multiline 
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
# xxx = readline(prompt="Press [enter] to continue, [ESC] to quit");
gggassign("RES", RES);
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
				
				val = property.set("source", val, paste0(fname, ":", line.no));
				
				RES = ini.assignVal(CURRENT_KEY, val, cparent, RES);
				
				CURRENT_KEY = "";				
				pkey = "";
				pval = val; 
				next;
				}
			# we are still in multiline, `fin` contains what we need ...
			pval = pkey = "";
			next; 		
			}


		line_ = str.trim(line);
		first = charAt(line_,1);
		
.__skip = function() {} 		

		# SKIP LINES ...
		if(first %in% COMMENTS) { next; }
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
		# # line = '"%~=%" = is.equal;  ;; this "=" is a problem ';	
				
		# # line = 'PRIME_CHOICE\t\t\t= "The greatest power members of the {humanVerse} possess is the ability to act as a ==free== agent.",';
				
				# line = '"%=%" = 5,'
				# generally, we don't allow = unless special functions 
				pos = str.pos("=", line);
				# first position GREATER THAN 5 ... 
				idx = v.which( pos > 5 , TRUE)[1];
				
				rkey = str.before("=", line, idx);  
				#rkey = str.before("=", line, 1);  # 99% of the time ...
				
				# minimum length to even have an equal ...
				# pos > 5 ... ignore = before the 5th index ... 
				
				
	
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
			if(is.null(fin)) { GTG = TRUE; } else { CURRENT_KEY = key; }
			if(GTG)
				{
				if(hasRcode) 	{ val = ini.evalMe(val, MEMORY, ignore.eval = ignore.eval ); }
				if(hasMemory) 	{ MEMORY[[key]] = val; }
				
				val = property.set("source", val, paste0(fname, ":", line.no));
				
				RES = ini.assignVal(key, val, cparent, RES);
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
	key = str.trim(key);
	
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
	
	
	