

init.assign = function(key, val, WHERE=.GlobalEnv)
	{
	# may update to do namespace 
	assign(key, val, envir=WHERE);	
	}
	


alias.init = function(use.cache = TRUE, recursive=FALSE)
	{
	if( !is.defined(B64LIST) ) { ._____init.settings(); }
	# recursive is  /config/alias/* , /config/user-alias.ini , /config/{mshaffer}-alias.ini 
	
	# to begin we need the values for users, so FALSE on first pass ... it will cache ...
	
	# we need location of path, which we may not have RUN_FIRST_TIME, but we can use source from system(path) of library with default alias/* and alias.rds in that folder ...
	# afold = "C:/_git_/github/MonteShaffer/humanVerse/humanVerse/inst/R/config/alias/"
	# files <- list.files(pattern = "\\.dbf$")
	# a.files = list.files(afold)
	# a.rds = paste0(afold, "alias.rds");
	# list.files(afold, full.names = TRUE, pattern = "\\.ini$"):

	a.folder = "C:/_git_/github/MonteShaffer/humanVerse/humanVerse/inst/R/config/alias/";
	a.rds = paste0(a.folder, "alias.rds");
	a.files = list.files(a.folder, full.names = TRUE, pattern = "\\.ini$");
	
	
	use.cache = FALSE;
#############
# PARENT
#############
	if(!file.exists_(a.rds) || !use.cache)
		{
		ALIAS = ini.parseFiles(a.files, a.rds, 
								use.cache = use.cache);
		} else {
				ALIAS = readRDS(a.rds);
				} 


	## for now ... 
	alias.process(ALIAS);

	# smart.file = function( stem, where = "", pkg = "") {}
	# where = LIB_PATHSUB$XXX ... "" EMPTY search everything (takes longer, but we can cache the search)
	# pkg = "" EMPTY search everything ... SOURCE / ANALYTICS 
	# smart.infile = function(search, where = "", pkg = "") {}
	# searching CONTENTS of file ... SEARCH EVERYTHING but better
	# TOKYO DB or fuzzy with TRIE ...
	
	# lazyLoad(   file.path(system.file("help", package=pkg), pkg),  envir = e )
	
	invisible(ALIAS);
	}
	
alias.process = function() {}
alias.process = function(ALIAS=list())
	{
	a = ALIAS;
	na = names(a); nn = length(na);
	for(i in 1:nn)
		{
		aa = a[[i]];
		fas = names(aa);
		fnn = length(fas);
		for(j in 1:fnn)
			{
			fkey = fas[j];
			fval = aa[[j]];
			Aname = na[i];
			
.cat("================> ", "i = ", i, "\t j = ", j)
		
# broken PIPE ... parser 		
# if(fkey %in% c("%","%=")) { next; } 
			
			.cat(Aname, " ::: ", fkey, " = ", fval);
	
	
	
	pkg = FALSE;
	if(str.starts("A-", Aname)) { pkg = TRUE; }
	if(pkg)
		{
		Atmp = str.explode("-", Aname);
		pkg.name = Atmp[2];
		
colons = "::";
if(str.contains(":::", fval)) { colons = ":::"; }
		# remove if it has it ...
		ftmp = str.explode(":", fval);
#flen = strlen(ftmp);

		### 
		
		fval = v.last(ftmp);
		
		# if(length(ftmp) == 2) { fval = ftmp[2]; }
		
		## could alias to ::: ... TODO 
		fvalnew = paste0(pkg.name, colons ,fval);
		
		} else { fvalnew = fval; }
	
	
	
	
	### WRAP IT ON THE OTHER SIDE
	fvalnew = ini.unwrapSpecial(fvalnew);
	

	if(str.isWrapped("%", fvalnew)) 
		{
		# quotes for eval ...
		fvalnew = paste0('"', fvalnew, '"');
		}
	 
	
	### WRAP IT ON THE OTHER SIDE
	fkeynew = ini.unwrapSpecial(fkey);
	if(str.isWrapped("%", fkeynew)) 
		{
		# quotes for eval 
		fkeynew = paste0('"', fkeynew, '"');
		}
	
	
			.cat(Aname, " ------> ", fkeynew, " = ", fvalnew);
	
alias.set(to = fkeynew, from = fvalnew);
		
		# estr = paste0( fkeynew, " = ", fvalnew, ";" );
		# eval(parse(text=estr));

			.cat(Aname, " ::: ", fkey, " = ", fvalnew);
			# stop("monte");
			# alias.set(fval, fkey);
			
			}		
		}
	


	
	}

alias.set = function(to="ceil", from="base::ceiling", WHERE=.GlobalEnv)
	{
	# 		# alias.set(fval, fkey);	
	estr = paste0( to, " = ", from, ";" );
	## to namespace ... if possible ... 
	eval(parse(text=estr), envir=WHERE);	
	
	}
	
	


number.init = function(use.cache=TRUE, recursive=FALSE)
	{
	if( !is.defined(B64LIST) ) { ._____init.settings(); }
	
	n.folder = "C:/_git_/github/MonteShaffer/humanVerse/humanVerse/inst/R/config/number/";
	n.rds = paste0(n.folder, "number.rds");
	n.files = list.files(n.folder, full.names = TRUE, pattern = "\\.ini$");
	
	
	use.cache = FALSE;
#############
# PARENT   
#############
	if(!file.exists_(n.rds) || !use.cache)
		{ 
		NUMBER = ini.parseFiles(n.files, master = n.rds, use.cache = use.cache, smart.num  = FALSE);
		} else {
				NUMBER = readRDS(n.rds);
				} 

	## for now ... 
	number.process(NUMBER);
	invisible(NUMBER);
	}


number.process = function() {} 
number.process = function(NUMBER=list())
	{
	nu = NUMBER;
	na = names(nu); nn = length(na);
	for(i in 1:nn)
		{
		nna = na[i];
		nnu = nu[[i]];
		
		# make it available to them as a list ... 
		nnag =  paste0("NUMBER__", nna);  #
				init.assign(nnag, nnu);
			
		if(!str.starts("*", nna))
			{
			next;
			}
			
		
		fnu = names(nnu);
		fnn = length(fnu);
		for(j in 1:fnn)
			{
			fkey = ini.unwrapSpecial(fnu[j]);
			fval = as.character(nnu[[j]]); # strip attribute ...
			fval = ini.unwrapSpecial(fval);
			
			nval = as.numeric(fval);
			ckey = paste0(fkey, "c");
			# store the original (as.numeric) ... 
			# make a copy (as.character) 
			# PI and PIc 
			init.assign(ckey, fval);
			init.assign(fkey, nval);
			}
		}
	}
	



system.init = function(use.cache=TRUE, recursive=FALSE)
	{
	if( !is.defined(B64LIST) ) { ._____init.settings(); }
	
	s.folder = "C:/_git_/github/MonteShaffer/humanVerse/humanVerse/inst/R/config/system/";
	s.rds = paste0(s.folder, "system.rds");
	s.files = list.files(s.folder, full.names = TRUE, pattern = "\\.ini$");
	
	
	use.cache = FALSE;
#############
# PARENT   
#############
	if(!file.exists_(s.rds) || !use.cache) 
		{ 
		SYSTEM = ini.parseFiles(s.files, master = s.rds, use.cache = use.cache);
		} else {
				SYSTEM = readRDS(s.rds);
				} 


### SYSTEM = ini.parseFiles(s.files, master = s.rds, use.cache = use.cache, verbose=FALSE, test.skip=3);

	## for now ... 
	system.process(SYSTEM);
	invisible(SYSTEM);
	}


system.process = function() {} 
system.process = function(SYSTEM=list())
	{
	# list-marge ... I think I just want to load all of the INI
	# in a cascade ... how to know "user-name" => mshaffer ?
	
	init.assign("SYSTEM__", SYSTEM);  # as-is 
	
	su = SYSTEM;
	na = names(su); nn = length(na);
	for(i in 1:nn)
		{
		sna = na[i];
		snu = su[[i]];	
		
		fsu = names(snu);
		fsn = length(fsu);
		# error on i=6; j=23;
		for(j in 1:fsn)
			{	
			fkey = ini.unwrapSpecial(fsu[j]);
			fval = property.clearALL(ini.unwrapSpecial(snu[[j]])); 
			
			if(str.begins("B64D__", fkey))
				{
				fkey = str.begins("B64D__", fkey, trim=TRUE);
				fval = base64.decode(str.trim(fval));
				}
			
			
			init.assign(fkey, fval);
			}
		}
	}




._____init.settings = function(use.cache=TRUE, force.reload=FALSE)
	{
	._____init.systemCONSTANTS();
	
	._____init.systemALIASES(); 
	
	# load system.INI and add to namespace 
##############
return(NULL);
############## 

	alias.init();
	number.init();
	system.init();

# JSON walk ... walk start("[" ... walk until ... "]" ... but you have to transverse each and make certain others are not found ... or use REGEX ... streaming idea is a "walk" .... could use str.between("[", line, "]" ... if result has another "[", keep going with a longer offset or "skip" ... skip as in pos ....  

# maybe set a flag in Sys.setenv() ... status of init ...
# if complete, don't run again, unlese force ...
	alias.init(recursive=FALSE);
	system.init(recursive=FALSE);
	#secret.init(recursive=FALSE);
	runtime.init(recursive=FALSE);
	number.init(recursive=FALSE);
	# PIc ... append lower case c for character ... 
	
	

	alias.init(recursive=TRUE);
	system.init(recursive=TRUE);
	#secret.init(recursive=TRUE);
	runtime.init(recursive=TRUE);
	number.init(recursive=TRUE);
	# PIc ... append lower case c for character ... 
	

	## append user-settings now 
	##alias.init(recursive=TRUE);

	# inifilesORDERmatters = c("system/10-constants.ini","system/20-humanVerse.ini","system/30-ascii.ini", "system/40-runtime.ini");
	
	inifilesORDERmatters = c("system/10-constants.ini","system/20-humanVerse.ini","system/30-ascii.ini");
	
	system_ = "cache/ini/system.rds";
	
	use.cache = TRUE;

	s = ini.parseFiles(inifilesORDERmatters, system_, use.cache);
							
	## therse are "GLOBALS"
	ns = names(s); nn = length(ns);
	for(i in 1:nn)
		{
		ss = s[[i]];
		gns = names(ss);
		gnn = length(gns);
		for(j in 1:gnn)
			{
			gkey = gns[j];
			gval = ss[[j]];
			gggassign(gkey, gval);
			}		
		}
	 
	 
	 
	# load alias.INI and add aliases to namespace 
	aliasfilesORDERmatters = c("system/88-alias.ini");
	
	alias_ = "cache/ini/alias.rds";
	
	use.cache = TRUE;

	a = ini.parseFiles(aliasfilesORDERmatters, alias_, use.cache);


	
	## therse are "ALIAS"
	na = names(a); nn = length(na);
	for(i in 1:nn)
		{
		aa = a[[i]];
		fas = names(aa);
		fnn = length(fas);
		for(j in 1:fnn)
			{
			fkey = fas[j];
			fval = aa[[j]];
			Aname = na[i];
		

		if(fkey %in% c("%","%=")) { next; } # broken PIPE ... parser 		
			
			.cat(Aname, " ::: ", fkey, " = ", fval);
	
	pkg = FALSE;
	if(str.contains("-", Aname)) { pkg = TRUE; }
	if(pkg)
		{
		Atmp = str.explode("-", Aname);
		pkg.name = Atmp[2];
		
		# remove if it has it ...
		ftmp = str.explode("::", fval);
		if(length(ftmp) == 2) { fval = ftmp[2]; }
		
		## could alias to ::: ... TODO 
		fvalnew = paste0(pkg.name,"::",fval);
		
		} else { fvalnew = fval; }
	
	if(str.contains("%", fvalnew))
		{
		# reverse .... #  nPr  =  %nPr%
		fvalnew = paste0('"', fvalnew, '"');
		}
	
	
	fkeynew = fkey;
	if(str.contains("%", fkey))
		{
		fkeynew = paste0('"', fkey, '"');
		}
	
	
			.cat(Aname, " ::: ", fkeynew, " = ", fvalnew);
	

	
		estr = paste0( fkeynew, " = ", fvalnew, ";" );
		eval(parse(text=estr));

			.cat(Aname, " ::: ", fkey, " = ", fvalnew);
			# stop("monte");
			# alias.set(fval, fkey);
			
			}		
		}
	



	
	# > fn.name = "str.pos"
# > is.function(fn.name)
# [1] FALSE
# > is.function(str.pos)
# [1] TRUE

# function.exists(fn.name)
# [1] TRUE


	
	
	# load system/40-runtime followed by user/runtime ...
	
	}

._____init.systemALIASES = function(force = FALSE)
	{
	if(!force && is.defined(ord)) { return(invisible(NULL));}
	
	# alias.set = function(to="ceil", from="base::ceiling", WHERE=.GlobalEnv)
	
	# so alias.init() will work on its own, 
	
	alias.set(to='"%THIS%"',		from = ".THIS.");
	alias.set(to='"%GLOBAL%"', 		from = ".GLOBAL.");
	
	# readTextFile 
	alias.set(to='"%+=%"', 			from = ".PLUS_EQUAL.");
	alias.set(to='"%++%"', 			from = ".PLUS_PLUS.");
	  
	# .THIS.
	alias.set(to='"%-=%"', 			from = ".MINUS_EQUAL.");
	alias.set(to='"%--%"', 			from = ".MINUS_MINUS.");
	
	alias.set(to='suppressWarning', from = "suppressWarnings");
	
	# js.b64  
	alias.set(to='ord', from = "utf8ToInt");
	alias.set(to='chr', from = "intToUtf8");
	
	alias.set(to='"%>>%"', 	from = ".SHIFT_R.");
	alias.set(to='"%<<%"', 	from = ".SHIFT_L.");
	alias.set(to='"%&%"', 	from = ".AND.");
	alias.set(to='"%|%"', 	from = ".OR.");
	
	# .b64_hex 
	alias.set(to='num.round', from = "int.round");

	
	}
	
._____init.systemCONSTANTS = function(force = FALSE)
	{
	if(!force && is.defined(B64LIST)) { return(invisible(NULL));}
	# basic constants that can't pass through well into R from a 'parsed' INI file ... the  'Я' BACKSLASH issue 
	
	## ... PLUS ... STRINGS used for PARSER manipulation ...
	############# BASE_64 #############
	B64			= "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";
	B64v		= str.explode("", B64); 
	
	B64LIST 	= list.create(B64v, 0:64);
	
	############# BASE_XX (0:32) #############
	BXX			= "0123456789ABCDEFGHIJKLMNOPQRSTUV";
	BXXv		= str.explode("", BXX);
		# needed for function `int2base` below	
		assign("BXXv", BXXv, envir = .GlobalEnv);
	
	Bits64		= int2base(0:63, base=2);
	SI_PREFIX 	= num.SIunits("regular");
	
	BASIC = list(
				B64 		= B64,
				B64v 		= B64v,
				B64LIST 	= B64LIST,
	
				BXX 		= BXX,
				BXXv 		= BXXv,
				Bits64 		= Bits64,
				SI_PREFIX 	= SI_PREFIX,
	
	
				BACKSLASH 			= "\\",
				DOUBLE_BACKSLASH 	= "\\\\",
				SINGLE_QUOTE 		= "'",
				SQ 					= "'",
				ESC_SQ 				= '\'',
				DOUBLE_QUOTE 		= '"',
				DQ 					= '"',
				ESC_DQ 				= "\"",
				TAB 				= "\t",
				NEWLINE 			= "\n",
				EOL 				= "\r\n",
				
				EXT					= ".",
				EMPTY				= "",
				DIR_LINUX			= "/",
				DIR_WINDOZE			= "\\",
				DOUBLE_SLASH		= "//",
				
				# alias.init()
				SLASH				= "/",
				DEFAULT_TIMEZONE	= "UTC",
				
				VSEP 				= "\\./",
				HUMANVERSE_SEP		= "\\./"
				);
	
	n = length(BASIC);
	bnames = names(BASIC);
	for(i in 1:n)
		{
		bkey = bnames[i];
		bval = BASIC[[bkey]];
		 
		## namespace assign ...
		
		gggassign(bkey,bval);		
		}
	
	invisible(BASIC);
	}
