
alias.init = function(use.cache=TRUE, recursive=FALSE)
	{
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
		ALIAS = ini.parseFiles(a.files, a.rds, use.cache);
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
		
# broken PIPE ... parser 		
if(fkey %in% c("%","%=")) { next; } 
			
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
	# if(str.contains("%", fvalnew))
	if(str.isWrapped(fvalnew, "%")) 
		{
		# reverse .... #  nPr  =  %nPr%
		fvalnew = paste0('"', fvalnew, '"');
		}
	 
	
	### WRAP IT ON THE OTHER SIDE
	fkeynew = fkey;
	# if(str.contains("%", fkey))
	if(str.isWrapped(fkey, "%")) 
		{
		fkeynew = paste0('"', fkey, '"');
		}
	
	
			.cat(Aname, " ::: ", fkeynew, " = ", fvalnew);
	
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
	
	