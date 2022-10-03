
init.settings = function(use.cache=TRUE)
	{
	# load system.INI and add to namespace 
##############
return(NULL);
##############
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

# onload
init = function() 
	{
	memory.init();
	
	# load .humanVerse from CONFIG and use its values, otherwise 
	
	
	par.saveStateInitial();
	options.saveStateInitial();
	system.saveStateInitial(); 
	timer.init(show.message=FALSE);  # this captures the local tz 
	
	# maybe add wrapper for formals athat allows base::as.POSIXct for clarity
	# doesn't seem to work ... 
	formals(as.POSIXct) = alist(x = , tz="UTC", ... = );
	formals(as.POSIXlt) = alist(x = , tz="UTC", ... = );
	# formalArgs(as.Date)
	# getFunction(name, generic=TRUE, mustFind=TRUE, where)
	# functionBody
	# 


	}  
# this function is linked in .onLoad()
# if we know WHERE a local config file is, go grab it ... 
# we should check colors and store it... ENABLE 

# onUnload
unload = function() { }  

  # memory.init();
  # memory.smartRestore(); # ... onUnload saveState ...