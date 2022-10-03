
init.settings = function(use.cache=TRUE)
	{
	# load system.INI and add to namespace 
	
	inifilesORDERmatters = c("system/10-constants.ini","system/20-humanVerse.ini","system/30-ascii.ini", "system/40-runtime.ini");
	
	system = "cache/ini/system.rds";
	
	use.cache = TRUE;

	s = ini.parseFiles(inifilesORDERmatters, system, use.cache);
							
	
	
	# load alias.INI and add aliases to namespace 
	
	
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