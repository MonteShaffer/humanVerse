

system.set_ = function(KEY, value=NULL)
	{
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.KEY = check.type(KEY);
	if(!ct.KEY || !is.character(KEY))	
		{ key = deparse(substitute(key)); } else { key = KEY; }
##########################################################
	# everything's a string ...
	str = paste0('Sys.setenv("',key,'" = "',value,'")');
				eval(parse(text=str));	
	(Sys.getenv(key));
	}
	
system.set = function(key, value=NULL, as.null = FALSE) 
	{
	# you may actually want to pass the value NULL in the "setter" ???
	# key is a list with names/values, so NULL get's passed 
	if(is.null(value) && as.null)  
		{
		if(is.list(key))
			{
			k = names(key);
				names(key) = NULL;
			value = unlist(key);
			key = k;
			} else { stop("ERROR with key/val"); }
		}
		
		
	# allows multivariate ... keys determine the replacements
	# if value gets to end, it will recycle to beginning
	n.key = length(key);
	n.val = length(value);
	idx.v = 1;
	
	res = list();
	for(i in 1:n.key)
		{
		str = paste0('Sys.setenv("',key[i],'" = "',value[idx.v],'")');
				eval(parse(text=str));
		res[i] = (Sys.getenv(key[i]));
		idx.v = 1 + idx.v; if(idx.v > n.val) { idx.v = 1; }  # allows for looping gracefully if UNEVEN
		}
	minvisible(res);	
	}
	

system.get = function(key, obj, wildcard = "keys-values" )
	{
	w  = prep.arg(wildcard, n=1, keep = "-");
	# System Environment

	res = Sys.getenv(key);
	# w == "k-v" means we look normally, than cascade our search from keys to values ... 
	if(res == "")
		{
		# let's try wildcard on Sys.getenv(keys) ... vals?
		vals = Sys.getenv();
		keys = names(vals);
			idxs.keys = regex.wildcardSearch(keys, key);			
		is.k = ( length(idxs.keys) > 0 );
			idxs.vals = regex.wildcardSearch(vals, key);
		is.v = ( length(idxs.vals) > 0 );
		
		if(!is.k && !is.v) { return(""); } # LIKE NORMAL
			res.keys = vals[idxs.keys];
			res.vals = vals[idxs.vals];			
		
		if(w == "k" && is.k) { return( res.keys ); }
		if(w == "v" && is.v) { return( res.vals ); }
		
		# search keys, than vals
		if(w == "k-v" && is.k) { return( res.keys ); }
		if(w == "k-v" && is.v) { return( res.vals ); }
		
		# search vals, then keys
		if(w == "v-k" && is.v) { return( res.vals ); }
		if(w == "v-k" && is.k) { return( res.keys ); }
		
		return(""); # we tried.
		}
		
	# names(s <- Sys.getenv());
	# if key had * and res is empty, # do wildcard search on names 
	# Sys.getenv("RSTUDIO*");
	# s[grep("^L(C|ANG)", names(s))]
	# property.get("RSTUDIO*", NULL, "system");
	# res = (s = Sys.getenv() );
	return (res);
	}
	

system.getALL = function() 
	{
	res = Sys.getenv();
	return(res);
	}
	
system.run = function() {} # maybe a wrapper for system() and system2()

system.saveState = function(key="DEFAULT")
	{
	memory.set(key, "-SYSTEM-", Sys.getenv() );	
	}

	
system.restoreState = function(key="DEFAULT") 
	{
	sys = memory.get(key, "-SYSTEM-");
	if(!is.null(sys))
		{
		Sys.setenv(sys); # will this work, or LOOP ?
		return(invisible(TRUE));
		}
	warning("There is no memory for key=XXX, nothing updated");
	return(invisible(TRUE));
	}



	
system.saveInitialState = function()
	{
	mhash = str.HASH();  # rainbow table?! nO 
	memory.set("system.INITIAL", "-SYSTEM-", mhash);
	system.saveState(mhash);
	}

system.restoreInitialState = function()
	{
	mhash = memory.get("system.INITIAL", "-SYSTEM-");
	system.restoreState(mhash);
	}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' property.set 
#'
#' This is a function to get/set the "attributes"/"attr"
#'
#' @param key NAME of the attribute
#' @param value VALUE to set NAME attribute
#' @param obj OBJECT to be updated
#'
#' @return obj UPDATED with attribute
#' @export 
#'
#' @examples
# .get has to be key first ... SYSTEM 
# set key on obj with value 
property.set_ = function(KEY, obj, value=NULL)
	{
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.KEY = check.type(KEY);
	if(!ct.KEY || !is.character(KEY))	
		{ key = deparse(substitute(key)); } else { key = KEY; }
##########################################################
	attributes(obj)[[ key[1] ]] = value;
	obj;	
	}
	
property.set = function(key, obj, value=NULL,
									as.null = FALSE,
									recycle = FALSE
									)
	{
	# !as.null vs as.null?
	if(is.null(value) && as.null)  # you may actually want to pass the value NULL in the "setter"
		{
		if(is.list(key))
			{
			k = names(key);
				names(key) = NULL;
			value = unlist(key);
			key = k;
			} else { stop("ERROR with key/val"); }
		}

	# allows multivariate ... keys determine the replacements
	# if value gets to end, it will recycle to beginning
	n.key = length(key);
	n.val = length(value);
	idx.v = 1;
		
	if(recycle)
		{
		for(i in 1:n.key)
			{
			attributes(obj)[[ key[i] ]] = value[idx.v];
			idx.v = 1 + idx.v; if(idx.v > n.val) { idx.v = 1; }  # allows for looping gracefully if UNEVEN
			}
		} else { attributes(obj)[[ key[1] ]] = value; }
	return (obj);
	}
	
 



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' property.get
#'
#' This is a function to get/set the "attributes"/"attr"
#'
#' @param key NAME of the attribute
#' @param value VALUE to set NAME attribute
#' @param obj OBJECT to be updated
#'
#' @return obj UPDATED with attribute
#' @export
#'
#' @examples
# has to be "key" first, or "system" has to pass in a NULL
property.get = function(key, obj, val=NULL)
	{
	res = attributes(obj)[[key]];
	return (res);
	}



#' @rdname getAttribute
#' @export
getAttribute = property.get;



# as dataframe?
property.getALL = function(obj=NULL)
	{
	res = attributes(obj);
	return (res);
	}


#' @rdname property.getAll
#' @export
property.getAll = property.getALL;

#' @rdname getAllAttributes
#' @export
getAllAttributes = property.getALL;





#' @rdname getALLAttributes
#' @export
getALLAttributes = property.getALL;

#' @rdname property.getAll
#' @export
property.getAll = property.getALL;


property.saveInitialState = function()
	{
	mhash = str.HASH();  # rainbow table?! nO 
	memory.set("property.INITIAL", "-SYSTEM-", mhash);
	property.saveState(mhash);
	}

property.restoreInitialState = function()
	{
	mhash = memory.get("property.INITIAL", "-SYSTEM-");
	property.restoreState(mhash);
	}













#' setOption
#'
#' @param myKey
#' @param myValue
#'
#' @return
#' @export
#'
#' @examples
option.set_ = function(KEY, value)
	{
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.KEY = check.type(KEY);
	if(!ct.KEY || !is.character(KEY))	
		{ key = deparse(substitute(key)); } else { key = KEY; }
##########################################################
	TEMPLATE = "options({key} = {value});"; 
	eval.fromTemplate(TEMPLATE, key, value);	
	}
	
options.set = function(keys, values)
	{
	n = length(keys);
	for(i in 1:n)
		{
		key = keys[i];
		value = values[i];
		TEMPLATE = "options({key} = {value});";  
			# options(str$digits.d = 3)
			# help.search.types
		eval.fromTemplate(TEMPLATE, key, value);
		# options(stats::setNames(list(key), value));
		}
	}

# options(warn=2);
# options("warn");	


	
setOption = options.set;
setOptions = options.set;
option.set = options.set;



#' getOptions
#'
#' @param keys
#'
#' @return
#' @export
#'
#' @examples
options.get = function(keys)
	{
	#  R::base has "getOption" but not "getOptions"	(multivariate)
	n = length(keys);
	res = vector("list", n);
	for(i in 1:n)
		{
		key = keys[i];
		res[[i]] = base::getOption(key);
		}
	list.return(res);
	}

getOptions = options.get;
option.get = options.get;


options.getALL = function()
	{
	options();
	}

option.getAll = options.getALL;
options.getAll = options.getALL;
option.getALL = options.getALL;

options.saveState = function(key="DEFAULT")
	{
	memory.set(key, "OPTIONS", options() );	
	}  

option.saveState = options.saveState;
	
options.restoreState = function(key="DEFAULT")
	{
	op = memory.get(key, "OPTIONS");
	if(!is.null(op))
		{
		options(op);
		return(invisible(TRUE));
		}
	warning("There is no memory for key=XXX, nothing updated");
	return(invisible(TRUE));
	}

option.restoreState = options.restoreState;





options.saveInitialState = function()
	{
	mhash = str.HASH();  # rainbow table?! nO 
	memory.set("options.INITIAL", "-SYSTEM-", mhash);
	options.saveState(mhash);
	}

options.restoreInitialState = function()
	{
	mhash = memory.get("options.INITIAL", "-SYSTEM-");
	options.restoreState(mhash);
	}





















































#' setPar
#'
#' @param myKey
#' @param myValue
#'
#' @return
#' @export
#'
#' @examples
# bazaar graphics::par opens a par window in RGui
par.set_ = function(KEY, value)
	{
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.KEY = check.type(KEY);
	if(!ct.KEY || !is.character(KEY))	
		{ key = deparse(substitute(key)); } else { key = KEY; }
##########################################################
	TEMPLATE = "graphics::par({key} = {value});"; 
	eval.fromTemplate(TEMPLATE, key, value);	
	}


par.set = function(keys, values)
	{
	pnames = names( graphics::par(no.readonly = TRUE) );
	n = length(keys);
	for(i in 1:n)
		{
		key = keys[i];
		value = values[i]; 
			if(n == 1) {value = values;}  # par(mar)
			if(is.list(values)) { value = values[[i]]; }  # don't know if this arises with 'par'
		if( key %in% pnames)
			{
			TEMPLATE = "graphics::par({key} = {value});"; 
			eval.fromTemplate(TEMPLATE, key, value);
			
			### str.replace("{value}", value) ... value is longer 1->m ... EDGE CASE ... I want it to be 1->1 
			
			# graphics::par(stats::setNames(list(key), value));
			} else { 
					warning.cat("key", key, " is either *NOT* a par key on this DEVICE or is READONLY"); 
					} 
		}
	}
	


setPar = par.set;



#' getPars
#'
#' @param keys
#'
#' @return
#' @export
#'
#' @examples
par.get = function(keys, vals=NULL, no.readonly = TRUE)
	{
	# NO HOPS ... NUMBER TICS ... call par() once, not three times 
	plist = graphics::par(no.readonly = no.readonly);	
	pnames = names( plist );
	n = length(keys);
	res = vector("list", n);
		# par.list = par();  # par()$key also works ... 
		# maybe list function that performs 
		# par.list[[keys]] ... par.list[[c(1,3]] ... ???
	for(i in 1:n)
		{
		key = keys[i];	
		res[[i]] = plist[[key]];
		}
	list.return(res);
	}
	
	
getPar = par.get;


par.getALL = function(no.readonly = TRUE)
	{
	par(no.readonly=no.readonly);  # only show ones we can change by default
	}

par.getAll = par.getALL;


	




	
par.saveInitialState = function()
	{
	mhash = str.HASH();  # rainbow table?! nO 
	memory.set("par.INITIAL", "-SYSTEM-", mhash);
	par.saveState(mhash);
	}

par.restoreInitialState = function()
	{
	mhash = memory.get("par.INITIAL", "-SYSTEM-");
	par.restoreState(mhash);
	}


par.saveState = function(key="DEFAULT", no.readonly = TRUE)
	{
	memory.set(key, "PAR", par(no.readonly = no.readonly) );	
	}

	
par.restoreState = function(key="DEFAULT") 
	{
	old.par = memory.get(key, "PAR");
	if(!is.null(old.par))
		{
		# in case the no.readonly protocol wasn't follwed
		suppressWarning( par(old.par) ); 
		return(invisible(TRUE));
		}
	warning("There is no memory for par key=XXX, nothing updated");
	return(invisible(TRUE));
	}















# get/set
# saveState(key)
# restoreState(key)

## similar to par ... see include.dir function 




# download ONE.R and install library, 
# housekeeping, remove the files/functions added to the ls() namespace ...
ls.saveState = function() {}
ls.restoreState = function() {}

