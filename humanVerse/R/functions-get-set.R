

# https://stackoverflow.com/questions/66329835/
# nice work :: B. Christian Kamgang
# .GlobalEnv$.function.args.memory ... key memory on last function call ... so I could reference outside the function

grabFunctionParameters <- function() 
	{
    pf			= parent.frame();    
    my.names	= ls(envir = pf, all.names = TRUE, sorted = FALSE);
	
	dots		= if("..." %in% my.names) { eval(quote(list(...)), envir = pf); } else { list(); }	
	dots.idx	= ( names(dots) != "" );
    
    remaining 	= sapply( setdiff(my.names, "..."), as.name);
	
	not.dots	= if(length(remaining) > 0) { lapply( remaining, eval, envir = pf);  } else { list(); }
	
   
	res = list();
	
		res$.fn. 			= as.character( sys.call(1L)[[1L]] );
		res$.scope. 		= pf;
		res$.keys. 			= names( not.dots );
		res$.vals. 			= not.dots; 							# unname(not_dots);  # I want keys on "vals"
		res$.dots.keys. 	= names( dots[dots.idx] );
		res$.dots.vals. 	= dots[dots.idx]; 						# unname(dots[dots.idx]); 

	res;
	}   



# save memory ... restoreState ... pdf 
# par(mar=c(0.25, 0.25, 0.25, 0.25)
	# R.O. indicates read-only arguments: These may only be used in queries and cannot be set. ("cin", "cra", "csi", "cxy", "din" and "page" are always read-only.)
	# https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/par
setParKey = function(myKey, myValue)
	{	
	pnames = names( par(no.readonly = TRUE) );
	if(is.element(myKey, pnames))
		{
		# par()[[myKey]] = myValue;
		par(setNames(list(myValue), myKey))
		}
	}
	
getParKey = function(myKey)
	{	
	pnames = names( par(no.readonly = FALSE) );
	if(is.element(myKey, pnames))
		{
		# par()[[myKey]];	
		par(myKey);
		}
	}


getAttributes = function(myObj)  # maybe getAllAttributes
	{
	attributes(myObj);
	}

getAttribute = function(myAttribute, myObj)
	{
	attributes(myObj)[[myAttribute]];	
	}
	
setAttribute = function(myAttribute, myValue, myObj)
	{
	attributes(myObj)[[myAttribute]] = myValue;	
	myObj;  # no object referencing, so I must return
	}


