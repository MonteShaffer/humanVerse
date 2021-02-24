

# https://stackoverflow.com/questions/66329835/
# nice work :: B. Christian Kamgang
# .GlobalEnv$.function.args.memory ... key memory on last function call ... so I could reference outside the function

grabFunctionParameters <- function() {
    pf <- parent.frame()    
    args_names <- ls(envir = pf, all.names = TRUE, sorted = FALSE)
    if("..." %in% args_names) {
    dots <- eval(quote(list(...)), envir = pf)
    }  else {
    dots = list()
    }
    args_names <- sapply(setdiff(args_names, "..."), as.name)
    if(length(args_names)) {
    not_dots <- lapply(args_names, eval, envir = pf) 
    } else {
    not_dots <- list()
    }
   # out <- c(not_dots, dots)
   # out[names(out) != ""]                                  # remove unnamed values in ... (if any)
   idx <- names(dots) != "";
   # res <- list(.keys. = names(not_dots), .vals. = unname(not_dots), .name. = "myFunction", .scope. = pf, .dot.keys. = names(dots[idx]), .dot.vals. = unname(dots[idx])); 
   
   res <- list()
	res$.keys. = names(not_dots);
	res$.vals. = not_dots; # unname(not_dots);
	res$.name. = "myFunction";
	res$.scope. = pf;
	res$.dot.keys. = names(dots[idx]);
	res$.dot.vals. = dots[idx]; # unname(dots[idx]); 
   
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


