	

obj.get = function(obj.str, ...) 
	{
	# get
	get(obj.str, ...);  			## envir is in the weeds
	} 

obj.set = function(obj.str, value, ...) 
	{
	# assign
	assign(obj.str, value, ...);	## envir is in the weeds
	} 



obj.fromString = function(obj.str)
	{
	obj.info = tryCatch	(

						{
						info = eval(parse(text = obj.str));
						},

						warning = function(w)
							{
							warning(paste0("### WARNING ###  throws a warning","\n\n",w));
							info; # let's still return the value 	.
							},
	
						error = function(e)
							{
							# warning(paste0("### ERROR ###  throws an error","\n\n",e));
							res = FALSE;
							res = property.set(res, "ERROR", e);
							return (res);
							},

						finally =
							{
				
							}
						);
	obj.info;
	}




# x can be actual object or string
# ... is for "more" ... but rm/remove have other options
obj.remove = function(..., list = character(), pos = -1,
       envir = as.environment(pos), inherits = FALSE)
	{


	}



obj.delete = obj.remove;














obj.access = function(str)
	{
	E = unlist( strsplit(as.character(str),"[@]") );
	k = length(E);
	if(k==1)
		{
		eval(parse(text=str));
		} else	{
				# k = 2
				nstr = paste("attributes(",E[1],")",sep="");
				nstr = paste(nstr,'$',E[2],sep="");

				if(k>2) 
					{
					for(i in 3:k)
						{
						nstr = paste("attributes(",nstr,")",sep="");
						nstr = paste(nstr,'$',E[i],sep="");
						}
					}
				obj.access(nstr);
				}	
	}



access <- `$$` <- function(str)
    {
    E = unlist( strsplit(as.character(str),"[@]") );
        k = length(E);
        if(k==1)
            {
            eval(parse(text=str));
            } else {
                # k = 2
                nstr = paste("attributes(",E[1],")",sep="");
                nstr = paste(nstr,'$',E[2],sep="");

                if(k>2) {
                    for(i in 3:k)
                        {
                        nstr = paste("attributes(",nstr,")",sep="");
                        nstr = paste(nstr,'$',E[i],sep="");
                        }
                    }
                access(nstr);
                }
    }

