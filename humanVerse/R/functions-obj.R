


is.json = function(str)
	{
	
	}

o.unserialize = function(str)
	{
	
	
	}

o.serialize = function(obj, jsonify=FALSE)
	{
	if(!jsonify)
		{
		# should be very compact ...
		os = .serialize(obj);
		oss = paste0( raw.toString(os), collapse="");
		b64 = .hex_b64(oss);
		} else {
				os = JSON.stringify(obj);
				b64 = str.toB64(os);
				}
	b64;
	}	
	
	
	
	
	
# x = as.raw(1:10); y = raw.toString(x); 
# z = raw.fromString(y); identical(x,z);

# x = as.raw(1:10); y = raw.toString(x, collapse=NULL);
# z = raw.fromString(y, splitN=FALSE); identical(x,z);

	}
 
.serialize = function(obj)
	{
	# may want to add "wrapper" features later 
	serialize(obj, NULL);
	}



.unserialize = function(obj.raw)
	{
	# may want to add "wrapper" features later 
	obj = unserialize(obj.raw);
	obj;
	}






obj.whatis = function(obj)
	{
	ty.obj = typeof(obj);
	tt.obj = v.type(obj);
	cl.obj = class(obj);
	ve.obj = is.vector(obj);
	at.obj = is.atomic(obj);
	co.obj = is.complex(obj);
	df.obj = is.data.frame(obj);
	li.obj = is.list(obj); # df are lists 
	fu.obj = is.function(obj);
	# what to return
	}




	
obj.dput = function(obj, ...)
	{
	if( !exists("control", inherits = FALSE ) )			
		{ 
		control = "all"; 
		}
	dput(obj, control=control, ...);
	}
	
	
	
# x = list(); obj.exists(x[["this"]][["doesn't"]][["exist"]], "monte");
obj.keyExists = function(key, obj)
	{
	# this traps NULL on obj 
	# Error in as.environment(where) : using 'as.environment(NULL)' is defunct
	# x = list(); exists("monte", x[["this"]][["doesn't"]][["exist"]]);
	# exists("monte", 
	if(is.null(obj)) { return(FALSE); }
	exists(key, obj);	
	}
	



obj.toMD5 = function(obj, ...)
	{
	# via JSON
	# via SERIALIZE
	 

	}


obj.fromMD5 = function(obj, ...)
	{
	# via JSON
	# via SERIALIZE

	# another easter egg

	}


obj.get = function(obj.str, ...) 
	{
	# get 
	get(obj.str, ...);  			
	## envir is in the weeds, we are inside a function, one layer up
	} 

obj.set = function(obj.str, value, ...) 
	{
	# assign
	assign(obj.str, value, ...);	
	## envir is in the weeds, we are inside a function, one layer up
	} 
	
# e1 <- new.env()
# unlist( mget(letters, e1, ifnotfound = unlist(as.list(LETTERS))) )
# assign("x", 3, e1)
# unlist( mget(letters, e1, ifnotfound = unlist(as.list(LETTERS))) )	
## https://stackoverflow.com/questions/59491195/is-there-a-way-to-list-all-environments-environment-names-in-r
##
## search();
## 
allParents = function(env = globalenv(), result = list()) {
  result = c(list(parent.env(env)), result)
  if(!identical(result[[1]], emptyenv())) {
    result <- allParents(result[[1]], result)
  }
  return(result)
}

## allParents()
## environmentName( environment())


obj.fromString = function(obj.str)
	{
	obj.info = tryCatch	(

						{
						info = eval(parse(text = obj.str));
						cat("\n ===== MONTE ===== \n");
						print(obj.str);
						cat("\n"); print(info); cat("\n");
						},

						warning = function(w)
							{
							warning(paste0("### WARNING ###  throws a warning","\n\n",w));
							info; # let's still return the value 	.
							},
	
						error = function(e)
							{
							warning(paste0("### ERROR ###  throws an error","\n\n",e));
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



