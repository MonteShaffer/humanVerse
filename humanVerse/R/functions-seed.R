



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' seed.init
#'
#' @param ... Parameters that can be passed onto the 'rand' function
#'
#' @return A single integer
#' @export
#'
#' @examples
#' # seed.init();
#' # seed.init(1, 10^5, method="floor");
seed.init = function(...)
	{
	rand(...)[1];	# if they wrongly passed in n > 1, we only return [1]
	}




seed.create = function(key = "LAST-SEED", ...)
	{
	s = seed.init(...);
	memory.set(key, "-SEED-", s);
	s;
	}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' seed.get
#'
#' @param key
#' @param keep.attributes
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
seed.get = function(key = "LAST-SEED", unused=NULL, details=FALSE)
	{
	memory.init();	
	seed.value = memory.get(key, "-SEED-");
	if(!details) { seed.value = as.integer(seed.value); }
	v.return(seed.value);
	}



#' seed.set
#'
#' @param seed
#' @param key
#' @param ...
#' @param args.set
#' @param print.seed
#' @param verbose
#'
#' @return
#' @export 
seed.set = function() {}
seed.set = function(key = "LAST-SEED", 
							seed.value=NULL,
							from.memory=TRUE, # if key, but no value
							... , 				# rand() parameters
							seed.args = list() 	# seed() parameters
					)
	{
debug=FALSE; 
	
	force.new = FALSE;
	# accounting for standard set.seed(NULL) logic ...
	if(is.null(key)) 	{ key = "LAST-SEED"; force.new = TRUE;}
	if(is.numeric(key)) { seed.value = key; key = "LAST-SEED"; }
	if(is.numeric(seed.value)) { seed.value = as.integer(seed.value); }	

#########################  SEED PARAMS #####################	
	# TRAPS "NULL" in a list ... 
	seed.args_ = list(	"kind" = NULL, 
						"normal.kind" = NULL, 
						"sample.kind" = NULL
					);
	## THIS SEEMS LIKE A SEPARATE 'VARIADIC' FUNCTION
	seed.args_ = map.args(seed.args_, seed.args);

if(debug)
	{	
cat("\n key: ", key, " \t\t seed.value : ", seed.value, " \n");
	}
	
	memory.init();
	if(is.null(seed.value) && from.memory && !force.new)
		{
		seed.value = seed.get(key, details=TRUE);
		}
	if(is.null(seed.value)) 
		{	
		seed.value = seed.init(...);
		
		seed.value = property.set("seed.args", seed.value, seed.args_);
		# timezone?
		seed.value = property.set("when", seed.value, Sys.time());
		memory.set(key, "-SEED-", seed.value);
		}

if(debug)
	{
cat("\n key: ", key, " \t\t seed.value : ", seed.value, " \n");
	}
	
	memory.append("-SEED-HISTORY-", "-SYSTEM-", seed.value);	
	

	## VERY END, so STACK doesn't change things 	
	set.seed(seed.value, 
				kind		= seed.args_$kind, 
				normal.kind	= seed.args_$normal.kind, 
				sample.kind	= seed.args_$sample.kind
			);
	seed.value;
	}
