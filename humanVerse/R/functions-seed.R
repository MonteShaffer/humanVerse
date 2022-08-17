
##################################################
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


#' @rdname initSeed
#' @export
initSeed = seed.init;









##################################################
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
seed.get = function(key, keep.attributes = FALSE, verbose = FALSE)
	{
	memory.init();
	# I could create a "keyed" list of memory, not just last ...
	if( missing(key) ) { key = "last"; }
	if(verbose) { cat("getSeed :: looking up key ... ", "\t", key); }
	if(exists(key, .GlobalEnv$.humanVerse[["seed"]]))
		{
		if(verbose) { cat("\n\t ... found with value: ", "\t", .GlobalEnv$.humanVerse[["seed"]][[key]], "\n"); }
		my.seed = .GlobalEnv$.humanVerse[["seed"]][[key]];
	if(!keep.attributes) { my.seed = as.integer(my.seed); }
	my.seed;
		} else { FALSE; }
	}


#' @rdname getSeed
#' @export
getSeed = seed.get;











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
seed.set = function(seed, key, ..., 
							args.set = list(), 
							print.seed = TRUE, 
							verbose = FALSE 
					)
	{
	memory.init();
	if( missing(key) ) { key = "last"; }
	if(is.null(seed))
		{
		seed = initSeed(...);
		if(verbose)
			{
			cat("setSeed :: generating new integer seed ... ", "\t", seed, "\n");
			}
		}
	
	### ... can't be used twice ... throws and error if extra elements
	if( !exists("kind", args.set) )			{ kind = NULL; }
	if( !exists("normal.kind", args.set) )	{ normal.kind = NULL; }
	if( !exists("sample.kind", args.set) )	{ sample.kind = NULL; }
	
	my.seed = seed;
	
	# is.set ISSUE about NULL
	vals = list("kind" = kind, "normal.kind" = normal.kind, "sample.kind" = sample.kind);	
	my.seed = property.set( my.seed, vals );
	
	.GlobalEnv$.humanVerse[["seed"]][[key]] = my.seed;
	if(verbose)
			{
			cat("setSeed :: global value stored [key] = ",key," ... [seed] = ",seed, "\n");
			}
		
	if(verbose)
			{
			cat("setSeed :: calling base::set.seed with seed ... ", seed, "\n");
			}

	if(print.seed)
	{
	cat("\n setSeed: ", seed, "\n");
	}
	
	set.seed(seed, kind=kind, normal.kind=normal.kind, sample.kind=sample.kind);
	}

#' @rdname setSeed
#' @export
setSeed = seed.set;



