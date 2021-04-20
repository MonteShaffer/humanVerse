
# Sys.setenv("HUMANVERSE_CACHE" = "R:/R-humanVerse-cache");


# maybe just one global container called ".humanVerse"
# I can init() and store to Global Space
# I can register() something like "random.seed" or another element
# "functions" which keeps track of getParameters externally
# "color" caches wheel,  md5(stringify(params)) as key?


# ## utils::globalVariables(c(".github.humanVerse.raw", ".github.humanVerse.view"));

## utils::globalVariables(c(".random.seed.memory"));

# utils::globalVariables(c(".humanVerse"));

## maybe create a generic set of functions ... get/set Memory


#' initMemory
#'
#' @param purge.memory
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
initMemory = function(purge.memory = FALSE, verbose = TRUE)
  {
  if(!exists(".humanVerse") || purge.memory)
    {
    if(verbose)
      {
      cat("humanVerse::initMemory ... initializing list '.humanVerse'", "\n");
      }

	# assign(".humanVerse", list(), envir = .GlobalEnv);

    .GlobalEnv$.humanVerse = list();
		initPathMemory();
		initInflationMemory();
		initColorMemory();
		initSQLMemory();
		initFunctionMemory();
		initSystemMemory();
		initSeedMemory();

	}
  }


#' initInflationMemory
#'
#' @param purge.memory
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
initInflationMemory = function(purge.memory = FALSE, verbose = FALSE)
	{
	if(!exists(".humanVerse")) { initMemory(); }


	if(!exists("inflation", .GlobalEnv$.humanVerse) || purge.memory)
		{
		if(verbose)
		  {
		  cat("humanVerse::initInflationMemory ... initializing list '.humanVerse[[\"inflation\"]]'", "\n");
		  }
		.GlobalEnv$.humanVerse[["inflation"]] = list();
		}
	}


#' initFunctionMemory
#'
#' @param purge.memory
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
initFunctionMemory = function(purge.memory = FALSE, verbose = FALSE)
	{
	if(!exists(".humanVerse")) { initMemory(); }

	if(!exists("functions", .GlobalEnv$.humanVerse) || purge.memory)
		{
		if(verbose)
		  {
		  cat("humanVerse::initFunctionMemory ... initializing list '.humanVerse[[\"functions\"]]'", "\n");
		  }
		# this is a list of "included" or "sourced" functions ...
		.GlobalEnv$.humanVerse[["functions"]] = list();
			.GlobalEnv$.humanVerse[["functions"]][["local"]] = list();
			.GlobalEnv$.humanVerse[["functions"]][["local-search"]] = list();
		# this is a list of functions referenced in stack using getFunctionParameters(TRUE)
		.GlobalEnv$.humanVerse[["stack"]] = list();
		.GlobalEnv$.humanVerse[["stack-order"]] = list();
		}
	}

#' initSQLMemory
#'
#' @param purge.memory
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
initSQLMemory = function(purge.memory = FALSE, verbose = FALSE)
	{
	if(!exists(".humanVerse")) { initMemory(); }

	if(!exists("sql", .GlobalEnv$.humanVerse) || purge.memory)
		{
		if(verbose)
		  {
		  cat("humanVerse::initSQLMemory ... initializing list '.humanVerse[[\"sql\"]]'", "\n");
		  }
		.GlobalEnv$.humanVerse[["sql"]] = list();
		}
	}

#' initSystemMemory
#'
#' @param purge.memory
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
initSystemMemory = function(purge.memory = FALSE, verbose = FALSE)
	{
	if(!exists(".humanVerse")) { initMemory(); }

	if(!exists("system", .GlobalEnv$.humanVerse) || purge.memory)
		{
		if(verbose)
		  {
		  cat("humanVerse::initSystemMemory ... initializing list '.humanVerse[[\"system\"]]'", "\n");
		  }
		.GlobalEnv$.humanVerse[["system"]] = list(
												"nine" = list(), # cache for functions-nines.R
												"stack-length" = 10,
												"max-print" = 225  # useful for reviewing global .humanVerse
												);

		# setOption("max.print", .GlobalEnv$.humanVerse[["system"]][["max-print"]]);
		# options(max.print = .GlobalEnv$.humanVerse[["system"]][["max-print"]]);
		}
	}

#' initPathMemory
#'
#' @param purge.memory
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
initPathMemory = function(purge.memory = FALSE, verbose = FALSE)
	{
	if(!exists(".humanVerse")) { initMemory(); }

	if(!exists("path", .GlobalEnv$.humanVerse) || purge.memory)
		{
		if(verbose)
		  {
		  cat("humanVerse::initPathMemory ... initializing list '.humanVerse[[\"path\"]]'", "\n");
		  }
		.GlobalEnv$.humanVerse[["path"]] = list(
												"CACHE" = getSourceLocation(),
												"github" = list( "main" = "https://github.com/MonteShaffer/humanVerse/",
																 "raw"  = "https://raw.githubusercontent.com/MonteShaffer/humanVerse/"
																 )
												);
		}
	}




#' initColorMemory
#'
#' @param purge.memory
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
initColorMemory = function(purge.memory = FALSE, verbose = FALSE)
  {
  if(!exists(".humanVerse")) { initMemory(); }



  if(!exists("color", .GlobalEnv$.humanVerse) || purge.memory)
    {
    if(verbose)
      {
	    cat("humanVerse::initColorMemory ... initializing list '.humanVerse[[\"color\"]]'", "\n");
      }
    .GlobalEnv$.humanVerse[["colors"]] = list();
		.GlobalEnv$.humanVerse[["colors"]][["random"]] = list();  			# captures get/set seed
		.GlobalEnv$.humanVerse[["colors"]][["lists"]] = list();				# keyed lists of hex with "alpha" maybe
		.GlobalEnv$.humanVerse[["colors"]][["dataframes"]] = list();		# cached tables
		.GlobalEnv$.humanVerse[["colors"]][["nearest"]] = list();			# cached "nearest-color" index
		.GlobalEnv$.humanVerse[["colors"]][["search"]] = list();			# cached "search" history
    }
  }


#' initSeedMemory
#'
#' @param purge.memory If TRUE, this memory is erased and reset.
#'
#' @return NULL (nothing)
#' @export
#'
#' @examples
#' # initSeedMemory();
#' # setSeed(); getSeed(); initSeedMemory(purge.memory = TRUE); getSeed();
initSeedMemory = function(purge.memory = FALSE, verbose = FALSE)
  {
  if(!exists(".humanVerse")) { initMemory(); }

  if(!exists("seed", .GlobalEnv$.humanVerse) || purge.memory)
    {
    if(verbose)
      {
	    cat("humanVerse::initSeedMemory ... initializing list '.humanVerse[[\"seed\"]]'", "\n");
      }
    .GlobalEnv$.humanVerse[["seed"]] = list();
    }
  }





