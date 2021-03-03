
# maybe just one global container called ".humanVerse"
# I can init() and store to Global Space
# I can register() something like "random.seed" or another element
# "functions" which keeps track of getParameters externally
# "color" caches wheel,  md5(stringify(params)) as key?


# ## utils::globalVariables(c(".github.humanVerse.raw", ".github.humanVerse.view"));

## utils::globalVariables(c(".random.seed.memory"));

# utils::globalVariables(c(".humanVerse"));


initMemory = function(purge.memory = FALSE, verbose = TRUE)
  {
  if(!exists(".humanVerse") || purge.memory)
    {
    if(verbose)
      {
      cat("humanVerse::initMemory ... initializing list '.humanVerse'", "\n");
      }
    .GlobalEnv$.humanVerse = list();
		initSeedMemory();
		initColorMemory();
		initInflationMemory();
		initPathMemory();
	}	
  }

	
	
	
