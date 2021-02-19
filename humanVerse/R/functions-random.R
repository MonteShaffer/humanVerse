

#' rand
#'
#' Generate random integers between two values (min, max).
#'
#' @param min By default -1*as.integer(Sys.time()), what is the minimum?  Could be negative.
#' @param max By default as.integer(Sys.time()), what is the maximum?
#' @param n By default 1, how many random elements do you want?
#' @param method By default "high-low" which slightly outperforms "floor" which both outperform "sample"
#' @param seed By default NULL, meaning we are not worried about tracking the seed here.
#' @param sample.replace For method "sample", will allow replace = FALSE (make certain n, min/max are comformable for this)
#'
#' @return
#' @export
#'
#' @examples
#' rand();            # positive or negative integer
#' rand(1);           # positive integer only
#' rand(1, n=3);      # returns 3 positive integers
#' rand(1,10, n=5, method="floor");  # Uses the floor method
#' rand(1,10, n=5, method="sample"); # Uses the sample method (available, but why?)
#' rand(1,10, n=5, method="sample", sample.replace=FALSE); # min, max, n must be comformable "with replacement = FALSE"
rand = function(min = -1*as.integer(Sys.time()), max = as.integer(Sys.time()), n = 1, method = "high-low", sample.replace = TRUE, seed = NULL)
    {
    # pracma::primes ... maybe seed with primes, using sieve attributed to Erasthostenes
    if(!is.null(seed)) { setSeed(seed); }
    me = substr( trimMe( tolower(method) ), 1, 2);
    if(me == "hi")  # high-low method
      {
      return( as.integer(( (max + 1) - min) * runif(n) + min) );
      }
    if(me == "fl")  # floor method
      {
      return( as.integer( floor( runif(n, min = min, max = (max + 1) ) ) ) );
      }
    if(me == "sa")  # sample method
      {
      if(!sample.replace)
        {
        len = (max - min) + 1;
        if(len < n)
          {
          warning( "sample.replace forced to TRUE" );
          sample.replace = TRUE;
          }
        }
      return( sample(min:max, n, replace = sample.replace) );
      }
    stop( paste0('Unknown method "', method, '" in function [rand]') );
    }



#' initSeed
#'
#' @param ... Parameters that can be passed onto the 'rand' function
#'
#' @return A single integer
#' @export
#'
#' @examples
#' initSeed();
#' initSeed(1, 10^5, method="floor");
initSeed = function(...)
  {
  rand(...)[1];  # if they wrongly passed in n > 1, we only return [1]
  }


#' initSeedMemory
#'
#' @param purge.memory If TRUE, this memory is erased and reset.
#'
#' @return NULL (nothing)
#' @export
#'
#' @examples
#' initSeedMemory();
#' setSeed(); getSeed(); initSeedMemory(purge.memory = TRUE); getSeed();
initSeedMemory = function(purge.memory = FALSE, verbose = TRUE)
  {
  if(!exists(".random.seed.memory") || purge.memory)
    {
    if(verbose)
      {
      cat("initSeedMemory :: initializing list '.random.seed.memory'", "\n");
      }
    .GlobalEnv$.random.seed.memory = list();
    }
  }

setSeed = function(seed, force.new = FALSE, key = "last", ..., args.set = list(), verbose = TRUE )
  {
  # can I have two ellipses in a single function?
    # one for initSeed
    # one for set.seed
    # nope, argslist
  if(is.null(seed) || force.new == TRUE)
    {
    seed = initSeed(...);
    if(verbose)
      {
      cat("setSeed :: generating new integer seed ... ", "\t", seed, "\n");
      }
    initSeedMemory();
    }
  .GlobalEnv$.random.seed.memory[[key]] = seed;
  if(verbose)
      {
      cat("setSeed :: global value stored [key] = ",key," ... [seed] = ",seed, "\n");
      }
  if( !exists("kind", args.set) )         { kind = NULL; }
  if( !exists("normal.kind", args.set) )  { normal.kind = NULL; }
  if( !exists("sample.kind", args.set) )  { sample.kind = NULL; }
  if(verbose)
      {
      cat("setSeed :: calling base::set.seed with seed ... ", seed, "\n");
      }
  set.seed(seed, kind=kind, normal.kind=normal.kind, sample.kind=sample.kind);
  }

# I could create a "keyed" list of memory, not just last ...
getSeed = function(key, verbose = TRUE)
  {
  if( missing(key) ) { key = "last"; }
  if(verbose) { cat("getSeed :: looking up key ... ", "\t", key); }
  if(exists(key, .random.seed.memory))
    {
    if(verbose) { cat("\n\t ... found with value: ", "\t", .GlobalEnv$.random.seed.memory[[key]], "\n"); }
    .GlobalEnv$.random.seed.memory[[key]];
    } else { FALSE; }
  }
