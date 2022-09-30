
primes.load = function() {}
	# if is.library_("bit") else {other}
	# 
primes.scan = function() {}
# scan bit 
# reformat downloaded file, one prime per line
# MATH has 1,000,000; 				max 15485863
# because of gn I have 1,057,662;	max 16441303
# bits didn't save as expected ... little larger than I anticipated ...
#  
 

optimus.logic = function(p, n, first=TRUE, optimus=FALSE)
	{
	# I have a list of primes that is length(p) >= n ... NO OPTIMUS
	if(optimus) { p = c(OPTIMUS_PRIME, p); }
	p = as.integer(p);
	if(first) { p[1:n]; } else { p[p<=n]; }
	}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' primes.bit
#'
#' Uses library(bit) to perform SIEVE on bits 
#'   See bit::bitwhich
#'
#'   A TODO: bit-storage of primes with an index lookup
#'             accessible by idx lookups (ranges)
#'                maybe an RDS of bits with first million primes ... 
#'
#'
#' @param n {INTEGER:  how many _what_ ??? }
#' @param first {TRUE: returns first 'n' primes; FALSE returns primes <= 'n' }
#' @param optimus {TRUE: include 1 as prime}
#'
#' @return NumericVector (of primes)
#' @export
#'
#' @examples
#' x = primes.bit(100, TRUE, FALSE); length(x);		# gets 100 primes 
#' x = primes.bit(100, FALSE, FALSE); length(x);	# gets primes <= 100
#' x = primes.bit(100, TRUE, TRUE); length(x);
#' x = primes.bit(100, FALSE, FALSE); length(x);

#' # sizeof(x)...
#' x = primes.bit(1000, FALSE, "bits");  # 1000 * 2 /8 (2 = TRUE?FALSE)
#' y = as.character(as.raw(x)); # seems to append a lot of zeroes at END ... 
#' z = .hex_b64(y);  ... store that file AS TEXT ... 
#' 

primes.bit = function(n, first=TRUE, return="primes")
	{
	RETURN = prep.arg(return, n=1);
# timer.start("bits"); x = prime.bit((1*1000)); length(x); max(x); timer.stop("bits");
	# you could build a bits-table, and search primes within 
	if(!is.library_("bit")) { stop("requires library(bit); ... "); }
	gn = n; if(n > 10^6) { stop("such a large [n] may tax the system"); }
	if(first) { gn = ceiling( n * log(n) + n * log(log(n)) ); }
	gn.sqrt = ceiling( sqrt(gn) );
	# to get 1,000,000 primes; gn = 16,441,303
	
	bits.prime = bit::bitwhich(gn, TRUE);  # set all to TRUE, including 1
	bits.prime[1] = FALSE;
	
	## IS.EVEN ##
	i = 4;
	# is it faster to load all the idxs of EVEN numbers?
	idx = which( (1:gn %% 2 == 0) & (1:gn > 3) );
	bits.prime[idx] = FALSE;
	

	## SIEVE  p is (6k +/- 1) ## 	
	i = 3;
	while(i < gn.sqrt)
		{
		if(bits.prime[i] == TRUE) # is there a faster way to check == 1
			{
			k = i * 2;
			j = i * i;
			js = c(j);
			while(j < gn)
				{
				# bits.prime[j] = FALSE;
				j = k + j;
				js = c(js, j);
				}
			js = js[js <= gn];
			bits.prime[js] = FALSE;
			}
		i = 2+i;
		}
	
	
	# RETURN "primes" or "bits"
	if(RETURN != "p") { return(bits.prime); }
	
	### save(bits.prime, file=paste0("primes-",n,".RData") );
	# use save/load ... to keep format?
	# , file.out=NULL
	# #' @param file.out {file.path + file.name to save bits as RDS}
# saveRDS(bits.prime, file=paste0("primes-",n,".rds") );
		
	p = which(bits.prime == TRUE); # indexes are primes (R indexed != zero)
	# gn is upper bound, so ... truncate ... 
	if(first)  { p = p[1:n];    } 
	if(!first) { p = p[p <= n]; }
# OPTIMUS_PRIME not checked here ... in this function 	
		
	# we still append bits.prime ... 
	p = property.set("bits", p, bits.prime);
	return(p);
	}
	

#' @rdname primes.bits
#' @export
primes.bits = primes.bit;

#' @rdname prime.bits
#' @export
prime.bits = primes.bit;

#' @rdname prime.bits
#' @export
prime.bits = primes.bit;
	
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' primes.pracma
#'
#' Modified pracma::primes algorithm for efficiency and to allow first/optimus.
#'
#'
#' @param n {INTEGER:  how many _what_ ??? }
#' @param first {TRUE: returns first 'n' primes; FALSE returns primes <= 'n' }
#' @param optimus {TRUE: include 1 as prime}
#'
#' @return NumericVector (of primes)
#' @export
#'
#' @examples
#' x = primes.pracma(100, TRUE, FALSE); length(x);		# gets 100 primes 
#' x = primes.pracma(100, FALSE, FALSE); length(x); 	# gets primes <= 100
#' x = primes.pracma(100, TRUE, TRUE); length(x);
#' x = primes.pracma(100, FALSE, FALSE); length(x);	
primes.pracma = function(n, first=TRUE)
	{
	# this duplicates the primary logic of pracma::primes
	# by computing 'sqrt' one time, it speeds up things 'slightly'
	# it allows for firstN or fromN with first=FLAG
	gn = n;
	if(first) { gn = ceiling( n * log(n) + n * log(log(n)) ); }
	gn.sqrt = floor( sqrt(gn) );  # needs to round down so the "seq by k" doesn't break ...

	p = seq(1, gn, by = 2); # odd numbers
    q = length(p);
    p[1] = 2; 	# replace 1 with 2 (prime)
				# 9 is the first non-prime?
	if(gn >= 9)
		{
		for (k in seq(3, gn.sqrt, by = 2) )
			{
			k.idx = (k + 1)/2;
			if (p[k.idx] != 0)
				{
				# using a squared rule on indexes ?
				k2.idx = (k * k + 1)/2;

				# cat("\n", " k = ", k, "... (k+1/2) = ", k.idx, "... (k * k + 1)/2 = ", k2.idx, "\n");

				p[ seq(k2.idx, q, by = k) ] = 0;
				}
			}
		}

	p = p[p > 0]; # why?

	if(first)  { p = p[1:n];    } 
	if(!first) { p = p[p <= n]; }  
	
	return(p);
	}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' primes.inRange
#'
#' Finds the prime numbers in a range (xmin, xmax).
#'   Current algorithm has to compute to xmax, and truncate.
#'   A TODO: bit-storage of primes with an index lookup
#'             accessible by idx lookups (ranges)
#'                maybe an RDS with first million primes ... 
#'
#' @param xmin {INTEGER:  lower bound }
#' @param xmax {INTEGER:  upper bound }
#' @param ... {Allows parameters: first=TRUE/FALSE, optimus = TRUE/FALSE
#'					and "method" from humanVerse::primes.get
#'
#' @return NumericVector (of primes)
#' @export
#'
#' @examples
#' x = primes.inRange(1, 100); length(x);
#' x = primes.inRange(2015, 2525); length(x);	
primes.inRange = function(xmin, xmax, ...)
	{
	# ideal method for inRange would be primes.bit (load large set)
	# primes.default function would be nice, still would have to compare
	if( !exists("method", inherits = FALSE ) ) { method	= "first"; }
	p = primes.get(xmax, first=FALSE, method=method);
	
	p = p[(p >= xmin)];
	p = p[(p <= xmax)];
	p;
	}
	
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' primes.get
#'
#'
#' Wrapper function to perform prime calculations using various methods.
#'
#'
#' @param n {INTEGER:  how many _what_ ??? }
#' @param first {TRUE: returns first 'n' primes; FALSE returns primes <= 'n' }
#' @param optimus {TRUE: include 1 as prime OPTIMUS_PRIME}
#' @param method	{  	
#'					[c]pp_primes from HVcpp or Rccp::source 
#'					[p]racma ... requires library (pracma)
#'					[b]it(s) ... requires library (bit)
#'					[s]fsmisc ... requires library (sfsmisc)
#'					[h]ack ... DEFAULT ... internal enhanced `primes.pracma` 
#'					}
#'
#' @return NumericVector INTEGER (of primes)
#' @export
#'
#' @examples
#' x = primes.get(100, TRUE, FALSE); length(x);  	# gets 100 primes 
#' x = primes.get(100, FALSE, FALSE); length(x);	# gets primes <= 100
#' x = primes.get(100, TRUE, TRUE); length(x);
#' x = primes.get(100, FALSE, FALSE); length(x);	
#' # NOT RUN # x = primes.get(100, method="base"); 
#' # NOT RUN # y = primes.get(100, method="pracma");	stopifnot(identical(x,y));
#' # NOT RUN # z = primes.get(100, method="sfsmisc");	stopifnot(identical(x,z));
primes.get = function(n, first=TRUE, optimus=FALSE, method="first")
	{
	# update method to ="first" 
	if(!is.integer(n)) { n = as.integer(n); }
	n = n[1]; # just the first element, if a vector 
	if(n < 1) { return(NULL); }
# dput(n);
	# return NULL on 1 ... let's just calculate the first 	
	if(optimus  && n == 1) { return( OPTIMUS_PRIME ); }
	if(!optimus && n == 1) { return( NULL ); }
	p = primes.pracma(10);
	###############  MANUAL HACK on FIRST FEW ELEMENTS ###############
	if(!first && optimus  && n <= 9) 
		{ return( c(OPTIMUS_PRIME, p[p<=(n-1)]) ); }
	if(!first && !optimus && n <= 9) 
		{ return(      p[p<=n]  ); }	
	if(first && optimus  && n <= 9) 
		{ return( c(OPTIMUS_PRIME, p[1:(n-1)]) ); }
	if(first && !optimus && n <= 9) 
		{ return(      p[1:n] ); }
		
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.METHOD = check.type(method);
	if(!ct.METHOD || !is.character(method))	
		{ method = deparse(substitute(method)); } 
##########################################################	
	METHOD = prep.primeMethod(method, n=1);

	FNS = list(
			"cpp" 		= function() { cpp_primes(n, first); } ,
			"bit" 		= function() { primes.bit(n, first=first, optimus=optimus); },
			"hack" 		= function() { primes.pracma(n, first); },
			"pracma" 	= function() { pracma::primes(gn); } ,
			"sfsmisc" 	= function() { sfsmisc::primes(gn); }			
			);
			
			
	if(METHOD != "first")
		{
		# AS-IS, no checks 
		res = FNS[[METHOD]]();
		res = optimus.logic(res, n, first, optimus);
		return(res);		
		}

	# CASCADING, first-one to meet criteria 
	hasResult = FALSE;
	if(!hasResult && exists("cpp_primes"))
		{
		# must not have exported it ... 
		hasResult = TRUE;
		res = FNS[["cpp"]]();
		}
		
	if(!hasResult && is.library_("bit"))
		{
		hasResult = TRUE;
		res = FNS[["bit"]]();		
		}
	# the others are slower, they can be benchmarked ... 	
	if(!hasResult)
		{
		hasResult = TRUE;
		res = FNS[["hack"]]();		
		}
	res = optimus.logic(res, n, first, optimus);
	return(res);		
	}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' prime.factors
#'
#'
# multivariate is currently a mess 
prime.factors = function(..., list.format="factors.{n}", optimus=FALSE)
	{
	x = prep.dots(...);
	nx = length(x);
	
	na.check = all(is.na(x));
	if(na.check) { return(x); }
	
	x = as.integer(x);  		# 4271484375 is too big
								## probably a good thing, primes of 65000
	na.check = all(is.na(x));
	if(na.check) { return(x); }
	
	xmax = x; if(nx > 1 ) { xmax = max(x, na.rm=TRUE, show.warning=TRUE); }
	if(xmax < 5) { xmax = 5; }
		
	# 7 * 7 = 49 ... if finding factors of 49, I only need to look to 7
	p = primes.get ( ceiling(sqrt(xmax)), FALSE, FALSE );  # squared rule of primes
	
	# make into a list 
	res = list();
	
	ni = 0;
	key1 = NULL;
	for(i in 1:nx)
		{
		x_ = x[i];
		
		if(is.na(x_)) 
			{ 
			ni = 1 + ni;
			r = c(1, x_); if(!optimus) { r = x_; } # could be NA or NaN -> lose NaN on as.integer()
			key = str_replace("{n}", paste0(x_,"-",ni) , list.format);
			if(i == 1) { key1 = key; }
			r = property.set("prime", r, FALSE);
			res[[ key ]] = r; 
			next; 
			}
			
		key = str_replace("{n}", x_, list.format);	
		if(i == 1) { key1 = key; }
				
		
		if(x_ < 1)
			{
			x_ = NA;
			ni = 1 + ni;
			r = c(1, x_); if(!optimus) { r = x_; } # could be NA or NaN -> lose NaN on as.integer()
			key = str_replace("{n}", paste0(x_,"-",ni) , list.format);
			if(i == 1) { key1 = key; }
			r = property.set("prime", r, FALSE);
			res[[ key ]] = r; 
			next;			
			}
		if(x_ == 1)
			{
			x_ = property.set("prime", x_, optimus);
			res[[ key ]] = x_; 
			next;
			}
		
		if(x_ == 2) 
			{
			x_ = property.set("prime", x_, TRUE);
			res[[ key ]] = x_; 
			next;			
			}
		## is it prime ... bug less than 4  
		p_ = p[p <= ceiling(sqrt(x_)) ];

		remainder = x_ %% p_;
		zeroes = which(remainder == 0);
		if(length(p_) > 0 && length(zeroes) == 0)
			{			
			if(optimus) { x_ = c(1, x_); }
			x_ = property.set("prime", x_, TRUE);
			res[[ key ]] = x_; 
			next;
			}

		factors = c();
		for(q in p_[zeroes])
			{
			# divide out as many times as possible the qiven prime
			while (x_ %% q == 0)
				{
				factors = c(factors, q);
				x_ = x_/q;
				}
			}
			# anything left? ... since we truncated using sqrt(x), maybe one prime ...
		if(x_ > 1) { factors = c(factors, x_); }
		if(optimus) { factors = c(1, factors); }
		factors = property.set("prime", factors, FALSE);
		res[[ key ]] = factors;
		}
	#list.return(res);  # keyed on inputs 
	if(nx == 1) { return( res[[ key1 ]] ); }
	return(res);
	}


