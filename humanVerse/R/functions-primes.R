
primes.scan = function() {}
# scan bit 
# reformat downloaded file, one prime per line
# MATH has 1,000,000; 				max 15485863
# because of gn I have 1,057,662;	max 16441303
# bits didn't save as expected ... 


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
primes.bit = function(n, first=TRUE, optimus=FALSE)
	{
# timer.start("bits"); x = prime.bits((1*1000)); length(x); max(x); timer.stop("bits");
	# you could build a bits-table, and search primes within 
	if(!is.library("bit")) { stop("requires library(bit); ... "); }
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
	
	save(bits.prime, file=paste0("primes-",n,".RData") );
	# use save/load ... to keep format?
	# , file.out=NULL
	# #' @param file.out {file.path + file.name to save bits as RDS}
# saveRDS(bits.prime, file=paste0("primes-",n,".rds") );
		
	p = which(bits.prime == TRUE); # indexes are primes (zero indexed???)
	# gn is upper bound, so ... truncate ... 
	if(first) { p = p[1:n];  	if(optimus) { p = c(1,p); } }
	if(!first) { p = p[p < n];	if(optimus) { p = c(1,p); } }  
	
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
primes.pracma = function(n, first=TRUE, optimus=FALSE)
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

	if(first) { p = p[1:n]; }
	if(!first) { p = p[p < n]; }
	if(optimus) { p = c(1,p); }
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
	# primes.default function would be nice, still would have to compare
	if( !exists("method", inherits = FALSE ) ) { method	= "base"; }
	p = primes.get(xmax, first=FALSE, method=method, optimus=FALSE);
	
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
#' @param optimus {TRUE: include 1 as prime}
#' @param method	{  	
#'					[cp]p_primes from HVcpp or Rccp::source 
#'					[pr]acma ... requires library (pracma)
#'					[bi]t(s) ... requires library (bit)
#'					[sf]smisc ... requires library (sfsmisc)
#'					[ba]se ... DEFAULT ... internal enhanced `primes.pracma` 
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
primes.get = function(n, first=TRUE, optimus=FALSE, method="base")
	{
	mm = functions.cleanKey(method, 2);
		
	if(mm == "cp" && exists("cpp_primes"))
		{
		res = cpp_primes(n, first);		
		if(optimus) { res = c(1, res); }  # will be off by one 
		return(as.integer(res));
		}
		
	if(mm == "pr" && is.library("pracma"))
		{
		gn = n;
		# upper bound 
		if(first) { gn = ceiling( n * log(n) + n * log(log(n)) ); }		
		res = pracma::primes(gn);
		if(first) { res = res[1:n]; }  # truncate to n 
		if(optimus) { res = c(1, res); }
		return(as.integer(res));
		}
	
	if(mm == "bi" && is.library("bit"))
		{
		res = primes.bit(n, first=first, optimus=optimus);
		return(res);
		}
	
	if(mm == "sf" && is.library("sfsmisc"))
		{
		gn = n;
		# upper bound 
		if(first) { gn = ceiling( n * log(n) + n * log(log(n)) ); }		
		res = sfsmisc::primes(gn);
		if(first) { res = res[1:n]; }  # truncate to n 
		if(optimus) { res = c(1, res); }
		return(as.integer(res));
		}
	
	
	res = primes.pracma(n, first=first, optimus=optimus);
	return(as.integer(res));
	}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' prime.factors
#'
#'
# multivariate is currently a mess 
prime.factors = function(x, ..., list.format="factors.{n}", optimus=FALSE)
	{
	x = dots.addTo(x, ...);
	nx = length(x);
	
	na.check = all(is.na(x));
	if(na.check) { return(x); }
	
	x = as.integer(x);  		# 4271484375 is too big
								## probably a good thing, primes of 65000
	na.check = all(is.na(x));
	if(na.check) { return(x); }
	
	xmax = x; if(nx > 1 ) { xmax = max(x, na.rm=TRUE); }
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


