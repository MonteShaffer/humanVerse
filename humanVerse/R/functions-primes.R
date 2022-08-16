
##################################################
#'
#' primes.bit
#'
#'
primes.bit = function(n, first=TRUE, optimus=FALSE)
	{
	# you could build a bits-table, and search primes within 
	if(!is.library("bit")) { stop("requires library(bit); ... "); }
	gn = n; if(n > 10^6) { stop("such a large [n] may tax the system"); }
	if(first) { gn = ceiling( n * log(n) + n * log(log(n)) ); }
	gn.sqrt = ceiling( sqrt(gn) );
	
	bits.prime = bit::bitwhich(gn, TRUE);  # set all to TRUE, including 1
	
	## IS.EVEN ##
	i = 4;
	while(i < gn)
		{
		bits.prime[i] = FALSE;
		i = 2 + i; 
		}

	## SIEVE  p is (6k +/- 1) ## 	
	i = 3;
	while(i < gn.sqrt)
		{
		if(bits.prime[i] == TRUE) # is there a faster way to check == 1
			{
			k = i * 2;
			j = i * i;
			while(j < gn)
				{
				bits.prime[j] = FALSE;
				j = k + j;
				}
			}
		i = 2+i;
		}
		
	p = which(bits.prime == TRUE); # indexes are primes (zero indexed???)
	if(first) { p = p[2:(n+1)];  if(optimus) { p = c(1,p); } }
	if(!first) { p = p[p < n]; if(!optimus) { p = p[-c(1)]; } }  
	return(p);
	}
	
##################################################
#'
#' primes.pracma
#'
#'	
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


##################################################
#'
#' primes.inRange
#'
#'	
primes.inRange = function(xmin, xmax, ...)
	{
	# primes.default function would be nice, still would have to compare
	if( !exists("method", inherits = FALSE ) ) { method	= "base"; }
	p = primes.get(xmax, first=FALSE, method=method, optimus=FALSE);
	
	p = p[(p >= xmin)];
	p = p[(p <= xmax)];
	p;
	}
	
##################################################
#'
#' primes.get
#'
#'	
primes.get = function(n, first=TRUE, method="base", optimus=FALSE)
	{
	m = functions.cleanKey(method, 1);
		
	if(m == "c" && exists("cpp_primes"))
		{
		res = cpp_primes(n, first);		
		if(optimus) { res = c(1, res); }  # will be off by one 
		return(res);
		}
		
	if(m == "p" && is.library("pracma"))
		{
		gn = n;
		# upper bound 
		if(first) { gn = ceiling( n * log(n) + n * log(log(n)) ); }		
		res = pracma::primes(gn);
		if(first) { res = res[1:n]; }  # truncate to n 
		if(optimus) { res = c(1, res); }
		return(res);
		}
	
	if(m == "b" && is.library("bit"))
		{
		res = primes.bit(n, first=first, optimus=optimus);
		return(res);
		}
	
	if(m == "s" && is.library("sfsmisc"))
		{
		gn = n;
		# upper bound 
		if(first) { gn = ceiling( n * log(n) + n * log(log(n)) ); }		
		res = sfsmisc::primes(gn);
		if(first) { res = res[1:n]; }  # truncate to n 
		if(optimus) { res = c(1, res); }
		return(res);
		}
	
	
	res = primes.pracma(n, first=first, optimus=optimus);
	return(res);
	}

##################################################
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


