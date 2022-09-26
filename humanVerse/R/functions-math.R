



# algorithms to compute next digits using a technique
# give STUDENTS a peek into ALGO development (like sort)
compute.pi = function() {}
compute.sqrt2 = function() {}



strsqrt = function() {} # old school method for by-hand sqrt 

strPoorManDivide = function() {} # old school Russian 
strdivide = function() {} # old school by-hand 
strsum = function(nums)
	{
	# like how we did it in grade school, line up digits and add/carry
	
	}
	
	
.hassum.inside = function(numstr, from=16, to=10)
	{
	#  BASE to DECIMAL 
	base.chars = c(as.character(0:9), LETTERS[1:22]);
	
	
	numstr = toupper(numstr);
	sn = str.explode("",numstr);	
	idx = rev( set.match(sn, base.chars) - 1);
	n = length(idx);
	sum = character(n);
	for(i in n:1)
		{
		sum[i] = strint( idx[i] * from^(i-1));		
		}
	
	
	# option.set("scipen", 999);
		# ss = as.character(5*10^222)
	# option.set("scipen", 0);
	
	slen = str.len(sum);
	vsum = check.list(str.explode("", str.pad(sum, max(slen), side = "LEFT")) );
	ms = max(slen);
	
	rsum = character(ms);
	idx = 0;
	carry = 0;
	for(j in ms:1)
		{
		idx %++%.
		row = list.getElements(vsum, j);
		rs = sum(as.numeric(row), carry );
		if(rs > 9 ) 
			{ 
			carry = as.integer(rs/10); 
			rs = rs - 10*carry; 
			} else { carry = 0; }
		rsum[j] = as.character(rs);		
		}
	if(carry > 0) { rsum = c(as.character(carry), rsum); }
	rsum = paste0( rsum,  collapse="");
	
	# we have a decimal number (any length) ??? 
	if(TO == "dec") { return(rsum); }
	
	
	
	
	
	
	
	
	n = length(xv);
	p = base^((n-1):0);
	
	sum( idx * p );   # this can get very big ... maybe GO BACK and read in REVERSE, slow but I deal with one digits at a time ...
					  # I may have overflow (remainder) into the next 
					  # so maybe I just do the calculation at each digit,
					  # store as a character vec ... str.pad 
					  # and do an old-school SUM with carryover ...
	
	#  DECIMAL to BASE 
	r = ""; if(num == 0) { return("0"); }
	while(num > 0) 
		{
		m = num %% base;
		r = paste0(base.chars[m + 1], r);
		num =  as.integer(num/base);
		}
	r;
	
	}


# I have a large number, convert to long integer string 
strint = function(num = 5*10^222)
	{
	if(is.infinite(num)) { stop("we have reached Infinity ... and beyond!"); }
	# univariate 
	# truncate floating-point noise ...
	x.sci = format(num, scientific=TRUE, digits=22);
	x.exp = str.split("e+", x.sci);
		wh = as.numeric( x.exp[1] );
		ex = as.integer( x.exp[2] );
	nu = str.split(".", wh);
	nu[2] = v.TO(nu[2], NA, "");

	# noise after 16
	nlen = str.len(nu);
	r = paste0(nu[1], substring(nu[2], 1, 16));
	ex = ex - nlen[2];
	r = paste0(r, str.rep("0",ex) );
	r;
	}

## online mathematica has "... more digits ... " option ...
## would be useful ... and have a map to the ois collection / lookup 
## download / parse / store a database 
## do it monthly ?
## 





# is this not just simple division 
num.den = function(num, den, expand=TRUE) 
			{ 
			if(!expand) { return (num/den); }
			nn = length(num);
			nd = length(den);			
			# normal recycling
			if(nn == 1 || nd == 1) { return (num/den); }
			
			# 0:10 %frac% 1:100
			# this is expand == TRUE
			# I want 0...10 / 1:100 ... all of them ...
			# 0:10/1 THEN 0:10/2 THEN 0:10/3 ...
			res = NULL;
			for(i in 1:nd)					# could have done nn 
				{
				res = c(res, num / den[i]); # could have done num[i]
				}
			res;
			}


