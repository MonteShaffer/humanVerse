
check.base = function(base = 10)
	{
	base = as.integer(base);
	if(base > 36 || base < 2) { stop("'base' must be between 2 and 36."); }
	base; 
	}
	
cleanup.base = function(xstr)
	{
	xstr = as.character(xstr);
	# left trim any non-elements ... maybe REGEX replace 0-9A-V
	# HEX 
	xstr = str.trim(str.replace(c("#","0x","0X"), "", xstr) );
	xstr;
	}


fromBase = function(xstr, ..., base=10)
	{
	xstr = dots.addTo(xstr, ...);
	xstr = cleanup.base(xstr);
dput(xstr);
	b = check.base(base);
	base.chars = c(as.character(0:9), LETTERS[1:22]);
	
	N = length(xstr);
	res = integer(N);
	for(i in 1:N)
		{
		xstri = toupper(xstr[i]);
		n = str.len(xstr);
		re = 0; power = 0;
		for(j in n:1)
			{
			xj = charAt(xstri,j);
			xn = which(base.chars == xj) - 1;			
			add = xn * b^power;
			re = re + add;
			power = 1 + power;
			}
		res[i] = re;
		}
	res;	
	}
	
base.from = fromBase;	
	

toBase = function(x, ..., base=10, to.length=NULL)
	{
	x = dots.addTo(x, ...)
	b = check.base(base);
	base.chars = c(as.character(0:9), LETTERS[1:22]);
	N = length(x);
	res = character(N);
	for(i in 1:N)
		{
		xi 	= x[i];
		n	= ceiling(log(xi, b));
		vec = NULL;
		val = xi
      
		while(n >= 0)
			{
			rem = val %/% b^n;
			val = val - rem * b^n;
			vec = c(vec, rem);
			n   = n - 1;
			}
		# truncate leading zeros ...
		while(vec[1] == 0 & length(vec) > 1)
			{
			vec = vec[-1];
			}
		# zero offset
		res[i] = paste0(base.chars[vec+1], collapse="");		
		}
	# str.pad("LEFT");
	if(!is.null(to.length)) { res = str.pad(res, to.length, "0", "LEFT"); }
	res;
	}

base.to = toBase;

convertBase = function(x, ..., from="binary", to="octal", to.length=NULL)
	{
	x = dots.addTo(x, ...);
	# first to decimal (integer) ... maybe lists ...
	F = prep.arg(from, 1, case="upper");
	T = prep.arg(to, 1, case="upper");
	
	xINT = switch(F,					  			
					  "B" 	= fromBase(x, base=2), 	# BINARY
					  "O"  	= fromBase(x, base=8),	# OCT
					  "H"	= fromBase(x, base=16),	# HEX					  
				fromBase(as.character(x), base=10)	# DEFAULT # DECIMAL (INT, BASE 10)
				);

	xOUT = switch(T,					  			
					  "B" 	= toBase(x, base=2, to.length=to.length), 	# BINARY
					  "O"  	= toBase(x, base=8, to.length=to.length),		# OCT
					  "H"	= toBase(x, base=16, to.length=to.length),	# HEX					  
				as.integer(toBase(x, base=10, to.length=to.length))		# DEFAULT DECIMAL (INT, BASE 10)
				);
	xOUT;
	}	

base.convert = convertBase;
	




choices = c("bin", "dec", "hex", "oct");
n = length(choices);
for(i in 1:n)
	{
	for(j in 1:n)
		{
		f = tolower(choices[i]); F = toupper(f);
		s = tolower(choices[j]); S = toupper(s);
		if(f != s)
			{
			row = '{f}2{s} = function(x, ..., to.length=to.length) { base.convert(x, ..., from="{F}", to="{S}", to.length=to.length); }';
			
			row = str.replace(c("{f}", "{s}", "{F}", "{S}"), c(f,s,F,S), row);
			cat(row, "\n\n");	
			}
		
		}
	}







bin2dec = function(x, ..., to.length=to.length) { base.convert(x, ..., from="BIN", to="DEC", to.length=to.length); } 

bin2hex = function(x, ..., to.length=to.length) { base.convert(x, ..., from="BIN", to="HEX", to.length=to.length); } 

bin2oct = function(x, ..., to.length=to.length) { base.convert(x, ..., from="BIN", to="OCT", to.length=to.length); } 

dec2bin = function(x, ..., to.length=to.length) { base.convert(x, ..., from="DEC", to="BIN", to.length=to.length); } 

dec2hex = function(x, ..., to.length=to.length) { base.convert(x, ..., from="DEC", to="HEX", to.length=to.length); } 

dec2oct = function(x, ..., to.length=to.length) { base.convert(x, ..., from="DEC", to="OCT", to.length=to.length); } 

hex2bin = function(x, ..., to.length=to.length) { base.convert(x, ..., from="HEX", to="BIN", to.length=to.length); } 

hex2dec = function(x, ..., to.length=to.length) { base.convert(x, ..., from="HEX", to="DEC", to.length=to.length); } 

hex2oct = function(x, ..., to.length=to.length) { base.convert(x, ..., from="HEX", to="OCT", to.length=to.length); } 

oct2bin = function(x, ..., to.length=to.length) { base.convert(x, ..., from="OCT", to="BIN", to.length=to.length); } 

oct2dec = function(x, ..., to.length=to.length) { base.convert(x, ..., from="OCT", to="DEC", to.length=to.length); } 

oct2hex = function(x, ..., to.length=to.length) { base.convert(x, ..., from="OCT", to="HEX", to.length=to.length); } 





hex2dec
dec2oct
oct2dec
dec2bin
bin2dec

bin2hex ... do all COMBOS ...
	
	

# maybe go back and README on colors and do color.convert ... to/fro on RGB

# library BMS? 

	
# bin2hex, hex2bin, and so on ...


 
