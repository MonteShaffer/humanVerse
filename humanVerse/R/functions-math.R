


 

gcd.lcm = function(x,y)
	{
	a=x;
	b=y;
	while (b != 0)
        {
		t = b;
		b = a %% b;
		a = t;
        }
	list("gcd"=a, "lcm"=(x*y)/a);
	}



# 4 nCr 2 ... choose vs lchoose ?
# ?utils::combn  ?choose 
# library(combinat); library(gtools);
# choose(4, 2) ... with replacement 
# make %nCr% and %nPr% functions ... 
# https://davetang.org/muse/2013/09/09/combinations-and-permutations-in-r/
# https://www.calculatorsoup.com/calculators/discretemathematics/permutationsreplacement.php
nCr = function(n, r, replace=FALSE) 
	{ 
	# same function (FALSE, with n+r-1)
	if(replace) { return( nCr( (n+r-1), r, replace=FALSE ) ); } 
	factorial(n) / ( factorial(r) * factorial(n-r) ); 
	}
"%ncr%" = "%nCr%" = nCr;

"%!%" = function(n, r=NULL) { factorial(n); }

nPr = function(n, r, replace=FALSE) 
	{ 
	if(replace) { return( n^r ); }
	factorial(n) / factorial(n-r); 
	}
"%npr%" = "%nPr%" = nPr;










# takes num/den 
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

"%frac%" = num.den;


num.toFrac = function() {}
num.toFrac = function(x, ..., 
								return = "last",
								max.depth = 12,  
								tol = sqrt(.Machine$double.eps) , 
								part="Re"
						)
	{
	r = functions.cleanKey(return, 1);
	x = dots.addTo(x, ...); 
	x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
	
	CF = num.toCFrac(x, max.depth=max.depth, tol=tol, part=part);
	# in this function, I have x ... other times I may not 
	# x = property.get("x", CF);
	CF = check.list(CF); # make certain it is a list 
	n = length(CF);
	e = vector("list", n);
	cr = character(n);  # character return of last 
	ce = numeric(n);	# error as attributes of above 
	nr = character(n); 	# character return of MAX based on return = 1000 (<= 100) in denominator)
	ne = numeric(n);	# error as attributes of above 
	for(i in 1:n)
		{
		cf = CF[[i]];
		x_ = x[i];
		
		ncf = length(cf);
		idx = -2:(ncf-1);
		ilen = length(idx);
		a = c(NA, NA, cf);
		num = den = integer(ilen);
		error.percent = numeric(ilen);
		ndchar = character(ilen);
		num[1] = 0; num[2] = 1;
		den[1] = 1; den[2] = 0;
		
		df = as.data.frame( cbind(idx, a, num, den) );
			df$ndchar = ndchar;  # do after above, or everything becomes CHAR
			df$error.percent = error.percent;
		cidx = 3;  # this is where we start
		while(cidx <= ilen)
			{
			if(is.na(df$a[cidx])) { break; }
			cnum = df$a[cidx] * df$num[(cidx-1)] + df$num[(cidx-2)];
			cden = df$a[cidx] * df$den[(cidx-1)] + df$den[(cidx-2)];
			
			cchar = paste0(cnum,"/",cden);
			# https://stackoverflow.com/a/64146458/184614
			cerr = signif( (100* (x_ - (cnum/cden) ) / x_ ), 5);
						
			df$num[cidx] = cnum;
			df$den[cidx] = cden;
			df$ndchar[cidx] = cchar;
			df$error.percent[cidx] = cerr;
			# sprintf("%0.5f%%", cerr * 100);
			
			cr[i] = cchar;
			ce[i] = cerr;
			
			if(is.numeric(return))
				{
				if(cden <= return)
					{
					nr[i] = cchar;
					ne[i] = cerr;
					}
				}
			
			cidx = 1 + cidx;
			}
		
		e[[i]] = df;
		}
	
	# maybe return a value that has maximum of 1000 in denominator 	
	if(is.numeric(return))
		{
		nr = property.set("x", nr, x);
		nr = property.set("error.percent", nr, ne);		
		return(nr);
		}
		
	if(r == "l" || r == "1") # confusion with "ell" vs "one"
		{
		cr = property.set("x", cr, x);
		cr = property.set("error.percent", cr, ce);		
		return(cr);
		}
	
		
	# this returns anything but [l]ast ... everything 
	e = list.return(e);  # does this preserve internal property.set ? NOPE
	e = property.set("CF", e, CF);
	e;
	}


# https://en.wikipedia.org/wiki/Continued_fraction#Continued_fraction_expansion_of_%CF%80_and_its_convergents
# https://oeis.org/A001203
# pi =  3, 7, 15, 1, 292, 1, 1, 1, 2, 1, 3, 1, 14, 2, 1, 1, 2, 2, 2, 2, 1, 84, 2, 1, 1, 15, 3, 13
# I am getting 
# 		3   7  15   1 292   1   1   1   2   1   3   1  14   3   3  23  NA
num.toCFrac = function() {}
num.toCFrac = function(x, ..., 
								max.depth = 12,  
								tol = sqrt(.Machine$double.eps) , 
								part="Re"
						)
	{
	x = dots.addTo(x, ...); 
	x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
	
	n = length(x);
	e = vector("list", n);
	for(i in 1:n)
		{
		x_ = x[i];
		
		j = 1;		
		w = as.integer(x_); # whole part 
		r = x_ - w; 		# remainder 	
		
		e[[i]] = c(w);
		while(j <= max.depth)  # max.depth + 1 allows for abcissa (whole number)
			{
			# internally if tolerance of eps (r) is reached, we break ...
			# for non-convergence, r just jumps around 
			if(abs(r) < tol) { break; }
			w = as.integer( 1/r );
			r = (1/r) - w;			# for next iteration 
			e[[i]] = c(e[[i]], w);
			 			
			j = 1 + j;
			}
		#	NA analagous to ... (continues), marker that we stopped manually.
		if(j > max.depth) { e[[i]] = c(e[[i]], NA); } 	
		}
	e = list.return(e);
	e = property.set("x", e, x);
	e;
	}

	
num.toContinuousFraction = num.toCFrac;

# phi = (1 + sqrt(5)) / 2
# x = c(1/7, 1/123, pi, phi)
 

































latex.fromCFrac = function() {}

num.toEFrac = function() {} 
# Egyptian Fractions 
# http://web.ff.cuni.cz/ustavy/egyptologie/pdf/Gardiner_signlist.pdf

egy.lists = function()
	{
	# https://en.wikipedia.org/wiki/Egyptian_Mathematical_Leather_Roll
	EMLR = list(
	"1:1_1/8" = c(10,40),		"2:1_1/15" = c(30,45,90),	"3:1_1/8" = c(10,40),			"4:1_1/12" = c(18,36),
	"1:2_1/4" = c(5,20),		"2:2_1/16" = c(24,48),		"3:2_1/4" = c(5,20),			"4:2_1/14" = c(21,42),
	"1:3_1/3" = c(4,12),		"2:3_1/12" = c(18,36),		"3:3_1/3" = c(4,12),			"4:3_1/30" = c(45,90),
	"1:4_1/5" = c(10,10),		"2:4_1/14" = c(21,42),		"3:4_1/5" = c(10,10),			"4:4_1/20" = c(30,60),
	"1:5_1/3" = c(6,6),			"2:5_1/30" = c(45,90),		"3:5_1/3" = c(6,6),				"4:5_1/10" = c(15,30),
	"1:6_1/2" = c(6,6,6),		"2:6_1/20" = c(30,60),		"3:6_1/2" = c(6,6,6),			"4:6_1/32" = c(48,96),
	"1:7_2/3" = c(3,3),			"2:7_1/10" = c(15,30),		"3:7_2/3" = c(3,3),				"4:7_1/64" = c(96,192),
	"1:8_1/8" = c(25,15,75,200),"2:8_1/32" = c(48,96),		"3:8_1/8" = c(25,15,75,200),
	"1:9_1/16" = 10*c(5,3,15,40),"2:9_1/64" = c(96,192),	"3:9_1/16" = c(50,30,150,400),
	"1:10_1/15" = c(25,50,150),								"3:10_1/15" = c(25,50,150),
	"1:11_1/6" = c(9,18),									"3:11_1/6" = c(9,18),
	"1:12_1/4" = c(7,14,28),								"3:12_1/4" = c(7,14,28),
	"1:13_1/8" = c(12,24),									"3:13_1/8" = c(12,24),
	"1:14_1/7" = c(14,21,42),								"3:14_1/7" = c(14,21,42),
	"1:15_1/9" = c(18,27,54),								"3:15_1/9" = c(18,27,54),
	"1:16_1/11" = c(22,33,66),								"3:16_1/11" = c(22,33,66),
	"1:17_1/13" = c(28,49,196),								"3:17_1/13" = c(28,49,196),
															"3:18_1/15" = c(30,45,90),
															"3:19_1/16" = c(24,28)
	);
	
	# https://en.wikipedia.org/wiki/Rhind_Mathematical_Papyrus_2/n_table
	# https://en.wikipedia.org/wiki/Red_auxiliary_number
	# https://en.wikipedia.org/wiki/Akhmim_wooden_tablets
	# https://en.wikipedia.org/wiki/Reisner_Papyrus
	# https://en.wikipedia.org/wiki/Egyptian_fraction
	# For instance, the primary pseudoperfect number 1806 is the product of the prime numbers 2, 3, 7, and 43, and gives rise to the Egyptian fraction 1 =  1 / 2  +  1 / 3  +  1 / 7  +  1 / 43  +  1 / 1806 .
	# nfr ... ZERO ... http://www.math.buffalo.edu/mad/Ancient-Africa/mad_ancient_egypt_zero.html
	# https://mathancientegypt-blog.tumblr.com/post/116476596099/zero-continued-again
	# Some people have made certain discoveries about the Great Pyramid, using maths: When using the Egyptian cubit the perimeter is 365.24 - the amount of days in the yearWhen doubling the perimeter, the answer is equal to one minute of one degree at the equatorThe apex to base slant is equal to 600th of a degree of latitudeThe height x 10 to the power of 9 gives approximately the distance from the earth to the sunThe perimeter divided by 2 x the height of the pyramid is equal to pi - 3.1416The weight of the pyramid x 10 to the power of 15 is equal to the approximate weight of the earthWhen the cross diagonals of the base are added together, the answer is equal to the amount of time (in years) that it takes for the earth’s polar axis to go back to its original starting point - 25,286.6 yearsThe measurements of the King’s Chamber gives 2-5-3 and 3-4-5 which are basic Pythagorean triangles
	# http://www.touregypt.net/featurestories/numbers.htm#ixzz3XOfqEpUy ... The Egyptians, though, had no concept for zero. 
	# https://mathigon.org/task/egyptian-fractions [memory ... changing one visual] *** very nice lesson, realistic, carving in stone ... 
	# 1308b,c,d are special fractions
	# 13421 is maybe the 10?
	# 133e4... numbers 1, 2, ... ?
	# https://discoveringegypt.com/egyptian-hieroglyphic-writing/egyptian-mathematics-numbers-hieroglyphs/
	# 7 ones for 7 or special symbol?
	# https://egyptianhieroglyphs.co.uk/
	# https://github.com/morrisfranken/glyphreader # Gardener labels
	# http://iamai.nl/downloads/GlyphDataset.zip
	# https://opennmt.net/ # can we diagnram sentences?
	# 
	
	
	
	RMP = list( 
	"1:1_2/3" = c(2,6),		"2:1_2/5" = c(3,15),	"3:1_2/7" = c(4,28),
	"1:2_2/9" = c(6,18),	"2:2_2/11" = c(6,66),	"3:2_2/13" = c(8,52,104),
	"1:3_2/15" = c(10,20),	"2:3_2/17" = c(12,51,68),"3:3_2/19" = c(12,76,114),
	"1:4_2/21" = c(14,42),	"2:4_2/23" = c(12,276),	"3:4_2/25" = c(15,75),
	"1:5_2/27" = c(18,54),	"2:5_2/29" = c(24,58,174,232),	"3:5_2/31" = c(20,124,155),
	"1:6_2/33" = c(22,66),	"2:6_2/35" = c(30,42),	"3:6_2/37" = c(24,111,296),
	"1:7_2/39" = c(26,78),	"2:7_2/41" = c(24,246,328),	"3:7_2/43" = c(42,86,129,301),
	"1:8_2/45" = c(30,90),	"2:8_2/47" = c(30,141,470),"3:8_2/49" = c(28,196),
	"1:9_2/51" = c(34,102),	"2:9_2/53" = c(30,318,795),	"3:9_2/55" = c(30,330),
	"1:10_2/57" = c(38,114),"2:10_2/59" = c(36,236,531),"3:10_2/61" = c(40,244,488,610),
	"1:11_2/63" = c(42,126),"2:11_2/65" = c(39,195),	"3:11_2/67" = c(40,335,536),
	"1:12_2/69" = c(46,138),"2:12_2/71" = c(40,568,710),"3:12_2/73" = c(60,219,292,315),
	"1:13_2/75" = c(50,150),"2:13_2/77" = c(44,308),"3:13_2/79" = c(60,237,316,790),
	"1:14_2/81" = c(54,162),"2:14_2/83" = c(60,332,415,498),"3:14_2/85" = c(51,255),
	"1:15_2/87" = c(58,174),"2:15_2/89" = c(60,356,534,890),"3:15_2/91" = c(70,130),
	"1:16_2/93" = c(62,186),"2:16_2/95" = c(60,380,570),	"3:16_2/97" = c(56,679,776),
	"1:17_2/99" = c(66,198),	"2:17_2/101" = c(101,202,303,606)	
	);
	
	
	
	}


# my.constants
# PI = 3.1415926535897932385626433
# PI == pi ... TRUE ??!>!?


toFrac = function(x, ...,	max.depth=16, tol = sqrt(.Machine$double.eps) , part="Re", return="n/d")	# could return Euclidean nested
	{	
	x = dots.addTo(x, ...); 
	x = if(part == "Im") { x = Im(x); } else { x = Re(x); }
	n = length(x);
	e = vector("list", n);
	for(i in 1:n)
		{
		x_ = x[i];
		
		j = 1;		
		w = as.integer(x_); # whole part 
		r = x_ - w; 		# remainder 
		e[[i]] = c(w);
		while(j < max.depth)
			{
			# internally if tolerance of eps (r) is reached, we break ...
			if(r < tol) { break; }
			w = as.integer( 1/r );
			r = (1/r) - w;
			e[[i]] = c(e[[i]], w);			
			j = 1 + j;
			}
		#	NA analagous to ... (continues), marker that we stopped manually.
		# if(j == max.depth) { e[[i]] = c(e[[i]], NA); } 	
		
			
		# https://math.stackexchange.com/questions/3084970/how-to-convert-continued-fractions-into-normal-fractions
		
		
		hm2 = 0; 
		hm1 = 1;
		km2 = 1;
		km1 = 0;
		
		# hn=anhn−1+hn−2
		# kn=ankn−1+kn−2
		
		num0 = den0 = 1;
		a = e[[i]]; alen = length(a);
		num = numeric(alen);	num[1] = a[2] * 1 + 0; num[2] = a[3] * num0 + 1;
		den = numeric(alen);	den[1] = a[2] * 0 + 1; den[2] = a[3] * den0 + 0;
		
		e[[i]] = property.set("info", e[[i]], list("depth" = j, "remainder" = r));
		}
	names(e) = x;
	e;
	}

# https://en.wikipedia.org/wiki/Continued_fraction#Some_useful_theorems
# error is ORIG - FRAC / ORIG 
# https://math.stackexchange.com/questions/3084970/how-to-convert-continued-fractions-into-normal-fractions















