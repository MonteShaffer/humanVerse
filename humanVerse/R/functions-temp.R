	
temp.constants = function(envir=parent.env(environment()))
	{
	ABS_ZERO_F = -459.67;
	ABS_ZERO_C = -273.15;
	ABS_ZERO_K = 0;
	ABS_ZERO_R = -459.67;
	
	assign("ABS_ZERO_F", ABS_ZERO_F, envir=envir);
	assign("ABS_ZERO_C", ABS_ZERO_C, envir=envir);
	assign("ABS_ZERO_K", ABS_ZERO_K, envir=envir);
	assign("ABS_ZERO_R", ABS_ZERO_R, envir=envir);	
	}

	
temp.isNA = function(degX, Xunits="celsius")
	{
	X = functions.cleanKey(Xunits, 1, case="upper");
	temp.constants();
		Xconstant.str = paste0("ABS_ZERO_",X);
		Xconstant = eval(parse(text = Xconstant.str));
#		dput(Xconstant);
	is.Z = (degX < Xconstant);
	if(any(is.Z)) { warning("one or more values below absolute zero"); }
	degX[is.Z] = NA;
	degX;	
	}


temp.convert = function(degX, ..., from="fahrenheit", to="celsius")
	{
	degX = dots.addTo(degX, ...);
	temp.constants();
	# convert everthing to "celsius" on first pass
	F = functions.cleanKey(from, 1, case="upper");
	T = functions.cleanKey(to, 1, case="upper");
# dput(degX); dput(F); dput(T); dput(ABS_ZERO_R);
cat("\n START degX ... ", degX, "\n");
	degC = switch(F,					  			
					  "F" 	= 5/9 * (degX - 32),
					  "C"	= degX,	
					  "K"  	= degX + ABS_ZERO_C,				
					  "R"  	= (degX + ABS_ZERO_R - 32) * (5/9),				
				degX											# DEFAULT
				);
cat("\n in degC ... ", degC, "\n");			
	# convert everything from "celsius" on second pass 	
	degN = switch(T,					  			
					  "F" 	= 9/5 * degC + 32,
					  "C"	= degC,	
					  "K"  	= degC - ABS_ZERO_C,				
					  "R"  	= (9/5 * degC) - ABS_ZERO_R + 32,				
				degC											# DEFAULT
				);
cat("\n in degN ... ", degN, "\n");					
	temp.isNA(degN, to);
	}
	


# 12 unique, meaningful conversions 


# F, C, K, R ... LOL
# x = c("F", "C", "K", "R");
# m = e1071::permutations(4);
# m2 =  matrix(x[m], ncol=2);
# m3 = m2[!duplicated(m2), ];
# m4 = m3[ c( which(m3[,1] == x[1]), which(m3[,1] == x[2]), which(m3[,1] == x[3]), which(m3[,1] == x[4]) ),  ];
# m4 = m3[ c(4,1,8,  2,7,3,  9,6,5, 11,10,12), ];

# for(i in 1:12) 
	# {
	# mm = m4[i, ];
	temp.c2f = 	function(degC) { temp.convert(degC, "C", "F"); }
	# row = paste0("temp.", tolower(mm[1]), "2", tolower(mm[2]), " = function(deg", toupper(mm[1]), ", ...) { temp.convert(deg", toupper(mm[1]), ", ...,  from=\"", toupper(mm[1]), "\", to=\"", toupper(mm[2]), "\"); } ");
	# print.noquote(row);
	# cat(row, "\n\n");
	# }


temp.f2c = function(degF, ...) { temp.convert(degF, ...,  from="F", to="C"); }  

temp.f2k = function(degF, ...) { temp.convert(degF, ...,  from="F", to="K"); }  

temp.f2r = function(degF, ...) { temp.convert(degF, ...,  from="F", to="R"); }  

temp.c2k = function(degC, ...) { temp.convert(degC, ...,  from="C", to="K"); }  

temp.c2r = function(degC, ...) { temp.convert(degC, ...,  from="C", to="R"); }  

temp.c2f = function(degC, ...) { temp.convert(degC, ...,  from="C", to="F"); }  

temp.k2r = function(degK, ...) { temp.convert(degK, ...,  from="K", to="R"); }  

temp.k2f = function(degK, ...) { temp.convert(degK, ...,  from="K", to="F"); }  

temp.k2c = function(degK, ...) { temp.convert(degK, ...,  from="K", to="C"); }  

temp.r2f = function(degR, ...) { temp.convert(degR, ...,  from="R", to="F"); }  

temp.r2c = function(degR, ...) { temp.convert(degR, ...,  from="R", to="C"); }  

temp.r2k = function(degR, ...) { temp.convert(degR, ...,  from="R", to="K"); }  



















# TI-83
# Eng (engineering) notation mode is similar to scientific notation. However, the number can have one, two, or three digits before the decimal; and the power-of-10 exponent is a multiple of three, as in 12.34567E3.

# randBin(# of coin flips, prob of heads, # of simulations)
