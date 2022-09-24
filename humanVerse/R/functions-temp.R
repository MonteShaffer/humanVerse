

temp.init = function()
	{
	if(is.undefined(ABS_ZERO_F)) { constants.default(); }
	# can we get this check to happen here, so we don't have to constantly check?
	
	
	}
	
temp.isNA = function(degX, Xunits="celsius")
	{
	X = prep.arg(Xunits, n=1, case="upper");
	temp.init();
	# temp.constants();
		Xconstant.str = paste0("ABS_ZERO_",X);
		Xconstant = eval(parse(text = Xconstant.str));
#		dput(Xconstant);
	is.Z = (degX < Xconstant);
	if(any(is.Z)) { warning("one or more values below absolute zero"); }
	degX[is.Z] = NA;
	degX;	
	}


temp.convert = function(..., from="fahrenheit", to="celsius")
	{
	degX = prep.dots(...);
	# temp.constants();
	temp.init();
	# convert everthing to "celsius" on first pass
	FROM = prep.arg(from, n=1, case="upper");  # good thing F doesn't mean FALSE anymore!?!
	TO = prep.arg(to, n=1, case="upper");
# dput(degX); dput(F); dput(T); dput(ABS_ZERO_R);
cat("\n START degX ... ", degX, "\n");
	degC = switch(FROM,					  			
					  "F" 	= 5/9 * (degX - 32),
					  "C"	= degX,	
					  "K"  	= degX + ABS_ZERO_C,				
					  "R"  	= (degX + ABS_ZERO_R - 32) * (5/9),				
				degX											# DEFAULT
				);
cat("\n in degC ... ", "\n");			
	# convert everything from "celsius" on second pass 	
	degN = switch(TO,					  			
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
	temp.c2f = 	function(degC) { temp.convert("C", "F"); }
	# row = paste0("temp.", tolower(mm[1]), "2", tolower(mm[2]), " = function(deg", toupper(mm[1]), ", ...) { temp.convert(deg", toupper(mm[1]), ", ...,  from=\"", toupper(mm[1]), "\", to=\"", toupper(mm[2]), "\"); } ");
	# print.noquote(row);
	# cat(row, "\n\n");
	# }


temp.f2c = function(...) { temp.convert(...,  from="F", to="C"); }   

temp.f2k = function(...) { temp.convert(...,  from="F", to="K"); }  

temp.f2r = function(...) { temp.convert(...,  from="F", to="R"); }  

temp.c2k = function(...) { temp.convert(...,  from="C", to="K"); }  

temp.c2r = function(...) { temp.convert(...,  from="C", to="R"); }  

temp.c2f = function(...) { temp.convert(...,  from="C", to="F"); }  

temp.k2r = function(...) { temp.convert(...,  from="K", to="R"); }  

temp.k2f = function(...) { temp.convert(...,  from="K", to="F"); }  

temp.k2c = function(...) { temp.convert(...,  from="K", to="C"); }  

temp.r2f = function(...) { temp.convert(...,  from="R", to="F"); }  

temp.r2c = function(...) { temp.convert(...,  from="R", to="C"); }  

temp.r2k = function(...) { temp.convert(...,  from="R", to="K"); }  

















