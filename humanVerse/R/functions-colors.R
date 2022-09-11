
# color-utils contain the lower-level functions 
# this contains "names", "lists", setOpacity, findNearest, palettes
# we use HEX/RGBa as our two primary color choices 
# for color.MATHs we use RGBa ...
# could I create RGB as tuple to do core Rmaths ... mean ... convert back to HEX ... 

# col = v.color(...);
# v.color("red", color.setOpacity("red", 0.5), "#FFFF00", "#FFFF0080" ...)


color.nearestNameFromHEX = function(vecHEX, ..., nearest=1, name.search="base", method="manhatten, euclidean, cosine")
	{
	# maybe do all three methods ... 
	# if nearest = 1, a vector is returned 
	# otherwise a list keyed on vecHEX with length of each
	
	}

# list("wsu" = list("crimson" = "#981e32", "gray" = "#717171") );
colorname.createList = function(listname="wsu", 
									keys = c("crimson", "gray"),
									vals = c("#981e32", "#717171")
							)
	{
# if(is.list(keys)) ... vals = uname(keys) ... keys = names(keys) 
# also allow the entire list ... # list("wsu" = list("crimson" = "#981e32", "gray" = "#717171") );						
# if(is.list(listanem)) ... maybe multivariate as well ...
		
	}


	
	
# registered lists are in everywhere ... 
# color.list$wsu$crimson ... nice keying ...
# color.list$base$red ... store as global ... not memory SET 
# same with color.dictionary ... GLOBAL  
# also a color.dictionary that is WSU|crimson|hexstr|r,g,b 
colorname.search = function(searchWILDCARD, name.search="EVERYWHERE")
	{
	
	
	}
	
colorname.toHEX = function() {} # trivial 
colorname.fromHEX = function() {} 


colorname.nearestHEX = function() {} # not found in fromHEX 


color.nameFromHEX = function(vecHEX, ..., name.search="base", force.match=FALSE)
	{
	vecHEX = dots.addTo(vecHEX, ...);
	
	}

v.color = function(colvec, ..., names.search="base", alpha=TRUE)
	{
	colvec = dots.addTo(colvec, ...);
	return(colvec);
	
	clvec = str.replace(" ", "", colvec); # collapse names with spaces
											# when we search, also collapse keys with spaces in names 
	
	
											
	
	# we will allow any vector input ... names/hex ...
	# names.search = base:colors() ... could create a vector of search lists ... ORDER of vector is priority of search ...
	# # https://brand.wsu.edu/visual/colors/ 
	# e.g., list("wsu" = list("crimson" = "#981e32", "gray" = "#717171") );
	# if they are named, we convert to hex ...
	# if they are hex with extra ALPHA, we set opacity ?
	# if(!alpha), just TRUNCATE the end ... 
	
	}
	
	
color.gradient = function(colvec, ..., n=5, force.length=TRUE)
	{
	colvec = dots.addTo(colvec, ...);
	nc = length(colvec);
	if(force.length && nc > n) { n = nc; }	
	vecHEX = v.color(colvec); # red==>FF0000FF 
	grDevices::colorRampPalette(vecHEX, alpha=TRUE)(n);
	}
	
	
	
# https://brand.wsu.edu/visual/colors/
wsu.crimson = "#981e32";
wsu.gray    = "#717171";


hexcolor.table = function() {}

hexcolor.display = function() {} # HTML or graphics 

hexcolor.wheel = function() {}

hexcolor.plotWheel = function() {}

hexcolor.chromatics = function() {}

# vecHEX = c("#FAFBFC", "#F3D1A8","#A0A3A9"); vecHEX; hexcolor.round(vecHEX);	
hexcolor.round = function(vecHEX, ..., n=9, full=FALSE)
	{
	n = as.integer(n); if(n == 0) { n = 1; }
	# F3D1A8 ==> 
	# vecHEX = c("#F3D1A8","A0A3A9");
	vecHEX = dots.addTo(vecHEX, ...);
	vecHEX = v.color(vecHEX); # should be HEX, but now it is with ALPHA
	# convert to RGB 
	RGB = hex2rgb(vecHEX);
	
	# # this round "FA" to "FC", "FD" to "FF"
	mod.round = function(x, tol = sqrt(.Machine$double.eps))
		{
		# n is scoped 'lexicologically' ?
		xMod = x %% n;
		# floor(x/n) * n;
		# same as mod.websafe but n = 51 
		res = ifelse( {xMod <= floor(n/2)} , 
					{ floor(x/n) * n; } , 
					{ ceiling(x/n) * n; }
					);
		res[res > 255] = 255;
		res;
		}
	
	RGB[1,] = mod.round(RGB[1,]);
	RGB[2,] = mod.round(RGB[2,]);
	RGB[3,] = mod.round(RGB[3,]);
	
	as.character(rgb2hex(RGB));	
	}



	
# vecHEX = c("#F3D1A8","A0A3A9"); vecHEX; hexcolor.websafe(vecHEX);	
hexcolor.websafe = function(vecHEX, ...)
	{
	# F3D1A8 ==> 
	# vecHEX = c("#F3D1A8","A0A3A9");
	vecHEX = dots.addTo(vecHEX, ...);
	vecHEX = v.color(vecHEX); # should be HEX, but now it is with ALPHA
	# convert to RGB 
	RGB = hex2rgb(vecHEX);
	
	mod.websafe = function(x, tol = sqrt(.Machine$double.eps))
		{
		xMod = x %% 51;
		# if(xMod <= 25) { floor(x/51) * 51; } else { ceiling(x/51) * 51; }
		# multivariate "C" abomination 
		res = ifelse(   {xMod <= 25} , 
						{ floor(x/51) * 51; } , 
						{ ceiling(x/51) * 51; }
						);
		res[res > 255] = 255;
		res;
		}
		
	RGB[1,] = mod.websafe(RGB[1,]);
	RGB[2,] = mod.websafe(RGB[2,]);
	RGB[3,] = mod.websafe(RGB[3,]);
	
	as.character(rgb2hex(RGB));	
	}






# (wsu.gradient = color.colorsInGradient(10, c( wsu.crimson,wsu.gray)  ) );
# color.chromatics
color.colorsInGradient = function(n, colvec=c("red","royalblue"), alpha=FALSE)
  {
  # alpha doesn't seem to work as expected ... unless I pass in RGBa?
  grDevices::colorRampPalette(colvec, alpha=alpha)(n);
  }
 # grDevices::colorRampPalette(c("#FF0000FF","#00FF0033"), alpha=TRUE)(n);
 # just use v.color as inputs which forces everything to ALPHA ??




	
	
	
	
	# color is 0 to 255 for 3 values ... or collapse 
	# 0, 
	my.numbers = rand(2^1 - 1, (2^8)^3 - 1, n);
	# dec2hex(0, to.length=6)
	# 

	# }

# (rgb = unlist ( color.randomRGB(key="key.to.remember.seed") ) );

###### R::humanVerse #####
# (hex = color.randomHEX() ) ;

# (hex = color.randomHEX(3, key="another.memory") ) ;