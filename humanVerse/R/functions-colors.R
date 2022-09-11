
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

#  "wsu:crimson"
#  "eku:colors" ... "ua:colors" (arizona) 
#  "byu:oldcolors, newcolors"
#  "utah:oldcolors, newcolors"
#  "steelers:oldcolors, newcolors"
#  "cincireds: oldcolors, newcolors"
#  "cfalls:oldcolors", "newcolors"
#  "arg:entina flag colors" 
v.color = function(colvec, ..., names.search="base", alpha=TRUE)
	{  
	colvec = dots.addTo(colvec, ...);
	hexVEC = colvec;
	res = color.hex(hexVEC, alpha=alpha);
	return(res);
	
	clvec = str.replace(" ", "", colvec); # collapse names with spaces
											# when we search, also collapse keys with spaces in names 
	
	
	# once we have hex values ... this will format and append ALPHA at end 
	res = color.hex(hexVEC);
	
	# we will allow any vector input ... names/hex ...
	# names.search = base:colors() ... could create a vector of search lists ... ORDER of vector is priority of search ...
	# # https://brand.wsu.edu/visual/colors/ 
	# e.g., list("wsu" = list("crimson" = "#981e32", "gray" = "#717171") );
	# if they are named, we convert to hex ...
	# if they are hex with extra ALPHA, we set opacity ?
	# if(!alpha), just TRUNCATE the end ... 
	
	}
	

	
hexcolor.gradient = function(vecHEX, ..., n=5, force.length=FALSE, alpha=FALSE, skip.checks=FALSE)
	{	
	if(!skip.checks)
		{
		# nested function can call a parent and have skip.checks=TRUE 
		# if the check was already performed in the child 
		vecHEX = dots.addTo(vecHEX, ...);	
		vecHEX = v.color(vecHEX, alpha=alpha); # should be HEX, but now it is with ALPHA
		}
	
	# the OUTPUT is univariate ... if colvec where a LIST .... then I could make it multivariate ... 
	nc = length(vecHEX);
	if(force.length && nc > n) { n = nc; }	
	hexcolor.return(grDevices::colorRampPalette(vecHEX, alpha=alpha)(n));
	}
	

hexcolor.table = function() {}

hexcolor.display = function() {} # HTML or graphics 

hexcolor.wheel = function(vecHEX, ..., steps=12, base.names=FALSE, alpha=FALSE, skip.checks=FALSE) 
	{
	if(!skip.checks)
		{
		# nested function can call a parent and have skip.checks=TRUE 
		# if the check was already performed in the child 
		vecHEX = dots.addTo(vecHEX, ...);	
		vecHEX = v.color(vecHEX, alpha=alpha); # should be HEX, but now it is with ALPHA
		}
	# wheel steps must be MOD of 360 
	if(steps < 2) { steps = 2; }
	if((360 %% steps) != 0) 
		{
		# up to, but not 7, will work ...
		cat.warning("wheel steps must be evenly divisible into 360, ... updating with a nearby value that meets that criteria.");
		steps = num.round(steps, by=6, how="integer");
		}
		
	one.step = 360 / steps;	
	degrees = seq(0, 360-one.step, by=one.step);
	
	hsl = hex2hsl(vecHEX);
	
	h2 = as.integer( round( one.step + hsl[1,] ) ) %% 360;
	
	h2 = as.integer( round( degrees + hsl[1,] ) ) %% 360;
	
	degrees + hsl[1,] ... two vectors ... looped addition
	%+% operator ...
	
	h2 = (degrees %+% hsl[1,]) %% 360;
	# modulus operator turns values from INT to NUM ... WOW 
	"%+%" = function(a,b)
				{
				a = as.numeric(a); 
				b = as.numeric(b);
				na = length(a);
				nb = length(b);
				out = matrix(as.integer(0), nrow=na, ncol=nb);
				for(r in 1:na)
					{
					for(c in 1:nb)
						{
						out[r,c] = as.integer(a[r] + b[c]);
						}
					}
				out;
				}
	
	
	h2 = as.integer( round( one.step + h ) );
			h2 = h2 %% 360;
			# update hsl
			hsl[1] = h2;

	
	# can I vectorize this?
	
	
	
	
	
	
	
	}

hexcolor.plotWheel = function() {}

# get values ... NULL as FF (100) ... 
hexcolor.getOpacity = function(vecHEX, ..., return="100", skip.checks=FALSE) 
	{
	if(!skip.checks)
		{
		# nested function can call a parent and have skip.checks=TRUE 
		# if the check was already performed in the child 
		vecHEX = dots.addTo(vecHEX, ...);	
		vecHEX = v.color(vecHEX); # should be HEX, but now it is with ALPHA
		}
	RETURN = prep.arg(return, n=3, case="upper");
	
	alphas = substring(vecHEX, 8,8+1);
	if(RETURN == "HEX") { return(alphas); }
	
	n255 =  hex2dec(alphas);
	if(RETURN == "255") { return(n255); }
	n100 = n255/255*100;
	if(RETURN == "100") { return(n100); }
	nprop = n100/100;
	# RETURN == "PROPORTION"
	return(nprop);
	}


# reset doesn't take old value ... if(!reset) ... compounding opacity
hexcolor.setOpacity = function(vecHEX, ..., opacity=50, reset=TRUE, skip.checks=FALSE) 
	{
	if(!skip.checks)
		{
		# nested function can call a parent and have skip.checks=TRUE 
		# if the check was already performed in the child 
		vecHEX = dots.addTo(vecHEX, ...);	
		vecHEX = v.color(vecHEX); # should be HEX, but now it is with ALPHA
		}
	
	# could make opacity VECTORIZED on some matching, but WHY?
	# this will return a list... intended to be univariate 
	# this will adjust the CURRENT opacity ... so if already 0.5, now 0.25
	
	alphas = substring(vecHEX, 8,8+1);	
	
	n255 = hex2dec(alphas);
	n100 = n255/255*100;
	if(reset) { n100 = 0*n100 + 100; }
	
	if(opacity > 100) 	{ opacity = opacity / 255 * 100; }
	if(opacity <= 1) 	{ opacity = opacity * 100; }
	# opacity 1 will become 100%
	# opacity 1.0001 will stay 1.0001%
	# if you want smaller than this, have to ENTER PROPORTION 
	
	new.opacity = n100/100 * opacity/100;
	minvisible(new.opacity);
	newalphas = ( dec2hex(255 * new.opacity, to.length=2) );
	
	hexstr = substring(vecHEX, 1, 6+1);
	
	hexcolor.return(paste0(hexstr,newalphas));	
	}
	
	
hexcolor.chromatics = function() {}
hexcolor.chromatics = function(vecHEX, ..., n=12, light="#FFFFFF", dark="#000000", alpha=FALSE, natural.alpha=TRUE, skip.checks=FALSE) 
	{
	if(!skip.checks)
		{
		# nested function can call a parent and have skip.checks=TRUE 
		# if the check was already performed in the child 
		vecHEX = dots.addTo(vecHEX, ...);	
		vecHEX = v.color(vecHEX, alpha=alpha); # should be HEX, but now it is with ALPHA
		}
	# this will return a list... intended to be univariate 
	  
	vlight = v.color(light, alpha=alpha);		
	vdark = v.color(dark, alpha=alpha);
	if(alpha && natural.alpha)
		{
		# we could let them override with their own opacities
		# but by default, they won't ... 
		# maybe have a force.alpha ... 
		# if natural.alpha is FALSE, they had to input the ALPHAS
		# on light, dark, and 
		vlight = hexcolor.setOpacity(vlight, opacity=100);
		vecHEX = hexcolor.setOpacity(vecHEX, opacity=50);
		vdark = hexcolor.setOpacity(vdark, opacity=0);
		}
		 
	vlight = hexcolor.return(vlight);
	vecHEX = hexcolor.return(vecHEX);
	vdark = hexcolor.return(vdark);
	
	n2 = ceiling(n/2);  # 11 will do 13 ... original doesn't count ...
	vlen = length(vecHEX);
	res = vector("list", vlen);
	for(i in 1:vlen)
		{
		tmplight = hexcolor.gradient( c(vlight,vecHEX[i]), 
										n=n2+1, 
										alpha=alpha, 
										skip.checks=TRUE
									);
		tmpdark = hexcolor.gradient(  c(vecHEX[i],vdark), 
										n=n2+1, 
										alpha=alpha, 
										skip.checks=TRUE
									);		
		res[[i]] = unique( c(tmplight, vecHEX[i], tmpdark) );
cat("\n length of chromatic: ", length(res[[i]]), " \n"); 
		}
	list.return(res);
	}


# vecHEX = c("#FAFBFC", "#F3D1A8","#A0A3A9"); vecHEX; hexcolor.round(vecHEX);	
hexcolor.round = function(vecHEX, ..., n=9, alpha=FALSE, skip.checks=FALSE)
	{
	if(!skip.checks)
		{
		# nested function can call a parent and have skip.checks=TRUE 
		# if the check was already performed in the child 
		vecHEX = dots.addTo(vecHEX, ...);	
		vecHEX = v.color(vecHEX, alpha=alpha); # should be HEX, but now it is with ALPHA
		}
	n = as.integer(n); if(n == 0) { n = 1; }
	# F3D1A8 ==> 
	# vecHEX = c("#F3D1A8","A0A3A9");
	
	
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
	if(alpha)
		{
		RGB[4,] = mod.round(RGB[4,]);
		}
	
	hexcolor.return(as.character(rgb2hex(RGB)));	
	}


# vecHEX = c("#F3D1A8","A0A3A9"); vecHEX; hexcolor.websafe(vecHEX);	
hexcolor.websafe = function(vecHEX, ..., skip.checks=FALSE)
	{
	if(!skip.checks)
		{
		# nested function can call a parent and have skip.checks=TRUE 
		# if the check was already performed in the child 
		vecHEX = dots.addTo(vecHEX, ...);	
		vecHEX = v.color(vecHEX); # should be HEX, but now it is with ALPHA
		}
	# F3D1A8 ==> 
	# vecHEX = c("#F3D1A8","A0A3A9");
	
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
	
	hexcolor.return(as.character(rgb2hex(RGB)));	
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