

  
color.library = function()
	{
	# C:\_git_\github\MonteShaffer\humanVerse.data\-colors-
	# libname, subname, hexes, colnames ...
	# append RGB and nearest distinct R-color color.nearest(1)
	# caching of color nearest ... 
	
	
	}



color.import = function(listname="wsu", 
						keys = c("crimson", "gray"),
						vals = c("#981e32", "#717171"), 
						append=TRUE
					)
	{
	if(is.null(listname))
		{
		# import baseR colors 
		listname = "baseR";
		NAMES = colors();
		RGB = color.col2rgb(NAMES);
		HEX = as.character(rgb2hex(RGB));	
		MAT = t(RGB);
		
		mylist = list(
					"byname" = list.create(NAMES, HEX), 
					"byhex" = list.create(HEX, NAMES)
					);
		
		df = dataframe( MAT[,1], MAT[,2], MAT[,3]);
			df$color.library = listname;
			df$color.hex = HEX;
			df$color.name = NAMES;
			df$index = 1:length(HEX);
		rownames(df) = NULL;
		colnames(df) = c("r", "g", "b", "color.library", "color.hex", "color.name", "index"); 
		
		 df = df.moveColumns(df, "index", "after", "color.library")
		  df = df.moveColumns(df, c("r","g","b"), "after", "color.name")
		df = df.sortBy(df, "color.name", "ASC");

		if(!append || !exists("color.dictionary"))
			{
			color.dictionary = df;
			color.list = list();
			color.list[[listname]] = mylist;
			"color.dictionary" %GLOBAL% color.dictionary;
			"color.list" %GLOBAL% color.list;
			} else {
					color.dictionary = ggget("color.dictionary", pos=1);
					color.list = ggget("color.list", pos=1);
					
					color.dictionary = rbind(color.dictionary,df);
					color.list[[listname]] = mylist;
					"color.dictionary" %GLOBAL% color.dictionary;
					"color.list" %GLOBAL% color.list;
					}
			
		} else {
				# import a list ...
				# seems like list.build / dataframe.build are FN
				# nearest, property.set ... top 10 on 3 techniques, return top 1 on given technique ... LIMIT to color.dictionary ... priority to baseR library (reimport X11, HTML8, WEBSAFE, and so on...)
		
				}
	
	}


# hexstr = c("#333333", "#999999"); RGB = hex2rgb(hexstr); num = hex2dec(hexstr);  dec2hex(mean(num)); RGB.m = (RGB[,1, drop=FALSE] + RGB[,2])/2; rgb2hex(RGB.m);
# maths work ... could I build a structure like `base` that allows me to actually add/substract, or maybe just a macro 
# v.hexmath = vecHEX, fun=mean ... 
## Since this argument follows ... its name cannot be abbreviated.
## ?*apply ... show all "apply" functions ...
 
## vapply(vecHEX, mean)



## color.addLIST ... 
color.init = function()
	{
	
	
	
	NAMES = colors();
	RGB = color.col2rgb(NAMES);
	HEX = rgb2hex(RGB);
	
	MAT = t(RGB);
	
	
	color.list = list( "base" = list(
				"byname" = list.create(names(HEX), as.character(HEX)), 
				"byhex" = list.create(as.character(HEX), names(HEX))
									)
					);
	
	# df = color.dictionary;
	# color.dictionary = df;
	
	df = dataframe( MAT[,1], MAT[,2], MAT[,3]);
		df$color.library = "R";
		df$color.hex = as.character(HEX);
		df$color.name = names(HEX);
		df$index = 1:length(HEX);
	rownames(df) = NULL;
	colnames(df) = c("r", "g", "b", "color.library", "color.hex", "color.name", "index"); 
	
	 df = df.moveColumns(df, "index", "after", "color.library")
	  df = df.moveColumns(df, c("r","g","b"), "after", "color.name")
	df = df.sortBy(df, "color.name", "ASC");

	
	
	
	}



# color-utils contain the lower-level functions 
# this contains "names", "lists", setOpacity, findNearest, palettes
# we use HEX/RGBa as our two primary color choices 
# for color.MATHs we use RGBa ...
# could I create RGB as tuple to do core Rmaths ... mean ... convert back to HEX ... 

# col = v.color(...);
# v.color("red", color.setOpacity("red", 0.5), "#FFFF00", "#FFFF0080" ...)


color.nearestNameFromHEX = function(vecHE..., nearest=1, name.search="base", method="manhatten, euclidean, cosine")
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


color.nameFromHEX = function(..., name.search="base", force.match=FALSE)
	{
	vecHEX = prep.dots(...);
	
	}

#  "wsu:crimson"
#  "eku:colors" ... "ua:colors" (arizona) 
#  "byu:oldcolors, newcolors"
#  "utah:oldcolors, newcolors"
#  "steelers:oldcolors, newcolors"
#  "cincireds: oldcolors, newcolors"
#  "cfalls:oldcolors", "newcolors"
#  "arg:entina flag colors" 
v.color = function(..., names.search="base", alpha=TRUE)
	{  
	colvec = prep.dots(...);
	
	
	# TEMP CODE 
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
	


# just use v.chain ... for hexcolor.math ... 
# v.chain(vec, hex2dec, mean, dec2hex, hex.prepend)
# FUN="stats.mean"

	
hexcolor.gradient = function(..., n=5, force.length=FALSE, alpha=FALSE, skip.checks=FALSE)
	{
	vecHEX = prep.dots(...);
	# with skip.checks, a parent/child may have not included alpha ...
	# internally, v.color should include alpha=TRUE ...
	# function needs to deal with that and strip if exists/necessary
	# for given logic ...
	if(!skip.checks)
		{
		# nested function can call a parent and have skip.checks=TRUE 
		# if the check was already performed in the child 
		vecHEX = v.color(vecHEX, alpha=alpha); # should be HEX, but now it is with ALPHA
		}
	
	# the OUTPUT is univariate ... if colvec where a LIST .... then I could make it multivariate ... 
	nc = length(vecHEX);
	if(force.length && nc > n) { n = nc; }	
	hexcolor.return(grDevices::colorRampPalette(vecHEX, alpha=alpha)(n));
	}
	

hexcolor.table = function() {}

hexcolor.display = function() {} # HTML or graphics 


## TODO :: make a function for a spiral ... 
## color.spiral(555)
## color.uniform(555) ... see mathematica sunflower 
## color.sunflower(555) 

## color.prand(555) ... dev.flush ... one at a time ... 
## progress bar ... 


# ggg.circle(0, 0, 100, fill.color='purple', fill.lines=NULL)

# color.plot( color.rand(555), size=1/5, thick=1/5 )
# now we know why color.nearest performs poorly around BLACK ...
# the 'BLACK' hole ... 
# color.plot( color.col2hex(colors()) );
# color.plot( color.css()$color.hex );
# color.plot( WP48 );

WP48 = c(
"ff8888", "ffff88", "88ff88", "00ff88", "88ffff", "0088ff", "ff88cc", "ff88ff", "ff0000", "ffff00", "88ff00", "00ff44", "00ffff", "0088cc", "8888cc", "ff00ff", "884444", "ff8844", "00ff00", "008888", "004488", "8888ff", "880044", "ff0088", "880000", "ff8800", "008800", "008844", "0000ff", "0000aa", "880088", "8800ff", "440000", "884400", "004400", "004444", "000088", "000044", "440044", "440088", "000000", "888800", "888844", "888888", "448888", "cccccc", "440044", "ffffff"
);
 
# color.plot( WP48 );

# maybe 

# color.plot( color.rand(555), size=1/5, thick=1/5 )


color.plot = function() {}
# hexcolors ... our color functions have hex as INPUTS
# color-utils manipulate so we can get that way ...
color.plot = function(..., angleOffset = 150, size=5, thick=5, rx=1, ry=rx)
	{
	hex = prep.dots(..., default = c("#C0FFEE", "#abcdef", "#c8008c") );
	
# c(`#C0FFEE` = 313.809523809524, `#ABCDEF` = 360, `#C8008C` = 468

	hex = color.hex(hex); 
	cex = size;
	lwd = thick;

		# given H, L ... could I find HEX ? 
	HSL = hex2hsl(hex);  H = HSL[1,];
	LAB = hex2lab(hex);  L = LAB[1,];
	
		# radius = rx = ry = 1;
		# ry = 10;
		rmax = max(rx, ry);
		xrange = c(-rmax,rmax); ydomain = c(-rmax,rmax);
		# angleOffset = 150;
	
	
	
	
	par(pty="s");   # plot(SQUARE);
	plot(0,0, pch="", 
			xlim = xrange, ylim = ydomain, 
			axes=FALSE, xaxt='n', yaxt='n', ann=FALSE);
			
			
		x = L/100 * rx * cos(deg2rad(H+angleOffset));  
		y =	L/100 * ry * sin(deg2rad(H+angleOffset));
		# white 
		wx = 1 * rx * cos(deg2rad(0+angleOffset));  
		wy = 1 * ry * sin(deg2rad(0+angleOffset));
		# black 
		bx = 0; 
		by = 0;
	
	
if(FALSE)
	{
	
	ggg.circle(0,0, rx, border.color="gray", border.thick = 0.5, border.style = "dashed", fill.color="#000000ff", fill.lines=NULL);
	
	ggg.circle(0,0, rx*0.8, border.color="gray", border.thick = 0.5, border.style = "dashed", fill.color="#333333cc", fill.lines=NULL);
	
	ggg.circle(0,0, rx*0.6, border.color="gray", border.thick = 0.5, border.style = "dashed", fill.color="#66666699", fill.lines=NULL);
	
	ggg.circle(0,0, rx*0.4, border.color="gray", border.thick = 0.5, border.style = "dashed", fill.color="#99999966", fill.lines=NULL);
	
	ggg.circle(0,0, rx*0.2, border.color="gray", border.thick = 0.5, border.style = "dashed", fill.color="#cccccc33", fill.lines=NULL);
	}
	
	ggg.circle(0,0, rx, ry, border.color="gray", border.thick = 0.5, border.style = "dashed", fill.color=NA, fill.lines=NULL);



		
		
	points(wx,wy, col="black", bg="white", pch=24, lwd=lwd, cex=cex);
	points(wx,wy, col="black", bg="white", pch=25, lwd=lwd, cex=cex);
	points(wx,wy, col="white", pch=22, lwd=lwd, cex=cex);
	points(wx,wy, col="white", pch=23, lwd=lwd, cex=cex);
	
	points(bx,by, col="black", pch=22, lwd=lwd, cex=cex);
	points(bx,by, col="black", pch=23, lwd=lwd, cex=cex);
	
	
	# maybe set transparency
	points(x,y, col=hex, pch=22, lwd=lwd, cex=cex);
	points(x,y, col=hex, pch=23, lwd=lwd, cex=cex);
	
	
	
	}








hexcolor.wheelPlot = function() 
	{
	angleOffset = 0;
	# var angleOffset = -30;  // rotation from 360 vertical is red
	## easyrgb.com 
	## YELLOW is UP 
	# angle offset like colors.mshaffer.com 
	# x=	L/100 * rx * Math.cos(deg2rad(H+angleOffset)) - w/2 + rx	;  
	# y=	L/100 * ry * Math.sin(deg2rad(H+angleOffset)) - h/2 + ry	;
	# cx, cy is center of circle? or radius x, y ?
	# w, h is range of viewing area ... 
	# calculatePosition(_color,Lab[0],hsv[0],height,width,center);
	# LAB ... 
	# offset so purple is UP ... already IS ... 
	# maybe shift -30 ... so it is LEFT SIDE, STRONG SIDE 
	# H = 300
	# L = 81
	# white/black are on zero-angle from CENTER outward 
	# what is negative white?
	# H = 0, L = 100 ... whhite ... shouldn't that be UP 
	# H = 0, L = 0 ... black ... 
	# H = 180, L=100 ... what is that ?
	
	HH = c("00", "FF");
	hex = NULL;
	for(i in 1:2)
		{
		for(j in 1:2)
			{
			for(k in 1:2)
				{
				hex = c(hex, paste0("#",HH[i], HH[j], HH[k]));
				}
			}
		}
		
	HSL = hex2hsl(hex);  H = HSL[1,];
	LAB = hex2lab(hex);  L = LAB[1,];
	
	# par(reset); # restore state
	
		
		# allows for elliptical distortion 
		radius = rx = ry = 100;
		xrange = c(-rx,rx); ydomain = c(-ry,ry);
		angleOffset = 150;
	
	
	
	
	par(pty="s");   # plot(SQUARE);
	plot(0,0, pch="", 
			xlim = xrange, ylim = ydomain, 
			axes=FALSE, xaxt='n', yaxt='n', ann=FALSE);
			
			
		x = L/100 * rx * cos(deg2rad(H+angleOffset));  
		y =	L/100 * ry * sin(deg2rad(H+angleOffset));
		# white 
		wx = 1 * rx * cos(deg2rad(0+angleOffset));  
		wy = 1 * ry * sin(deg2rad(0+angleOffset));
		# black 
		bx = 0; 
		by = 0;
	
	ggg.circle(0,0, rx, border.color="gray", border.thick = 0.5, border.style = "dashed", fill.color=NA, fill.lines=NULL);
		
		
	points(wx,wy, col="black", bg="white", pch=24,   cex=cex);
	points(wx,wy, col="black", bg="white", pch=25,   cex=cex);
	points(wx,wy, col="white", pch=22,   cex=cex);
	points(wx,wy, col="white", pch=23,   cex=cex);
	
	points(bx,by, col="black", pch=22,   cex=cex);
	points(bx,by, col="black", pch=23,   cex=cex);
	
	
	# maybe set transparency
	points(x,y, col=hex, pch=22,   cex=cex);
	points(x,y, col=hex, pch=23,   cex=cex);
	
	
	# colors = c("#FF00FF", "#FF0000", 
	# yellow:  h = 50, L = 90
	#


	# - complement ... 180 degrees from original
    # - split-complement ... 150/210 degrees from original
    # - analagous  ... +30/-30 degrees from original
    #               (split ... inverse of split-complement)
    # - triad ... +120/-120 degrees from original
    # - square ... +90/-90/+180
    # - rectangle ... complement, +30 and it's complement

  # blank canvas
  graphics::plot.new( );
  graphics::plot.window(
              xlim=c(-1.5,1.5), # unit circle is 1
              ylim=c(-1.5,1.5),
              log="",
              graphics::par(mar=c(0.25, 0.25, 0.25, 0.25)) # outer margins
            );

  # maybe put a marker like a clock on 12 ("up")
  # figure out the aspect ratio
  radius = 1;
      x0 = 0;
      y0 = 0; # center of circle

  plotrix::draw.circle(x0,y0, radius, col="gray");

    original = df[1,];
      x = x0 + radius * sin( deg2rad( original$wheel ) );
      y = y0 + radius * cos( deg2rad( original$wheel ) );
        plotrix::draw.circle(x,y, radius/3, col=original$hex.color);
        graphics::text(x,y, adj=c(0.5,0.5), cex=1, labels=original$hex.color);
            # maybe add names to wheel.table
            # maybe write function "best contrast" to determine
            # foreground color

    remaining = df[-c(1),];
    nr = dim(remaining)[1];
    for(i in 1:nr)
      {
      x = x0 + radius * sin( deg2rad( remaining$wheel[i] ) );
      y = y0 + radius * cos( deg2rad( remaining$wheel[i] ) );
        plotrix::draw.circle(x,y, radius/6, col=remaining$hex.color[i]);
        graphics::text(x,y, adj=c(0.5,0.5), cex=1/2, labels=remaining$hex.color[i]);
      }

	
	}


hexcolor.wheel = function(..., steps=12, base.names=FALSE, alpha=FALSE, skip.checks=FALSE) 
	{
	vecHEX = prep.dots(...);
	if(!skip.checks)
		{
		# nested function can call a parent and have skip.checks=TRUE 
		# if the check was already performed in the child 
		vecHEX = v.color(vecHEX, alpha=alpha); # should be HEX, but now it is with ALPHA
		}
	# wheel steps must be MOD of 360 
	if(steps < 2) { steps = 2; }
	if((360 %% steps) != 0) 
		{
		# up to, but not 7, will work ...
		cat.warning("wheel steps must be evenly divisible into 360, ... updating with a nearby value that meets that criteria.");
		steps = int.round(steps, by=6, how="integer");
		} 
		
	one.step = 360 / steps;	
	degrees = seq(0, 360-one.step, by=one.step);
	nd = length(degrees);
	
	
	hsl = hex2hsl(vecHEX);
 
	n = length(vecHEX);
	res = vector("list",n);
	for(i in 1:n)
		{		
		one.hsl = hsl[,i, drop=FALSE];
		# h2 = as.integer( round( degrees + one.hsl[1] ) ) %% 360;
		h2 = math.cleanup ( ( degrees + one.hsl[1] ) %% 360);
		
		
		my.hsl = matrix.rep(one.hsl, times=(nd), by="col");
		my.hsl[1,] = h2;
			colnames(my.hsl) = NULL;
			

		hex = hsl2hex(my.hsl);
					
		names(hex) = h2;
		
		res[[i]] = hex;	
		res[[i]] = property.set("angles", res[[i]], h2);
		}
	# modulus operator turns values from INT to NUM ... WOW 
	minvisible(res, print=FALSE);
	list.return(res);
	}
  



# get values ... NULL as FF (100) ... 
hexcolor.getOpacity = function(..., return="100", skip.checks=FALSE) 
	{
	vecHEX = prep.dots(...);
	if(!skip.checks)
		{
		# nested function can call a parent and have skip.checks=TRUE 
		# if the check was already performed in the child 
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
hexcolor.setOpacity = function(..., opacity=50, reset=TRUE, skip.checks=FALSE) 
	{
	vecHEX = prep.dots(...);
	if(!skip.checks)
		{
		# nested function can call a parent and have skip.checks=TRUE 
		# if the check was already performed in the child 
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
hexcolor.chromatics = function(..., n=12, light="#FFFFFF", dark="#000000", alpha=FALSE, natural.alpha=TRUE, skip.checks=FALSE) 
	{
	vecHEX = prep.dots(...);
	if(!skip.checks)
		{
		# nested function can call a parent and have skip.checks=TRUE 
		# if the check was already performed in the child 
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
hexcolor.round = function(..., n=9, alpha=FALSE, skip.checks=FALSE)
	{
	vecHEX = prep.dots(...);
	if(!skip.checks)
		{
		# nested function can call a parent and have skip.checks=TRUE 
		# if the check was already performed in the child 
		vecHEX = v.color(vecHEX, alpha=alpha); # should be HEX, but now it is with ALPHA
		}
	n = as.integer(n); if(n == 0) { n = 1; }
	# F3D1A8 ==> 
	# vecHEX = c("#F3D1A8","A0A3A9");
	
	
	# convert to RGB 
	RGB = hex2rgb(vecHEX);
	
	# # this round "FA" to "FC", "FD" to "FF"
	mod.round = function(x, tol = DEFAULT_TOLERANCE)
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
hexcolor.websafe = function(..., skip.checks=FALSE)
	{
	vecHEX = prep.dots(...);
	if(!skip.checks)
		{
		# nested function can call a parent and have skip.checks=TRUE 
		# if the check was already performed in the child 
		vecHEX = v.color(vecHEX); # should be HEX, but now it is with ALPHA
		}
	# F3D1A8 ==> 
	# vecHEX = c("#F3D1A8","A0A3A9");
	
	# convert to RGB 
	RGB = hex2rgb(vecHEX);
	
	mod.websafe = function(x, tol = DEFAULT_TOLERANCE)
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
#######	my.numbers = rand(2^1 - 1, (2^8)^3 - 1, n);
	# dec2hex(0, to.length=6)
	# 

	# }

# (rgb = unlist ( color.randomRGB(key="key.to.remember.seed") ) );

###### R::humanVerse #####
# (hex = color.randomHEX() ) ;

# (hex = color.randomHEX(3, key="another.memory") ) ;