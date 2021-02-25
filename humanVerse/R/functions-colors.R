


# as.hexmode(333)
# base::strtoi
# strtoi(c("0xff", "077", "123"))  # string to integer
# For decimal strings as.integer is equally useful.
# stringToInteger( c("0xff", "077", "123"), TRUE );
# stringToInteger( c("0xff", "077", "123"), FALSE );
stringToInteger = function(strvec, isHEX = FALSE)
	{
	if(isHEX)
		{
		strtoi( tolower(strvec) );  # we could have other "base 10"?
		} else {
				as.integer(strvec);
				}	
	}


# http://php.net/manual/en/function.hexdec.php
# http://php.net/manual/en/function.dechex.php
# java conversions:: http://www.cs.rit.edu/~ncs/color/t_convert.html
#	http://www.easyrgb.com/math.php?MATH=M19#text19

# DDEECC -> rounds to dcedcb
# hexadecimal to decimal
# hexdec("FF");
# alias hex2dec
hexdec = function(hexstr)  
	{
	# rather than checking, let's remove and add leading "0x"
	hexstr = paste0("0x", str_replace("0x", "", trimMe(tolower(hexstr))) );
	stringToInteger(hexstr, TRUE);
	}

# decimal to hexadecimal
# alias dec2hex
dechex = function(intdec, n=NULL)  
	{
	res = toupper( as.character( as.hexmode( as.integer( round(intdec) ) ) ) );
	if(!is.null(n)) { res = strPadLeft( res, n, "0"); 	}
	res;
	}



	
	
# hex2rgb("monte");
# hex2rgb("red");
# hex2rgb( "#abcdef" );
# as.numeric( unlist( hex2rgb( "abcdef" ) ) );
# unlist( hex2rgb( "ABC" ) );
hex2rgb = function(hex)
	{
	hex = checkHEX(hex);
	
	hex = str_replace("#", "", trimMe(toupper(hex)));
		hexV = charVector(hex);
	if(length(hexV) == 3)
		{
		#  'FFF' => 'FFFFFF'
		hexV = c(hexV[1], hexV[1], hexV[2], hexV[2], hexV[3], hexV[3]);
		}
	
	list(
		"r" = hexdec( paste(hexV[1:2], collapse="") ),
		"g" = hexdec( paste(hexV[3:4], collapse="") ),
		"b" = hexdec( paste(hexV[5:6], collapse="") )
		);	
	}
	

# this check if the hex was past; if it was "color", it sees if it can look it up
checkHEX = function(hex)
	{
	hex = str_replace("#", "", trimMe(tolower(hex)));
		
	###############  CHECK if [hex] is [colorname]  ###############
	# insert "color" check here ... if it is "red", let's get "hex" from that ...
	# "^[A-Fa-f0-9]{6}|[A-Fa-f0-9]{3}$"
	# "^#?(([0-9a-fA-F]{2}){3}|([0-9a-fA-F]){3})$"
	search = grep("^#?(([0-9a-fA-F]{2}){3}|([0-9a-fA-F]){3})$", hex);
	if(length(search) < 1)
		{
		hasFound = FALSE;
		# look in base for now
		color.idx = which( colors(TRUE) == hex );
		if(length(color.idx) > 0)
			{ 
			col.rgb = cleanupRGB(col2rgb(hex));
			hex = rgb2hex( col.rgb );
			hasFound = TRUE;
			}
		if(!hasFound) { stop( paste0(" Bad input in function [hex2rgb] : ", hex) ); }
		}
	
	toupper( paste0("#", hex) );
	}

# this checks if the rgb was past, if it was hex, it proceeds ... 
checkRGB = function(rgb)
	{
	if(length(rgb) == 1) { rgb = hex2rgb(rgb); } # they can pass in "hex" 
	cleanupRGB(rgb);	
	}

	
	
rgb2hex = function(rgb, pre="#")
	{
	rgb = cleanupRGB(rgb);
	paste0(pre, dechex(rgb[1],2), dechex(rgb[2],2), dechex(rgb[3],2) );
	}
	

cleanupRGB = function(rgb)
	{
	rgb = as.numeric( unlist( rgb ) ); # just in case ... 	
	if( max(rgb) <= 1 ) { rgb = 255 * rgb; } # we should be in 255 as "1"	
	rgb;
	}


hue2rgb = function(hue)
	{
	hue = as.numeric( unlist( hue ) ); # just in case ... 	
	v1 = hue[1];
	v2 = hue[2];
	vH = hue[3];
	
	if(vH < 0) { vH = vH + 1;}
	if(vH > 1) { vH = vH - 1;}	
	
	once = v1;
	useOnce = FALSE;
	
	if ( (( 6 * vH ) < 1) && !useOnce ) {once = ( v1 + ( v2 - v1 ) * 6 * vH ); useOnce = TRUE;}
	if ( (( 2 * vH ) < 1) && !useOnce ) {once= ( v2 ); useOnce = TRUE;}
	if ( (( 3 * vH ) < 2) && !useOnce ) {once =( v1 + ( v2 - v1 ) * ( ( 2 / 3 ) - vH ) * 6 );}
	
	255 * once;	
	}
	
	
hsl2rgb = function(hsl)
	{
	hsl = as.numeric( unlist( hsl ) ); # just in case ... 	
	# cat("\n", "===", print(hsl), "===", "\n");
	
	h = hsl[1] / 360;  # fraction of circle in degrees 
	s = hsl[2];
	l = hsl[3];
	
	if(s == 0)
		{
		r = g = b = l * 255; # gray
		} else 	{
				v2	=	if(l < 0.5) {l * (1+s); } else {(l + s) - (s*l); }
				v1	=	2*l-v2;
				
				r	= hue2rgb(c(v1,v2,h+1/3));
				g	= hue2rgb(c(v1,v2,h));
				b	= hue2rgb(c(v1,v2,h-1/3));
				}
	
	list(
		"r" = as.integer( r ),
		"g" = as.integer( g ),
		"b" = as.integer( b )
		);
	}
	

# rgb = hex2rgb("#abcdef"); unlist(rgb);
# rgb2hex(rgb);
# hsl = rgb2hsl(rgb);  unlist(hsl);
# unlist( hsl2rgb(hsl) );


rgb2hsl = function(rgb)
	{	
	rgb = checkRGB(rgb);
	rgb = rgb / 255; # we want it on the [0,1] scale 
		myMin = min(rgb);
		v = myMax = max(rgb);
		myRange = myMax - myMin;
		l = myMid = (myMax + myMin) / 2;
	r = rgb[1];
	g = rgb[2];
	b = rgb[3];
			
	if(myRange == 0)  ## gray
		{		
		s = 0;
		
		h = 0;
		} else 	{
				# this line is only difference with rgb2hsv
				# l is myMid, v = myMax
				s = if(myMid < 0.5) { myRange / (myMax + myMin)} else {myRange / (2-myMax-myMin); }
								
				useOnce = FALSE;
					deltaR = ((myMax - r / 6) + (myRange /2))/myRange;
					deltaG = ((myMax - g / 6) + (myRange /2))/myRange;
					deltaB = ((myMax - b / 6) + (myRange /2))/myRange;
				if(myMax == r && !useOnce) { h = deltaB - deltaG; useOnce = TRUE; }
				if(myMax == g && !useOnce) { h = (1/3) + deltaR - deltaB; useOnce = TRUE; }
				if(myMax == b && !useOnce) { h = (2/3) + deltaG - deltaR; useOnce = TRUE; }
				if(h < 0) { h = h + 1;}
				if(h > 1) { h = h - 1;}			
				}
				
		l = myMid;
		h = 360*h; # 360 degrees in a circle
	
	list(
		"h" = h,
		"s" = s,
		"l" = l
		);
	}
	



# rgb = hex2rgb("#abcdef"); unlist(rgb);
# rgb2hex(rgb);
# hsv = rgb2hsv(rgb);  unlist(hsv);
# unlist( hsv2rgb(hsv) );


hsv2rgb = function(hsv)
	{
	hsv = as.numeric( unlist( hsv ) ); # just in case ... 	
	h = hsv[1];
	s = hsv[2];
	v = hsv[3];
	
	if(s == 0)
		{
		r = g = b = v; # gray
		} else 	{
				vH	=	h * 6;
				# mod 6
				vH = if(vH == 6) { 0; } else { vH; }
				vI = as.integer(vH);	
				
				v1	=	v * (1-s);
				v2	=	v * (1-s*(vH-vI));
				v3	=	v * (1-s*(1-(vH-vI)));
				
				if( vI == 0 )
					{
					r = v;
					g = v3;
					b = v1;
					} else if( vI == 1 )
						{
						r = v2;
						g = v;
						b = v1;
						}  else if( vI == 2 )
							{
							r = v1;
							g = v;
							b = v3;
							} else if( vI == 3 )
								{
								r = v1;
								g = v2;
								b = v;
								} else if( vI == 4 )
									{
									r = v3;
									g = v1;
									b = v;
									} else 	{  
											# default case "5"
											r = v;
											g = v1;
											b = v2;
											}			
				
				}
	
	list(
		"r" = as.integer( 255*r ),
		"g" = as.integer( 255*g ),
		"b" = as.integer( 255*b )
		);
	}
	
	
rgb2hsv = function(rgb)
	{	
	rgb = checkRGB(rgb);
	rgb = rgb / 255; # we want it on the [0,1] scale 
		myMin = min(rgb);
		v = myMax = max(rgb);
		myRange = myMax - myMin;
		l = myMid = (myMax + myMin) / 2;
	r = rgb[1];
	g = rgb[2];
	b = rgb[3];
			
	if(myRange == 0)  ## gray
		{		
		s = 0;
		
		h = 0;
		} else 	{
				# this line is only difference with rgb2hsl
				# l is myMid, v = myMax
				s = myRange / myMax;  
								
				useOnce = FALSE;
					deltaR = ((myMax - r / 6) + (myRange /2))/myRange;
					deltaG = ((myMax - g / 6) + (myRange /2))/myRange;
					deltaB = ((myMax - b / 6) + (myRange /2))/myRange;
				if(myMax == r && !useOnce) { h = deltaB - deltaG; useOnce = TRUE; }
				if(myMax == g && !useOnce) { h = (1/3) + deltaR - deltaB; useOnce = TRUE; }
				if(myMax == b && !useOnce) { h = (2/3) + deltaG - deltaR; useOnce = TRUE; }
				if(h < 0) { h = h + 1;}
				if(h > 1) { h = h - 1;}			
				}
				
		l = myMid;
		h = 360*h; # 360 degrees in a circle
	
	list(
		"h" = h,
		"s" = s,
		"v" = v
		);
	}






# rgb = hex2rgb("#abcdef"); unlist(rgb);
# rgb2hex(rgb);
# cmyk = rgb2cmyk(rgb);  unlist(cmyk);
# unlist( cmyk2rgb(cmyk) );


cmyk2rgb = function(cmyk)
	{
	cmyk = as.numeric( unlist( cmyk ) ); # just in case ... 	
	c = cmyk[1];
	m = cmyk[2];
	y = cmyk[3];
	k = cmyk[4];
	
	# cmyk -> CMY
	C = ( c * ( 1 - k ) + k );
	M = ( m * ( 1 - k ) + k );
	Y = ( y * ( 1 - k ) + k );


	# CMY -> RGB
	r = ( 1 - C );
	g = ( 1 - M );
	b = ( 1 - Y );
	
	list(
		"r" = as.integer( 255*r ),
		"g" = as.integer( 255*g ),
		"b" = as.integer( 255*b )
		);
	}

rgb2cmyk = function(rgb)
	{
	rgb = checkRGB(rgb);
		myMin = min(rgb);
		v = myMax = max(rgb);
		myRange = myMax - myMin;
		l = myMid = (myMax + myMin) / 2;
	r = rgb[1];
	g = rgb[2];
	b = rgb[3];
			
	# RGB -> CMY
		C = 1 - ( r / 255 );
		M = 1 - ( g / 255 );
		Y = 1 - ( b / 255 );

	# CMY -> cmyk

		vK = 1;

		# min ?
		if ( C < vK )   {vK = C;}
		if ( M < vK )   {vK = M;}
		if ( Y < vK )   {vK = Y;}
		if ( vK == 1 ) 
			{ 
			# Black
			c = 0;
			m = 0;
			y = 0;
			}
			else 
				{
				c = ( C - vK ) / ( 1 - vK );
				m = ( M - vK ) / ( 1 - vK );
				y = ( Y - vK ) / ( 1 - vK );
				}
		k = vK;
	
	# these are  [0,1]
	list(
		"c" = c ,
		"m" = m ,
		"y" = y ,
		"k" = k 
		);
	}



# ALMOST WORKING CORRECTLY ... 
# color.findNearestName("#8B8378", how.many = 12, scale.me = TRUE);
color.findNearestName = function(hex, how.many = 1, scale.me = TRUE, how="distance", ...)
	{
	hex = checkHEX(hex);	
		
	df = color.buildTable();
#################  SEE if HEX is in the TABLE  #################	
	## let's just look for it ...
		color.idx = which(df$hex.color == hex);
	if(length(color.idx) > 0) 
		{ 
		res = df$color[color.idx];
		x = res;
			x = setAttribute("match", "exact", x);
			x = setAttribute("distance", 0, x);
			x = setAttribute("hex", rgb2hex(col2rgb( res )), x);
		return (x);
		} # should just be one, but maybe more 

#################  USE DISTANCE/COSINE SIMILARITY  #################	
	rgb = cleanupRGB( hex2rgb(hex) );	
		r = rgb[1];
		g = rgb[2];
		b = rgb[3];
	row = c("===search===", hex, r, g, b); 
	
	df = rbind(row, df);
	df = assignColumnsTypeInDataFrame(c("r","g","b"), "numeric", df);
	
		
	Xs = ( as.matrix( cbind(df$r, df$g, df$b) ) );
		if(scale.me) { Xs = scale(Xs); }
	
	if(how == "difference")
		{
		# is this not "manhattan"
		vsearch = Xs[1,];
		vcolors = Xs[-c(1),];
		
		vdiff = abs(vcolors - vsearch);
		vdiff.rowS = rowSums(vdiff);
		
		names(vdiff.rowS) = df$color[-c(1)];
		
		one = sort(vdiff.rowS);
		
		res = one[1:how.many];
		x = names(res);
			x = setAttribute("match", "difference", x);
			x = setAttribute("distance", as.numeric( res ), x);
			x = setAttribute("hex", rgb2col(col2rgb( names(res) )), x);
		return (x);
		}
	if(how == "distance")
		{
		X.d = stats::dist( Xs, method="euclidean");
		X.m = round( as.matrix(X.d), 2);
		X.df = as.data.frame( X.m );
			colnames(X.df) = rownames(X.df) = df$color;
			
		one = X.df[,1]; names(one) = df$color;
		one = sort(one);
		one = one[-c(1)];
		
		res = one[1:how.many];
		x = names(res);
			x = setAttribute("match", "distance", x);
			x = setAttribute("distance", as.numeric( res ), x);
			x = setAttribute("hex", rgb2col(col2rgb( names(res) )), x);
		return (x);
		}
	if(how == "cosine")
		{
		# vsearch = Xs[1,];
		# vcolors = Xs[-c(1),];
		
		# v.cos = cosine(vsearch, vcolors); # raw no distance or SVD
		
		# https://stackoverflow.com/questions/18946966/
		X.d = stats::dist( Xs, method="euclidean");
		X.m = round( as.matrix(X.d), 2);
		X.df = as.data.frame( X.m );
			colnames(X.df) = rownames(X.df) = df$color;
			
			
		
			
		## library(lsa); # very slow, replace
		X.cos = as.data.frame( 1 - round( cosine(X.m),4 ) );
			colnames(X.df) = rownames(X.df) = df$color;
			
		one = X.cos[,1]; names(one) = df$color;
		one = sort(one);
		one = one[-c(1)];
		
		res = one[1:how.many];
		x = names(res);
			x = setAttribute("match", "cosine", x);
			x = setAttribute("distance", as.numeric( res ), x);
			x = setAttribute("hex", rgb2col(col2rgb( names(res) )), x);
		return (x);			
		}	
	}



# we need to cache this in "last" memory
color.buildTable = function(cvec = colors(TRUE))
	{
	n = length(cvec);
	hvec = character(n);
	r = g = b = numeric(n);
	
	for(i in 1:n)
		{
		color = cvec[i];
		rgb = cleanupRGB( col2rgb(color) );
		hvec[i] = rgb2hex( rgb );		
		r[i] = rgb[1]; g[i] = rgb[2]; b[i] = rgb[3];				
		}
		
	df = as.data.frame(cbind(cvec, hvec, r, g, b));
		colnames(df) = c("color", "hex.color", "r", "g", "b");	
		
	df = assignColumnsTypeInDataFrame(c("r","g","b"), "numeric", df);
	
	df;	
	}

color.setOpacity = function(hexvec, opacity=100)
	{
	alpha = dechex(255 * opacity/100, 2);
	paste0(hexvec,alpha);
	}

# we need to cache this in "last" memory
# accessor can get elements without having to rebuild
color.chromatics = function(rgb, n = 12) # mono steps of monochronic ... half on "white" / half on "black"
	{
	if(length(rgb) == 1) { rgb = hex2rgb(rgb); } # they can pass in "hex" 
	hex = rgb2hex(rgb);
	
	n2 = ceiling(n/2);  # 11 will do 13 ... original doesn't count ...	
	res = c( colorsInGradient(n2+1, c("#FFFFFF", hex)), hex, colorsInGradient(n2+1, c(hex, "#000000")) );	
	res = unique(res);		
	
	which.hex = which(res == hex)[1];
	idx = -1*( 1:length(res) - which.hex );  
	
	df = as.data.frame(cbind(idx, res));
		colnames(df) = c("index", "hex.color");		
		df = setAttribute("hex", hex, df);
		df = setAttribute("rgb", unlist(rgb), df);
	
	
	df;
	}



## http://c.mshaffer.com/js/colorpicker/functions.colors.js
# we need to cache this in "last" memory
# accessor can get elements without having to rebuild
# - complement, split, split-complement, triad, square, rectangle, and so on ... 
color.wheel = function(rgb, wheel.steps = 12)  # wheel steps needs to be divisible by 360?
	{
	if(length(rgb) == 1) { rgb = hex2rgb(rgb); } # they can pass in "hex" 
	hex = rgb2hex(rgb);
	
	hsl = rgb2hsl(rgb);
	hsl = as.numeric( unlist( hsl ) );
		h = hsl[1];
		s = hsl[2];
		l = hsl[3];

	res = character(wheel.steps);
	myh = numeric(wheel.steps);
	deg = numeric(wheel.steps);
	one.step = 360 / wheel.steps;	
	degrees = 0;
	res[1] = rgb2hex(rgb); # starting color ... this is also the ending color, but fine 
	myh[1] = h;
	deg[1] = degrees;
	for(i in 2:wheel.steps)
		{
		degrees = degrees + one.step;
		h2 = as.integer( round( one.step + h ) );
			h2 = h2 %% 360;
			# update hsl 
			hsl[1] = h2;
		res[i] = rgb2hex( hsl2rgb(hsl) );
		deg[i] = degrees;
		h = h2;
		myh[i] = h;		
		}
	
	df = as.data.frame(cbind(deg, res, myh));
		colnames(df) = c("degrees", "hex.color", "wheel");
		
		df = setAttribute("hex", hex, df);
		df = setAttribute("rgb", unlist(rgb), df);
	
	
	df;
	}




color.webSafeHEX = function(rgb)
	{
	rgb = checkRGB(rgb);
	r = rgb[1];
	g = rgb[2];
	b = rgb[3];
	
	doMod = function(x)
		{
		xMod = x %% 51;
		if(xMod <= 25) { floor(x/51) * 51; } else { ceiling(x/51) * 51; }		
		}
	
	rgb = list(
			"r" = doMod(r),
			"g" = doMod(g),
			"b" = doMod(b)
			);	
			
	rgb2hex(rgb);
	}
	
color.randomHEX = function()
	{
	rgb2hex( color.randomRGB() );
	}
	
	
color.randomRGB = function()
	{	
	list(
		"r" = rand(0,255),
		"g" = rand(0,255),
		"b" = rand(0,255)
		);	
	}


























color.roundHEX = function(rgb, n=3, full=FALSE)
	{
	# this round "FA" to "FC", "FD" to "FF" 	
	rgb = checkRGB(rgb);
	if(full)  # this rounds at the whole "FF" value, not just the last element ...
		{
		r = rgb[1];
		g = rgb[2];
		b = rgb[3];
		
		doMod = function(x)
			{
			xMod = x %% n;
			floor(x/n) * n;	
			}
		
		rgb = list(
				"r" = doMod(r),
				"g" = doMod(g),
				"b" = doMod(b)
				);	
				
		rgb2hex(rgb);
		} else {
				# round at just the last hex value 
				hex = rgb2hex(rgb);
				hex = str_replace("#", "", hex);
				hexV = charVector(hex); 
				decV = hexdec(hexV);
					decV[2] = doMod(decV[2]);
					decV[4] = doMod(decV[4]);
					decV[6] = doMod(decV[6]);
				hexV[2] = dechex(decV[2]);
				hexV[4] = dechex(decV[4]);
				hexV[6] = dechex(decV[6]);
				
				hex = paste0("#", paste(hexV, collapse=""));
				hex;
				}
	}
	
	









## ... base R cleanup ...


#' colorsInGradient
#'
#' @param n Number of colors to return
#' @param colvec Vector of color names "red" or RGB "#FF0000" or RGBa "#4169E1FF"
#' @param alpha Pass transparency filter "alpha" as TRUE or FALSE
#'
#' @return vector of colors in RGB or RGBa form (depending on alpha)
#' @export
#'
#' @examples
#'
#' colorsInGradient(4, c("red", "royalblue"));
#' colorsInGradient(4, c("#FF000000", "#FF0000FF"), TRUE);  # red through alphas
#' colorsInGradient(4, c("#FF000000", "#4169E1FF"), TRUE);
#'
colorsInGradient = function(n, colvec=c("red","royalblue"), alpha=FALSE)
  {
  # colorsInGradient(4, c("red", "royalblue"));
  # colorsInGradient(4, c("#FF000000", "#FF0000FF"), TRUE);  # red through alphas
  # colorsInGradient(4, c("#FF000000", "#4169E1FF"), TRUE);  # red->royalblue through alphas
  # rgb2col( col2rgb("royalblue") );

  # alpha doesn't seem to work as expected ... unless I pass in RGBa?
  grDevices::colorRampPalette(colvec, alpha=alpha)(n);
  }



#' rgb2col
#'
#' Reverse the built-in grDevices::col2rgb function
#' [ See grDevices::convertColor or grDevices::make.rgb ]
#'
#' @param x vector of colors
#'
#' @return vector of colors in RGB hex format
#' @export
#'
#' @examples
#'
#' rgb2col( col2rgb("red") );
#' rgb2col( col2rgb("red", alpha=TRUE) );
#' rgb2col( col2rgb("#FF0000FF", alpha=TRUE) );
#' rgb2col( col2rgb("#FF000033", alpha=TRUE) );
#'
rgb2col = function(x)
  {
  # reverses col2rgb function
  x.n = dim(x)[1];
  if(x.n == 4)
    {
    x.rgb = t(x[1:4,]) /255;
    grDevices::rgb(   as.numeric(x.rgb[,1]),
                      as.numeric(x.rgb[,2]),
                      as.numeric(x.rgb[,3]),
                      as.numeric(x.rgb[,4]),
      names=rownames(x.rgb) );
    } else {
            x.rgb = t(x[1:3,]) /255;
            grDevices::rgb( x.rgb, names=rownames(x.rgb) );
            }
  }







