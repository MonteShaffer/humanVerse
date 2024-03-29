

# from "name" ... would have to be in a resource colors() is default list 
# https://www.colorbook.io/pages/hex-to-cmyk-conversion
# hex, cymk, rgb, hsl 
# I also have hue and hsv ...
# I don't think this is a ONE -> BUILD MANY; rather MANY -> BUILD ONE ...
# BUT it does allow PERMUATION conversions ...
# I need rgb2hex and hex2rgb ... rgb to/from for each:  hex, cmyk, hsl, hue, hsv ... 
# names ...

# assume I have a good RGBMatirx ... could do cleanup/check ... 
# matrixRGB = color.col2rgb("red","#FF9944",2,"#336699AA");

.rgb2hsl = function(matrixRGB)
	{
	# we want it on the [0,1] scale
	if(max(matrixRGB) > 1) { matrixRGB = matrixRGB/255; }
	
	# delete alpha if exists 
	# force input to [3 x n] matrix
	matrixHSL = keep.matrix( 0*matrixRGB[-c(4),] );	
		colnames(matrixHSL) = colnames(matrixRGB);
		rownames(matrixHSL) = c("h","s","l");
	n = ncol(matrixHSL);
	for(i in 1:n)
		{
		one = matrixRGB[,i];
			r = one[1];
			g = one[2];
			b = one[3];		
		one.min = min(r,g,b);
		one.max = max(r,g,b); 					v = one.max;
		
		one.range = one.max - one.min;
		one.mid = (one.min + one.max) / 2;		l = one.mid;
			
		if(one.range == 0) # we have GRAY?
			{
			s = 0;
			h = 0;
			} else 	{
					# this line is only difference with rgb2hsv
					# l is one.mid, v = one.max
					# s = one.range / one.max;
					s = if(one.mid < 0.5) { one.range / (one.max + one.min)} else {one.range / (2-one.max-one.min); }
					# http://c.mshaffer.com/js/colorpicker/functions.colors.js SEEMS to have (0+5) bug ... 


					useOnce = FALSE;
						deltaR = ((one.max - r / 6) + (one.range /2))/one.range;
						deltaG = ((one.max - g / 6) + (one.range /2))/one.range;
						deltaB = ((one.max - b / 6) + (one.range /2))/one.range;
					if(one.max == r && !useOnce) { h = deltaB - deltaG; useOnce = TRUE; }
					if(one.max == g && !useOnce) { h = (1/3) + deltaR - deltaB; useOnce = TRUE; }
					if(one.max == b && !useOnce) { h = (2/3) + deltaG - deltaR; useOnce = TRUE; }
					if(h < 0) { h = h + 1;}
					if(h > 1) { h = h - 1;}
					}

			l = one.mid;
			h = 360*h; # 360 degrees in a circle

		matrixHSL[,i] = c(h,s,l);
		}
	matrixHSL;	
	}

	

.color.hue = function(X)
	{
	# force input to [3 x n] matrix
	matrixHUE = keep.matrix(X);
	n = ncol(matrixHUE);
	res = numeric(n);  # one number per column 
	for(i in 1:n)
		{
		hue = matrixHUE[,i];
		
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

		res[i] = 255 * once;		
		}
	res;	
	}
	

.hsl2rgb = function(matrixHSL)
	{
	matrixRGB = keep.matrix( 0*matrixHSL ); 
		colnames(matrixRGB) = colnames(matrixHSL);
		rownames(matrixRGB) = c("r","g","b");
	n = ncol(matrixRGB);
	
	for(i in 1:n)
		{
		hsl = matrixHSL[,i];

		h = hsl[1] / 360;  # fraction of circle in degrees
		s = hsl[2];
		l = hsl[3];

		if(s == 0)
			{
			r = g = b = as.integer(l * 255); # gray
			} else 	{
					v2	=	if(l < 0.5) {l * (1+s); } else {(l + s) - (s*l); }
					v1	=	2*l-v2;

					r	= as.integer(.color.hue(c(v1,v2,h+1/3)));
					g	= as.integer(.color.hue(c(v1,v2,h)));
					b	= as.integer(.color.hue(c(v1,v2,h-1/3)));
					}

		matrixRGB[,i] = c(r,g,b);
		}
	matrixRGB;	
	}


# redundant a bit, but just do conversions, nothing ELSE
# no color name matching 
.hex2rgb = function(vecHEX)
	{
	vecHEX = color.hex(vecHEX);
	color.col2rgb(vecHEX);
	}
	
	

	  
.rgb2hex = function(matrixRGB)
	{
	color.rgb2col(matrixRGB);
	}






	# http://c.mshaffer.com/js/colorpicker/functions.colors.js
	# LOOKS like he has changed the formulas to matrix form, using ORIGINAL for now...
#	http://www.brucelindbloom.com/index.html?Eqn_RGB_to_XYZ.html
#  D65 is the default,

.rgb2lab = function(matrixRGB)
	{  
	# we want it on the [0,1] scale
	if(max(matrixRGB) > 1) { matrixRGB = matrixRGB/255; }
		
	matrixLAB = convertColor( t(matrixRGB), 
					from = "sRGB", to = "Lab", scale.in = 1);
	
	rownames(matrixLAB) = colnames(matrixRGB);
	t(matrixLAB);
	}
	
.lab2rgb = function(matrixLAB)
	{
	XXX = convertColor( t(matrixLAB), 
					from = "Lab", to = "sRGB", scale.out = 255);
	# maybe a function m.as(matrix, "integer") ... 
	colnames(XXX) = c("red", "green", "blue");
	matrixRGB = m.as( round(t(XXX)), type="integer");
	matrixRGB;
	}
	
 	







	

.rgb2hsv = function(matrixRGB)
	{	
	# we want it on the [0,1] scale
	if(max(matrixRGB) > 1) { matrixRGB = matrixRGB/255; }
	
	# delete alpha if exists 
	# force input to [3 x n] matrix
	matrixHSV = keep.matrix( 0*matrixRGB[-c(4),] );
		colnames(matrixHSV) = colnames(matrixRGB);
		rownames(matrixHSV) = c("h","s","v");
	n = ncol(matrixHSV);
	
	for(i in 1:n)
		{
		one = matrixRGB[,i];
			r = one[1];
			g = one[2];
			b = one[3];		
		one.min = min(r,g,b);
		one.max = max(r,g,b); 					v = one.max;
		
		one.range = one.max - one.min;
		one.mid = (one.min + one.max) / 2;		l = one.mid;
			
		if(one.range == 0) # we have GRAY?
			{
			s = 0;
			h = 0;
			} else 	{
					# this line is only difference with rgb2hsl
					# l is one.mid, v = one.max
					s = one.range / one.max;
					# s = if(one.mid < 0.5) { one.range / (one.max + one.min)} else {one.range / (2-one.max-one.min); }

					useOnce = FALSE;
						deltaR = ((one.max - r / 6) + (one.range /2))/one.range;
						deltaG = ((one.max - g / 6) + (one.range /2))/one.range;
						deltaB = ((one.max - b / 6) + (one.range /2))/one.range;
					if(one.max == r && !useOnce) { h = deltaB - deltaG; useOnce = TRUE; }
					if(one.max == g && !useOnce) { h = (1/3) + deltaR - deltaB; useOnce = TRUE; }
					if(one.max == b && !useOnce) { h = (2/3) + deltaG - deltaR; useOnce = TRUE; }
					if(h < 0) { h = h + 1;}
					if(h > 1) { h = h - 1;}
					}

			l = one.mid;
			h = 360*h; # 360 degrees in a circle

		matrixHSV[,i] = c(h,s,v);
		}
	matrixHSV;	
	}

  


	
	
.hsv2rgb = function(matrixHSV)
	{
	# ### SEEMS TO BE A BUG on "blue"
		
	# force input to [3 x n] matrix
	matrixRGB = keep.matrix( 0*matrixHSV );
		colnames(matrixRGB) = colnames(matrixHSV);
		rownames(matrixRGB) = c("r","g","b");
	
	n = ncol(matrixRGB); 
	
	for(i in 1:n)
		{
		hsv = matrixHSV[,i];

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

		r = as.integer( 255*r );
		g = as.integer( 255*g );
		b = as.integer( 255*b );
		
		matrixRGB[,i] = c(r,g,b);
		}
	matrixRGB;	
	}







.rgb2cmyk = function(matrixRGB)
	{
	# we want it on the [0,255] scale
	if(max(matrixRGB) <= 1) { matrixRGB = matrixRGB*255; }
		
	# delete alpha if exists 
	# force input to [3 x n] matrix
	matrixCMYK = keep.matrix( 0*matrixRGB[-c(4),] );
	matrixCMYK = rbind(matrixCMYK, matrixCMYK[3,]);
		colnames(matrixCMYK) = colnames(matrixRGB);
		rownames(matrixCMYK) = c("c","m","y","k");
	n = ncol(matrixCMYK);
	for(i in 1:n)
		{
		one = matrixRGB[,i];
			r = one[1];
			g = one[2];
			b = one[3];		
		# RGB -> CMY
				C = 1 - ( r / 255 );
				M = 1 - ( g / 255 );
				Y = 1 - ( b / 255 );
			
		# CMY -> cmyk
				vK = 1;
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
		matrixCMYK[,i] = c(c,m,y,k);
		}
	matrixCMYK;	
	}




.cmyk2rgb = function(matrixCMYK)
	{
	matrixRGB = keep.matrix( 0*matrixCMYK[-c(4),] ); 
		colnames(matrixRGB) = colnames(matrixCMYK);
		rownames(matrixRGB) = c("r","g","b");
	n = ncol(matrixRGB);
	for(i in 1:n)
		{
		cmyk = matrixCMYK[,i];

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

		r = as.integer( 255*r );
		g = as.integer( 255*g );
		b = as.integer( 255*b );
		
		matrixRGB[,i] = c(r,g,b);
		}
	matrixRGB;	
	}


 



color.convert = function(..., from="hex", to="cmyk")
	{
	x = prep.dots(..., default="#c8008c");
	 
	
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.FROM = check.type(from);
	if(!ct.FROM || !is.character(from))	
		{ from = deparse(substitute(from)); } 
##########################################################



##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.TO = check.type(to);
	if(!ct.TO || !is.character(to))	
		{ to = deparse(substitute(to)); } 
##########################################################

	# first to RGB everything ... 	
	FROM 	= prep.arg(from, n=3, case="upper");
	TO 		= prep.arg(to,   n=3, case="upper");
	 
	RGB = switch(FROM,					  			
					  "CMY" = .cmyk2rgb(x), 	# CMY
					  "HSL" = .hsl2rgb(x), 		# HSL
					  "HSV" = .hsv2rgb(x),		# HSV
					  "HEX"	= .hex2rgb(x),		# HEX	
					  "LAB"	= .lab2rgb(x),		# HEX	
				x	# DEFAULT # RGB
				);
	OUT = switch(TO,					  			
					  "CMY" = .rgb2cmyk(RGB), 	# CMY
					  "HSL" = .rgb2hsl(RGB), 	# HSL
					  "HSV" = .rgb2hsv(RGB),	# HSV
					  "HEX"	= .rgb2hex(RGB),	# HEX
					  "LAB"	= .rgb2lab(RGB),	# HEX 
				RGB	# DEFAULT # RGB
				);
	OUT;
	}	






# hsl2rgb = function(matrixHSL)
# rgb2hsl = function(matrixRGB)
# hex2rgb = function(vecHEX)
## ... really only works on hex unless my dots add to [updates] and can merge MATRICES



# color.convert = function(..., from="hex", to="cmyk")

############################################################
############################### HELPER FUNCTIONS ###########
############################################################


color.buildHelperFunctions = function()
	{	
	choices = c("rgb", "hsl", "hsv", "hex", "lab", "cmyk");
	###if they don't exist ... COMBOS ... PRIVATE/PUBLIC [.]
	n = length(choices);
	for(i in 1:n)
		{
		for(j in 1:n)
			{
			f = tolower(choices[i]); F = toupper(f);
			s = tolower(choices[j]); S = toupper(s);
			if(f != s)
				{
				row = '{f}2{s} = function(...) { color.convert(..., from="{F}", to="{S}"); }';
				
				row = str.replace(c("{f}", "{s}", "{F}", "{S}"), c(f,s,F,S), row);	

				.cat(row);	 
				}
			
			}
		}
	}
	
	
############## OUTPUT HERE (COPY/PASTE CURRENTLY) ###########



rgb2hsl = function(...) { color.convert(..., from="RGB", to="HSL"); } 

rgb2hsv = function(...) { color.convert(..., from="RGB", to="HSV"); } 

rgb2hex = function(...) { color.convert(..., from="RGB", to="HEX"); } 

rgb2lab = function(...) { color.convert(..., from="RGB", to="LAB"); } 

rgb2cmyk = function(...) { color.convert(..., from="RGB", to="CMYK"); } 

hsl2rgb = function(...) { color.convert(..., from="HSL", to="RGB"); } 

hsl2hsv = function(...) { color.convert(..., from="HSL", to="HSV"); } 

hsl2hex = function(...) { color.convert(..., from="HSL", to="HEX"); } 

hsl2lab = function(...) { color.convert(..., from="HSL", to="LAB"); } 

hsl2cmyk = function(...) { color.convert(..., from="HSL", to="CMYK"); } 

hsv2rgb = function(...) { color.convert(..., from="HSV", to="RGB"); } 

hsv2hsl = function(...) { color.convert(..., from="HSV", to="HSL"); } 

hsv2hex = function(...) { color.convert(..., from="HSV", to="HEX"); } 

hsv2lab = function(...) { color.convert(..., from="HSV", to="LAB"); } 

hsv2cmyk = function(...) { color.convert(..., from="HSV", to="CMYK"); } 

hex2rgb = function(...) { color.convert(..., from="HEX", to="RGB"); } 

hex2hsl = function(...) { color.convert(..., from="HEX", to="HSL"); } 

hex2hsv = function(...) { color.convert(..., from="HEX", to="HSV"); } 

hex2lab = function(...) { color.convert(..., from="HEX", to="LAB"); } 

hex2cmyk = function(...) { color.convert(..., from="HEX", to="CMYK"); } 

lab2rgb = function(...) { color.convert(..., from="LAB", to="RGB"); } 

lab2hsl = function(...) { color.convert(..., from="LAB", to="HSL"); } 

lab2hsv = function(...) { color.convert(..., from="LAB", to="HSV"); } 

lab2hex = function(...) { color.convert(..., from="LAB", to="HEX"); } 

lab2cmyk = function(...) { color.convert(..., from="LAB", to="CMYK"); } 

cmyk2rgb = function(...) { color.convert(..., from="CMYK", to="RGB"); } 

cmyk2hsl = function(...) { color.convert(..., from="CMYK", to="HSL"); } 

cmyk2hsv = function(...) { color.convert(..., from="CMYK", to="HSV"); } 

cmyk2hex = function(...) { color.convert(..., from="CMYK", to="HEX"); } 

cmyk2lab = function(...) { color.convert(..., from="CMYK", to="LAB"); } 

















































	