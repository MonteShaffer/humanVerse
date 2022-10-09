

color.default = function(distinct = TRUE, type="RGB", scale.RGB = TRUE)
	{
	TYPE 	= prep.arg(type, n=3, keep="-", case="upper");
	mkey 	= .MD5( paste0(as.character(distinct),
						TYPE, as.character(scale.RGB )) );
						
	df		= memory.get(mkey, "-COLORS-");
	if(!is.null(df)) { return(df); } 
	
	colors	= sort(as.character( colors(distinct = distinct) ));
	n 		= length(colors);	
	RGB 	= color.col2rgb(colors); 
	XXX = NULL;
	TYPES = v.remove( str.explode("-", TYPE), "RGB" );
	if(INN(TYPES))
		{
		XXX = list();
		# may have multiple, I want cmyk, hsl 
		nt = length(TYPES);
		for(i in 1:nt)
			{
			XXX[[i]] = color.convert(RGB, from=RGB, to=TYPES[i]);
			}
		}
		 
	HEX 	= as.character( color.rgb2col(RGB) );
	# HEX = color.convert(RGB, from="RGB", to="HEX");
	
	if(scale.RGB) { RGB = RGB / 255; } 
	
	rtype = typeof(RGB);
	
	df = dataframe( cbind( colors, HEX ) );
	df = cbind(df,	as.type(RGB[1,], type=rtype), 
					as.type(RGB[2,], type=rtype), 
					as.type(RGB[3,], type=rtype) );

	xnames = NULL;
	if(INN(XXX))
		{
		xnames = NULL;
		for(i in 1:nt)
			{
			rxnames = rownames(XXX[[i]]);
			xnames = c(xnames, rxnames);
			nj = length(rxnames);
			njt = typeof(XXX[[i]][1,]);
			for(j in 1:nj)
				{
				df = cbind(df, as.type(XXX[[i]][j,], type=njt));
				}
			}
		}
				
	rownames(df) = 1:n;
	colnames(df) = c("color", "hex", 
						"red", "green", "blue", xnames);
	
	df = property.set("md5", df, mkey);
	memory.set(mkey, "-COLORS-", df);
	df; 
	}
	 
color.baseHEX = function() {}	
color.baseHEX = function(cnames = c("mediumvioletred", "deeppink", "deeppink2", "deeppink3", NA), B = color.default())
	{
	idx = set.match(cnames, B$color);
	B$hex[idx];
	}
	 
color.nearest = function() {}  
# C0FFEE
color.nearest = function(aHEX="#c8008c", B = color.default(type="CMYK-HSL"), n=5, return="best")
	{
	n = as.integer(n); 
	# return == "best" ... or "everything""
	RETURN = prep.arg(return, n=1);
.cat("Dimension of B: ", dim(B) );
	akey = toupper( str.replace("#","", aHEX) );
	bkey = property.get("md5", B);
	# keep a memory ...  "89b284a63613278c9da2506f6ce43324"
	mkey = .MD5( paste0( c(n, akey, B$hex, bkey), collapse="") );
	res = memory.get(mkey, "-COLOR-NEAREST-");
	if(!is.null(res)) 
		{ 
		.cat("Input was: ", aHEX);
		if(RETURN == "b")
			{
			best = property.get("best", res); 
			return(best);			
			}		
		return(res); 
		}
	
	
	# first see if it is the set ...
	r = s = NULL;
	idx = v.which(B$hex, toupper(aHEX));
	if(!is.null(idx)) 
		{ 
		r = B$color[ idx ]; 
		s = rep(1, length(idx));
		}
	r = v.fill(r, to.length=n, with=NA);
	s = v.fill(s, to.length=n, with=NA);	
	
	
	nb = df.getColumnTypes(B);
	logic = (nb %in% c("integer", "double"));
	nms = colnames(B);
	
	b = as.matrix( B[, logic] );
		rownames(b) = B$color;
		colnames(b) = nms[logic];
	
	# singleton RGB 
	a = as.matrix( t(.hex2rgb(aHEX)) );	
		anames = c("red", "green", "blue");
	a = a/255;
	
	NMS = paste0(nms[logic], collapse="");
	if(str.contains("cmyk", NMS))
		{		
		cmyk = as.matrix( t(hex2cmyk(aHEX)) );
		a = cbind(a, cmyk);
		anames = c(anames, "c","m","y","k");		
		}
	if(str.contains("hsl", NMS))
		{		
		hsl = as.matrix( t(hex2hsl(aHEX)) );
		a = cbind(a, hsl);
		anames = c(anames, "h","s", "l");
		}		
	colnames(a) = anames;	
	
a %GLOBAL%.
b %GLOBAL%. 
 	
	cs = cosine.similarity(a,b);
	xs = .sort(cs, "DESC");
	# ("black as 0,0,0") for cosine.similarity ... TROUBLE 
	# maybe add a "fudge.factor" 
	# "black" is unique, will get picked up on "exact" and "dist"
	# "white" will match all GREAY, ... angle on the wheel ...
	rcs = names(xs);
	scs = as.numeric(xs);
	# shouldn't need this... we will truncate at END 
	rcs = v.fill(rcs, to.length=n, with=NA);
	scs = v.fill(scs, to.length=n, with=NA);
		
		
	
	# expensive doing all pairwsie ... 
	## I need a vec/matrix like cs ... 
	## or generic distance that figures it out ...
	## euclidean.norm?
	di = matrix.dist( rbind(a,b) , method="euclidean");  
	do = di[,1]; # row or column 
	me = do[1]; dis = do[-c(1)];
		xd = .sort(dis, "ASC");
	
	rdi = names(xd);
	sdi = as.numeric(xd);
	
	# shouldn't need this... we will truncate at END 
	rdi = v.fill(rdi, to.length=n, with=NA);
	sdi = v.fill(sdi, to.length=n, with=NA);
	
	# cs = math.cleanup(cs);
	# dis = math.cleanup(dis);
# .cat("MONTE");
# dput.one(cs);  
cs %GLOBAL% .; 
# .cat("ALEX");
# dput(dis);		
dis %GLOBAL% .; 


	# struggles with YELLOW ... RG = Y 
	# or colors() is biased against yellow ? 
	# add cmyk and hsl ... 10 dimensions of color ... 
	
	# do a weighting, to get a final score
	# we shouldn't have any NA's any more with BLACK (cmyk)
	logic = (cs == 1);
	logic = v.TO(logic, NA, FALSE);
	
	logic = (cs == 1);
	logic = v.TO(logic, NA, FALSE);  
	
	fsc 		= dis * 100*100;
	fsc[!logic] = (1-cs[!logic]) * dis[!logic] * 100*100*100; 
	 
	# fsc = 100* fsc / (2^(8*3)) / 22;
	
	fsc = fsc / (2^(8*3)) / 22;  # max is about 0.95
	
	fsc = 296239876 * fsc / 122215;
	
	fsc = 10^8 * fsc / 592;
	fsc = 1.0013428 * pi * fsc * 2 / 100265080;
	fsc = 7592 * fsc / 23456.789 / 108;
	fsc = 10^9 * fsc / 1.03231973;  # #91FF25
	fsc = fsc / 1.091322;
	
	fsc = fsc / 3.2575884 / 1.000001550; 
	fsc = fsc / 100.0013652422;
	fsc = 5*fsc;
	
	
	
	
	
	
	# max dis is 360?
	
	# black, reverse  
	# lightpink1 351858781, lightpink3 351500101
	# white 2236068

	
	CF = 25;  			# cos.sim weights more 
	WF = 1/5; 			# overall scale 
	
	# lol, CFalls, WhiteFish
	
	#fsc 		= dis;
	#fsc[!logic] = (1-cs[!logic]) + dis[!logic] + (100*100*100*CF * (1-cs[!logic]) * dis[!logic]); 
	
	# fsc = CF * (1-cs) + dis + (100*100*100*CF * (1-cs) * dis); 
		
	# fsc = CF*cs + (1-dis) + (100*100*100*CF * (1-cs) * dis );
	# fsc = CF*cs + (1-dis);
	# fsc = (1-cs) + CF*dis;
	# fsc = CF * cs + 1/(dis+DEFAULT_TOLERANCE);
	
	# make absolute, so comparable across lookups 
	# fsc = 1 - fsc * WF + 1/WF;
	# fsc = v.TO(fsc, is.negative(fsc), 0);
	
		fd = .sort(fsc, "ASC");
	rfi = names(fd);
	sfi = as.numeric(fd);
	## still not helping on "GRAYS" ... 
	
	
	# not 'on' as RESERVED??
	on = length(r);  # if it is larger than n, so be it ...
	
	r = r[1:on]; s = s[1:on]; h = color.baseHEX(r, B);
	rcs = rcs[1:on]; scs = scs[1:on]; hcs = color.baseHEX(rcs, B);
	rdi = rdi[1:on]; sdi = sdi[1:on]; hdi = color.baseHEX(rdi, B);
	rfi = rfi[1:on]; sfi = sfi[1:on]; hfi = color.baseHEX(rfi, B);
	
	
	df = dataframe( cbind(h, r) );
	df = cbind( df, s, 
					hcs, rcs, scs, # round(scs, 3), 
					hdi, rdi, sdi, # round(sdi, 3),
					hfi, rfi, sfi  # round(sfi, 5)
					);
	
	colnames(df) = c(	"hex.m", "match", 	"sim", 
						"hex.c", "cosine",  	"sim", 
						"hex.e", "euclidean", "dist",
						"hex.h", "humanVerse",  "HVscore"
						);
			# there is no maroon5 ...
			# Input was:  #A800A8 ... n = 33 
			# 
	h.best = unique(c(v.TO(df$hex.m, NA, NULL), df$hex.h));
	n.best = unique(c(v.TO(df$match, NA, NULL), df$humanVerse));
	hV 	   = df$HVscore;
	
	# lower = R.colors ... R and HTML (140)  
	best = n.best[1:on];
	names(best) = h.best[1:on];
	best = property.set("HVscore", best, hV[1:on]);
	
	res = df;
	res = property.set("input.was", res, aHEX); 
	res = property.set("best", res, best); 
	
	
	
	memory.set(mkey, "-COLOR-NEAREST-", res);
	
	.cat("Input was: ", aHEX);
	if(RETURN == "b")
		{ 
		return(best);			
		}
	res;
	}


color.random = function(n=1, to="HEX", memory.key="-LAST-RANDOM-", seed=NULL)
	{
	TO = prep.arg(to, n=3, case="upper");
	min = 0; max = (2^8)^3 - 1;
		# seed, seed.set/get ... 

	
	s = seed.set(seed);
	r = rand(min, max, n=n)	
		
	
	HEX = color.hex( dec2hex( r ) );
	if(TO == "HEX")
		{
		res = HEX;
		} else {
				res = color.convert(HEX, from="hex", to=to);
				}
		
	memory.set(memory.key, "RANDOM-COLORS", res);
	res = property.set("seed", res, as.integer(s));
	minvisible(res, print=FALSE);
	return(res);
	
	return(minvisible(HEX, print=FALSE));
	}
	

color.rand = color.random;




# color.col2rgb("red","#FF9944",2,"#336699AA", alpha=TRUE);
color.col2rgb = function(..., alpha=FALSE)
	{
	colors = prep.dots(..., default="purple");
	res = grDevices::col2rgb(colors, alpha=alpha); 
	colnames(res) = colors;  # original, so inverse will know
	res;
	}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#'
#' color.rgb2col (as rgb2col)
#'
#' Reverse the built-in grDevices::col2rgb function
#' [ See grDevices::convertColor or grDevices::make.rgb ]
#'
#' @param matrixRGB matrix of RGB or RGBa colors (with names)
#' @param force.hex TRUE (returns HEX)
#'					FALSE (names if exist; HEX otherwise)
#'
#'
#' @return CharacterVector of names (if exist), hex colors if not 
#' @export
#'
#' @examples
#'
color.rgb2col = function(matrixRGB)
	{
	res = grDevices::rgb( t(matrixRGB)/255, names=colnames(matrixRGB));
	res;
	}	
	  
	  
#' @rdname rgb2col
#' @export
rgb2col = color.rgb2col; 	# also works as an inverse for the base



hexcolor.return = function(hexstr="#Dc8aa8cD", case="upper", alpha.case="lower")
	{
	# this is an INTERNAL, well formatted #FFFFFFaa or #FFFFFF
	# make it more palpable to read ... FFFFFFaa
	hlen = str.len(hexstr);
	pre = substr(hexstr, 1, 6+1); # we have it in regular format #ABCDEF
	# can I do str.subtraction? ... left side / right side ...
	alpha = substr(hexstr, 6+2, 6+3);
	
	pre = prep.case(pre, case=case);
	alpha = prep.case(alpha, case=alpha.case);
	paste0(pre,alpha);	
	}
  
# as.hexcolor = color.hex; 
hex.prepend = function(..., prepend="#")
	{
	hexstr = prep.dots(..., default="c8008c");
	hexstr = str.replace(c("O","o"),"0", hexstr); # oh's not zeroes'
	hexstr = cleanup.base(hexstr); 
	paste0(prepend, hexstr);	
	}
  
color.hex = function() {}
color.hex = function(..., 
						alpha=FALSE,
						three.to.six=TRUE,
						prepend="#", 
						case="upper"
					)
	{
	hexstr = prep.dots(..., default="cob");
dput(hexstr);
	hexstr = str.replace(c("O","o"),"0", hexstr); # oh's not zeroes'
	hexstr = cleanup.base(hexstr); 
	if(three.to.six)  #   #F0C ==> FF00CC [old school]
		{
		hlen = str.len(hexstr);
		idx = v.which(hlen, 3);
		if(!is.null(idx))
			{
			sub = hexstr[idx];
			n = length(sub);
			nsub = sub;
			for(i in 1:n)
				{
				su = sub[i];
				s = str.explode("",su);
				nsub[i] = paste0( c(s[1], s[1], s[2], s[2], s[3], s[3]), collapse="");
				}
			hexstr[idx] = nsub;			
			}
		}
	
	hexstr = str.pad(hexstr, 6, "0", "LEFT");
	
	if(alpha)         #  #FF00CC00 is same, ##FF00CC becomes #FF00CCFF
		{
		hexstr = str.pad(hexstr, 8, "F", "RIGHT");
		} else {
				# we will truncate alpha if they exist ...
				hexstr = str.truncate(hexstr, to.length=6, keep="left")
				} 
	hexstr = prep.case(hexstr,case=case);	
	hexstr = paste0(prepend, hexstr);
	hexstr;	
	}


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
				x	# DEFAULT # RGB
				);
	OUT = switch(TO,					  			
					  "CMY" = .rgb2cmyk(RGB), 	# CMY
					  "HSL" = .rgb2hsl(RGB), 	# HSL
					  "HSV" = .rgb2hsv(RGB),	# HSV
					  "HEX"	= .rgb2hex(RGB),	# HEX					  
				RGB	# DEFAULT # RGB
				);
	OUT;
	}	



# choices = c("rgb", "hsl", "hsv", "hex", "cmyk");
####if they don't exist ... COMBOS ... PRIVATE/PUBLIC [.]
# n = length(choices);
# for(i in 1:n)
	# {
	# for(j in 1:n)
		# {
		# f = tolower(choices[i]); F = toupper(f);
		# s = tolower(choices[j]); S = toupper(s);
		# if(f != s)
			# {
			# row = '{f}2{s} = function(...) { color.convert(..., from="{F}", to="{S}"); }';
			
			# row = str.replace(c("{f}", "{s}", "{F}", "{S}"), c(f,s,F,S), row);	

			# cat(row, "\n\n");	
			# }
		
		# }
	# }





# hsl2rgb = function(matrixHSL)
# rgb2hsl = function(matrixRGB)
# hex2rgb = function(vecHEX)
## ... really only works on hex unless my dots add to [updates] and can merge MATRICES



# color.convert = function(..., from="hex", to="cmyk")




rgb2hsl = function(...) { color.convert(..., from="RGB", to="HSL"); } 

rgb2hsv = function(...) { color.convert(..., from="RGB", to="HSV"); } 

rgb2hex = function(...) { color.convert(..., from="RGB", to="HEX"); } 

rgb2cmyk = function(...) { color.convert(..., from="RGB", to="CMYK"); } 

hsl2rgb = function(...) { color.convert(..., from="HSL", to="RGB"); } 

hsl2hsv = function(...) { color.convert(..., from="HSL", to="HSV"); } 

hsl2hex = function(...) { color.convert(..., from="HSL", to="HEX"); } 

hsl2cmyk = function(...) { color.convert(..., from="HSL", to="CMYK"); } 

hsv2rgb = function(...) { color.convert(..., from="HSV", to="RGB"); } 

hsv2hsl = function(...) { color.convert(..., from="HSV", to="HSL"); } 

hsv2hex = function(...) { color.convert(..., from="HSV", to="HEX"); } 

hsv2cmyk = function(...) { color.convert(..., from="HSV", to="CMYK"); } 

hex2rgb = function(...) { color.convert(..., from="HEX", to="RGB"); } 

hex2hsl = function(...) { color.convert(..., from="HEX", to="HSL"); } 

hex2hsv = function(...) { color.convert(..., from="HEX", to="HSV"); } 

hex2cmyk = function(...) { color.convert(..., from="HEX", to="CMYK"); } 

cmyk2rgb = function(...) { color.convert(..., from="CMYK", to="RGB"); } 

cmyk2hsl = function(...) { color.convert(..., from="CMYK", to="HSL"); } 

cmyk2hsv = function(...) { color.convert(..., from="CMYK", to="HSV"); } 

cmyk2hex = function(...) { color.convert(..., from="CMYK", to="HEX"); } 


































































v.cmyk = function(c,m,y,k)
	{
	matrixCMYK = as.matrix( rbind(c,m,y,k) );
	if(max(matrixCMYK) > 1) { matrixCMYK = matrixCMYK/255; }	
	rownames(matrixCMYK) = c("[c]yan", "[m]agenta", "[y]ellow", "blac[k]");
	colnames(matrixCMYK) = names(c);
	cmyk2hex(matrixCMYK);	
	}
	
v.hsv = function(h,s,v)
	{
	# length checking, parallel arrays ...
	matrixHSV = as.matrix( rbind(h,s,v) );	
		# The hue of the color specified as an angle in the range [0,360]. 0 yields red, 120 yields green 240 yields blue, etc.
		rownames(matrixHSV) = c("hue", "saturation", "value");
		colnames(matrixHSV) = names(h);
	hsv2hex(matrixHSV);
	}
v.hsl = function(h,s,l)
	{
	# length checking, parallel arrays ...
	# these are MY FORMATS of color, not *hsv* or *hcl*
	matrixHSL = as.matrix( rbind(h,s,l) );
		# The hue of the color specified as an angle in the range [0,360]. 0 yields red, 120 yields green 240 yields blue, etc.
		rownames(matrixHSL) = c("hue", "saturation", "luminance");
		colnames(matrixHSL) = names(h);
	hsl2hex(matrixHSL);
	}
	
# base::rgb requires (0,1) but col2rgb requires (0,255) ???
v.rgb = function(r,g,b)
	{
	# length checking, parallel arrays ...
	matrixRGB = as.matrix( rbind(r,g,b) );
	if(max(matrixRGB) <= 1) { matrixRGB = matrixRGB*255; }	
		rownames(matrixRGB) = c("red", "green", "blue");
		colnames(matrixRGB) = names(r);
	rgb2hex(matrixRGB);
	}
