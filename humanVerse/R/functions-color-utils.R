

color.random = function(n=1, to="HEX", seed=NULL)
	{
	n = as.integer(n);
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
	# strips properties from seed ... 
	s = as.integer(s);
.cat("Seed was: ", s);
	res = property.set("seed", res, s);
	res;
	}
	

color.col2hex = function(..., alpha=FALSE)
	{
	colors = prep.dots(..., default="purple");
	RGB = grDevices::col2rgb(colors, alpha=alpha); 
	colnames(RGB) = colors;  # original, so inverse will know
	HEX = rgb2hex(RGB);
	HEX;
	}

# inverse is matching, plus nearest on not found ...	
color.hex2col = function(...)
	{
	hex = prep.dots(..., default="#FF00FF");
	# FOR NOW, this is only INTERNAL R-BASED 
	DICTIONARY = color.col2hex(colors(distinct=TRUE));
	
	IDX = set.match(hex, DICTIONARY);
	idx.NA = v.which(IDX, NA);
	idx.good = v.which(IDX, NA, invert=TRUE);
	
	res = names(DICTIONARY)[IDX];
	if(!is.null(idx.NA))
		{
		n = length(idx.NA);
		for(i in 1:n)
			{
			h = color.nearest( hex[ idx.NA[i] ], n=1);
			res[ idx.NA[i] ] = h;
			}
		}
	res;
	}
	
# color.col2rgb("red","#FF9944",2,"#336699AA", alpha=TRUE);
color.col2rgb = function(..., alpha=FALSE)
	{
	colors = prep.dots(..., default="purple");
	res = grDevices::col2rgb(colors, alpha=alpha); 
	colnames(res) = colors;  # original, so inverse will know
	res;
	}

color.rgb2col = function(matrixRGB)
	{
	res = grDevices::rgb( t(matrixRGB)/255, names=colnames(matrixRGB));
	res;
	}
	
## maybe do v.lab ?	
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


v.HV = function(vS = c("!0", "A9", "Z9", "S9", "J9", "00", "33", "66", "99") )
	{
	# angle offset ? 3:16PM
	
	}
	
	
v.xela = function(H, L)
	{
	# H is angle in degrees , from 'Hsl'
	# L is intensity (magnitude of vector), from 'Lab'
	
	# convert .rgb2xela() ... .xela2rgb() ... 
	# add to color converter ... 
	
	matrixHV = 0;
	
	}







	

color.rand = color.random;



	
	  
	  
#' @rdname rgb2col
#' @export
rgb2col = color.rgb2col; 	# also works as an inverse for the base
















































color.return = function() {}
color.return = function(hexstr="#Dc8aa8cD", case="upper", alpha.case="lower")
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
hex.prepend = function() {}
hex.prepend = function(..., prepend="#")
	{
	hexstr = prep.dots(..., default="c8008c");
	hexstr = str.replace(c("O","o"),"0", hexstr); # oh's not zeroes'
	hexstr = cleanup.base(hexstr); 
	paste0(prepend, hexstr);	
	}
  
color.hex = function() {}
color.hex = function(..., alpha=FALSE, three.to.six=FALSE,
									prepend="#", case="upper" )
	{
	hexstr = prep.dots(..., default="cob");
# dput(hexstr);
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
color.nearest = function(aHEX="#c8008c", B = color.default(type="CMYK-HSL"), n=5, return="best", based.on="Cosine Similarity")
	{
	## matching to BLACK is BAD ...  vector of 0 0000 1 
	## 010101 fails miserably, but 050505 works ... 
	## xx = color.nearest("#050505", n=505, return="evelkjf"); head(xx); tail(xx); str(xx);
	## nature of the mathematics ... 
	n = as.integer(n); 
	# return == "best" ... or "everything""
	# based.on is just reviewing the CACHE in a different way ...
	# I don't want to do the logic at top and bottom, so ADD to CACHE ... 
	BON = prep.arg(based.on, n=1);
		CRITERIA = "humanVerse";
	if(BON %in% c("e", "d")) { CRITERIA = "Euclidean Distance"; }
	if(BON %in% c("c", "s")) { CRITERIA = "Cosine Similarity"; }


	RETURN = prep.arg(return, n=1);
.cat("Dimension of B: ", dim(B) );
	akey = toupper( str.replace("#","", aHEX) );
	bkey = property.get("md5", B);
	# keep a memory ...  "89b284a63613278c9da2506f6ce43324"
	mkey = .MD5( paste0( c(n, akey, B$hex, bkey, CRITERIA), collapse="") );
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
	
	colnames(df) = c(	"hex.m", "match", 	"is", 
						"hex.c", "cosine",  	"sim", 
						"hex.e", "euclidean", "dist",
						"hex.h", "humanVerse",  "HVscore"
						);
			# there is no maroon5 ...
			# Input was:  #A800A8 ... n = 33 
			# 


	
	
if(CRITERIA == "humanVerse")
	{
	h.best = unique(c(v.TO(df$hex.m, NA, NULL), df$hex.h));

	n.best = unique(c(v.TO(df$match, NA, NULL), df$humanVerse));
	hV 	   = df$HVscore;
	
	# lower = R.colors ... R and HTML (140)  
	best = n.best[1:on];
	best = property.set("score", best, hV[1:on]);
		alpha = 0.01/2; 
		alpha = 0.01/5/2;  # changes based on colorspace?
	best = property.set("good.match", best, (hV[1:on] < alpha) );
	
	best = property.set("color.hex", best, h.best[1:on]);
	}
if(CRITERIA == "Euclidean Distance")
	{
	h.best = unique(c(v.TO(df$hex.m, NA, NULL), df$hex.e));

	n.best = unique(c(v.TO(df$match, NA, NULL), df$euclidean));
	hV 	   = df$dist;
	
	# lower = R.colors ... R and HTML (140)  
	best = n.best[1:on];
	best = property.set("score", best, hV[1:on]);
		alpha = (3/5)^2;
	best = property.set("good.match", best, (hV[1:on] < alpha) );
	
	best = property.set("color.hex", best, h.best[1:on]);
	}	

if(CRITERIA == "Cosine Similarity")
	{
	h.best = unique(c(v.TO(df$hex.m, NA, NULL), df$hex.c));

	n.best = unique(c(v.TO(df$match, NA, NULL), df$cosine));
	hV 	   = df$sim;
	
	# lower = R.colors ... R and HTML (140)  
	best = n.best[1:on];
	best = property.set("score", best, hV[1:on]);
		alpha = 1-(5/3 / 1000 / 1000);
		alpha = 1-(1 / 1000 / 1000);
	best = property.set("good.match", best, (hV[1:on] > alpha) );
	 
	best = property.set("color.hex", best, h.best[1:on]);
	}		
	
	res = df;
	res = property.set("input.was", res, aHEX); 
	res = property.set("best", res, best); 
	res = property.set("based.on", res, CRITERIA); 
	
	
	
	memory.set(mkey, "-COLOR-NEAREST-", res);
	
	.cat("Input was: ", aHEX);
	if(RETURN == "b")
		{ 
		return(best);			
		}
	res;
	}



















 

