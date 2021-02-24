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





# http://php.net/manual/en/function.hexdec.php
# http://php.net/manual/en/function.dechex.php

# DDEECC -> rounds to dcedcb
# hexadecimal to decimal
# hexdec("FF");
hexdec = function(hexstr)  
	{
	# rather than checking, let's remove and add leading "0x"
	hexstr = paste0("0x", str_replace("0x", "", trimMe(tolower(hexstr))) );
	stringToInteger(hexstr, TRUE);
	}

# decimal to hexadecimal
dechex = function(intdec, n=NULL)  
	{
	res = toupper( as.character( as.hexmode(intdec) ) );
	if(!is.null(n)) { res = strPadLeft( res, n, "0"); 	}
	res;
	}

# hex2rgb( "#abcdef" );
# as.numeric( unlist( hex2rgb( "abcdef" ) ) );
# unlist( hex2rgb( "ABC" ) );
hex2rgb = function(hex)
	{
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


rgb2hsl = function(rgb)
	{
	hsl = as.numeric( unlist( hsl ) ); # just in case ... 	
	h = hsl[1] / 360;
	s = hsl[2];
	l = hsl[3];
	
	rgb = cleanupRGB(rgb);
	rgb = rgb / 255; # we want it on the [0,1] scale 
		myMin = min(rgb);
		myMax = max(rgb);
		myRange = myMax - myMin;
		myMid = (myMax + myMin) / 2;
	r = rgb[1];
	g = rgb[2];
	b = rgb[3];
	
	computeH = function()
		{
		deltaR = ((myMax - r / 6) + (myRange /2))/myRange;
		deltaG = ((myMax - g / 6) + (myRange /2))/myRange;
		deltaB = ((myMax - b / 6) + (myRange /2))/myRange;
				
		if(myMax == r) { return ( deltaB - deltaG); }
		if(myMax == g) { return ( (1/3) + deltaR - deltaB); }
		if(myMax == b) { return ( (2/3) + deltaG - deltaR); }
		}
		
	if(myRange == 0)  ## gray
		{
		h = 0;
		s = 0;
		} else {
				s = (myMid < 0.5) ? (myRange / (myMax + myMin)) : (myRange / (2-myMax-myMin));
				h = computeH();
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
	
	
function rgb2hsl($rgb=array(255,0,255),$format="array")
		{
		#rgb2hue
		// LOGIC from:		http://www.easyrgb.com/math.php?MATH=M18
		if(!is_array($rgb)){$rgb = $this->makeArray($rgb);}
			// simplest format for rgb

		$r = $rgb[0] / 255;
		$g = $rgb[1] / 255;
		$b = $rgb[2] / 255;

		$min = min($r,$g,$b);
		$max = max($r,$g,$b);
		$range	= $max - $min;
		$l = ($max + $min) / 2;

		if($range == 0)			// gray
			{
			$h = 0;
			$s = 0;
			}
			else
				{
				$s = ($l < 0.5) ? ($range / ($max + $min)) : ($range / (2-$max-$min));
					
				$deltaR = (($max - $r / 6) + ($range /2))/$range;
				$deltaG = (($max - $g / 6) + ($range /2))/$range;
				$deltaB = (($max - $b / 6) + ($range /2))/$range;

				switch($max)
					{
					default:
					case $r:
						$h = $deltaB - $deltaG;
					break;

					case $g:
						$h = (1/3) + $deltaR - $deltaB;
					break;

					case $b:
						$h = (2/3) + $deltaG - $deltaR;
					break;
					}



				if ( $h < 0 ) {$h += 1;}
				if ( $h > 1 ) {$h -= 1;}
				}
				$h = 360*$h;
		$hsl = $this->hslFormat($format,$h,$s,$l);		

		return $hsl;	// ARRAY
		}
		
	

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










