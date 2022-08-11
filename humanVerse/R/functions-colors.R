
# ?convertColor 
# ?colorConverter
# Conversion algorithms from http://www.brucelindbloom.com.
# ?col2rgb

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
#' rgb2col( grDevices::col2rgb("red") );
#' rgb2col( grDevices::col2rgb("red", alpha=TRUE) );
#' rgb2col( grDevices::col2rgb("#FF0000FF", alpha=TRUE) );
#' rgb2col( grDevices::col2rgb("#FF000033", alpha=TRUE) );
#' # it's UNKNOWABLE what the input was so we will later wrap a return method.
rgb2col = function(x)
  {
  # reverses grDevices::col2rgb function
  x.rgb = t(x)/255; # transpose, norm on [0,1]
  n = ncol(x.rgb);	# how many cols ... if 4 ... alpha = TRUE 
  res = grDevices::rgb( x.rgb, names=rownames(x.rgb) );
  res;
  }




# extends basic function by allowing 
	# ... as list of colors
# color.col2rgb(c("red","#FF9944",2));
# color.col2rgb("red","#FF9944",2);
color.col2rgb = function(colors, ..., alpha=FALSE)
	{
	more = unlist(list(...));
	colors = c(colors, more);
	# colors are "red", "#FF9944", or "i" (integer), as in `i=2; palette()[i];` 
	# intermixed works ... grDevices::col2rgb(c("red","#FF9944",2), alpha=TRUE);
	grDevices::col2rgb(colors, alpha=alpha);  
	}

# will map names and colors?
color.buildSet = function(setname="base", alpha=TRUE, colors=NULL)
	{
	memory.init();
	
	if(exists(setname, .GlobalEnv$.humanVerse[["colors"]][["tables"]]))
		{
		return(.GlobalEnv$.humanVerse[["colors"]][["tables"]][[setname]]);
		}
		else {
			if(setname == "base") 
				{ 
				colors = grDevices::colors()[1:10]; 
				x = col2rgb(colors);
				hexcolors = rgb2col(x); 
				
				xt = as.data.frame( t(x) );
				if(!is.set(xt$alpha)) { xt$alpha = 255; }
				xt$color = colors;
				xt$hex.color = hexcolors;
				
				## rownames(xt) = NULL;  # DOESN'T WORK
				## xt = property.set(xt, "row.names", NULL, as.null=TRUE); ## ALSO not working, 8/10/22 one of those days
				
				
				cols = c("hex.color", "color", "red", "green", "blue", "alpha");
				xt = df.setColumnOrder(xt, cols);
				
				}
			## TODO, custom palette names 
			if(is.null(colors)) 
				{ 
				stop("You need colors");
				}			
			}
	
	}

# extends basic function by allowing
	# as.names=NULL, force.nearest=TRUE, ...
	# if(is.null(as.names)) { return(res); }
	# by default, look in colors() palette, but we could look IN any named list
	# if(force.nearest), map to a NAME for each color ... is the method of search (e.g., cosine similarity)
# x = color.col2rgb("red","#FF9944",2,"#ff994466", alpha=TRUE);
color.rgb2col = function(x, 
							as.names=TRUE, 		# if TRUE, map to names
							force.nearest=TRUE, # if TRUE, force the map (return all names)
							set = "base",		# where are we looking for names?
							...					# "base" is grDevices::colors()
						)
	{
	hexstr = rgb2col(x);  # "#FF0000" "#FF9944" "#DF536B" "#FF9944"
	search = color.buildSet(set);
	
	}


if(!is.null(key))
			{
			if(exists(key, .GlobalEnv$.humanVerse[["colors"]][["lists"]]))
				{
				colvec = .GlobalEnv$.humanVerse[["colors"]][["lists"]][[key]];
				}
			}



if(exists("monte", .GlobalEnv$.humanVerse[["colors"]][["lists"]]))
				{
				print("hello");
				}
				










































































#' .color.average
#'
#'
#' This is univariate
#'
#' @param a.hex The first color 'a' in hexcolor form
#' @param b.hex The second color 'b' in hexcolor form
#'
#' @return An "average" color in hexcolor form
#' @export
#'
#' @examples
#' .color.average("#abcdef", "#123456");
.color.average = function(a.hex, b.hex)
  {
  # maybe write generic color "function", FUN = mean
  a.rgb = checkRGB(a.hex);
  b.rgb = checkRGB(b.hex);

  n.rgb = (a.rgb + b.rgb) / 2 ;
  rgb2hex(n.rgb);
  }



#' color.setOpacity
#'
#' @param hexvec color in hexform
#' @param opacity opacity level from 0 to 100
#'
#' @return a hexform in RGBa format
#' @export
#'
#' @examples
#' color.setOpacity("#abcdef", 0);
#' color.setOpacity("#abcdef", 50);
#' color.setOpacity("#abcdef", 100);
color.setOpacity = function(hexvec, opacity=100)
	{
	hexvec = checkHEX(hexvec);  # this allows "color.names"
	alpha = dechex(255 * opacity/100, n=2);
	paste0(hexvec,alpha);
  }


#' color.buildTable
#'
#' @param colvec This is the colors we wish to include in our table
#' (a subset of the function ?colors)
#' @param key This is where we cache the listname tied to colvec
#' @param save.key This is where we cache the output dataframe generated
#'
#' @return dataframe with color (name), hex.color, r, g, b
#' @export
#'
#' @examples
#' # color.buildTable();
color.buildTable = function(colvec=NULL, key=NULL, save.key=NULL)
	{
	args = getFunctionParameters(TRUE);
	# print(args);
  # we need to cache this in "last" memory
	# colvec is cached as "key"  ... auto-caching
	# the output of this function is cached as "save.key" ... must be specified to cache
	if(is.null(colvec))
		{
		if(!is.null(key))
			{
			if(exists(key, .GlobalEnv$.humanVerse[["colors"]][["lists"]]))
				{
				colvec = .GlobalEnv$.humanVerse[["colors"]][["lists"]][[key]];
				}
			}
		if(is.null(colvec)) { key = "ALL"; colvec = grDevices::colors(TRUE); }  # not found, so let's set to default 'ALL'
		}

	if(is.null(key)) { key = md5.object( colvec ); } # make a "hash-table" of an un-named list


  if(!is.character(save.key)) { save.key = md5.object(args); }
		  cat("\n", "save.key:", save.key, "\n\n");

	if(exists(save.key, .GlobalEnv$.humanVerse[["colors"]][["dataframes"]]))
			{
			return( .GlobalEnv$.humanVerse[["colors"]][["dataframes"]][[save.key]] );
	    }

	n = length(colvec);
	hvec = character(n);
	r = g = b = numeric(n);

	for(i in 1:n)
		{
		color = colvec[i];
		rgb = cleanupRGB( grDevices::col2rgb(color) );
		hvec[i] = rgb2hex( rgb );
		r[i] = rgb[1]; g[i] = rgb[2]; b[i] = rgb[3];
		}

	df = as.data.frame(cbind(colvec, hvec, r, g, b));
		colnames(df) = c("color", "hex.color", "r", "g", "b");

	df = assignColumnsTypeInDataFrame(c("r","g","b"), "numeric", df);

	df = setAttribute("key", key, df);
	df = setAttribute("save.key", save.key, df);

	.GlobalEnv$.humanVerse[["colors"]][["dataframes"]][[key]] = df; # stored in memory
  .GlobalEnv$.humanVerse[["colors"]][["dataframes"]][[save.key]] = df;


	df;
	}








#' color.nameSearch
#'
#' @param skey
#' @param colors.df
#' @param col.name
#' @param ...
#'
#' @return
#' @export
color.nameSearch = function(skey, colors.df = NULL, col.name="color", ...)
	{
	# color "search" by wildcard
	# cache color tables (such as wheels)
	# this caching is WEIRD
	if(is.null(colors.df))
		{
		colors.df = color.buildTable();
		} else {
				# key in color memory
				if(is.character(colors.df))
					{
					if(exists(colors.df, .GlobalEnv$.humanVerse[["colors"]][["dataframes"]]))
						{
						colors.df = .GlobalEnv$.humanVerse[["colors"]][["dataframes"]][[colors.df]];
						}
					}
				}
	if(!is.data.frame(colors.df)) { stop("humanVerse::color.nameSearch ... 'colors.df' not correctly specified"); }

	res = wildcardSearch(skey, col.name, ..., df=colors.df);
	res;
	}










#' color.findNearestName
#'
#' @param hex
#' @param how.many
#' @param scale.me
#' @param how
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
color.findNearestName = function(hex, how.many = 1, scale.me = TRUE, how="cosine", ...)
	{
	# ALMOST WORKING CORRECTLY ...
	# color.findNearestName("#8B8378", how.many = 12, scale.me = TRUE);

	hex = checkHEX(hex);
	  if(is.null(hex)) { return (NULL); }

	# we need a caching mechanism on this ...
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
			x = setAttribute("hex", rgb2hex(grDevices::col2rgb( res )), x);
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
			x = setAttribute("hex", rgb2col(grDevices::col2rgb( names(res) )), x);
		return (x);
		}
	if(how == "distance")
		{
		X.d = stats::dist( Xs, method="euclidean");
		X.m = round( as.matrix(X.d), 2);
		X.df = as.data.frame( X.m );
			colnames(X.df) = rownames(X.df) = df$color;

		one = X.df[,1]; names(one) = df$color;
		one = one[-c(1)];  # first is "self"
		one = sort(one);

		res = one[1:how.many];
		x = names(res);
			x = setAttribute("match", "distance", x);
			x = setAttribute("distance", as.numeric( res ), x);
			x = setAttribute("hex", rgb2col(grDevices::col2rgb( names(res) )), x);
		return (x);
		}
	if(how == "cosine")
		{
	  vsearch = Xs[1,];
	      vsearch = setAttribute("name", df$color[1], vsearch);
	      # names(vsearch) = df$color[1];
		vcolors = Xs[-c(1),];
		    names(vcolors) = rownames(vcolors) = df$color[-c(1)];

		vCosine = computeCosineSimilarity(vsearch, vcolors);
		  vCosine$name = df$color;
		  rownames(vCosine) = df$color;
		vCosine = assignColumnsTypeInDataFrame(c("cosine.similarity"), "numeric", vCosine);

		# vsearch = Xs[1,];
		# vcolors = Xs[-c(1),];

	  # https://stackoverflow.com/questions/1746501/
	  # a = c(2,1,0,2,0,1,1,1)
	  # b = c(2,1,1,1,1,0,1,1)
	  # d = (a %*% b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))

	  ## OR

	  # e = crossprod(a, b) / (sqrt(crossprod(a, a)) * sqrt(crossprod(b, b)))


		# v.cos = cosine(vsearch, vcolors); # raw no distance or SVD

		# https://stackoverflow.com/questions/18946966/
		# X.d = stats::dist( Xs, method="euclidean");
		# X.m = round( as.matrix(X.d), 2);
		# X.df = as.data.frame( X.m );
		# 	colnames(X.df) = rownames(X.df) = df$color;




		## library(lsa); # very slow, replace
		#X.cos = as.data.frame( 1 - round( cosine(X.m),4 ) );
		#	colnames(X.df) = rownames(X.df) = df$color;

		#one = X.cos[,1]; names(one) = df$color;


		## similarity is closeness, distance is farness
		one = 1 - vCosine$cosine.similarity;  names(one) = df$color;
		one = one[-c(1)];   # first is "self"

		one = sort(one);  # what if I have negative values ?

		res = one[1:how.many];
		x = names(res);
			x = setAttribute("match", "cosine", x);
			x = setAttribute("distance", as.numeric( res ), x);
			x = setAttribute("hex", rgb2col(grDevices::col2rgb( names(res) )), x);
		return (x);
		}
	}













#' color.chromatics
#'
#' @param rgb
#' @param n
#' @param save.key
#'
#' @return
#' @export
#'
#' @examples
color.chromatics = function(rgb, n = 12, save.key = NULL) # mono steps of monochronic ... half on "white" / half on "black"
	{
	args = getFunctionParameters(TRUE);
	# print(args);
	# we need to cache this in "last" memory
	# accessor can get elements without having to rebuild
	if(!is.null(save.key))
		{
		if(!is.character(save.key)) { save.key = md5.object(args); }
		cat("\n", "save.key:", save.key, "\n\n");
		if(exists(save.key, .GlobalEnv$.humanVerse[["colors"]][["dataframes"]]))
			{
			return( .GlobalEnv$.humanVerse[["colors"]][["dataframes"]][[save.key]] );
			}
		}

	if(length(rgb) == 1) { rgb = hex2rgb(rgb); } # they can pass in "hex"
	hex = rgb2hex(rgb);

	n2 = ceiling(n/2);  # 11 will do 13 ... original doesn't count ...
	res = c( color.colorsInGradient(n2+1, c("#FFFFFF", hex)),
	            hex,
	         color.colorsInGradient(n2+1, c(hex, "#000000"))
	        );
	res = unique(res);

	which.hex = which(res == hex)[1];
	idx = -1*( 1:length(res) - which.hex );

	df = as.data.frame(cbind(idx, res));
		colnames(df) = c("index", "hex.color");
		df = setAttribute("hex", hex, df);
		df = setAttribute("rgb", unlist(rgb), df);

		df = setAttribute("save.key", save.key, df);
	# cache it ...
	if(!is.null(save.key))
		{
		.GlobalEnv$.humanVerse[["colors"]][["dataframes"]][[save.key]] = df;
		}


	df;
	}



#' color.buildWheel
#'
#' @param rgb
#' @param wheel.steps
#' @param find.names
#'
#' @return
#' @export
#'
#' @examples
color.buildWheel = function(rgb, wheel.steps = 12, find.names=FALSE)  # wheel steps needs to be divisible by 360?
	{
	## http://c.mshaffer.com/js/colorpicker/functions.colors.js
	# we need to cache this in "last" memory
	# accessor can get elements without having to rebuild
	# - complement, split, split-complement, triad, square, rectangle, and so on ...

	if(length(rgb) == 1) { rgb = hex2rgb(rgb); } # they can pass in "hex"
	hex = rgb2hex(rgb);

	hsl = rgb2hsl(rgb);
	hsl = as.numeric( unlist( hsl ) );
		h = hsl[1];
		s = hsl[2];
		l = hsl[3];

	res = character(wheel.steps);
	names = character(wheel.steps);
	myh = numeric(wheel.steps);
	deg = numeric(wheel.steps);
	one.step = 360 / wheel.steps;
	degrees = 0;
	  my.hex = rgb2hex(rgb); # starting color ... this is also the ending color, but fine
	  if(find.names)
	    {
	    my.name = color.findNearestName(my.hex);
	    if(getAttribute("match",my.name) != "exact") { my.name = paste0("~",my.name); }
	    } else { my.name = my.hex; }
	res[1] = my.hex;
	names[1] = my.name;
	myh[1] = h;
	deg[1] = degrees;
	for(i in 2:wheel.steps)
		{
		degrees = degrees + one.step;
		h2 = as.integer( round( one.step + h ) );
			h2 = h2 %% 360;
			# update hsl
			hsl[1] = h2;

		my.hex = rgb2hex( hsl2rgb(hsl) );
		if(find.names)
	    {
	    my.name = color.findNearestName(my.hex);
	    if(getAttribute("match",my.name) != "exact") { my.name = paste0("~",my.name); }
	    } else { my.name = my.hex; }
		my.name = color.findNearestName(my.hex);
	    if(getAttribute("match",my.name) != "exact") { my.name = paste0("~",my.name); }
		res[i] = my.hex;
	  names[i] = my.name;
		deg[i] = degrees;
		h = h2;
		myh[i] = h;
		}

	df = as.data.frame(cbind(deg, res, names, myh));
		colnames(df) = c("degrees", "hex.color", "color.names", "wheel");

		df = setAttribute("hex", hex, df);
		df = setAttribute("rgb", unlist(rgb), df);


	df;
	}




#' color.webSafeHEX
#'
#' @param rgb
#'
#' @return
#' @export
#'
#' @examples
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

#' color.randomHEX
#'
#' @param n
#' @param my.seed
#' @param key
#'
#' @return
#' @export
#'
#' @examples
color.randomHEX = function(n=1, my.seed=NULL, key=NULL)
	{
	if(!is.null(key))
		{
		if(exists(key, .GlobalEnv$.humanVerse[["colors"]][["random"]]))
			{
			my.seed = .GlobalEnv$.humanVerse[["colors"]][["random"]][[key]];
			}
		} else { key = "last"; }

	if(is.null(my.seed))
		{
		setSeed(NULL,"color"); my.seed = getSeed("color");
		}
	.GlobalEnv$.humanVerse[["colors"]][["random"]][[key]] = my.seed;
	setSeed(my.seed,"color"); my.numbers = rand(2^1 - 1, (2^8)^3 - 1, n);

	my.hexs = paste0("#",dechex(my.numbers, n=6));
	my.hexs;
	}


#' color.randomRGB
#'
#' @param n
#' @param my.seed
#' @param key
#'
#' @return
#' @export
#'
#' @examples
color.randomRGB = function(n=1, my.seed=NULL, key=NULL)
	{

	my.hexs = color.randomHEX(n=n, my.seed=my.seed, key=key);
	rgb = hex2rgb( my.hexs );
	rgb = cleanupRGB(rgb);

	rgb;
	}


























#' color.roundHEX
#'
#' @param rgb
#' @param n
#' @param full
#'
#' @return
#' @export
#'
#' @examples
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
				hexV[2] = dechex(decV[2], n=1);
				hexV[4] = dechex(decV[4], n=1);
				hexV[6] = dechex(decV[6], n=1);

				hex = paste0("#", paste(hexV, collapse=""));
				hex;
				}
	}


#' color.plotWheel
#'
#' @param df
#' @param harmony
#'
#' @return
#' @export
#'
#' @examples
color.plotWheel = function(df = color.buildWheel("red"), harmony="all")
  {

  df = assignColumnsTypeInDataFrame(c("degrees","wheel"), "numeric", df);
  # subset df based on elements that fit with harmony value
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


#' color.displayColorOptions
#'
#' @param my.colors
#' @param showHEX
#' @param alpha
#' @param xlim
#' @param ylim
#' @param cex
#' @param ncol
#' @param nrow
#'
#' @return
#' @export
#'
#' @examples
color.displayColorOptions = function(my.colors = grDevices::colors(),
                              showHEX = FALSE,
                              alpha = TRUE, # works with showHEX = TRUE
                              xlim=c(0,10),
                              ylim=c(0,10),
                              cex = 0.75,
                              ncol=2, nrow=10)
  {
  # http://www.sthda.com/english/wiki/colors-in-r

  ## build a collection of graphs, each two columsn ...
  ## limit the total number per row of a column ... pagination ...
  ## how to let them copy it?  identify? clipboard?
  nc = length(my.colors);
  per.page = ncol*nrow;
  pages = ceiling(nc / per.page);

  i = 1;
  page = 1;

  xunit = diff(xlim) / ncol;
  yunit = diff(ylim) / nrow;

  while(page <= pages)
    {
    xstart = xlim[1];
    ystart = ylim[2];
    graphics::plot.new( );
    graphics::plot.window(
                xlim=xlim,
                ylim=ylim,
                log="",
                graphics::par(mar=c(0.25, 0.25, 0.25, 0.25))

              );

    for(c in 1: ncol)
      {
      xleft = xstart + (c-1) * xunit;
      ytop  = ystart;
      for(r in 1: nrow)
        {
        mycolor = as.character(my.colors[i]);
        if(is.na(mycolor)) { break; break; }
        mycolor.name = names(my.colors)[i];

        hexcolor = rgb2col( grDevices::col2rgb(mycolor, alpha=alpha)  );

        if(is.null(mycolor.name)) { mycolor.name = mycolor;}

        if(!showHEX)
          {
          mycolor.label = paste0(mycolor.name, "  ...  ", i);
          } else {
                  mycolor.label = paste0(mycolor.name, "  .  ", i ,
                          "  .  ", hexcolor   );
                  }




        xright  = xleft + xunit;
        ybottom = ytop - yunit;

        graphics::rect(xleft, ybottom, xright, ytop, col=hexcolor);  # hexcolor is safer

          top.y = mean(c(ytop,ytop,ytop,ybottom));
        graphics::text(xleft, top.y, label=mycolor.label,
                              cex=cex, pos=4, col="black");
          bottom.y = mean(c(ytop,ybottom,ybottom,ybottom));
        graphics::text(xleft, bottom.y, label=mycolor.label,
                              cex=cex, pos=4, col="white");

        i = 1 + i;
        ytop = ybottom;
        }
      ytop  = ystart;
      }
    page = 1 + page;
    }
  }





################### HELPER FUNCTIONS ###################


#' color.colorsInGradient
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
#' color.colorsInGradient(4, c("red", "royalblue"));
#' color.colorsInGradient(4, c("#FF000000", "#FF0000FF"), TRUE);  # red through alphas
#' color.colorsInGradient(4, c("#FF000000", "#4169E1FF"), TRUE);
#'
color.colorsInGradient = function(n, colvec=c("red","royalblue"), alpha=FALSE)
  {
  # alpha doesn't seem to work as expected ... unless I pass in RGBa?
  grDevices::colorRampPalette(colvec, alpha=alpha)(n);
  }














#' stringToInteger
#'
#' @param strvec Vector of strings
#' @param isHEX Are we looking at hexadecimal values
#'
#' @return vector of integers
#' @export
#'
#' @examples
#' stringToInteger( c("0xff", "077", "123"), FALSE );
#' stringToInteger( c("0xff", "077", "123"), TRUE );
stringToInteger = function(strvec, isHEX = FALSE)
	{
  # as.hexmode(333)
  # base::strtoi
  # strtoi(c("0xff", "077", "123"))  # string to integer
  # For decimal strings as.integer is equally useful.
	if(isHEX)
		{
		strtoi( tolower(strvec) );  # we could have other "base 10"?
		} else {
				as.integer(strvec);
				}
	}







# DDEECC -> rounds to dcedcb
# hexadecimal to decimal
# hexdec("FF");
# alias hex2dec


#' hexdec
#'
#' Converts a hexadecimal to decimal
#'
#' @param hexstr vector of one or more hex values as a string
#' @param ... vector of one or more hex values as a string
#'
#' @return vector of one or more integer values
#' @export
#' @alias hex2dec
#'
#' @examples
#' hexdec("FF");
#' hexdec("0xFFFF");
#' hexdec("0xFFFF", "#FF");
hexdec = function(hexstr, ...)
	{
  # http://php.net/manual/en/function.hexdec.php
  # http://php.net/manual/en/function.dechex.php
  # java conversions:: http://www.cs.rit.edu/~ncs/color/t_convert.html
  #	http://www.easyrgb.com/math.php?MATH=M19#text19

	more = unlist(list(...));
	hexstr = c(hexstr, more);

	# if it has "color" pre-pend, remove it ...
	hexstr = str_replace("#", "", hexstr);
	# rather than checking, let's remove and add leading "0x"
	hexstr = paste0("0x", str_replace("0x", "", trimMe(tolower(hexstr))) );
	stringToInteger(hexstr, TRUE);
	}




#' dechex
#'
#' Converts a decimal to hexadecimal
#'
#' @param intdec vector of one or more integer values
#' @param ... vector of one or more integer values
#' @param n Should we prepend some zeroes, if so how many?
#' @param hash Should we pre..pre-pend the "#" for colors?
#'
#' @return vector of one or more hex values as a string
#' @export
#' @alias dec2hex
#'
#' @examples
#' dechex(123,255,50, n=2, hash=FALSE);
#' dechex(16581375,12581375,50, n=6, hash=TRUE);
#' dechex(16581375,12581375,50, hash=FALSE);
#' dechex(255,133,50, hash=FALSE);
dechex = function(intdec, ..., n=NULL, hash=FALSE)
	{
	more = unlist(list(...));
	intdec = c(intdec, more);

	res = toupper( as.character( as.hexmode( as.integer( round(intdec) ) ) ) );
	# if the vector already has two-character mode ... dechex( 0:255);  ... n is not necessary
	if(!is.null(n)) { res = strPadLeft( res, n, "0"); 	}
	if(hash) { res = paste0("#",res); }
	res;
	}




# source('C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-colors.R')
# source('C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-str.R')
# hex2rgb("monte");
# hex2rgb("red");
# hex2rgb( "#abcdef" );
# as.numeric( unlist( hex2rgb( "abcdef" ) ) );
# unlist( hex2rgb( "ABC" ) );


#' hex2rgb
#'
#' Converts a hexform of color to rgb form
#' Input can be of RGB or RRGGBB hexadecimal form
#' Input can also be a named color
#'
#' @param hexvec vector of one or more hexforms of color
#' @param ... vector of one or more hexforms of color
#'
#' @return list of one or more rgb forms of color
#' @export
#'
#' @examples
#' hex2rgb("#FF0000");
#' hex2rgb(c("#FF0000", "#00FF00"));
#' hex2rgb("#FF0000", "#FFFFFF");
#'
#'
#' hex2rgb("red");
#' hex2rgb(c("red", "green"));
#' hex2rgb("red", "white");
#'
#' hex2rgb("alex");
#'
#' as.numeric( unlist( hex2rgb( "abcdef" ) ) );
#' unlist( hex2rgb( "ABC" ) );
#'
hex2rgb = function(hexvec, ...)
	{
	more = unlist(list(...));
	hexvec = c(hexvec, more);

	m = length(hexvec);
	result = list();

	for(i in 1:m)
		{
		hex = hexvec[i];
		hex = checkHEX(hex);
		if(is.null(hex))
		  {
		  res = NA;
		  } else {

          		hex = str_replace("#", "", trimMe(toupper(hex)));
          			hexV = charVector(hex);
          		if(length(hexV) == 3)
          			{
          			#  'FFF' => 'FFFFFF'
          			hexV = c(hexV[1], hexV[1], hexV[2], hexV[2], hexV[3], hexV[3]);
          			}

          			res = list(
          					"r" = hexdec( paste(hexV[1:2], collapse="") ),
          					"g" = hexdec( paste(hexV[3:4], collapse="") ),
          					"b" = hexdec( paste(hexV[5:6], collapse="") )
          					);
		          }

		result[[i]] = res;
		}

	if(m > 1) { result; } else { result[[1]]; }
	}




#' checkHEX
#'
#' This checks a hexadecimal input
#'
#' @param hex the hex form of the color
#'
#' @return cleansed hex form of the color
#' @export
#'
#' @examples
#' checkHEX("ABC");  # returns an updated 3-digit form
#' checkHEX("ABCDEF");
#' checkHEX("123456");
#'
#' checkHEX("red"); # looks this up
#' checkHEX("alex"); # can't find anything in the lookup
#'
checkHEX = function(hex)
	{
  # this check if the hex was past; if it was "color", it sees if it can look it up
  # this is a vectorized function ...
	hex = str_replace("#", "", trimMe(tolower(hex)));

	## CHECK if [hex] is [colorname] ##
	    # insert "color" check here ... if it is "red", let's get "hex" from that ...
	    # "^[A-Fa-f0-9]{6}|[A-Fa-f0-9]{3}$"
	    # "^#?(([0-9a-fA-F]{2}){3}|([0-9a-fA-F]){3})$"
	#### ALPHA SEARCH FIRST
	search.alpha = grep("^#?(([0-9a-fA-F]{2}){4}|([0-9a-fA-F]){4})$", hex);
	if(length(search.alpha) > 0)
	  {
	  return ( str_replace("##", "#", toupper( paste0("#", hex) ) ) );
	  }
	search = grep("^#?(([0-9a-fA-F]{2}){3}|([0-9a-fA-F]){3})$", hex);
	if(length(search) < 1)
		{
		hasFound = FALSE;
		# look in base for now
		# color.idx = which( colors(TRUE) == hex );
		color.idx = match(hex, grDevices::colors(TRUE));  # %in% is currently defined as "%in%" <- function(x, table) match(x, table, nomatch = 0) > 0
		if(is.na(color.idx)) { color.idx = NULL;}
		if(length(color.idx) > 0)
			{
			hasFound = TRUE;
			hex = rgb2hex(grDevices::col2rgb(hex));
			hasFound = TRUE;
			}
		if(!hasFound)
		    {
		    warning( paste0(" Bad input in function [hex2rgb] : ", hex, "\n") );
		    return (NULL);
		    }
		}

	return ( str_replace("##", "#", toupper( paste0("#", hex) ) ) );
	}



#' checkRGB
#'
#' This checks a rgb input and replaces a bad "hex"
#'
#'
#' @param rgb the rgb input
#'
#' @return cleansed rgb
#' @export
#'
#' @examples
#'
#' checkRGB("red");
#' checkRGB(hex2rgb("red"));
#' checkRGB(hex2rgb("#F00"));
#'
checkRGB = function(rgb)
	{
  # this checks if the rgb was past, if it was hex, it proceeds ...

	if(is.null( dim(rgb) ) )
		{
		# my.names = sort(unique(names(rgb)));  	# if I did "unlist"
		# my.length = length(rgb);				# if I did as.numeric on "unlist"
												# my.length %% 3
		if(is.character(rgb))
			{
			rgb = hex2rgb(rgb);
			}
		} # they can pass in "hex"
	cleanupRGB(rgb);
	}


#' cleanupRGB
#'
#' @param rgb matrix of rgb values
#'
#' @return matrix of rgb values on 255 scale
#' @export
#'
#' @examples
#'
#' rgbs = hex2rgb("#F00", "pink");
#' cleanupRGB(rgbs);
cleanupRGB = function(rgb)
	{

	rgb = as.numeric( unlist( rgb ) ); # just in case ...
		rgb = matrix(rgb, nrow=3);

	if( max(rgb) <= 1 ) { rgb = 255 * rgb; } # we should be in 255 as "1"

	rgb;
	}


#' rgb2hex
#'
#' @param rgb matrix of RGB values
#' @param pre do we want to pre-pend the hash "#" ?
#'
#' @return vector of hexform colors
#' @export
#'
#' @examples
#' rgbs = hex2rgb("#F00", "pink");
#' rgb2hex(rgbs);#'
rgb2hex = function(rgb, pre="#")
	{

	rgb = cleanupRGB(rgb);
	# print(rgb);
	paste0(pre, dechex(rgb[1,], n=2), dechex(rgb[2,], n=2), dechex(rgb[3,], n=2) );
	}






#' hue2rgb
#'
#' Converts hue color format to rgb color format
#'
#' @param hues matrix of hues
#'
#' @return matrix of rgbs
#' @export
hue2rgb = function(hues)
	{
  # TODO: where is rgb2hue ???
	hues = as.numeric( unlist( hues ) ); # just in case ...
	hues = matrix(hues, nrow=3);

	m = ncol(hues);
	res = numeric(m);
	for(i in 1:m)
		{
		hue = hues[,i];

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


#' hsl2rgb
#'
#' @param hsls
#'
#' @return
#' @export

hsl2rgb = function(hsls)
	{
	hsls = as.numeric( unlist( hsls ) ); # just in case ...
	hsls = matrix(hsls, nrow=3);

	m = ncol(hsls);
	res = list();
	for(i in 1:m)
		{
		hsl = hsls[,i];

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

		res[[i]] = list(
						"r" = as.integer( r ),
						"g" = as.integer( g ),
						"b" = as.integer( b )
						);
		}

	if(m > 1) { res; } else { res[[1]]; }
	}





#' rgb2hsl
#'
#' @param rgb
#'
#' @return
#' @export
rgb2hsl = function(rgb)
	{
  # rgb = hex2rgb("#abcdef"); unlist(rgb);
# rgb2hex(rgb);
# hsl = rgb2hsl(rgb);  unlist(hsl);
# unlist( hsl2rgb(hsl) );

	rgb = checkRGB(rgb);
	rgb = rgb / 255; # we want it on the [0,1] scale

	m = ncol(rgb);
	res = list();
	rgbs = rgb;

	for(i in 1:m)
		{
		rgb = rgbs[,i];

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

		res[[i]] = list(
					"h" = h,
					"s" = s,
					"l" = l
					);
		}

	if(m > 1) { res; } else { res[[1]]; }
	}






#' hsv2rgb
#'
#' @param hsvs
#'
#' @return
#' @export
#'
#' @examples
hsv2rgb = function(hsvs)
	{
  # rgb = hex2rgb("#abcdef"); unlist(rgb);
# rgb2hex(rgb);
# hsv = rgb2hsv(rgb);  unlist(hsv);
# unlist( hsv2rgb(hsv) );


# wsu.crimson = "#981e32";
# (rgb = unlist ( hex2rgb( wsu.crimson ) ) );
# cat("\n", "===============", "\n\n");
# (hsv = unlist(rgb2hsv(rgb) ) );
# cat("\n", "===============", "\n\n");
# unlist ( hsv2rgb( hsv ) );
### SEEMS TO BE A BUG on "blue"

	hsvs = as.numeric( unlist( hsvs ) ); # just in case ...
	hsvs = matrix(hsvs, nrow=3);

	m = ncol(hsvs);
	res = list();
	for(i in 1:m)
		{
		hsv = hsvs[,i];

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

				res[[i]] = list(
					"r" = as.integer( 255*r ),
					"g" = as.integer( 255*g ),
					"b" = as.integer( 255*b )
					);
		}

	if(m > 1) { res; } else { res[[1]]; }
	}



#' rgb2hsv
#'
#' @param rgb
#'
#' @return
#' @export
# THIS FUNCTION EXISTS in base ... 
# http://www.brucelindbloom.com/
# Conversion algorithms from http://www.brucelindbloom.com.
rgb2hsv = function(rgb)
	{
  # package:grDevices
  # ? grDevices::rgb2hsv (r,g,b)
	rgb = checkRGB(rgb);
	rgb = rgb / 255; # we want it on the [0,1] scale

	m = ncol(rgb);
	res = list();
	rgbs = rgb;

	for(i in 1:m)
		{
		rgb = rgbs[,i];
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

			res[[i]] = list(
						"h" = h,
						"s" = s,
						"v" = v
						);


		}

	if(m > 1) { res; } else { res[[1]]; }
	}









#' cmyk2rgb
#'
#' @param cmyks
#' @param ...
#'
#' @return
#' @export
cmyk2rgb = function(cmyks, ...)
	{
  # rgb = hex2rgb("#abcdef"); unlist(rgb);
# rgb2hex(rgb);
# cmyk = rgb2cmyk(rgb);  unlist(cmyk);
# unlist( cmyk2rgb(cmyk) );
	more = unlist(list(...));
	cmyks = c(cmyks, more);

	cmyks = as.numeric( unlist( cmyks ) ); # just in case ...
	cmyks = matrix(cmyks, nrow=4);

	m = ncol(cmyks);
	res = list();
	for(i in 1:m)
		{
		cmyk = cmyks[,i];


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

		res[[i]] = 	list(
						"r" = as.integer( 255*r ),
						"g" = as.integer( 255*g ),
						"b" = as.integer( 255*b )
						);
		}

	if(m > 1) { res; } else { res[[1]]; }

	}

#' rgb2cmyk
#'
#' @param rgb
#'
#' @return
#' @export
rgb2cmyk = function(rgb)
	{
	rgb = checkRGB(rgb);

	m = ncol(rgb);
	res = list();
	rgbs = rgb;

	for(i in 1:m)
		{
		rgb = rgbs[,i];

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
		res[[i]] = list(
						"c" = c ,
						"m" = m ,
						"y" = y ,
						"k" = k
						);


		}

	if(m > 1) { res; } else { res[[1]]; }


	}
