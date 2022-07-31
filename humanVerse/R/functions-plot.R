

plot.polygonsBetweenTwoData = function(x1, y1, x2, y2, ...)
  {
  # https://onertipaday.blogspot.com/2007/04/highlight-overlapping-area-between-two.html
  # This "shades" between two lines or other data, specificy a color in ..
  graphics::polygon (  x = c(x1, rev(x2), x1[1]),  # x1, reverse on x2, and reconnect to first point of x1
             y = c(y1, rev(y2), y1[1]),
            ...
          );
  }




plot.myFlorencePlot = function(df)
	{
	# radial plot using plotrix ...

	# plot.buildAxis ... axes=F in "plot" ...
  # axis(1, lwd.tick=0, labels=FALSE)


  # df$v and df$year and df$subyear unit
  # cumsum using Archimedean radial ...
	}







#' stats.prepareTukeyPlot
#'
#' @param x.list
#' @param heresy
#' @param sort.me
#' @param sort.list
#' @param sort.key
#'
#' @return
#' @export
stats.prepareTukeyPlot = function(x.list, heresy=FALSE,
									sort.me = TRUE, sort.list="ASC", sort.key="median")
	{

	# with ASC ... it will put smallest first, we will plot from first index ...
	x.stats = list();
		all = c();
		med = c();
		mea = c();
		nam = c();
	n = length(x.list);
	for(i in 1:n)
		{
		my.x = sort(x.list[[i]]);
			all = c(all, my.x);
		info = doStatsSummary(my.x);
			med = c(med, info$myMedian);
			mea = c(mea, info$myMean);
			nam = c(nam, names(x.list)[i]);
		x.stats[[i]] = info;
		}


	# ordered ...

	if(sort.me)
		{
		df = as.data.frame( cbind(1:n, med, mea) );
			colnames(df) = c("i", "median", "mean");
		df$name = nam;
		df = sortDataFrameByNumericColumns(df, sort.key, direction=sort.list);

		new.stats = list();
		for(i in 1:n)
			{
			j = df$i[i];
			new.stats[[j]] = x.stats[[i]];
			}
		} else { new.stats = x.stats; }



	# global min/max
	myMin = min(all, na.rm=TRUE);
	myMax = max(all, na.rm=TRUE);

	new.stats = setAttribute("min", myMin, new.stats);
	new.stats = setAttribute("max", myMax, new.stats);
		names(new.stats) = df$name;

	new.stats;
	}













plot.z.score = function(z, which="z", zrange=c(-4,4),
									background.color 	= "#4DBF4D", background.opacity = 80,
									overlay.color 		= "#4D4DBF", overlay.opacity = 80,
									arrow.color 		= "#BF4D4D", arrow.opacity = 80)
	{
	if(which != "z")
		{
		# let's do PROBABILITY
		z = qnorm(z);
		}
	if(z > max(abs(zrange))) 
		{ 
		zrange = c(-1*abs(z),abs(z)); 
		}
	
################   ===   NORMAL	   ===   ################
		fstr = paste0("Normal: ", zrange[1], " , ", zrange[2], "  ; ");

			myMain = paste0("z-distribution  ");
		fstr = paste0(fstr, " myMain='",myMain,"'; ");

		# print(fstr);
		finfo = computeNumericalIntegrationString(fstr, ylim=c(0,yMax),
							polygon.col.pos = color.setOpacity(background.color, background.opacity)
							);	
	
	
	
	}



#' plot.t.test
#'
#' @param x
#' @param y
#' @param ...
#' @param alpha
#' @param truncate.t
#' @param plotHistogram
#' @param x.lim
#' @param background.color
#' @param background.opacity
#' @param overlay.color
#' @param overlay.opacity
#' @param arrow.color
#' @param arrow.opacity
#'
#' @return
#' @export
#'
#' @examples
plot.t.test = function(x, y=NULL, ..., alpha=0.05, truncate.t = 4, plotHistogram = FALSE,
									x.lim = c(-4,4),
									background.color 	= "#4DBF4D", background.opacity = 80,
									overlay.color 		= "#4D4DBF", overlay.opacity = 80,
									arrow.color 		= "#BF4D4D", arrow.opacity = 80)
	{
	my.test = stats::t.test(x, y=y, ...);

	# plot.myHistogram(x, steps.z=1/12, polygon.col.pos = color.setOpacity("#981e32", 80) );
# wsu.crimson = "#981e32";
# wsu.gray    = "#717171";

# visualize.t.test(c(4,5,6), c(1,2,3));
# visualize.t.test(c(44,55,99), c(1,2,3), x.lim=c(-4.5,4.5) );
# visualize.t.test(x,y);
# set.seed(12222015); x = runif(100); set.seed(1281839016); y = runif(100); vtt = visualize.t.test(x/y,y*x, background.color = "#981e32", background.opacity = 80,
#									overlay.color = "#717171", overlay.opacity = 50); vtt$t.stat;  # t.stat was 3.14 ...
#
# use getSeed/setSeed to find it again ... maybe with 99 df ...
# 1] "C:/_git_/WSU_STATS419_FALL2020" > dput(x.list, file="3.14");
# x = rnorm(100); y = runif(100);
# visualize.t.test = function(x
###########################

	# args = getFunctionParameters();
	# print(args);

	# args2 = grabFunctionParameters();
	# print(args2);

	yMax = 0.55;

	print(my.test);
	# https://statistics.berkeley.edu/computing/r-t-tests
	# https://www.youtube.com/watch?v=oJjkjkY6mmA
	# https://www.youtube.com/watch?v=5Z9OIYA8He8

	t.stat = as.numeric(my.test$statistic);
	d.of.f = as.numeric(my.test$parameter);
	pvalue = as.numeric(my.test$p.value  );

	t.crit = stats::qt(1-alpha/2, d.of.f); # should be positive
	t.crit = abs(t.crit);


	p.cut = list( 		"0.10" = isTRUE(pvalue < 0.10),
						"0.05" = isTRUE(pvalue < 0.05),
						"0.01" = isTRUE(pvalue < 0.01) );




	cat("\n", " === SIGNIFICANCE OF DIFFERENCE AT ALPHA LEVELS === ", "\n\n");
		print( unlist(p.cut) );


	if(plotHistogram)
		{
		if(!is.null(y))
			{
			graphics::par(mfrow=c(1,2));
				x.h = plot.myHistogram(x, myMain="- x -");
				y.h = plot.myHistogram(y, myMain="- y -");
			graphics::par(mfrow=c(1,1));
			} else  {
					x.h = plot.myHistogram(x, myMain="- x -");
					}
		}


	### show normal with t-test overlay ###
################   ===   NORMAL	   ===   ################
		fstr = paste0("Normal: ", x.lim[1], " , ", x.lim[2], " > xlab='difference'; ");

			myMain = paste0("t-distribution with [", round(d.of.f, 1), "] df");
		fstr = paste0(fstr, " myMain='",myMain,"'; ");

		# print(fstr);
		finfo = computeNumericalIntegrationString(fstr, ylim=c(0,yMax),
							polygon.col.pos = color.setOpacity(background.color, background.opacity)
							);


################   ===   T	   ===   ################
	fstr = paste0("t: ", x.lim[1], " , ", x.lim[2], " > df=",d.of.f,"; ");

	finfo = computeNumericalIntegrationString(fstr, parNew=TRUE, ylim=c(0,yMax),
				start = -1*t.crit, stop = t.crit,
				polygon.col.pos = color.setOpacity(overlay.color, overlay.opacity) );

		graphics::text(0, 0.1, label = paste0( " ", 100*(1-alpha), "% \n of Area"), col="white", cex=1.25);


################   ===   LINES/ARROW	   ===   ################
	graphics::arrows(0, yMax-0.10, t.stat, 0, code=2, lwd=6, col=color.setOpacity(arrow.color, arrow.opacity));
			graphics::text(0, yMax-0.10, label = truncateNumeric(t.stat, truncate.t), col=arrow.color, cex=1.5, pos=plot.setPosition("above"));


	y.crit = 0.15;
		graphics::segments(t.crit, 0, t.crit, y.crit);
			graphics::text(t.crit, y.crit, label = round(t.crit,2), col="black", cex=0.75, pos=plot.setPosition("above"));
		graphics::segments(-1*t.crit, 0, -1*t.crit, y.crit);
			graphics::text(-1*t.crit, y.crit, label = round(-1*t.crit,2), col="black", cex=0.75, pos=plot.setPosition("above"));


	res = list("info" = my.test, "alpha" = alpha, "dof" = d.of.f, "t.stat" = t.stat, "t.crit" = t.crit, "p.value" = pvalue);

	invisible(res);
	}
###########################


























#' plot.myTukeyPlot
#'
#' @param x.list
#' @param heresy
#' @param sort
#' @param margin
#' @param border
#' @param common.opacity
#' @param color.bg
#' @param color.field
#' @param color.hash
#'
#' @return
#' @export
# x.list = list(); x.list[[1]] = rnorm(100); x.list[[2]] = rnorm(100, mean=3, sd=2);
plot.myTukeyPlot = function(x.list, heresy=FALSE,
							sort = TRUE,
							margin = 0.25,
							border = 10,
							common.opacity = 66,
							color.bg = "#eeeeee",
							color.field = "white",
							color.hash = "#cccccc"   # maybe color.palette for the plots ... points, median, etc. ... baseColor with gradient to black?

	)
	{
  # maybe rename "Tesla" since I anchor to "3"
  # for now, one or two ... horizontal only
# if heresy use SD, SE.mean .. never do heresy ... JK

###########################################################
	# x.list must be a list, need not have same length
	if(!is.list(x.list)) { ox = x.list; x.list = list(); x.list[[1]] = ox; }
	n = length(x.list);



	# summarize and get max/min to sort ... so side-by-side are meaningful

	mar = c(margin, margin, margin, margin);

	xscale = c(0,100);
	yscale = c(0,63);
	y.offset = 1;
	# golden ratio is based on phi ...
		xlim=c(xscale[1] - border, xscale[2] + border);
		ylim=c(yscale[1] - border, yscale[2] + 2*y.offset + border);
			# place "1" extra margin top/bottom, so we have 63 to work with (golden ration)
			# workspace is 1/3 larger than football field ...

	graphics::plot.new( );
	graphics::plot.window( 	xlim = xlim,
					ylim = ylim,
					log  = "",
					graphics::par(mar=mar)
				);

###########################################################
	# background rectange
	graphics::rect(		xlim[1] + 1,
						ylim[1] - 1,
						xlim[2] + 1,
						ylim[2] - 1,
				col=color.bg, border=NA);
		xleft 	= xlim[1] + 1;
		xright 	= xlim[2] + 1;
		ybottom = ylim[1] - 1;
		ytop 	= ylim[2] - 1;
###########################################################
	# football field(s)
	n.football = n - 1;
	height.plot = (yscale[2] * 3) / (3*n + (n-1));
	height.football = height.plot/3;
	if(n.football > 0)
		{
		y0 = 0;
		y1 = 0;
		for(i in 1:n.football)
			{
			x0 = xleft;
			x1 = xright;
			y0 = y0 + height.plot;
			y1 = y0 + height.football;
# cat("\n", "x0: ", x0, " ... x1: ", x1, " ... y0: ", y0, " ... y1: ", y1, "\n");
			graphics::rect(x0, y0, x1, y1, col=color.field, border=NA);

			for(j in 0:10)
				{
				myCol = color.bg;
				if(j == 0 || j == 5 || j == 10) { myCol = color.hash; }
					x0 = 10*j;
					x1 = 10*j+0.5;
				graphics::rect(x0, y0, x1, y1, col=myCol, border=NA);
				}

			y0 = y0 + height.football;
			}
		}

###########################################################
	x.stats = stats.prepareTukeyPlot(x.list, heresy=heresy);
		myMin = getAttribute("min", x.stats);
		myMax = getAttribute("max", x.stats);
	data.scale = c(myMin,myMax); # data.scale
###########################################################
# ECDF
# NA, N

		y0 = ybottom + border/2;
		y1 = 0;
		x0 = xleft;
		x1 = xright;
	for(i in 1:n)
		{
		info = x.stats[[i]];
		my.name = names(x.stats)[i];
		if(is.null(my.name)) { my.name = paste0("V", strPadLeft(i, (1+ceiling( log10(n) ) ), "0") ); }
			graphics::text(xleft, y0 + height.plot, labels = paste0(" - ",my.name," - "), cex=1.5,
							pos=plot.setPosition("right"),
							col=color.setOpacity("#000000",common.opacity) );

		x.niniles = convertFromOneRangeToAnother( as.numeric(info$niniles), xscale, data.scale);
			for(d in 1:8)
				{
					graphics::segments(
											x.niniles[d],
											y0 + 1*height.plot/3,
											x.niniles[d],
											y0 + 2*height.plot/3,
								col=color.setOpacity("#000099",common.opacity), lwd=1);
				}


		x.IDR3 = convertFromOneRangeToAnother(info$IDR.3$IDR$xlim, xscale, data.scale);
			graphics::rect(		x.IDR3[1],
								y0,
								x.IDR3[2],
								y0 + height.plot,
							col=color.setOpacity("#FF69B4",common.opacity), border="black");
		x.mid = mean(x.IDR3);
			graphics::text(		x.mid,
								y0,
							labels = "INNER TRECILE", cex=0.75,
							pos=plot.setPosition("below"),
							col=color.setOpacity("#000000",common.opacity) );




		x.median = convertFromOneRangeToAnother(info$myMedian, xscale, data.scale);
			graphics::segments(x.median, y0, x.median, y0 + height.plot,
							col=color.setOpacity("#3CB371",common.opacity), lwd=2);

			graphics::text(x.mid, y0 + height.plot , labels = round(info$myMedian,2), cex=0.75,
							pos=plot.setPosition("above"),
							col=color.setOpacity("#000000",common.opacity) );


			x.ecdf = convertFromOneRangeToAnother(info$sorted, xscale, data.scale);
		nx = info$length.good;
		na = info$length.na;
			x.steps 	= height.plot / nx;
			y.vertical 	= x.steps * 1:nx;
			
			# pch for points?
		graphics::points(x.ecdf, y0 + y.vertical,
							col=color.setOpacity("#000000",common.opacity) );
		# put n =
			graphics::text(xright, y0 + 1*border/2, labels = paste0("n = ",  nx), cex=0.75,
						pos=plot.setPosition("left"),
						col="black" );
		# put NAs:
			graphics::text(xright, y0 + 0*border/2, labels = paste0("NAs: ", na), cex=0.75,
						pos=plot.setPosition("left"),
						col="#666666" );



		y0 = y0 + height.plot + height.football + border;
		}
###########################################################




	}







#' plot.setPosition
#'
#' @param where
#'
#' @return
#' @export
#'
#' @examples
plot.setPosition = function(where)
	{
  # argh! can't ever remember the numbers ...
	# w = substr(trimMe(tolower(where)),1,1);
  w = charAt( trimMe(tolower(where)), 1);
	if(w == "b") 				{ return(1); }  				# below, bottom
	if(w == "l") 				{ return(2); }  				# left
	if(w == "a" || w == "t") 	{ return(3); }  				# above, top
	if(w == "r") 				{ return(4); }  				# right
	stop("bad input");
	}





#' plot.myHistogram
#'
#' @param x
#' @param myMain
#' @param steps.z
#' @param range.z
#' @param ylim
#' @param background.color
#' @param background.opacity
#' @param overlay.color
#' @param overlay.opacity
#' @param do.density
#' @param density.color
#' @param density.opacity
#' @param ...
#'
#' @return
#' @export
plot.myHistogram = function(x,
							myMain = "",
							steps.z = 1/3, range.z = c(-3,3), ylim = c(0,0.4),
							background.color = "#981e32", background.opacity = 50,
							overlay.color = "#717171", overlay.opacity = 50,
							do.density = TRUE,
							density.color = "#000080", density.opacity = 50, ... )
	{
###	stepIntoFunction(getFunctionParameters(TRUE));  # for debugging, this will autopopulate the parameters...

  # multivariate histogram, overlay two on the same graph ...
# the z-shift is right or left (mu) ... and we strech the polygons (sigma)
# how to plot 2 area normal curves in the background with these scales is the functional challenge


# x = runif(1000); plot.myHistogram(x);
# x = rnorm(1000); plot.myHistogram(x);
# 	plot.myHistogram(x, steps.z=1/12);
# 	plot.myHistogram(x, steps.z=1/12, polygon.col.pos = "#981e32" );
# 	plot.myHistogram(x, steps.z=1/12, polygon.col.pos = color.setOpacity("#981e32", 80) );
# wsu.crimson = "#981e32";
# wsu.gray    = "#717171";
# ylim # Height of normal is 0.39 or so
# https://stats.stackexchange.com/questions/2492/is-normality-testing-essentially-useless


	if(is.empty(polygon.col.pos)) { polygon.col.pos = color.setOpacity(background.color, background.opacity); }

	fstr = "Normal: -4 , 4  >  xlab='z'; ylab=''; ";
	fstr = paste0(fstr, " myMain='",myMain,"'; ");
		finfo = computeNumericalIntegrationString(fstr, ...);

	z 		= calculateZscores(x);
	z.cut 	= cutZ(z, range.z, steps.z, verbose=FALSE);
	z.table = table(z.cut$member);
	zCutOverlay(z.table, steps.z, ylim=ylim, myColor = color.setOpacity(overlay.color, overlay.opacity) );

	if(do.density)
		{
		graphics::par(new=TRUE);
		z.d = stats::density(z);  # maybe scale so it stays in the 0.4 viewing window?
		plot( z.d	, axes=F, ylim=ylim, xlim=c(-4,4), main="", xlab="", ylab="",
					lwd = 4, col=color.setOpacity(density.color, density.opacity)
					);
		}
	}





