

# snails.pace(snails=1, auto.play=1/50);

snails.pace = function() {} 

# TODO ... color.setOpacity()
#  snails.pace(snail.icon = u.getSymbol(c("U+1F925"), collapse=TRUE))
snails.pace = function(snails = 6, finish.line = 8, moves = 200, 
							auto.play = 1/32,  y.factor = 4/3, 
							intro.pause = TRUE, snail.par = TRUE, 
							snail.pch=16, snail.cex=6, snail.icon = u.getSymbol(c("U+22EF","U+1F40C","U+22EF"), collapse=TRUE),
							snail.col = c("orange", "blue", "pink", "green", "yellow", "red"), ...
						)
	{
	envir = environment(); 
	
	old.par = par(no.readonly = TRUE);
	move.history = NULL;
	on.exit({par(new = FALSE); par(old.par); invisible(move.history)}); # add=TRUE to on.exit ... always, maybe like dput(all)
	
	snail.x = 0*(1:snails); 
	
	
	ymax = y.factor * snails;  # 1/6 above & 1/6 below
	
	y.header 	= (1/6)*ymax;        # about 16%
	y.ceiling 	= ymax - y.header;
	y.floor 	= 0;  # graph starts at -1 
	
	y.range = y.ceiling - y.floor; 
	y.scale = y.range/snails;
	snail.y = y.scale*(0:(snails-1));
	
	
	
	# RANDOM colors ... `sample(colors(), snails);`
	n.col = length(snail.col);
	n.missing = snails - n.col;
	### collision is possible, highly improbable
	if(n.missing > 0) 
		{ 
		# if snails is too big, this breaks
		snail.col = c(snail.col, sample(colors(), n.missing, replace=TRUE)); 
		}
		
	snail.rank = 0*snail.x; 
	crank = 1; # current rank 	
	move.number = 0;
	n = 0; # current number randomized (color)
	snail.lab = "";
	
	# I could set.seed and play the entire game in one sample(1:6, 200, replace=TRUE)
	
	snails.round = function(n, by = 5)
		{
		ceiling(n/by) * by;
		}
	
	snails.plot = function() 
		{ 
		xmax = max(finish.line, max(snail.x) );
		# define reasonable xmax if overpainting 
		# SIM to solve ?
		if(snail.par) 
			{ 
			xmax = (1/0.7)* moves/snails; 
			if(xmax > moves) { xmax = moves; }
			if(xmax < finish.line) { xmax = finish.line; }
			}		
		
		# https://decodeunicode.org/en/u+1F463
		# utf8ToInt("U+1F40C")
		# utf8ToInt("\U1F40C"); # 128012; # intToUtf8(128012)
		# U+1F40C [snail]
		# plot(1, pch= intToUtf8(128024) )
		# uu = u.getSymbol(c("U+22EF","U+1F40C","U+22EF"), collapse=TRUE);
		plot(snail.x, snail.y, 
				col=snail.col, pch=snail.icon, 
				cex= (1/2*snail.cex), 
				xlim=c(-4, snails.round(xmax, 5) ), 
				ylim=c(-1, ymax ), 
				axes=FALSE, frame.plot=FALSE, 
				xlab="", ylab="", main=""
				); 
		axis(1); # maybe only redo axis if the RANGE changed 
			has.rank = (snail.rank != 0);
			snail.lab = paste0(snail.x, "*", snail.rank);
			snail.lab[!has.rank] = snail.x[!has.rank];
			# assign("snail.lab", snail.lab, envir=parent.env(environment()) );
			snail.lab %to% envir; 
		# overlay "points" again so trail doesn't have text ...
		# maybe not even use plot ?
		# overlay doesn't work with BAD UTF character ... weird ?
		if(snail.par)
			{
			points(snail.x, snail.y, col=snail.col, pch=snail.pch, cex=snail.cex);
			}
		# place text with current number PLUS * rank if finish.line 
		# text(snail.x, y=snail.y, labels=snail.lab, col="black");

		rect(-2 , snail.y-1, 2, snail.y+1, border=NA, col=snail.col[n]);
			
			
		text(0*snail.y, y=snail.y, labels=snail.lab, col="black"); 
		abline(v = finish.line, col="gray", lty="dashed");
		
		# main in plot is updating, so place a textbox (white) to overwrite?
		# white out overlay 
		rect(0,(ymax-5/6*y.header), 1.5*xmax, (ymax-1/6*y.header), border=NA, col="white");
		
		status=paste0("Move #", move.number, " of ", moves);
			r.col = "white"; if(n != 0) { r.col = snail.col[n]; }
		# length is the current color's position
			xlen = finish.line; if( n != 0 ) { xlen = snail.x[n]; }
								if(xlen == 0) { xlen = finish.line; }		
		rect(0,(ymax-5/6*y.header), xlen, (ymax-1/6*y.header), border=NA, col=r.col);
		text(0, y=(ymax-1/3*y.header), labels=status, col="black", pos=4); # to the right 
		text(xlen, y=(ymax-2/3*y.header), labels=status, col="white", pos=2); # to the left 
		
		if(snail.par) { par(new = TRUE); }
		}
	snails.update = function() 
		{
		if(intro.pause && move.number < 2)
			{
			x = readline(prompt="Press [enter] to continue, [ESC] to quit");
			} else if(is.null(auto.play))
				{
				x = readline(prompt="Press [enter] to continue, [ESC] to quit");
				} 
		# n = sample((1:snails), 1);	n %to% envir;
		n = v.shuffle((1:snails), 1);	n %to% envir;
		
		snail.x[n] = 1 + snail.x[n];
		if( (snail.rank[n] == 0) && (snail.x[n] >= finish.line) )
			{ 			
			snail.rank[n] = crank;		snail.rank 	%to% envir;
			crank %++%. ;				crank 		%to% envir;
			}		
		snail.x;
		}
		
	par(new = FALSE); 	
	par(mar=c(2,1,1,1));
	snails.plot(); 
	while(move.number < moves)
		{
		move.number %++%. ; 
		snail.x = snails.update();
		move.history = c(move.history, n);  # prevent multi-color BUG
		snails.plot();	
		if(!is.null(auto.play)) 
			{ 
			dev.flush();
			Sys.sleep(auto.play); 
			}
		}
	attr(move.history, "color") = snail.col;
	attr(move.history, "info") 	= snail.lab;
	minvisible(move.history, print=FALSE);	
	}



