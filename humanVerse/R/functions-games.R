
snails.pace = function() {} 
# x = snails.pace(auto.play=1/50, s.par=TRUE);
# x = snails.pace(auto.play=1/50, s.par=TRUE, intro.pause=FALSE);
snails.pace = function(snails = 6, finish.line = 8, moves = 200,
							auto.play = NULL, intro.pause = TRUE,
							ymax = 2 + snails,
							s.par = FALSE, s.pch=(10+snails), s.cex=snails,
							snail.col = c("orange", "blue", "pink", "green", "yellow", "red"), ...
						)
	{
	old.par = par(no.readonly = TRUE);
	move.history = NULL;
	on.exit({par(new = FALSE); par(old.par); invisible(move.history)}); # add=TRUE to on.exit ... always, maybe like dput(all)
	
	snail.x = 0*(1:snails); 
		if(ymax < snails) { ymax = snails; }
		y.d = ymax - snails; 
		scale.y = ymax / snails;
	snail.y = (scale.y*(1:snails)) - (1/(snails/2) * y.d);
	snail.y = snail.y - 1; # moved the rect to top, so adjust 
	
	# RANDOM colors ... `sample(colors(), snails);`
	n.col = length(snail.col);
	n.missing = snails - n.col;
	### collision is possible, highly improbable
	if(n.missing > 0) { snail.col = c(snail.col, sample(colors(), n.missing)); }
		
	snail.rank = 0*snail.x; 
	crank = 1; # current rank 	
	move.number = 0;
	n = 0; # current number randomized (color)
	snail.lab = "";
	
	snails.round = function(n, by = 5)
		{
		ceiling(n/by) * by;
		}
	
	snails.plot = function() 
		{ 
		xmax = max(finish.line, max(snail.x) );
		# define reasonable xmax if overpainting 
		# SIM to solve ?
		if(s.par) { xmax = (1/0.7)* moves/snails;  }		
		
		# U+1F40C [snail]
		plot(snail.x, snail.y, 
				col=snail.col, pch=s.pch, cex=s.cex, 
				xlim=c(-1, snails.round(xmax, 5) ), 
				ylim=c(-1, ymax ), 
				axes=FALSE, frame.plot=FALSE, 
				xlab="", ylab="", main=""
				); 
		axis(1); # maybe only redo axis if the RANGE changed 
			has.rank = (snail.rank != 0);
			snail.lab = paste0(snail.x, "*", snail.rank);
			snail.lab[!has.rank] = snail.x[!has.rank];
			assign("snail.lab", snail.lab, envir=parent.env(environment()) );
		# overlay "points" again so trail doesn't have text ...
		# maybe not even use plot ?
		# overlay doesn't work with BAD UTF character ... weird ?
		if(s.par)
			{
			points(snail.x, snail.y, col=snail.col, pch=s.pch, cex=1.3*s.cex);
			}
		# place text with current number PLUS * rank if finish.line 
		text(snail.x, y=snail.y, labels=snail.lab, col="black"); 
		abline(v = finish.line, col="gray", lty="dashed");
		
		# main in plot is updating, so place a textbox (white) to overwrite?
		# white out overlay 
		rect(0,ymax, xmax, (ymax-1), border=NA, col="white");
		
		status=paste0("Move #", move.number, " of ", moves);
			r.col = "white"; if(n != 0) { r.col = snail.col[n]; }
		# length is the current color's position
			xlen = finish.line; if( n != 0 ) { xlen = snail.x[n]; }
								if(xlen == 0) { xlen = finish.line; }		
		rect(0,ymax, xlen, (ymax-1), border=NA, col=r.col);
		text(0, y=(ymax-1/3), labels=status, col="black", pos=4); # to the right 
		text(xlen, y=(ymax-2/3), labels=status, col="white", pos=2); # to the left 
		
		if(s.par) { par(new = TRUE); }
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
		n = sample(1:snails, 1);
		assign("n", n, envir=parent.env(environment()) );
		
		snail.x[n] = 1 + snail.x[n];
		if( (snail.rank[n] == 0) && (snail.x[n] >= finish.line) )
			{ 			
			snail.rank[n] = crank;
			crank = 1 + crank; 			
			# update to MAIN environment
			assign("snail.rank", snail.rank, envir=parent.env(environment()) );
			assign("crank", crank, envir=parent.env(environment()) );
			}		
		snail.x;
		}
		
	par(new = FALSE); 	
	par(mar=c(2,1,1,1));
	snails.plot(); 
	while(move.number < moves)
		{
		move.number = 1 + move.number;
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
	attr(move.history, "info") = snail.lab;
	invisible(move.history);	
	}



