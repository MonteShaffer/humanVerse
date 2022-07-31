

benchmark.efficiency = function(mb.res)
	{
	mb.res$which = as.numeric(mb.res$expr);

	mb.ids = unique(as.numeric(mb.res$expr));
	mb.names = unique(as.character(mb.res$expr));

	out = NULL;

	A.order 	= which(mb.ids == 1); # this is the order of A
	A.name 		= mb.names[A.order];
	A 			= subset(mb.res, which==A.order)$time;
	A.median 	= doMedian(A)/1000; #microseconds

	row = c(1,A.order,A.name,A.median,0); # BENCHMARK, delta efficiency is 0
	out = rbind(out, row);

	n.ids = length(mb.ids);
	if(n.ids > 1)
		{
		for(i in 2:n.ids)
			{
			B.order 	= which(mb.ids == i); # this is the order of B
			B.name 		= mb.names[B.order];
			B 			= subset(mb.res, which==B.order)$time;
			B.median 	= doMedian(B)/1000; #microseconds
			B.eff 		= round(100* (A.median-B.median)/A.median , 2);

			row = c(i,B.order,B.name,B.median,B.eff);
			out = rbind(out, row);
			}
		}

	out = data.frame(out);
	colnames(out)=c("Index","Order","Name","Median","Delta.Efficiency");
	# rownames(out)=out$Name;
	rownames(out) = NULL;
		out$Median = as.numeric(out$Median);
		out$Delta.Efficiency = as.numeric(out$Delta.Efficiency);
		out$Ranking = order(out$Delta.Efficiency, decreasing=TRUE); # not to be confused with 'rank'

	out;
	}
	
	
	