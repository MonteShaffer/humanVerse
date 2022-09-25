
## display microbenchmark results
##    mb.res = microbenchmark::microbenchmark(); 
ggg.mb = function(mb.res, show="milliseconds", plot=TRUE, caching=TRUE)
	{
	# maybe write my own ... see Dirk's 
	# Rput.OUT ?? 
	# on.exit(return WHAT YOU HAVE) ... 
	# warmpu ... why ... 
	# are time units absolute ? ... format to milliseconds ... 
	# let's scale to the number of runs ... e.g., 1000 
	# times=1000
	# ntests = length(mb.names);
	# nall = length(mb.res$time);
	# time is always in nano ... divide by 1000000 ... get to millis
	mb.res$time = mb.res$time / 1000 / 1000;
	# I report millis per single unit  
	SHOW = prep.arg(show, n=3);
	mb.res$time = switch(SHOW,
							"mil" = mb.res$time,
							"mic" = mb.res$time*1000,
							"nan" = mb.res$time*1000*1000,
							"sec" = mb.res$time/1000,
							"min" = mb.res$time/1000/60,
							"hou" = mb.res$time/1000/60/60,
							"day" = mb.res$time/1000/60/60/24,
						 mb.res$time
						 );
	PSHOW = switch(SHOW,
							"mil" = "milliseconds (ms)",
							"mic" = "microseconds (µs)",
							"nan" = "nanoseconds (ns)",
							"sec" = "seconds",
							"min" = "minutes",
							"hou" = "hours",
							"day" = "days",
						 "milliseconds (ms)"
						 );
	mb.names = levels(mb.res$expr);	
	
	mb.md5 = str.toMD5( JSON.stringify( list("data" = mb.res$time, "names" = mb.names, "pshow" = PSHOW, "show" = SHOW) ) );
	
dput(PSHOW);
dput(SHOW);
dput(mb.md5);

	if(caching) 
		{ 
		out = memory.get(mb.md5, "-CACHE-"); 
		if(!is.null(out))
			{
			time.is = paste0("[",PSHOW, "] per call");
cat("\n\n time.is ... ", time.is, "\n\n");
			return( minvisible(out, display="print") );			
			}
		}
	
	
	
	out = NULL;
	
	A.name 		= mb.names[1];
	A 			= subset(mb.res, expr==A.name)$time;
	#A.info 		= stats.summary(A / length(A));  # expensive with z-scores 
	A.info		= myfive(A/length(A));

	# We BENCHMARK to the first element examined ...
	# maybe TODO ... allow BENCHMARK to the fastest ...
	# row = df.row(1,A.name,A.info$Ns[1],A.info$Ns[4], A.info$median, A.info$Ns[7], A.info$Ns[10], 0, 1, use.names=FALSE); 
	row = df.row(1, A.name, A.info, 0, 1, use.names=FALSE); 
	
	out = rbind(out, row);

	n.names = length(mb.names);
	if(n.names > 1)
		{
		for(i in 2:n.names)
			{
			B.name 		= mb.names[i];
			B 			= subset(mb.res, expr==B.name)$time;
			# B.info 		= stats.summary(B / length(B) );  # expensive with z-scores 
			B.info		= myfive(B/length(B));
				
			B.eff 		= round(100* (A.info[3]-B.info[3])/A.info[3] , 2);
			
			B.factor 	= round(B.info[3]/A.info[3] , 5);

			# row = df.row(i,B.name,B.info$Ns[1],B.info$Ns[4], B.info$median, B.info$Ns[7], B.info$Ns[10], B.eff, B.factor, use.names=FALSE); 
			
			row = df.row(1, B.name, B.info, 0, 1, use.names=FALSE); 

			
			out = rbind(out, row);
			}
		}
	rownames(out) = out$V1;
	colnames(out) = c("idx", "expression", "min", "lower-trecile", "median", "upper-trecile", "max", "relative.efficiency", "relative.factor");
	
	out$Rank = rank(out$median, ties="first");
	out = df.sortBy(out, "Rank"); 
	
	time.is = paste0("[",PSHOW, "] per call");
cat("\n\n time.is ... ", time.is, "\n\n");
	out = property.set("time.is", out, time.is);
	
	# caching at the time level ... new time interval, new cache 
	if(caching) { memory.set(mb.md5, "-CACHE-", out); }
	# maybe also log the md5 somewhere ...
	
	minvisible(out, display="print");
	}
	



















bm.prep = function(dlist)
	{
	n = length(dlist);
	nlist = list();
	keys = character(n);
	for(i in 1:n)
		{
		keys[i] = paste0("--", deparse(dlist[[i]]), "--");
		val = paste0("function () { ", deparse(dlist[[i]]), " };");
		fn = eval(parse(text=val));
		fn = property.set("srcref", fn, NULL);
		nlist[[i]] = fn;
		}
	names(nlist) = paste0("V", 1:n);
	nlist = property.set("keys", nlist, keys);
	minvisible(nlist);
	}

bm = function() {}   # identical.to = "referent" ... "consensus" 
bm = function(..., list=NULL, 
					setup = list("nsim" = 100, "compare.values" = TRUE, "identical.to" = "referent", "tol" = 0, "Rprof" = FALSE, "Rprofmem" = FALSE), 
					params = list(
							"str" = c("<i>hello friend</i>", " hello there world ", " ¡hola!, ¿que tal? "), 
							"sep" = c(" ", "", "¿", "</i>")
								)
					)
	{
	timer.init();
	timer.clear("bm");
	timer.start("bm");
	
	fns = bm.prep( c(as.list(match.call(expand.dots = FALSE)$...), list) );

	timer.stop("bm", marker="prep.fns");
	
	# times the internal workings of benchmarking process	  
	NULL.name = str.uniqid("NULL.FUNCTION.");
	FN = list();   
	fnN = function() { }
	fnN = property.set("srcref", fnN, NULL);
	
	FN[[NULL.name]] =  fnN;
	
	
	FNS = c( FN, fns);

	N = list("nsim" = setup$nsim, "fns" = length(fns));
	np = length(params);
	pnames = names(params);
	for(i in 1:np)
		{
		key = paste0("param.",pnames[i]);
		N[[ key ]] = length(unlist(params[i]));
		}
	P = list.getElements(N, 1);
cat("\n\n Combinations to be performed: ", paste0(P, collapse=u.toSymbol("U+00D7")), " = ", num.commas(prod(P)), "\n\n");  # add commas on prodP (sprintf)

# hit ENTER to continue
x = readline(prompt="Press [enter] to continue, [ESC] to quit");

print(str(FNS));
dput(FNS); stop("monte");
			
	
	# FN (null) not counted ...
	df = NULL;
	#  fn    N   param.str  param.sep  seed.fn  seed.str  seed.sep  time
	#   1:13 i,j,k    1:3      1:4       s         s          s       diff(timer.stop(relative))
	
	FNS.names = names(FNS);
	fidx = 1:length(FNS.names);
	
	
	timer.stop("bm", marker="prep.nsim");
	
	for(i in 1:N$nsim)
		{
		timer.stop("bm", marker=paste0("nsim-",i));
# cat with flush.console and "PROGRESS BAR" ...
# writing overself with latest TASK ...
# 		
		seed.fn = seed.create();
		FUNCTIONS = v.sample(fidx, seed=seed.fn);
		
		OPTIONS = list();
		seeds = numeric(np);
		combos = "";
		for(j in 1:np)
			{
			# loop over the params 
			pname = pnames[j];
			key = paste0("param.",pname);
			vals = unname(unlist(params[j]));
			nv = length(vals);
			pidx = 1:nv;
			seed.param = seed.create();
			seeds[j] = seed.param;
			
			OPTIONS[[key]] = v.sample(pidx, seed=seed.param);
			timer.stop("bm", marker=paste0("nsim-",i,"param-",j));
			}
		# Somehow, collapse to one for() loop ... 
		# it has to be all the PERMUTATIONS of the variadic # of parameters
		# 1,3,2 ... 3,1,4,2 ... 1-3,1,4,2   3-3,1,4,2  2-3,1,4,2
		# 1,3,2 ... 3,1,4,2 ... 2,1 ...  1-3-2,1    1-1-2,1  1-4-2,1  1-2-2,1 ... 
		# how to do this recursively ... 1-1-1-1-1-1 

# timer.fns with .timer  ... faster(less verbose) ... 

		## GTG ... GOOD TO GO ... CONTACT (1996)
		for(FUNCTION in FUNCTIONS)
			{
			FNAME = FNS.names[FUNCTION];
			# how to do a for(i,j,k) loops that is variadic 
			
cat("\n\n FUNCTION: ", FUNCTION, "\t FNAME: ", FNAME, "\t FNS : ", paste0(str.removeWhiteSpace( lang2str(FNS[[FNAME]]), 0), collapse=""), "\n\n");

			# assign AS-IS has a benefit here ...
			
			# assign("str", strval, HERE);
			# assign("sep", sepval, HERE);
			# if my functions have (str,sep) ... GTG (lexical)
			
			et = timer.stop("bm");  # do one 
			ot = FNS[[ FNS.names[FUNCTION] ]]();		# call function (based on idx) ... idx didn't work
			et = timer.stop("bm", marker=paste0("nsim-",i,".FUN-",FUNCTION));	# do another, store et ...
			# 
			if(is.null(ot)) { ot = "--NULL--"; }
			# for given simulation (all.equal)
			row = df.row(i, FUNCTION, et, ot);
			df = rbind(df, row);
			}		
		}	
	minvisible(df);
	}
	
	