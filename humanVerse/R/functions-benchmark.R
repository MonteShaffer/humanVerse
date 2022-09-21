
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
	
	