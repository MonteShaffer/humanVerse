# numerology, western, see # http://nephi-john.today/apps/numerology/


nine.remainder = function(n, casting=9)
	{
	rem = n %% casting;	
	if(rem == 0) { rem = casting; }
	rem;
	}

nine.list = function(reduced=TRUE)
	{
	if(length(.GlobalEnv$.humanVerse[["system"]][["nine"]]) == 0)
		{
		# this stores a map that can be directly accessed, faster than a constant lookup
		res = list();
		for(letter in letters)
			{
			w = which(letters == letter);
			v = nine.remainder(w);
			res[[letter]] = v;
			}
		for(i in 0:9)
			{
			res[[as.character(i)]] = i;
			}
		.GlobalEnv$.humanVerse[["system"]][["nine"]] = res;
		} else {
				res = .GlobalEnv$.humanVerse[["system"]][["nine"]];
				}
		
	invisible(res);		
	}
	
nine.returnNumber = function(word, method = "memory", reduced=TRUE)
	{
	word = as.character(word);
	word = trimMe(tolower(word));
	chars = explodeMe("", word);
	res = numeric( length(chars) );
	
	if(method == "memory")
		{
		i = 0;
		map = nine.list();
		for(char in chars)
			{
			i = 1 + i;
			res[i] = map[[char]];
			}
		return(res);  # this is only reduced = TRUE
		}
	
	
	
	## one at a time
	uchars = unique(chars);	
	for(uchar in uchars)
		{		
		w = which(letters == uchar);
		if(length(w) != 1) 
			{ 
			v = 0; 
			} else {
					if(!reduced) { v = w; } else { v = nine.remainder(w); }
					}
		# cat("\n", "uchar: ", uchar, " ... v: ", v, "\n");
		idx = which(chars == uchar);
		res[idx] = v;
		}
	res;
	}

# YYYY = 2015; MM = 12; DD = 22;
nine.computeBirthdate = function(YYYY, MM, DD, reduced = TRUE)
	{
	YYYY = as.integer(YYYY);
	MM = as.integer(MM);
	DD = as.integer(DD);
	
	res = list();
	n = 3;
	dates = c(YYYY, MM, DD);
	sums = numeric(n);
	
	for(i in 1:n)
		{
		mydate = dates[i];
		res[[i]] = nine.returnNumber(mydate); 	# uses memory 	
												# because numbers, could be as.integer(explode)
		sums[i] = sum(res[[i]]);
		}
	mysum = sum(unlist(res));
	myreduced = nine.remainder(mysum);
	specialsum = sums[1] + sums[2] + DD;  # for 11, 22, 33 
	
	sinfo = list();
		sinfo$total 	= mysum;
		sinfo$final 	= myreduced;
		sinfo$special 	= specialsum;
		sinfo$each 		= sums;		
	
	res = setAttribute("sum-info", sinfo, res);
	
	
	res;	
	}
	
# myName = "Alexander Jay ' Shaffer";	
nine.computeName = function(myName, reduced=TRUE)	
	{
	myName 	= removeFunnyCharacters(myName, "alpha");
	myName 	= removeWhiteSpace(myName);
	
	names 	= explodeMe(" ", myName);
	
	n = length(names);
	sums = numeric(n);
	res = list();
	for(i in 1:n)
		{
		name = names[i];
		res[[i]] = nine.returnNumber(name); # uses memory 	
		sums[i] = sum(res[[i]]);
		}
	mysum = sum(unlist(res));
	myreduced = nine.remainder(mysum);
	
	sinfo = list();
		sinfo$total = mysum;
		sinfo$final = myreduced;
		sinfo$each 	= sums;		
	
	res = setAttribute("sum-info", sinfo, res);
	
	res;
	}




