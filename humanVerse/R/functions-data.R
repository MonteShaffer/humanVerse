

# personality.df = data.load("personality", "personality-raw")
# personality.df2 = data.load("personality", "personality-raw", mode="txt")
# personality.names = data.load("personality", "personality_self-en", mode="json")



data.load = function(dfolder, dstem = NULL, 	
							mode="rds", main=NULL, raw=NULL, sub="data", 
							access.remotely = FALSE, force.download = FALSE)
	{
	if(is.null(main)) { main = .GlobalEnv$.humanVerse[["path"]]$github$main; }
		# main = paste0(main, "/", sub, "/", dfolder, "/"); 
	if(is.null(raw))  { raw  = .GlobalEnv$.humanVerse[["path"]]$github$raw;  }
		# raw  = paste0(raw,  "/", sub, "/", dfolder, "/"); 
	if(is.null(dstem)) { dstem = dfolder; }	
	
	if(mode == "rds")
		{
		if(access.remotely)
			{
			my.file = paste0(main, "blob/main/", sub, "/", dfolder, "/", dstem, ".rds", "?raw=true");
			return ( readRDS.url(my.file) );			
			} else 	{
					my.file = paste0(raw, "main/", sub, "/", dfolder, "/", dstem, ".rds", "");
					local = getRemoteAndCache(my.file, force.download = force.download);
					return( readRDS(local) );
					}		
		}
	if(mode == "txt")
		{
		if(access.remotely)
			{
			my.file = paste0(main, "blob/main/", sub, "/", dfolder, "/", dstem, ".txt");
			my.file = cleanup.url(my.file);
			return ( readFromPipe(my.file) );			
			} else 	{
					my.file = paste0(raw, "main/", sub, "/", dfolder, "/", dstem, ".txt", "");
					local = getRemoteAndCache(my.file, force.download = force.download);
					return( readFromPipe(local) );
					}
		}
	if(mode == "json")
		{
		if(access.remotely)
			{
			my.file = paste0(main, "blob/main/", sub, "/", dfolder, "/", dstem, ".json");
			return ( rjson::fromJSON( readStringFromFile(my.file) ) );			
			} else 	{
					my.file = paste0(raw, "main/", sub, "/", dfolder, "/", dstem, ".json", "");
					local = getRemoteAndCache(my.file, force.download = force.download);
					return ( rjson::fromJSON( readStringFromFile(local) ) );	
					}
		}
	}



data.prepare.personality = function()
	{
	p.df 		= data.load("personality", "personality-raw", mode="txt");
	# dim(p.df); # 838  63	
	
	p.df 		= removeColumnsFromDataFrame(p.df, "V00");
					ywd.cols = c("year","week","day");
						ywd = convertDateStringToFormat( p.df$date_test,
															c("%Y","%W","%j"),
															ywd.cols,
															"%m/%d/%Y %H:%M"
													  );

	p.df = replaceDateStringWithDateColumns(p.df,"date_test",ywd);
	p.df = sortDataFrameByNumericColumns(p.df, ywd.cols, "DESC");
	p.df = removeDuplicatesFromDataFrame(p.df, "md5_email");
	dim(p.df); # 678  64
	
	rownames(p.df) = p.df$md5_email;
	
	
	s.names 	= data.load("personality", "personality_self-en", mode="json");
		cols = 5:34;
		i = 1;
	vnames = names( p.df[, cols] );
	for(vname in vnames)
		{
		mycol 	= cols[i];
		myname 	= paste0(vname, ":", s.names[[vname]]$word)
			myname = str_replace("-", "", myname);
			myname = str_replace(" ", "", myname);
		
		colnames(p.df)[mycol] = myname;
		
		p.df[, mycol] = setAttribute("word", s.names[[vname]]$word, p.df[, mycol]);
		p.df[, mycol] = setAttribute("definitions", s.names[[vname]]$definitions, p.df[, mycol]);
		
		i = 1 + i;
		}
	
	
	
	o.names 	= data.load("personality", "personality_other-en", mode="json");
		cols = 35:64;
		i = 1;
	vnames = names( p.df[, cols] );
	for(vname in vnames)
		{
		mycol 	= cols[i];
		myname 	= paste0(vname, ":", o.names[[vname]]$word)
			myname = str_replace("-", "", myname);
			myname = str_replace(" ", "", myname);
		
		colnames(p.df)[mycol] = myname;
		
		p.df[, mycol] = setAttribute("word", 		o.names[[vname]]$word, p.df[, mycol]);
		p.df[, mycol] = setAttribute("definitions",	o.names[[vname]]$definitions, p.df[, mycol]);
		
		i = 1 + i;
		}

	p.df;	
	}







