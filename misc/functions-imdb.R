.GlobalEnv$imdb = list();

imdb.setPath = function(path)
	{
	.GlobalEnv$imdb$dataPath = path;
	}
	
imdb.setWhen = function(when="")
	{
	if(is.empty(when)) { when = Sys.time(); }
	.GlobalEnv$imdb$when = getDate("%Y-%m", when);
	}	
	
imdb.urlTemplates = function()
	{	
	tmp = list();
		tmp[["movie-info"]] 				= "https://www.imdb.com/title/{ttid}/";
		tmp[["movie-crew"]] 				= "https://www.imdb.com/title/{ttid}/fullcredits";
		tmp[["movie-companies"]] 			= "https://www.imdb.com/title/{ttid}/companycredits";
		tmp[["movie-boxoffice"]] 			= "https://www.boxofficemojo.com/title/{ttid}/";
		
		tmp[["movie-boxoffice-release"]]	= "https://www.boxofficemojo.com/release/{rlid}/weekend/";
		tmp[["movie-list-by-year"]] 		= "https://www.imdb.com/search/title/?title_type=feature&year={date-start},{date-stop}&start={start}&sort={sort}";
		
		
		tmp[["actor-info"]] 				= "https://www.imdb.com/name/{nmid}/";
		tmp[["actor-bio"]] 					= "https://www.imdb.com/name/{nmid}/bio";
		
		
	.GlobalEnv$imdb$urlTemplates = tmp;
	}


	
	
	
	
	