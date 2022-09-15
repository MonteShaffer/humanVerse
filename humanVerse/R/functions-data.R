
# hadley
# https://stackoverflow.com/a/1816487/184614
# 

## timelib ... store list of regex
## is_america
## which_correction (1572, 1752) ... original, english
## input.calendar = "[G]regorian"
## output.calendar = "[G]" ... bouond to which_correction ... return NA if out of bounds ... 
## input.tz = UTC
## output.tx = your time zone
## origin = date.getOrigin
## input.format = "GUESS"
## output.formt = "%d/%Y"
## output.type = asPOS
## month.is = 30 days  ## DATE, INTERVAL issues ... 
## week.is = 7 days 



FN.INTRO = c(	"name|data.load", 
				"family|data",
				"export|TRUE",
				"alias[es]|data.load, load.data, dataLoad, loadData",
				"title|Generically load a data object from a resource (likely URL)",
				"description|",  # replace newlines with \n like paste0 or cat ... basic HTML <i><b><u>  ... <ol><li></ol> ... compressed to one line ... could be an external file or a comment code /** in RhV **/ THAT I parse ... PIPES not allowed, that is how I know I found a new key ... <citep>Shaffer:1999</citep> will work INLINE if there is a citation HERE or ANYWHERE in library ... <see></see> <url></url> or <a href><a> ... <url> and <see> get auto-indexed in sections below ... <fg "red"></fg>  <bg "red"></bg> <BR /> ... nl2br ... ```this can be used instead of code``` or `this` ... <color fg="wsu:crimson" bg="wsu:gray"> .... [sq] = single quote, [dq] = double quote, [tab], [line] for \t and \n ... I don't require them to do any of the [sq]/[dq][tab] cleanup, that is in my PARSER ... 
				"note|",
				"section:customname|",
				"url|",
				"cite|",  	# bibtex, zero or more [References]
				"see-also|", 	# based on internals, I could populate this [see-auto] vs [see-manual]
				"examples|", # this may be able to autopopulate based on PARAM detail ... Param is not binary but ternary (3): key, value, options
				"author(s)|",
				"example|", #multiple elements same key ...
				"example|TITLE, <code></code>", # examples from parameters if examples ...
				"methods:S3|", # if I have plot.tukey ... and this is null, it builds 'export' correctly in NAMESPACE ... 
				"methods:S4|", 
				"value|", # what is being returned, here or BELOW ... it is not a PARAM ? ... character vector of fraction, or dataframe of everything ... see <param>return</param> for more info
				"what-else|"
				
				);
				
# key, type, default value ... x = c(1,2,3) for a vector or simple dataframe, from dput ... , options, description ... 
# autopopulate ... from formals ... function.info ... ABOVE function I create /** FN = [fn.name] DOCUMENTATION ... \n\n **/ ... if you call "rebuildFN(fn.name) or ALL ... it will create a COPY above any previous documentation, if it has changed ... store as string, compare to current documentation, if unchanged ... but this allows a SYSTEM version and a user version ... first version is USER ... second is SYSTEM, which gets overwritten /** USER FN = [fn.name] **/ vs /** -SYSTEM- FN = [fn.name] **/ ... just two copies ...

# num.toFrac = function(x, ..., 
								# return = "last",
								# max.depth = 12,  
								# tol = sqrt(.Machine$double.eps) , 
								# part="Re"
						# ) 
						
# xls when I copy, can I get the cells that were copied, so I can populate each cell back with FORMULAS with correct referencing (ANOVA)
# give P(A) = p1 and P(B) = p2, P(!A) = 1-p1 ... that is easy to plot at they are independent ... what about P(A INTERSECT B) ... where would the overlay and intersection points be, area of circle is at most 1 based on p1 ... p2 ... 

# see http://127.0.0.1:20336/library/graphics/html/par.html ?par 
# these parameters need to be searchable with help   ?lty ... options need to be specified as k=>v where appropriate ... 
# color=transparent is allowed ... NA is not allowed for lty 
# (lty = 2:6) correspond to c("44", "13", "1343", "73", "2262").
# something about hexadecmial?
# I need map ... 
# look at 'xaxp' DESC and for options ... key=>val and descp ... then MORE 
# (0=blank, 1=solid (default), 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash) or as one of the character strings "blank", "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash", where "blank" uses ‘invisible lines’ (i.e., does not draw them).
# what does col2rgb("transparent") do? ... white with zero alpha ... FFFFFF00 ... 
# 'xaxs" ... (Only "r" and "i" styles have been implemented in R.) ... why tell me about the others ... 
# <fn>fn.name</fn> would cite a function in help ... maybe 
# <fn>package:::fn.name</fn> as an example of full where no package, :: for public, ::: for private ... 
# <param>lty</parma> could have <param grDevices::par>lty</param>
# <cite>xxx</cite> would be a generic help file, not a function, like index or DESCRIPTION? ... maybe just use <help>xxx</help> ... 
# <help grDevices>DESCRIPTION</help>
# <code> keeps spacing with a tabindent=4 feature as an option for the parser ... similar to verbatim tab in LATEX ?
# <math latex>\frac{1}{n}</math> ... 
# <math><latex>\frac{1}{n}</latex><text>1/n</text></math> 

# let's use num.toFrac here ... 
FN.PARAM = c(	"x|numeric|",
				"...|lazy loading of additional x values|",
				"return|character|[l]ast : returns the last 'good fraction' computed;\n *[e]verything : returns the entire dataframe of info used to make computation (continuous fraction, iteration to build normal fractions, error from TRUE value, etc.);\n [0-9] you can specify a number that represents the maximum size of the denominator, so for `pi` and `return=100/110/120` the result would be interesting ... / indicates separate runs ...  ",
				"max.depth|12|Passed internally into <fn>num.toCFrac<fn> to computed COntinuous fraction ... beyoned this value is 'mute' as the data is erroneous due to floating-point and system precision, try 'pi' with more digits and compare to TRUE answers.  12 seems like a good stopping point",
				"tol|sqrt(.Machine$double.eps)|Passed internally into <fn>num.toCFrac<fn> to computed COntinuous fraction ... [insert auto from that fn parameter descrption]",
				"part|Re|[Re]al or [Im]aginary part for complex ... TODO recursive call to allow [Bo]th ... if complex, maybe just do an auto detect ... OVERALL, fn seems to be computationally intensive"
			);


#' data.load
#'
#' Generically load a data object from a resource (likely URL)
#'
#' @param dfolder
#' @param dstem
#' @param mode Is it ".rds" or ".json" or ".txt" file?
#' @param main If null, it will grab from humanVerse github
#' @param raw If null, it will grab from humanVerse github
#' @param sub Is there a subfolder to use?
#' @param access.remotely Should I just access remotely OR download/cache it?
#' @param force.download If it is cached, should I force a new download?
#'
#' @return data object (likely a dataframe or a list)
#' @export
#'
#' @examples
#' ## https://github.com/MonteShaffer/humanVerse/tree/main/data/personality
#' # prds.df = data.load("personality", "personality-raw");  # from .rds
#' # ptxt.df = data.load("personality", "personality-raw", mode="txt");
#' # p.self = data.load("personality", "personality_self-en", mode="json");
#' # p.other = data.load("personality", "personality_other-en", mode="json");
#'
#'
#' ## OTHER github source
#' ## https://github.com/DataWar/imdb/tree/main/2020-Sept
#' # main = "https://github.com/DataWar/imdb/";
#' # raw = "https://raw.githubusercontent.com/DataWar/imdb/";
#' # imdb.data = data.load("2020-Sept", "imdb", mode="rds", main=main, raw=raw, sub="");
#' ## This imdb.data is 60MB compressed, 400+MB uncompressed ...
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
					cat("\n\n =-=-=-=-=-=-=-=-=-=- data.load =-=-=-=-=-=-=-=-=-=- \n\n");
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



#' data.prepare.personality
#'
#' See <https://github.com/MonteShaffer/humanVerse/tree/main/data/personality>
#' See <https://github.com/MonteShaffer/humanVerse/blob/main/data/personality/personality-raw.md>
#'
#'
#' @return a dataframe with the personality data loaded (via URL) and cleansed
#' @export
#'
#' @examples
#' # p.df = data.prepare.personality();
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







