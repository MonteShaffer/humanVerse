

#' inflation.currentData
#'
#' This loads the most up-to-date inflation data from URL.
#'
#' @param store.global If TRUE, it will be accessible for inflation functions
#'
#' @return data.frame, invisible
#' @export
#' 
#' @family Inflation
#' 
#' 
#' @aliases currentInflationData
inflation.currentData = function(store.global = TRUE)
  {
  html = "https://www.officialdata.org/us/inflation/2000?endYear=1920&amount=1000000";
  local = getRemoteAndCache(html, force.download = TRUE, append=".html");
  cache = str_replace(".html", ".cache", as.character(local));
  
  if(file.exists(cache))
	{
	res = readFromPipe(res, cache);
	} else 	{
			str = readStringFromFile(local);

			  info = parse.sliceDiceContent(str, start='<TABLE class="regular-data table-striped" style="margin: 0 auto">', end='</TABLE>');

			  info2 = parse.sliceDiceContent(info, start='<TBODY>', end='</TBODY>');

			  res = NULL;
			  rows = explodeMe('</tr>',info2);
			  i = 1;
			  for(row in rows)
				{
				row = trimMe(row);
				cols = explodeMe('</td>', row);
				j = 1;
				myres = c();
				for(col in cols)
					{

					col = trimMe(strip_tags(col));
						col = str_replace("$", "", col);
						col = str_replace(",", "", col);
						col = str_replace("%", "", col);
					col = trimMe(strip_tags(col));
					col = as.numeric(col);
					myres = c(myres, col);
					j = 1 + j;
					}
				if(length(myres) == 3)
					{
					res = rbind(res, myres);
					}
				i = 1 + i;
				}

			  res = as.data.frame(res);
				colnames(res) = c("year", "dollar", "percent");
				rownames(res) = res$year;

			  res = assignColumnsTypeInDataFrame(c("year", "dollar"), "integer", res);
			  
			writeToPipe(res, cache);
			}
  

  if(store.global) { .GlobalEnv$.humanVerse[["inflation"]] = res; }

  invisible(res);
  }



#' inflation.loadData
#'
#' This data was pulled from
#' \url{https://www.officialdata.org/us/inflation/2000?endYear=1920&amount=1000000}
#' in September 2020.
#'
#' It contains dollar information from 1920 to 2020, 101 observations.
#'
#' Likely with throw an error if you specify a value not in that range.
#'
#' @family Inflation
#'
#'   inflation.df global gets assigned to the inflation.rds (equivalent to
#'   inflation.txt)
#' @export
#' 
#' @aliases loadInflationData
#' @aliases loadDataInflation
inflation.loadData = function()
  {
  # idf = readRDS( system.file("extdata", "inflation.rds", package="humanVerseWSU") );
  idf = data.load("inflation");
  .GlobalEnv$.humanVerse[["inflation"]] = idf;

  invisible(idf);
  }






#' inflation.standardizeDollarsInDataFrame
#'
#' @family Inflation
#'
#' @param df dataframe containing dollar.source and year.source
#' @param anchor.year base year to convert all dollars to
#' @param dollar.source column name (in df) with raw dollars
#' @param year.source column name (in df) with 4-digit years (1920 - 2020)
#' @param dollar.out new column name (in df) to be created
#' @param idf inflation data frame
#'
#' @return dataframe, updated
#' @export
#' 
#' @aliases standardizeDollarsInDataFrame
#' @examples
#' # loadInflationData();
#' # todo once I get Will/Denzel data ...
inflation.standardizeDollarsInDataFrame = function(df, anchor.year, dollar.source, year.source, dollar.out, idf=.GlobalEnv$.humanVerse[["inflation"]])
	{
	dollars = as.numeric( unlist(df[dollar.source]) ); # we assume this is numeric ...
	years = as.numeric( unlist(df[year.source]) ); # I could do unique on years, to speed this up slightly

	# not efficient #

	dollars.n = length(dollars);

	newdollars = numeric(dollars.n);

	for(i in 1:dollars.n)
		{
		mydollar = dollars[i];
		myyear = years[i];
		nd = NA;
		if(!is.na(mydollar)) { nd = adjustDollarForInflation(mydollar,myyear,anchor.year,idf=idf); }
    # if(is.null(nd)) { nd = NA; }
		# cat("\n nd: ", nd, "\n");
		newdollars[i] = nd;
		}

	df[dollar.out] = newdollars;

	df;
	}


#' inflation.lookupDollar
#'
#' @family Inflation
#'
#' @param year numeric, 4-digit year   (1920 - 2020)
#' @param idf inflation data frame
#'
#' @return numeric dollar from idf table for that year
#' @export
#' 
#' @aliases lookupInflationDollar
#' @examples
#' # loadInflationData();  # does lookup to create ratios
#' # lookupInflationDollar( 1943 ); # $  865,000
#' # lookupInflationDollar( 2000 ); # $8,610,000
#'
inflation.lookupDollar = function(year,idf=.GlobalEnv$.humanVerse[["inflation"]])
	{
	res = idf[idf$year==year,2];  # single form ...
	if(length(res) == 0) { res = NA; }
	res;
	}




#' inflation.adjustDollar
#'
#' @family Inflation
#'
#' @param mydollar current dollar value
#' @param myyear current year value (4-digit format, 1920 to 2020)
#' @param newyear new year value (4-digit format, 1920 to 2020)
#' @param idf inflation data frame
#'
#' @return dollar, updated (adjusted from myyear to newyear)
#' @export
#' 
#' @aliases adjustDollarForInflation
#' @examples
#' # loadInflationData();
#' # adjustDollarForInflation( 123, 1943, 2000 ); # $123 in 1943 is about $1224.31 in 2000
#' # adjustDollarForInflation( 123, 2000, 1943 ); # $123 in 2000 is about $12.36 in 1943
#'
inflation.adjustDollar = function(mydollar,myyear,newyear,idf=.GlobalEnv$.humanVerse[["inflation"]])
	{
  # cat("\n", "mydollar: ", mydollar, " ... myyear: ", myyear, " ... newyear: ", newyear, "\n");
	# use basic ratio
	dollar.myyear 	= lookupInflationDollar(myyear,idf);
	dollar.newyear 	= lookupInflationDollar(newyear,idf);
	
	# this will return NA if one is not found ?

	ratio = dollar.newyear/dollar.myyear;

	nd = mydollar*ratio;

	nd[1];
	}




