#' convertDateStringToFormat
#'
#' basically wraps strptime and strftime into this single call
#'
#' I have this vector of dates in string format;
#' I want to convert it to this format (numeric),
#' and currently, they are of this format.
#'
#' @family Date-Time
#'
#' @param strvec one or more strings, such as \code{"3/24/2010 18:33"}
#' @param to how you want to return the date \code{"\%Y"} is just Year
#' @param to.name name(s) given to the \code{to} column(s)
#' @param from format of input, default \code{"\%Y-\%m-\%d \%H:\%M:\%S"}
#' @param num if TRUE (default), will return numeric form.
#'
#' @return dataframe same length as strvec, with one or more columns
#' @export
#' @examples
#'
#' date.strings = c("3/24/2010 18:33", "9/3/2009 17:28", "10/14/2009 11:40",
#'                  "7/3/2015 11:16", "11/18/2010 1:29", "4/23/2011 0:08",
#'                  "10/6/2010 11:13", "7/26/2009 13:23","4/9/2008 13:40",
#'                  "8/20/2008 11:32");
#'
#' years = convertDateStringToFormat(date.strings,
#'                              "%Y", "years",
#'                                                            "%m/%d/%Y %H:%M");
#'
#' weeks = convertDateStringToFormat(date.strings,
#'                              "%W","weeks",
#'                                                             "%m/%d/%Y %H:%M");
#'
#' days = convertDateStringToFormat(date.strings,"
#'                               %j", "days",
#'                                                            "%m/%d/%Y %H:%M");
#'
#' ywd = convertDateStringToFormat( date.strings,
#'                              c("%Y","%W","%j"), c("year","week","day"),
#'                                                            "%m/%d/%Y %H:%M");
#'
#'
#' Ymd = convertDateStringToFormat(date.strings,
#'                               "%Y-%m-%d","ISO8601",
#'                                                  "%m/%d/%Y %H:%M",num=FALSE);
#'
convertDateStringToFormat = function (strvec,to="%Y",to.name="years",from="%Y-%m-%d %H:%M:%S",num=TRUE)
	{
  nt = length(to);
  result = NULL;
  for(i in 1:nt)
    {
	  p.obj = strptime(strvec, format=from );
	  o.obj = strftime(p.obj,  format=to[i] );
	  n.obj = if(num) { as.numeric(o.obj); } else { o.obj; }
	  result = cbind(result, n.obj);
    }
    colnames(result) = to.name;
	as.data.frame(result);
	}


toSystemTime = function(numvec, which="ct", origin="1970-01-01")
	{	
	# reverses as.numeric(Sys.time());	
	
	# https://rstudio-pubs-static.s3.amazonaws.com/28038_1bcb9aa80ca84f27ace07d612872861a.html
	# now <- Sys.time(); class(now);
	# https://stat.ethz.ch/R-manual/R-devel/library/base/html/as.POSIXlt.html
	# try formats ...
	
	if(which != "ct")
		{
		as.POSIXlt(numvec, origin=origin);
		} else	{
				as.POSIXct(numvec, origin=origin);
				}	
	}
	
	
asDateTime = function(strvec, from="%Y-%m-%d %H:%M:%S", to="", num=TRUE)
	{
	# ?as.Date
	# https://www.r-bloggers.com/2013/08/date-formats-in-r/
	p.obj = strptime(strvec, format=from );
	res = p.obj;
	if(to != "") { o.obj = strftime(p.obj,  format=to); res = o.obj; }
	if(num) { res = as.numeric(res);}
	
	res;	
	}


#' getDate
#'
#' Wrapper for 'format' function
#'
#' @param how See options using ?as.Date for codes
#' @param when You can pass in a time, or let it grab current time
#'
#' @return date string
#' @export
#'
#' @examples
#' getDate();
#' getDate("%Y-%m-%d");
#' getDate("%Y-%m-%d %H:%M:%S");
#'
#' getDate("%Y-%m-%d", strptime("23mar1973", "%d%b%Y") );
getDate = function(how="%Y-%m", when = Sys.time())
	{
  # php date syntax is a bit different
  # doesn't allow for microtime
  # this needs to be updated to a modern version of the ISO standard (R::base)
	format(when, how);
	}


#' date.isLeapYear
#'
#' @param year
#'
#' @return
#' @export
#'
#' @examples
date.isLeapYear = function(year)
	{
	# _adodb_is_leap_year

	if (year %% 4 != 0) 					        { return(FALSE); }
	if (year %% 400 == 0) 					      { return(TRUE);  }
	if (year %% 100 == 0  && year > 1582) { return(FALSE); }

	return(TRUE);
	}



