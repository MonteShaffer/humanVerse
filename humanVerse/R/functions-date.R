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


date.monthDictionary()
	{
	# https://connectpals.org/twelve-months-in-different-languages-with-audios/
	# https://www.loc.gov/standards/iso639-2/php/code_list.php
	en = "1-January; 2-February; 3-March; 4-April; 5-May; 6-June; 7-July; 8-August; 9-September; 10-October; 11-November; 12-December"; # english
	fr = "1-Janvier; 2-Février; 3-Mars; 4-Avril; 5-Mai; 6-Juin; 7-Juillet; 8-Août; 9-Septembre; 10-Octobre; 11-Novembre; 12-Décembre"; # french
	de = "1-Januar; 2-Februar; 3-März; 4-April; 5-Mai; 6-Juni; 7-Juli; 8-August; 9-September; 10-Oktober; 11-November; 12-Dezember"; # german
	it = "1-Gennaio; 2-Febbraio; 3-Marzo; 4-Aprile; 5-Maggio; 6-Giugno; 7-Luglio; 8-Agosto; 9-Settembre; 10-Ottobre; 11-Novembre; 12-Dicembre"; # italian
	pt = "1-Janeiro; 2-Fevereiro; 3-Março; 4-Abril; 5-Maio; 6-Junho; 7-Julho; 8-Agosto; 9-Setembro; 10-Outubro; 11-Novembro; 12-Dezembro"; # portuguese
	es = "1-Enero, 2-Febrero, 3-Marzo, 4-Abril, 5-Mayo, 6-Junio, 7-Julio, 8-Agosto, 9-Setiembre, 10-Octubre, 11-Noviembre, 12-Diciembre"; # spanish
	# https://blogs.transparent.com/latin/months-of-the-year/
	# https://glosbe.com/en/la/month's%20name
	la = "1-Martius, 2-Aprilis, 3-Maius, 4-Iunius, 5-Quintilis, 6-Sextilis, 7-September, 8-October, 9-November, 10-December, 11-Ianuarius, 12-Februarius"; # latin (pre-Caesar)
	laj = "1-Ianuarius, 2-Februarius, 3-Martius, 4-Aprilis, 5-Maius, 6-Iunius, 7-Iulius, 8-Augustus, 9-September, 10-October, 11-November, 12-December"; # latin (Julius-Caesar)
	
	
	}

# https://www.php.net/manual/en/function.strftime.php
# https://www.php.net/manual/en/function.strtotime.php
# https://adodb.org/dokuwiki/doku.php?id=v5:datetime:datetime_index
# adodb::date
date.strtotime = function(str, weight="MDY", deep=FALSE)
	{
	# https://cran.r-project.org/web/packages/stringr/vignettes/regular-expressions.html
	#
	# https://riptutorial.com/regex/example/12415/match-a-date
	#(0?[1-9]|1[0-2])([.-\\/ ]?)(0?[1-9]|[12][0-9]|3[01])\2(?:19|20)[0-9]{2} // MMDDYYYY
    #                                             ^ refer to [-/ ]
	# (0?[1-9]|[12][0-9]|3[01])([.-\\/ ]?)(0?[1-9]|1[0-2])\2(?:19|20)[0-9]{2} // DDMMYYYY
	# (?:19|20)[0-9]{2}([.-\\/ ]?)(0?[1-9]|1[0-2])\2(0?[1-9]|[12][0-9]|3[01]) // YYYYMMDD
	
	# grep("(0?[1-9]|1[0-2])([.-\\/ ]?)(0?[1-9]|[12][0-9]|3[01])\2(?:19|20)[0-9]{2}", "8/20/2008", perl=TRUE)
	# "8/20/2008"
	# "7/26/2009 13:23"
	# "7/26/2009 1:23PM"
	# "23mar1973"
	# 12/11/2015 or 11/12/2015 should give two results ... 
	# split = c("/",".","-"," ")
	# time.split = c(":", " ")
	# if deep, loop over languages ... 
	# https://connectpals.org/twelve-months-in-different-languages-with-audios/
	# "10/11/06" can be interpreted as "10 November 2006" in the DMY format, "October 11, 2006" in MDY, and "2010 November 6" in YMD.
	# The ISO 8601 format YYYY-MM-DD
	
	# Sys.setlocale("LC_TIME","Spanish Modern Sort"); weekdays(Sys.Date()+0:6)
	# Sys.setlocale("LC_TIME","English United States"); weekdays(Sys.Date()+0:6)
	# ?Sys.setlocale
	
	# https://en.wikipedia.org/wiki/Date_format_by_country
	# B – big-endian (year, month, day), e.g. 2006-04-22 or 2006.04.22 or 2006/04/22 or 2006 April 22
	# L – little-endian (day, month, year), e.g. 22.04.2006 or 22/4/2006 or 22-04-2006 or 22 April 2006
	# M – middle-endian (month, day, year), e.g. 04/22/2006 or April 22, 2006
	
	# International standard ISO 8601 (Representation of dates and times) defines unambiguous written all-numeric big-endian formats for dates, such as 2006-12-31 for 31 December 2006, and time, such as 23:59:58 for 23 hours, 59 minutes, and 58 seconds.
	
	# https://en.wikipedia.org/wiki/Date_and_time_representation_by_country
	# In certain languages such as Spanish, Portuguese, Dutch, and English the hour is divided into quarters and halves, spoken of relative to the closest hour. In Arabic, thirds of an hour are also used. (xx:20, xx:40)
	
	# In the French language, the quarters are expressed as additions or subtractions of the full hour: 
	
	
	# returns a 'Sys.time()' type element
	}
	
# https://stackoverflow.com/questions/17031002/get-weekdays-in-english-in-r	
	
# do I need to worry about precision?	
# ## Julian Day Number (JDN, https://en.wikipedia.org/wiki/Julian_day)

	
date.isLeapYear = function(year)
	{
	# _adodb_is_leap_year
	
	if ($year %% 4 != 0) 					{ return(FALSE); }
	if ($year %% 400 == 0) 					{ return(TRUE);  }
	if ($year %% 100 == 0  && $year > 1582) { return(FALSE); }
	
	return(TRUE);
	}



