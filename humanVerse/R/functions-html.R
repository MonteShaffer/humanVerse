
# https://developers.google.com/profile/u/106141697625724185423
# YOUTUBE API ... get transcripts
# https://developers.google.com/youtube/v3/code_samples/php
# # https://developers.google.com/s/results/youtube/v3/?q=transcript
# https://pypi.org/project/youtube-transcript-api/
# https://stackoverflow.com/questions/14061195/how-to-get-transcript-in-youtube-api-v3
# https://www.youtube.com/watch?v=K7FtUlnIXd0
# https://www.captionsgrabber.com/8302/get-captions.00.php?id=K7FtUlnIXd0
# view-source:https://www.captionsgrabber.com/
# ERROR:  https://gdata.youtube.com/feeds/api/videos/K7FtUlnIXd0?v=2&fields=published,title,author










#' castStringAsHTML
#'
#' RStudio/Knitr doesn't play nice with external HTML, this may fix it?
#'
#' @param str string that is HTML code
#'
#' @return same string with appended attribute
#' @export
castStringAsHTML = function(str)
	{
  # remote / local file?
	str = setAttribute("html", TRUE, str);

	class(str) = c("html", "character");
	str;
	}


