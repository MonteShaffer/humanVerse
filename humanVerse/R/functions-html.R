


stringAsHTML = function(str, .noWS = NULL)
	{
	html = str;
	html = setAttribute("html", TRUE, html);
	html = setAttribute("noWS", .noWS, html);
	
	class(html) = c("html", "character");
	html;	
	}
	
# remote / local file?
	
