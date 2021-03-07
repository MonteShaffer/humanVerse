



	
# remote / local file?
	
castStringAsHTML = function(str)
	{
	str = setAttribute("html", TRUE, str);

	class(str) = c("html", "character");
	str;
	}

	
