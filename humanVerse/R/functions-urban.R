
folder.cleanup = function(folder)
	{
	# replace _DATA_ , _SANDBOX_ , _CODE_ , _CONFIG_ 
	# try to do relative ./../../
	# remove // to / ... we assume this is a folder, not a URL 
	folder = str.replace("///", "/", folder);
	folder = str.replace("//", "/", folder);
	folder;
	}
 

# _DATA_ , _SANDBOX_ , _CODE_ , _CONFIG_ 
urban.buildWordList = function(where="_DATA_/web/", howOften = "yearly")
	{
	folder =  folder.cleanup( paste0(where, "/", "-URBAN-", "/", "YYYY", "/") );
	sitemap = folder.cleanup( paste0(folder, "/", "sitemap", "/") );
	
	
	}



# ntypes = df.getColumnTypes(x);
# paste0(ntypes, collapse="^");
# y = readFromPipe("times.txt", comment.char="#")
# later, scan header ... parse META, grab types ...
# setTYpes ... # override as.POSIX functions with origin = date.getOrigin()






## https://www.urbandictionary.com/define.php?term=Git-R-Done
## http://feeds.urbandictionary.com/UrbanWordOfTheDay
## looks like obfuscated 
## https://github.com/aflah02/Urban-Dictionary-Scraping
## https://api.urbandictionary.com/v0/uncacheable?ids=123
## https://www.freecodecamp.org/news/better-web-scraping-in-python-with-selenium-beautiful-soup-and-pandas-d6390592e251/

## get headers 
## https://pypi.org/project/selenium-wire/
## https://stackoverflow.com/questions/62262261/
## see get.asin.py ... make it better ...
# https://rapidapi.com/auth/sign-up?referral=/community/api/urban-dictionary
# https://rapidapi.com/community/api/urban-dictionary/
# key/to/api ... httr parses JSON ... 
# https://www.urbandictionary.com/robots.txt
# https://www.urbandictionary.com/sitemap-https.xml.gz
# <loc>https://www.urbandictionary.com/sitemap0-https.xml.gz</loc>
# <loc>https://www.urbandictionary.com/define.php?term=ydylle</loc>

# https://stat.ethz.ch/pipermail/r-devel/2014-May/069116.html
