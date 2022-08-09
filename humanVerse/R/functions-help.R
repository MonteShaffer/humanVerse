


# https://www.alphr.com/how-to-get-transcript-youtube-video/#:~:text=Open%20YouTube%20and%20the%20video%20you%20want.&text=Tap%20on%20the%20three%20dots%20below%20the%20video%20to%20select%20Open%20transcript.&text=Click%20on%20the%20three%20dots%20on%20the%20transcription.&text=Select%20the%20transcription%20using%20the%20mouse.

# https://github.com/eddelbuettel/gsir-te/blob/master/docs/Getting-Started-in-R.pdf
# https://github.com/eddelbuettel/gsir-te/blob/master/docs/Getting-Started-in-R.p1p2.png
# ?X is help(X)
# examples(X)
# apropos("rand")

# > ?%in%
# Error: unexpected SPECIAL in "?%in%"
# > help("%in%")
# > ?`%in%`

# break out "+" ... ESCAPE key

# sign(-2.9)


# https://www.tinyverse.org/
# tools::package_dependencies(package="liteq", recursive=FALSE, db=AP)$liteq
# tools::package_dependencies(package="liteq", recursive=TRUE, db=AP)$liteq

# AP = readRDS("https://cran.r-project.org/web/packages/packages.rds");

# tools::package_dependencies(package="liteq", recursive=TRUE)[["liteq"]]
# installed.packages()
# tools::installed_packages();

# https://www.analyticsvidhya.com/blog/2016/05/data-table-data-frame-work-large-data-sets/
# multi-line comments ... https://www.programiz.com/r/comments

help.packageInfo = function(package, key="dependencies", ...)
	{
	# number of authors
	# function to PARSE DESCRIPTION FILE
	
	tools::package_dependencies(package=package, recursive=TRUE)[[package]];
	}


# pkg <- "stats"
# desc_path <- system.file("DESCRIPTION", package = pkg)
# (z = read.dcf(desc_path) )

#  packagePath <- find.package(package, lib.loc, quiet = TRUE)

# https://www.crockford.com/javascript/
# https://www.crockford.com/javascript/javascript.html
# https://en.wikipedia.org/wiki/JavaScript
# https://en.wikipedia.org/wiki/Java_(programming_language)



