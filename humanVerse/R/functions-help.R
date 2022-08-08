
# https://github.com/eddelbuettel/gsir-te/blob/master/docs/Getting-Started-in-R.pdf
# https://github.com/eddelbuettel/gsir-te/blob/master/docs/Getting-Started-in-R.p1p2.png
# ?X is help(X)
# examples(X)
# apropos("rand")

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



