


# ?Syntax ... not ?Symbols or ?Operators
# .end question ... THIS ... based on idx in array (matrix, dataframe, vector)
# abort()
# http://127.0.0.1:14469/library/base/html/Reserved.html
# > ?"for"
# > ?Control
# > ?Reserved
# help...
# is.name((y = substitute(stats))); as.character(y);
# is.symbol is more precise ... 
# casting topic to a string as well ... 
# ischar <- tryCatch(is.character(topic) && length(topic) ==     1L, error = function(e) FALSE)
#  deparse1(substitute(topic))
## utils:::index.search ???
## ?find.package
## packages <- .packages(TRUE, lib)
## .libPaths()  

## launch browser, custom template ... reorganize the HELP data ... into tabs?
## TEXT only ... ( x = library(help = package, lib.loc = NULL, character.only = TRUE) )
## x.help = x$info[[2]];
## NOT even odd, "/t" separated ?
## x.more = str.removeWhiteSpace(x.help, "|", pre.trim=FALSE);
## lines = str.removeWhiteSpace(x.help, "|"); # if not a key|value, overflow from previous line ... 
	
# df = help.parseFromLibrary("stats"); head(df); str(df)
# mask.ok ... they can make mistakes on NAMING, we can't 
# fn and line number ?
help.parseFromLibrary = function(pkg = "base", lib.loc = NULL, ...)
	{
	x = library(help = pkg, lib.loc = lib.loc, character.only = TRUE, ...);
	x.help = x$info[[2]];
	
	### if not a key|value, overflow from previous line ... 
	lines = str.removeWhiteSpace(x.help, "|"); 
	n = length(lines);
	res = list();
	plin = NULL;
	for(i in 1:n)
		{
		line = lines[i];
		lin = str.explode("|",line);
		if(!is.na(lin[2]))
			{
			res[[ lin[1] ]] = lin[2];		
			} else {
					res[[ plin[1] ]] = paste0( res[[ plin[1] ]] , lin[1] );
					}
		plin = lin;  # first one can't be EMPTY
		}

	df = as.data.frame( cbind( names(res), unname(unlist(res)) ) );
		colnames(df) = c("search", "short.description");
		
	# is.function
	# cat( normalizePath("C:/PROGRA~1/R/R-42~1.1/library/base") );
	# find.package(pkg); # [1] "C:/PROGRA~1/R/R-42~1.1/library/base"
	# utils:::index.search("sum", find.package("base") ); # [1] "C:/PROGRA~1/R/R-42~1.1/library/base/help/sum"
	## doesn't always work, is the location of the help file, not src ...
	## whereis 
	# df$whereis = utils:::index.search(df$search, find.package(pkg) );
	# C:\Program Files\R\R-4.2.1\library\base\R
	# lazyLoad(   file.path(system.file("help", package=pkg), pkg),  envir = e )
	# base.rdx and base.rdb
	# folder = file.path( system.file("R", package=pkg) )
	# files = list.files(folder, pattern = "[.][RrSsQq]+[.]$");
	# https://stackoverflow.com/questions/46184224/how-to-open-rdb-file-using-r
	# tools:::fetchRdDB # https://cran.r-project.org/doc/manuals/r-patched/R-ints.pdf
	# Adding a primitive ... [For R-core use
	# Directory Meta contains several files in .rds format, that is serialized R objects written by saveRDS. All packages have files Rd.rds, hsearch.rds, links.rds, features.rds, and package.rds. Packages with namespaces have a file nsInfo.rds, and those with data, demos or vignettes have data.rds, demo.rds or vignette.rds files.
	# hsearch.rds 
	
unser <- function(s){
  i <- as.numeric(s)
  return(rdx$variables[[i]])
}

readRDB <- function(filename, offset, size, type = 'gzip') {
       f <- file(filename, 'rb')
       on.exit(close(f))
       seek(f, offset + 4)
       unserialize(memDecompress(readBin(f, 'raw', size - 4), type), refhook=unser)
}

	# filename: the .rdb file
# offset, size: the pair of values from the .rdx
# type: 'gzip' if $compressed is TRUE, 'bzip2' for 2, 'xz' for 3
# https://stackoverflow.com/a/62394942/184614
readRDB <- function(filename, offset, size, type = 'gzip') {
        f <- file(filename, 'rb')
        on.exit(close(f))
        seek(f, offset + 4)
        unserialize(memDecompress(readBin(f, 'raw', size - 4), type))
}
		
	df = property.set("path", df, x$path);
	df = property.set("dcf", df, x$info[[1]]);
	
	df;
	}
	
	
help.index = function(deep = TRUE) {}

## str(x.help);
# ? help.start
# prompt() builds default HELP page ...
## key values as odd even 
## all = 1:length(x.help);  keys = all[is.odd(all)]; vals = all[is.even(all)];
## x.info = list.create(x.help[keys], x.help[vals]);
### x.info = x.help[vals];
### names(x.info) = x.help[keys];
## This lists all topics ... 

## port <- tools::startDynamicHelp(NA)
## package = "base";
## browser = getOption("browser")
## browseURL(paste0("http://127.0.0.1:", port, "/library/", package, "/html/00Index.html"), browser)

# synthesize META ... seems like d/l file gets recycled and original .Rd files are destroeyed as META is built with LAZY LOADING ... 
# -SANDBOX-/downloads/-github- or /-CRAN- 
# /downloads/-wiki-/
# downloads/generic/
# /downloads/-urban-/

# meta-CRAN ... # https://www.r-pkg.org/

# whois ... https://www.r-pkg.org/maint/hadley@rstudio.com
# Update the CRANDB database from CRAN, one a minute. (We only perform a HEAD request and check the Etag to see if there is anything new, and don't actually download anything, unless there is something new.)

## ACTIVE
# https://cloud.r-project.org/robots.txt
## ACTIVE/ARCHIVED
# view-source:https://cran.r-project.org/src/contrib/Archive/
# ARCHIVED b/c "problems"
# https://cran.r-project.org/web/packages/RBerkeley/
# https://cran.r-project.org/src/contrib/Archive/RBerkeley/
# https://dbdb.io/db/tokyo-cabinet
# https://dbdb.io/db/kyoto-cabinet
# https://stackoverflow.com/questions/43731456/how-to-construct-reverse-btree
# https://dbmx.net/kyotocabinet/pythondoc/
# http://fallabs.com/kyotocabinet/api/  # C++
# http://fallabs.com/kyotocabinet/api/namespacekyotocabinet.html#a58209fa250ad75178ca0379f1034ad5e
# https://dbmx.net/tkrzw/
# https://dbmx.net/tkrzw/#overview
# store "base::" and "base:::" as two entries in "packages.tkh"
# values are binary store of functions ... as.bin(serialize(vector))
# store "base::fn" or "base:::fn" as entries in "functions.tkh"
# values are binary store (src, body, formal, location, line number, whatever)
# store "base::help-topic" in "help.tkh"
## index with performance tuning ... # This is a code example which represents a more serious use case with performance tuning and thorough error checks.

# maybe separate "wildcard.tkh" structure, maybe DAWG?
# DAWG 
# https://dawg.readthedocs.io/en/latest/
# https://code.google.com/archive/p/dawgdic/
# # http://libb64.sourceforge.net/ # base64 encode
## python https://openbase.com/python/DAWG
# ?https://en.wikipedia.org/wiki/Suffix_automaton
# ? javascript version on github : 
# https://github.com/mckoss/dawg
# https://johnresig.com/blog/javascript-trie-performance-analysis/
# https://en.wikipedia.org/wiki/Suffix_automaton#Construction_algorithm
# https://johnresig.com/blog/javascript-trie-performance-analysis/
# https://johnresig.com/blog/revised-javascript-dictionary-search/
# SUCCINT TRIE
# https://web.archive.org/web/20170324192341/http://lookups.pageforest.com/index.html
# https://github.com/mckoss/lookups
## 
## SUCCINT ... http://stevehanov.ca/blog/index.php?id=120
## https://www.hanovsolutions.com/trie/Bits.js
## http://stevehanov.ca/blog/index.php?id=115
## Ensure that words are inserted in alphabetical order. That way, when you insert a word, you will then know for sure whether the previous word ended an entire branch. For example, "cat" followed by "catnip" does not result in a branch, because the s just added to the end. But when you follow it with "cats" you know that the "nip" part of the previous word needs checking.
## Each time you complete a branch in the trie, check it for duplicate nodes. When a duplicate is found, redirect all incoming edges to the existing one and eliminate the duplicate.
## https://rhymebrain.com/en/What_rhymes_with_cat.html
## http://iswsa.acm.org/mphf/index.html
## http://iswsa.acm.org/mphf/mphf.js
## http://stevehanov.ca/blog/?id=130
## http://stevehanov.ca/blog/?id=120
## ## First, we add a "super root". This is just an additional node above the root. It's there to make the math work out later.
## claims it is also pagerank
## https://gist.github.com/smhanov/94230b422c2100ae4218
## # https://pkg.go.dev/github.com/smhanov/dawg?utm_source=godoc
## https://github.com/smhanov/dawg
## I could use 2-bits ... 
## https://johnresig.com/about/
## created jquery
## Today I'm announcing a major change in my life: I'm leaving Mozilla Corporation and joining Khan Academy. I joined the Mozilla Corporation in January of 2007, ...  ## Posted: May 3rd, 2011
# MA-FSA ... https://pkg.go.dev/github.com/smartystreets/mafsa#section-readme
## https://stackoverflow.com/questions/14025709/how-to-create-a-dawg
## build so javascript compatibile, wildcard compatible ... 
## https://en.wikipedia.org/wiki/DFA_minimization
## wildcard DAWG
## https://stackoverflow.com/questions/2815083/efficient-data-structure-for-word-lookup-with-wildcards
## http://stevehanov.ca/blog/?id=114
## levenstien distance 

## http://24.186.219.208/PeopleWeb/?q=*ill+smith&x=0&y=0&all=on
## http://www.softcorporation.com/products/suggester/source/com/softcorporation/suggester/
## JAVA 
## http://fiber-space.de/wordpress/
## ## http://fiber-space.de/wordpress/2011/01/07/fuzzy-string-matching-ii-matching-wordlists/
## 
## http://stevehanov.ca/blog/?id=115
## https://gist.githubusercontent.com/smhanov/94230b422c2100ae4218/raw/f4b7880e2eca8d7a22f070821b2e4ef1871810cc/dawg.py
## not fuzzy? >>> http://iswsa.acm.org/mphf/mphf.js
## http://stevehanov.ca/blog/index.php?id=119
## ## Unfortunately, to *guarantee* (strictly speaking) that the key was in the table, you do need to store a copy of the full key. This is what "standard" hashtables do; see e.g. HashMap.java (openjdk jdk8 source, line 280).
# http://iswsa.acm.org/phf-mysqlstopwords.c
# http://iswsa.acm.org/mphf/openDSAPerfectHashAnimation/perfectHashAV.html
## http://cmph.sourceforge.net/

## https://randorithms.com/2019/09/12/MPH-functions.html
## https://blog.demofox.org/2015/12/14/o1-data-lookups-with-minimal-perfect-hashing/

## http://stevehanov.ca/blog/index.php?id=119





# See NAMESPACE
# functions.whereis("useDynLib")
# functions.whereis("export")
# importFrom(utils


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



