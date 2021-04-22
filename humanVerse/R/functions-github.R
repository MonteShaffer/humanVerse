
github.listFiles = function(github.user="MonteShaffer", github.repo="humanVerse", github.path="", ...)
	{
	args = getFunctionParameters();
	force.download = isForceDownload(args);
	cat("\n", "force.download ... ", force.download, "\n\n");

	url = github.buildPath(github.user, github.repo);
	url = paste0(url, github.path);

	res = github.parseList(url, force.download = force.download);
		res = setAttribute("url", url, res);
		res = setAttribute("force.download", force.download, res);
	res;
	}



github.buildFromRepo = function(github.user="MonteShaffer", github.repo="humanVerse", force.download=FALSE, candidate=1)
	{
	## this will get the zip file from the webpage ...
			# url = github.buildPath(github.user, github.repo);
			# res = github.parseList(url, force.download = force.download);

			# https://codeload.github.com/MonteShaffer/humanVerse/legacy.tar.gz/main

			# zip.url = getAttribute("zipclone", res);
			# if(is.null(zip.url))
				# {
				# stop("no zip.url attached to url");
				# }

	
	tarball = github.buildPath(github.user=github.user, github.repo=github.repo, which="tar.gz");
	
	cat("\n\n", "\t DOWNLOADING tarball: \n\n\t\t", tarball, "\n\n");	
	
	local = getRemoteAndCache(tarball, force.download=force.download, append=".tar.gz");
	
	cat("\n\n", "\t LOCAL [.tar.gz]: \n\n\t\t", as.character(local), "\n\n");	
	
	outpath = cleanup.local( paste0(dirname(local), "/", basename(local), "-untar-/") );
		createDirectoryRecursive(outpath);
	cat("\n\n", "\t UNPACKING ... \n\n\t\t", outpath, "\n\n");
	untar(local, exdir = outpath); # will uncompress
	
	myfiles = list.files(outpath, pattern = "\\.Rbuildignore$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE, all.files = TRUE);
	
	cat("\n\n =-=-=-=-=-=-=-=-=-=- CANDIDATES =-=-=-=-=-=-=-=-=-=- \n\n");
	cat("\n\n", "  [ first ---> ]", paste(myfiles, collapse="\n\t"), "\n\n\n");
	
	if(length(myfiles) == 0)
		{
		stop("no eligible candidate with '.Rbuildignore' found!");
		}
	
	buildpath = cleanup.local( dirname(myfiles[candidate]) );
	cat("\n\n", "\t Build Path ... \n\n\t\t", buildpath, "\n\n");
	setwd(buildpath);
	
	cat("\n\n", "\t Executing [shell] 'R CMD build ./'", "\n\n");	
	shell( paste0("R CMD build ./"), intern=T);
	}


#' github.installBinary
#'
#' @return
#' @export
#'
#' @examples
github.installBinary = function(github.user="MonteShaffer", github.repo="humanVerse", github.path="", pattern = "\\.zip$", github.version="latest", ...)
	{
	## This doesn't grab the clone element, assumes you have a .zip uploaded ...
	## https://stackoverflow.com/questions/67144476/
	## you could download "clone", look for .Rproj, BUILD that folder to ZIP, then INSTALL

	## https://github.com/MonteShaffer/humanVerse/tree/main/humanVerse

	## https://github.com/r-lib/devtools/archive/refs/heads/master.zip
	## for me, this is the outer layer ...
	## https://github.com/MonteShaffer/humanVerseWSU/archive/refs/heads/master.zip
	## unzip ... look for github.repo ... humanVerse.Rproj ... or /R /man DESCRIPTION NAMESPACE in folder to figure out which folder ... maybe '.Rbuildignore'
	## build ...  ?build vs ?devtools::build
	## just build a .tar.gz from github ...


	# .git.ignore is not allowing the other ...
	# build-source ==> .tar.gz
	# build-binary ==> .zip  [WINDOZE]
	# maybe ==> .tgz
	# ?install.packages
	github.df = github.listFiles(github.user=github.user, github.repo=github.repo, github.path=github.path, ...);

	#idx.zips = grep("\\.zip$", github.df$links);
	#idx.tars = NULL;
	# idx.tars = grep("\\.tar.giz$", github.df$links);
	# github.idx = c(idx.zips, idx.tars);
	
	github.idx = grep(pattern, github.df$links);
	
	if(length(github.idx) == 0)
		{
		stop("no candidates");
		}

	github.candidates = github.df[github.idx,] ;
	##

	cat("\n\n =-=-=-=-=-=-=-=-=-=- CANDIDATES =-=-=-=-=-=-=-=-=-=- \n\n");
	cat("\n\n", "  [ latest ---> ]", paste(github.candidates$name, collapse="\n\t"), "\n\n\n");

	github.zip = github.candidates[1,];  # these are sorted by latest ...

	if(github.version != "latest")
		{
		# github.version is wildcard, grab first ...
		grx = utils::glob2rx(github.version);
		grx.grep = grep(grx, github.candidates$links);
			if(length(github.grep) > 0)
				{
				my.idx = grx.grep[1]; # first match
				github.zip = github.candidates[my.idx,];
				}
		}

	cat("\n\n", paste( github.zip[c(1,2)], collapse = "\t\t"), "\n\n\n");
	zip.url = as.character(github.zip[8]);

	cat("\n\n", "\t\t", "      FROM: ", as.character(github.zip[8]), "\n\n");
	myzip = getRemoteAndCache(zip.url, ...);

	cat("\n\n", "\t\t", "TO INSTALL: ", as.character(github.zip[1]), "\n\n");
	install.packages(myzip, repos=NULL, type="source");
		# unzips into folder 'C:/Users/Alexander Nevsky/Documents/R/win-library/4.0'
	}


github.buildPath = function(github.user="", github.repo="", which="http")
	{
	method = tolower(which);
	
	str = switch(method,
            "raw"    = paste0("https://raw.githubusercontent.com/", github.user, "/", github.repo, "/"),
			
            "https"    = paste0("https://github.com/", github.user, "/", github.repo, "/"),
			"http"    = paste0("https://github.com/", github.user, "/", github.repo, "/"),
			
			"legacy"    = paste0("https://codeload.github.com/", github.user, "/", github.repo, "/legacy.tar.gz/main"),
			"tar.gz"    = paste0("https://codeload.github.com/", github.user, "/", github.repo, "/legacy.tar.gz/main"),
			
			

           paste0("https://github.com/", github.user, "/", github.repo, "/") # default case of switch
          );
		  
		  
	cleanup.url(str);
	}



#' github.includeFolder
#'
#' @param url
#' @param ...
#'
#' @return
#' @export
#'
#' @aliases includeGithubFolder
#'
#' @examples
github.includeFolder = function(url, ...)  # pattern = "[.][RrSsQq]$",
	{
	args = getFunctionParameters();
	##cat("\n\n === MY-ARGS === \n\n");	# print(args);	# dput(args);

	force.download = isForceDownload(args);
	cat("\n", "force.download ... ", force.download, "\n\n");

	github.df = github.parseList(url, force.download = force.download);
	github.df = subsetDataFrame(github.df, "folder", "==", FALSE);

	links = na.omit(github.df$links);

	includeRemoteFiles(links, ...);
	}
	



#' github.parseList
#'
#' @param url
#' @param force.download
#'
#' @return
#' @export
#'
#' @examples
github.parseList = function(url, force.download = FALSE)
	{
	html.local = getRemoteAndCache(url, force.download = force.download);
	html.cache = str_replace(".html", ".cache", html.local);

	github.base = github.buildPath();
	github.raw 	= github.buildPath(which="raw");

	### Could we do API/JSON instead of HTML CACHING?
		### github.api = "https://api.github.com/";
		## https://api.github.com/repos/MonteShaffer/humanVerse/git/trees/main
		##  ==> https://api.github.com/repos/MonteShaffer/humanVerse/git/trees/75741912434181b468b761303eaa3ec312998e1d
		### if(type == "blob") AND extension = ".R" ... include ...
		### less to parse with HTML

	if(file.exists(html.cache) && !force.download)
		{
		cat("\n", "============", "GRABBING FROM CACHE", "============", "\n");
		results = readRDS(html.cache);
		} else {
		    cat("\n", "============", "DOWNLOADING DIRECTORY PAGE", "============", "\n");
				html.str = readStringFromFile(html.local);

				page.info = sliceDiceContent(html.str, start='<div class="js-details-container Details">', end='<div class="Details-content--shown', strip=FALSE, direction="start");

				results = NULL;
				page.rows = explodeMe('<div role="row"', page.info);
				nr = length(page.rows);
				for(i in 2:nr)
					{
					row = explodeMe('<span', page.rows[i]);
						row.dt = explodeMe("T", sliceDiceContent(row[3], start='datetime="', end='"', strip=TRUE, direction="start") );



						row.link = sliceDiceContent(row[2], start='href="', end='"', strip=TRUE, direction="start");
							tmp = explodeMe("/", row.link); ntmp = length(tmp);
						row.name = tmp[ntmp];
						row.commit = sliceDiceContent(row[3], start='href="', end='"', strip=TRUE, direction="start");
						row.commit.info = sliceDiceContent(row[3], start="\n", end='<a>', strip=TRUE, direction="start");

						row.time = paste0(row.dt[1], " ", str_replace("Z", "", row.dt[2]) );


					rinfo = c(row.name, row.time, row.link, row.commit, row.commit.info);

					results = rbind(results, rinfo);
					}

				results = as.data.frame(results);
					colnames(results) = c("name", "when", "url", "commit", "commit.info");
					# rownames(results) = results$name; # should be unique
					rownames(results) = NULL;
				results$when.time = asDateTime(results$when);
				results = sortDataFrameByNumericColumns(results,"when.time","DESC"); # newest first
				results$folder = !is.substring(results$url, "/blob/"); # blobs are files ...

					links = cleanup.url( paste0(github.raw, results$url) );
					links = str_replace("/blob/", "/", links);
				results$links = "";
				results$links[ which(!results$folder) ] = links [ which(!results$folder) ];

				
				zipclone = sliceDiceContent(html.str, start='data-open-app="link" href="', end='">', strip=FALSE, direction="start");
				zipclone = cleanup.url( paste0(github.base, zipclone) );

				results = setAttribute("zipclone", zipclone, results);

				# pipe loses "attributes" ...
				# writeToPipe(results, html.cache);
				writeRDS(results, html.cache);
				}
	results;
	}	
