

IMDB.buildSocialNetwork = function(return="both", fill=1, ttids=NULL, imdb.data=imdb.data, use.cpp="auto")
	{	
	if(is.character(use.cpp)) 
		{ 
		use.cpp = requireNamespace("Rcpp", quietly = TRUE); 
		} else 	{
				if(use.cpp) 
					{ 
					use.cpp = requireNamespace("Rcpp", quietly = TRUE); 
					if(!use.cpp)
						{
						warning("You wanted to use 'Rcpp', but not found, using traditional");
						}
					}
				}
	
	network = imdb.data$movies.df$cast; 		
	
	if(is.null(ttids)) { ttids = network.ttids; }  # all of them 
		nids = as.data.frame(cbind(ttids)); colnames(nids) = "ttid";		
		net = merge(network, nids, by="ttid");		
	
		net.ttids = sort( unique(net$ttid) );
		net.nmids = sort( unique(net$nmid) );
		
		n.ttids = length(net.ttids);
		n.nmids = length(net.nmids);
		
		if(n.ttids < 2)
			{
			print(net);
			stop("we don't have enought data");
			}
			
	
	
	# AM = actors in row, M in cols
	AM = matrix(0, nrow=n.nmids, ncol=n.ttids);
		rownames(AM) = net.nmids;
		colnames(AM) = net.ttids;
	# this does not use Matrix "sparse" class yet ...	
		
	# dim(AM);
	# AM[1:10,1:5];
	
	
	

nrow = nrow(network);
nrow;

for(i in 1:nrow)
  {
  if(i %% 25000 == 1) { print(i); flush.console();}
  row = network[i,];
  ttid = row$ttid;
  nmid = row$nmid;

  r = which(my.nmids == nmid);
  c = which(my.ttids == ttid);
  AM[r,c] = 1;
  }

sum(AM);
	
	# nrow = nrow(network);
# nrow;
# 
# for(i in 1:nrow)
#   {
#   if(i %% 25000 == 1) { print(i); flush.console();}
#   row = network[i,];
#   ttid = row$ttid;
#   nmid = row$nmid;
#   
#   r = which(my.nmids == nmid);
#   c = which(my.ttids == ttid);
#   AM[r,c] = 1;
#   }
# 
# sum(AM);

#AA = AM %*% t(AM);
#dim(AA);

# MM = t(AM) %*% (AM);
# dim(MM);
		
		
		
	}

IMDB.loadData = function(which="2020-Sept", store.global=TRUE)
	{
	imdb.data = NULL;
	if(which == "2020-Sept")
		{
		main = github.buildPath("DataWar", "imdb");
		raw  = github.buildPath("DataWar", "imdb", "raw");
		imdb.data = data.load("2020-Sept", "imdb", mode="rds", main=main, raw=raw, sub="");
		}
	if(store.global && !is.null(imdb.data)) { .GlobalEnv$imdb.data = imdb.data; }
	}

#' IMDB.getMovieInfoFromActorSearch
#'
#' @param ttid Film identifier `ttid`
#' @param return.cols list of columns you want to display, by default all
#' @param imdb If you want to apply this function to a different dataframe, it's possible
#'
#' @return dataframe of results ... ...
#' @export
IMDB.getMovieInfoFromActorSearch = function(ttid, return.cols=NULL, imdb=imdb.data$all.actors.movies)
  {
  info = stats::na.omit( imdb[imdb$ttid == ttid, ] ) ;
  if(is.null(return.cols)) { info; } else { info[, return.cols]; }
  }


#' IMDB.searchMovieTitle
#'
#' @param str Search string with basic wildcard `*` operator
#' @param return.cols list of columns you want to display, by default all
#'
#' @param ignore.case Defaults to TRUE, matches "mont" and "Mont"
#' @param perl Defaults to FALSE, convert string query to PERL regex?
#' @param imdb If you want to apply this function to a different dataframe, it's possible
#'
#'
#' @return dataframe of results
#' @export
IMDB.searchMovieTitle = function(str, return.cols=NULL, ignore.case=TRUE, perl=FALSE, imdb=imdb.data$all.actors.movies)
  {
  grx = utils::glob2rx(str);  # https://stackoverflow.com/questions/5823503/
  grx.grep = grep(grx,imdb$title, ignore.case=ignore.case, perl=perl);
  rows = imdb[grx.grep, ];
  if(is.null(return.cols)) { rows; } else { rows[, return.cols]; }
  }


#' IMDB.getPersonInfo
#'
#' @param nmid  Person identifier `nmid`
#' @param return.cols list of columns you want to display, by default all
#' @param imdb If you want to apply this function to a different dataframe, it's possible
#'
#' # dim(imdb.data$all.actors.info);  # 71332    10
#'
#' @return dataframe of results
#' @export
IMDB.getPersonInfo = function(nmid, return.cols=NULL, imdb=imdb.data$all.actors.info)
  {
  info = imdb[imdb$nmid == nmid, ];
  if(is.null(return.cols)) { info; } else { info[, return.cols]; }
  }



#' IMDB.getMoviesForPerson
#'
#' @param nmid  Person identifier `nmid`
#' @param return.cols list of columns you want to display, by default all
#' @param imdb If you want to apply this function to a different dataframe, it's possible
#'
#' dim(imdb.data$all.actors.info);     # 71332      10
#' dim(imdb.data$all.actors.movies);   # 282062     11
#' # glue table ::  nmid|ttid|rank ... rank from ActorSearchMovies
#' dim(imdb$all.actors.rank);          # 1842305     3
#'
#' @return dataframe of results
#' @export
IMDB.getMoviesForPerson = function(nmid, return.cols=NULL, imdb=imdb.data)
  {
  info = imdb$all.actors.rank[imdb$all.actors.rank$nmid == nmid, ];
  info.more = merge(info, imdb$all.actors.movies, by="ttid");
  info.more = sortDataFrameByNumericColumns(info.more,"rank", "ASC");

  if(is.null(return.cols)) { info.more; } else { info.more[, return.cols]; }
  }


#' IMDB.getActorsFromMovie
#'
#' @param ttid  Movie identifier `ttid`
#' @param return.cols list of columns you want to display, by default all
#' @param imdb If you want to apply this function to a different dataframe, it's possible
#'
#' dim(imdb.data$all.movies.actors.characters);     # 901324      4
#'
#' @return dataframe of results
#' @export
IMDB.getActorsFromMovie = function(ttid, return.cols=NULL, imdb=imdb.data)
  {
  info = imdb.data$all.movies.actors.characters[imdb.data$all.movies.actors.characters$ttid == ttid, ];
  info.more = info;
  info.more = sortDataFrameByNumericColumns(info.more,"actor.rank", "ASC");
  if(is.null(return.cols)) { info.more; } else { info.more[, return.cols]; }
  }


#' IMDB.searchPersonName
#'
#' @param str Search string with basic wildcard `*` operator
#' @param return.cols list of columns you want to display, by default all
#'
#' @param ignore.case Defaults to TRUE, matches "mont" and "Mont"
#' @param perl Defaults to FALSE, convert string query to PERL regex?
#' @param imdb If you want to apply this function to a different dataframe, it's possible
#'
#'
#' @return dataframe of results
#' @export
IMDB.searchPersonName = function(str, return.cols=NULL, ignore.case=TRUE, perl=FALSE, imdb=imdb.data$all.actors.info)
  {
  grx = utils::glob2rx(str);  # https://stackoverflow.com/questions/5823503/
  grx.grep = grep(grx,imdb$name, ignore.case=ignore.case, perl=perl);
  rows = imdb[grx.grep, ];
  if(is.null(return.cols)) { rows; } else { rows[, return.cols]; }
  }


#' IMDB.genericSearch
#'
#' @param str Search string with basic wildcard `*` operator
#' @param col.name The column to perform search on
#' @param return.cols list of columns you want to display, by default all
#'
#' @param ignore.case Defaults to TRUE, matches "mont" and "Mont"
#' @param perl Defaults to FALSE, convert string query to PERL regex?
#' @param imdb If you want to apply this function to a different dataframe, it's possible

#'
#' @return dataframe of results
#' @export
IMDB.genericSearch = function(str, col.name, return.cols=NULL, ignore.case=TRUE, perl=FALSE, imdb=imdb.data$all.actors.info)
  {
  grx = utils::glob2rx(str);  # https://stackoverflow.com/questions/5823503/
  grx.grep = grep(grx,imdb[col.name][[1]], ignore.case=ignore.case, perl=perl);
  rows = imdb[grx.grep, ];
  if(is.null(return.cols)) { rows; } else { rows[, return.cols]; }
  }



#' IMDB.getUniqueCharactersForPerson
#'
#' This is currently a function of `$all.movies.actors.characters`
#'  and *NOT* `$all.actors.info`
#'
#' @param nmid  Person identifier `nmid`
#' @param imdb If you want to apply this function to a different dataframe, it's possible
#'
#'
#' @return dataframe of results
#' @export
IMDB.getUniqueCharactersForPerson = function(nmid, imdb=imdb.data$all.movies.actors.characters )
  {
  rows = imdb[imdb$nmid==nmid, ];
  characters = as.data.frame( table( stats::na.omit( rows$character ) ) )[,c(2,1)];
      colnames(characters) = c("count","character");
  sortDataFrameByNumericColumns(characters,"count");
  }


