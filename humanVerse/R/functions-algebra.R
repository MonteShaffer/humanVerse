# https://stackoverflow.com/questions/1746501/
	  # a = c(2,1,0,2,0,1,1,1)
	  # b = c(2,1,1,1,1,0,1,1)
	  # d = (a %*% b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))

	  ## OR

	  # e = crossprod(a, b) / (sqrt(crossprod(a, a)) * sqrt(crossprod(b, b)))




#' .cosine.similarity
#'
#' This is a univariate calculation
#'
#' See <https://en.wikipedia.org/wiki/Cosine_similarity#Definition>
#'
#' @param a vector 'a'
#' @param b vector 'b'
#' @param method use 'crossprod' or less-efficient default option
#'
#' @return the cosine similarity
#' @export
#'
#' @examples
#' a = c(2,1,0,2,0,1,1,1); b = c(2,1,1,1,1,0,1,1);
#' .cosine.similarity( a,b );
#'
.cosine.similarity = function(a, b, method="crossprod")
  {
  # cat("\n\n ==================== COSINE SIMILARITY (a,b) ========== \n\n");
  # cat("\n", " ===  a === "); print(a); cat("\n");
  # cat("\n", " ===  b === "); print(a); cat("\n");

  # maybe perform some non-zero vector "checks"
  # sum(a); sum(b);  if "fails", return NA ... with warning()
  if(sum(a) == 0 && sum(b) == 0)
    {
    return (NA);
    }

  if(method == "crossprod")
    {
    theta = crossprod(a, b) / (sqrt(crossprod(a, a)) * sqrt(crossprod(b, b)));
    } else  {
            theta = (a %*% b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)));
            }

  as.numeric(theta);
  }



#' .angular.distance
#'
#' This is a univariate calculation
#'
#' See <https://en.wikipedia.org/wiki/Cosine_similarity#Angular_distance_and_similarity>
#'
#' @param a vector 'a'
#' @param b vector 'b'
#'
#' @return the angular distance
#' @export
#'
#' @examples
#' a = c(2,1,0,2,0,1,1,1); b = c(2,1,1,1,1,0,1,1);
#' .angular.distance(  a,b );
#'
.angular.distance = function(a,b)
  {
  # maybe do "auto-detect" on v.neg at the a, b level?
  cos.sim = .cosine.similarity( a,b );
  v.neg = ( sum( is.negative(a,b) ) > 0 );
  if(v.neg)
    {
    1 * acos(cos.sim) / pi;
    } else  {
            2 * acos(cos.sim) / pi;
            }
  }


# computeCosineSimilarityMatrix = function(A, angle.distance=FALSE)
#   {
#   # compute a score similar to a "dist" matrix ... lower triangle?
#   }



#' computeCosineSimilarity
#'
#' This is a multivariate (one-many) computation ...
#'
#' @param a vector 'a'
#' @param bs vectors 'bs' (e.g., a matrix)
#'
#' @return dataframe
#' @export
#'
#' @examples
#' a = c(2,1,0,2,0,1,1,1); a = setAttribute("name", "a", a);
#' b = c(2,1,1,1,1,0,1,1);
#' bs = as.matrix( cbind(a, b) );
#' computeCosineSimilarity(a, bs);
#'
computeCosineSimilarity = function(a, bs)
  {

  a.name = getAttribute("name", a);
  a  = as.vector(a);  n = length(a);


  bs = as.matrix(bs); r = dim(bs)[1];
                      c = dim(bs)[2];

  nc = c; # number to compare
  b.names = colnames(bs);
  ## generic "match" length?
  if(n != r)
    {
    bs = matrix.transpose(bs);
    b.names = colnames(bs);
    nc = r;
    if(n != c)
      {
      stop( paste0(" function [cosine.similarity] has 'a' of length: ", n,
                    "\n\t", " ... and 'bs' of length: ", c,
                    "\n\t", " ... and 'bs' of width: ", r, "\n") );
      }
    }
  # print(dim(bs));

  # maybe see if any "bs" are unique, and prevent the re-calculation
  # lsa::cosine has a "recursive" call which may be the reason it is so slow
  res = numeric(nc + 1);
  names = character(nc + 1);

  res[1] = .cosine.similarity(a, a);
  names[1] = paste0(a.name, "-", a.name);
  for(i in 1:nc)
    {
    res[i+1] = .cosine.similarity(a, bs[,i]);
    names[i+1] = paste0(a.name, "-", b.names[i]);
    }

  df = as.data.frame( cbind(names,res) );
    colnames(df) = c("name", "cosine.similarity");
  df;
  }




#' deg2rad
#'
#' Convert angles from degrees to radians.
#' Similar to pracma::deg2rad however is vectorized (multivariate).
#'
#' @param degs One or more angles in degrees
#' @param ...  One or more angles in degrees
#'
#' @return One or more angles in radians.
#' @export
#'
#' @examples
#' deg2rad(c(1,3,34));
#' deg2rad(1,3,34);
#' deg2rad(1,3,"alex");
#'
deg2rad = function(degs, ...)
	{
	more = unlist(list(...));
	degs = c(degs, more);

	res = list();
	i = 0;
	for(deg in degs)
		{
		i = 1 + i;
		ndeg = suppressWarnings(as.numeric(deg));
		rad = NaN;
		if( !is.na(ndeg) )  { rad = (pi/180) * ndeg; }
		res[[i]] = rad;
		}
	returnList(res);
	}

#' rad2deg
#'
#' Convert angles from radians to degrees.
#' Similar to pracma::rad2deg however is vectorized (multivariate).
#'
#' @param degs One or more angles in radians.
#' @param ...  One or more angles in radians.
#'
#' @return One or more angles in degrees.
#' @export
#'
#' @examples
#' rad2deg(c(1,3,34));
#' rad2deg(1,3,34);
#' rad2deg(1,3,"alex");
#'
rad2deg = function(rads, ...)
	{
	more = unlist(list(...));
	rads = c(rads, more);

	res = list();
	i = 0;
	for(rad in rads)
		{
		nrad = suppressWarnings(as.numeric(rad));
		i = 1 + i;
		deg = NaN;
		if( !is.na(nrad) )  { deg = (180/pi) * nrad; }
		res[[i]] = deg;
		}
	returnList(res);
	}




