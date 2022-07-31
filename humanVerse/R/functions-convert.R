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
	
	
	
	
	
	
	
	
	
	
	
	