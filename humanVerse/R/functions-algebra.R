# https://stackoverflow.com/questions/1746501/
	  # a = c(2,1,0,2,0,1,1,1)
	  # b = c(2,1,1,1,1,0,1,1)
	  # d = (a %*% b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))

	  ## OR

	  # e = crossprod(a, b) / (sqrt(crossprod(a, a)) * sqrt(crossprod(b, b)))


.angular.distance = function(cos.sim, v.neg = FALSE)
  {
  if(v.neg)
    {
    1 * acos(cos.sim) / pi;  
    } else  {
            2 * acos(cos.sim) / pi; 
            }
  }
  
.cosine.similarity = function(a, b, method="crossprod")
  {
  # cat("\n\n ==================== COSINE SIMILARITY (a,b) ========== \n\n");
  # cat("\n", " ===  a === "); print(a); cat("\n");
  # cat("\n", " ===  b === "); print(a); cat("\n");

  # maybe perform some non-zero vector "checks"
  # sum(a); sum(b);  if "fails", return NA ... with warning()
  if(method == "crossprod")
    {
    theta = crossprod(a, b) / (sqrt(crossprod(a, a)) * sqrt(crossprod(b, b)));
    } else  {
            theta = (a %*% b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)));
            }

  as.numeric(theta);
  }

computeCosineSimilarityMatrix = function(A, angle.distance=FALSE)
  {
  # compute a score similar to a "dist" matrix ... lower triangle?
  }

computeCosineSimilarity = function(a, bs)
  {
  my.names = c(names(a), names(bs));

  a  = as.vector(a);  n = length(a);
  bs = as.matrix(bs); r = dim(bs)[1];
                      c = dim(bs)[2];

  if(n != c)
    {
    stop( paste0(" function [cosine.similarity] has 'a' of length: ", n,
                    "\n\t", " ... and 'bs' of length: ", c, "\n") );
    }

  # maybe see if any "bs" are unique, and prevent the re-calculation
  # lsa::cosine has a "recursive" call which may be the reason it is so slow
  res = numeric(r + 1);
  res[1] = .cosine.similarity(a, a);
  for(i in 1:r)
    {
    res[i+1] = .cosine.similarity(a, bs[i,]);
    }

  df = as.data.frame( cbind(names,res) );
    colnames(df) = c("color", "cosine.similarity");
  df;
  }



# multivariate 
# deg2rad(c(1,3,34))
# deg2rad(1,3,34)
# deg2rad(1,3,"alex")
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

# multivariate 
# rad2deg(c(1,3,34))
# rad2deg(1,3,34)
# rad2deg(1,3,"alex")
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


	
	
	