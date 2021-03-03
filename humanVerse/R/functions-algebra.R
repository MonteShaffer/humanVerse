# https://stackoverflow.com/questions/1746501/
	  # a = c(2,1,0,2,0,1,1,1)
	  # b = c(2,1,1,1,1,0,1,1)
	  # d = (a %*% b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))

	  ## OR 

	  # e = crossprod(a, b) / (sqrt(crossprod(a, a)) * sqrt(crossprod(b, b)))


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



# compute eigenRank here using the Romani / Del Corsi approach
# append super node to end, use inverse "solve" or ginv


# source multiply.cpp from library  ... inst/cpp"
# ? system.file
# lives in "inst/extdata"
# idf = readRDS( system.file("extdata", "inflation.rds", package="humanVerseWSU") );
  #assign("inflation.df",idf);
# .GlobalEnv$inflation.df = idf;


