
# # https://stackoverflow.com/questions/28687806/a-better-way-to-push-and-pop-to-from-lists-in-r

# pushList = function(nlist, veclist, n=length(vlist))
  # {

  # }
# popList = function(nlist, veclist, n=length(vlist))
  # {

  # }


#' popVector
#'
#' @param vec
#' @param idx
#' @param method
#'
#' @return
#' @export
popVector = function(vec, idx=1, method="FIFO")  # Inf would work for unlimited stack
  {
  # https://stackoverflow.com/questions/2805102/how-is-pushing-and-popping-defined
# vec = 1: 10;
# popVector(vec)
# popVector(vec, method="LIFO-LILO")
  if(method=="FIFO")   # QUEUING
    {
    val = vec[idx];
    vec = vec[-c(idx)];
    } else {
            n = length(vec) + 1 - idx;
            val = vec[n];
            vec = vec[-c(n)];
    }
  # list or "attributes" ?
  list("val" = val, "vec" = vec, "popped" = NULL); # updated ...
  }


#' pushVector
#'
#' @param val
#' @param vec
#' @param n.max
#' @param method
#'
#' @return
#' @export
#'
#' @examples
pushVector = function(val, vec, n.max=1+length(vec), method="FIFO")
  {
  # vec = 1: 10;
# popVector(pushVector(13, vec)$vec)
# pushVector(13, vec, n.max=5)
# pushVector(13, vec, n.max=5, method="LIFO-LILO")


  # n.max is max size, so vals popped may return ...
  n = length(vec);
  popped = NULL;
  if(method=="FIFO")  # in this model, new values are added to end
    {
    if(n < n.max)
      {
      vec = c(vec,val);
      } else {
              vec = c(vec,val);
              nn = 1 + n;
              nd = nn - n.max;
              if(nd > 0)
                {
                popped = vec[1:nd];
                vec = vec[(1+nd):nn];
                }
              }
    } else {        # in this model, new values are added to beginning
            if(n < n.max)
              {
              vec = c(val,vec);
              } else {
                      vec = c(val,vec);
                      nn = 1 + n;
                      if(nn > (1+n.max))
                        {
                        popped = vec[(1+n.max):nn];  # off the end ?
                        vec = vec[1:n.max];
                        }
                      }
            }
  # list or "attributes" ?
  list("vec" = vec, "popped" = popped, "val"=val);
  }
