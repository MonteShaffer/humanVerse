



#' md5.digest
#'
#' @param strvec a character vector of strings
#' @param serialize a parameter passed to the function digest()
#' @param times how many times `n` you want to perform the md5(str)
#'
#' @return a character vector of equal length, with md5 hashes of each string in original vector
#' @export
#'
#' @examples
#' md5.digest("password");
#'   # the encryption is one-way, yet a dictionary can "hack"
#'   # See <https://crackstation.net/> for an example Rainbow Table
#'   # The best modern tool I have found is SJCL <https://github.com/bitwiseshiftleft/sjcl>
#' md5.digest("password", times=9); # this will make the Rainbow Table work
#'   # if you know the original value and the times=n offset, you could build an internal Rainbow Table
#'
#' md5.digest("The quick brown fox jumps over the lazy dog");
#' md5.digest( unlist( base::strsplit("The quick brown fox jumps over the lazy dog", " ", fixed=TRUE) ) );
#'
#' md5.digest("monte.shaffer@gmail.com", 9);

md5.digest = function(strvec, times=1, serialize=FALSE)
  {
  if (requireNamespace("digest", quietly = TRUE))
    {
	  ####################### digest is not vector driven ... # `apply` could work ...
	  n = length(strvec);
	  nvec = c();
	  for(i in 1:n)
		{
		myval = digest::digest(strvec[i], algo="md5", serialize=serialize);
		if(times > 1)
		  {
		  for(j in 2:times)
			{
			myval = digest::digest(myval, algo="md5", serialize=serialize);
			}
		  }
		nvec[i] = myval;
		}
	  nvec;
	} else { return (NULL); }
  }


#' md5
#'
#' @param strvec a character vector of strings
#' @param serialize a parameter passed to the function digest()
#' @param times how many times `n` you want to perform the md5(str)
#'
#' @return a character vector of equal length, with md5 hashes of each string in original vector
#' @export
#'
#' @examples
#' md5("password");
#'   # the encryption is one-way, yet a dictionary can "hack"
#'   # See <https://crackstation.net/> for an example Rainbow Table
#'   # The best modern tool I have found is SJCL <https://github.com/bitwiseshiftleft/sjcl>
#' md5("password", times=9); # this will make the Rainbow Table work
#'   # if you know the original value and the times=n offset, you could build an internal Rainbow Table
#'
#' md5("The quick brown fox jumps over the lazy dog");
#' md5( unlist( base::strsplit("The quick brown fox jumps over the lazy dog", " ", fixed=TRUE) ) );
#'
#' md5("monte.shaffer@gmail.com", 9);
md5 = function(strvec, times=1)
  {
  # https://cran.r-project.org/web/packages/openssl/openssl.pdf
  # md5(serialize)?
  # digest is not vector driven ... # `apply` could work ...
  if (requireNamespace("openssl", quietly = TRUE))
    {
    nstrvec = openssl::md5(strvec);
    if(times > 1)
        {
        for(j in 2:times)
          {
          nstrvec = openssl::md5(nstrvec);
          }
        }
    return( nstrvec );
    } else {
            if (requireNamespace("digest", quietly = TRUE))
              {
              return( md5.digest(strvec,times) );
              } else {
                      # do INTERNAL .md5 very slow
          					  n = length(strvec);
          					  nstrvec = character(n);
          					  for(i in 1:n)
          						{
          						nstrvec[i] = .md5(strvec[i]);
          						for(j in 2:times)
          						  {
          						  nstrvec[i] = .md5(nstrvec[i]);
          						  }
          						}
          					  return( nstrvec );
                      }

            }
	# should never get here with SLOW INTERNAL
  stop("`openssl` or `digest` must be installed to use `md5()`");
  }





#' .md5
#'
#' This is a univariate function ...
#'
#' @param s a string as an input
#'
#' @return an md5-string output
#' @export
#'
#' @examples
#' .md5("The quick brown fox jumps over the lazy dog");
#' .md5("alex 22");
.md5 = function(s)
  {
  # http://md5.mshaffer.com/         ... circa 2005 ???
  # http://md5.mshaffer.com/md5.js
  # https://tools.ietf.org/html/rfc1321 ... ported from c -> javascript -> R

  s = s[1]; # this is not vectorized ... very slow
  w = 8 * nchar( as.character(s), type="chars");
  hex = "0123456789abcdef";
  # w is length, so >>> should be >>
  L = bitShiftL( bitShiftR(w+64,9, TRUE), 4) + 15;
  x = numeric(L+15);
  i = 1; j = 1;
  while(i < w)
    {
    idx = bitShiftR(i,5) + 1;
    # print(idx);
    mychar = bitShiftL( bitwAnd( charCodeAt(s,j), 255), ((i-1) %% 32));
    nx = bitwOr(x[idx], mychar); # print(nx);
    x[idx] = nx;
    i = 8 + i;
    j = 1 + j;
    }


  idx = bitShiftR(w,5)+1;
  # x[w>>5] |= 0x80 << ((w)%32);
  # nx = bitwOr( x[idx], bitShiftL( 0x80, (w %% 32)) );
  nx = bitOr( x[idx], bitShiftL( 0x80, (w %% 32), unsigned=TRUE) );  # prevent some overflow


  x[idx] = nx;
  x[L] = w;

############### .INTERNAL FUNCTIONS ###############
# Shifting is done assuming the values represent unsigned integers.
  X = function (xx,yy)
    {
    l = bitwAnd(xx, 0xFFFF) + bitwAnd(yy, 0xFFFF);
    m = bitShiftR(xx,16) + bitShiftR(yy,16) + bitShiftR(l,16);
    bitwOr( bitShiftL(m,16),  bitwAnd(l, 0xFFFF) ); ## will this overflow?
    # bitOr
    }
  Y = function (qi,aa,bb,xi,si,ti)
    {
    X(Z(X(X(aa,qi),X(xi,ti)),si),bb);
    }
  Z = function (ni,ci)
    {
    # print(ni);
    # print(ci);
    bitwOr( bitShiftL(ni,ci), bitShiftR(ni,32-ci,TRUE) );
    }


  A = function (aa,bb,cc,dd,xi,si,ti)
    {
    Y( (bitwOr( bitwAnd(bb,cc), bitwAnd(bitwNot(bb),dd) )),
        aa,bb,xi,si,ti);
    }
  B = function (aa,bb,cc,dd,xi,si,ti)
    {
    Y( (bitwOr( bitwAnd(bb,dd), bitwAnd(cc,bitwNot(dd)) )),
        aa,bb,xi,si,ti);
    }
	C = function (aa,bb,cc,dd,xi,si,ti){
	  Y( (bitwXor(bb,bitwXor(cc,dd))),
	      aa,bb,xi,si,ti);
	  }
	D = function (aa,bb,cc,dd,xi,si,ti)
	  {
	  Y( (bitwXor(cc, (bitwOr(bb,bitwNot(dd))))),
	      aa,bb,xi,si,ti);
	  }
############### INTERNAL FUNCTIONS. ###############


############### DIGEST ###############
  a=1732584193; b=-271733879; c=-1732584194; d=271733878;
  i = 1;

  while(i < (1+L))
    {
    oa = a; ob = b; oc = c; od = d;

      a= A(a,b,c,d,x[i],    7, -680876936);
      d= A(d,a,b,c,x[i+1], 12, -389564586);
      c= A(c,d,a,b,x[i+2], 17,  606105819);
      b= A(b,c,d,a,x[i+3], 22, -1044525330);

    a=A(a,b,c,d,x[i+4],    7, -176418897);
    d=A(d,a,b,c,x[i+5],   12,  1200080426);
    c=A(c,d,a,b,x[i+6],   17, -1473231341);
    b=A(b,c,d,a,x[i+7],   22, -45705983);

      a=A(a,b,c,d,x[i+8],  7,  1770035416);
      d=A(d,a,b,c,x[i+9], 12, -1958414417);

      c=A(c,d,a,b,x[i+10],17, -42063);
      b=A(b,c,d,a,x[i+11],22, -1990404162);

    a=A(a,b,c,d,x[i+12],   7,  1804603682);
    d=A(d,a,b,c,x[i+13],  12, -40341101);
    c=A(c,d,a,b,x[i+14],  17, -1502002290);
    b=A(b,c,d,a,x[i+15],  22,  1236535329);

      a=B(a,b,c,d,x[i+1],  5, -165796510);
      d=B(d,a,b,c,x[i+6],  9, -1069501632);
      c=B(c,d,a,b,x[i+11],14,  643717713);
      b=B(b,c,d,a,x[i],   20, -373897302);

    a=B(a,b,c,d,x[i+5],    5, -701558691);
    d=B(d,a,b,c,x[i+10],   9,  38016083);
    c=B(c,d,a,b,x[i+15],  14, -660478335);
    b=B(b,c,d,a,x[i+4],   20, -405537848);

      a=B(a,b,c,d,x[i+9],  5,  568446438);
      d=B(d,a,b,c,x[i+14], 9, -1019803690);
      c=B(c,d,a,b,x[i+3], 14, -187363961);
      b=B(b,c,d,a,x[i+8], 20,  1163531501);

    a=B(a,b,c,d,x[i+13],   5, -1444681467);
    d=B(d,a,b,c,x[i+2],    9, -51403784);
    c=B(c,d,a,b,x[i+7],   14,  1735328473);
    b=B(b,c,d,a,x[i+12],  20, -1926607734);

      a=C(a,b,c,d,x[i+5],  4, -378558);
      d=C(d,a,b,c,x[i+8], 11, -2022574463);
      c=C(c,d,a,b,x[i+11],16,  1839030562);
      b=C(b,c,d,a,x[i+14],23, -35309556);

    a=C(a,b,c,d,x[i+1],    4, -1530992060);
    d=C(d,a,b,c,x[i+4],   11,  1272893353);
    c=C(c,d,a,b,x[i+7],   16, -155497632);
    b=C(b,c,d,a,x[i+10],  23, -1094730640);

      a=C(a,b,c,d,x[i+13], 4,  681279174);
      d=C(d,a,b,c,x[i],   11, -358537222);
      c=C(c,d,a,b,x[i+3], 16, -722521979);
      b=C(b,c,d,a,x[i+6], 23,  76029189);

    a=C(a,b,c,d,x[i+9],    4, -640364487);
    d=C(d,a,b,c,x[i+12],  11, -421815835);
    c=C(c,d,a,b,x[i+15],  16,  530742520);
    b=C(b,c,d,a,x[i+2],   23, -995338651);

      a=D(a,b,c,d,x[i],    6, -198630844);
      d=D(d,a,b,c,x[i+7], 10,  1126891415);
      c=D(c,d,a,b,x[i+14],15, -1416354905);
      b=D(b,c,d,a,x[i+5], 21, -57434055);

    a=D(a,b,c,d,x[i+12],   6,  1700485571);
    d=D(d,a,b,c,x[i+3],   10, -1894986606);
    c=D(c,d,a,b,x[i+10],  15, -1051523);
    b=D(b,c,d,a,x[i+1],   21, -2054922799);

      a= D(a,b,c,d,x[i+8],  6,  1873313359);
      d=D(d,a,b,c,x[i+15],10, -30611744);
      c=D(c,d,a,b,x[i+6], 15, -1560198380);
      b=D(b,c,d,a,x[i+13],21,  1309151649);

    a=D(a,b,c,d,x[i+4],    6, -145523070);
    d=D(d,a,b,c,x[i+11],  10, -1120210379);
    c=D(c,d,a,b,x[i+2],   15,  718787259);
    b=D(b,c,d,a,x[i+9],   21, -343485551);

      a=X(a,oa);
  		b=X(b,ob);
  		c=X(c,oc);
  		d=X(d,od);

    i = 16 + i;
    }
############### CONVERT TO HEXADECIMAL ###############
	xb= c(a,b,c,d);
  o = "";
  for(i in 0:15)
    {
    idx = 1 + bitwAnd( bitwShiftR( xb[ (bitwShiftR(i,2) + 1)] ,  ((i%%4)*8+4)), 0xF);
      o = paste0(o, charAt(hex,idx) );
    idx = 1 + bitwAnd( bitwShiftR( xb[ (bitwShiftR(i,2) + 1)] ,  ((i%%4)*8)), 0xF);
      o = paste0(o, charAt(hex,idx) );
    }
	o = setAttribute("xb", xb, o);
	o;
  }



#' .md5_out
#'
#' @param xb Vector of 4 integer keys
#'
#' @return md5 output of those integer keys
#' @export
#'
#' @examples
#' .md5_out();
.md5_out = function(xb = c(-1652748130, -2101990601, 891148395, -702962622))
  {

  hex = "0123456789abcdef";
  o = "";
  for(i in 0:15)
    {
    idx = 1 + bitwAnd( bitwShiftR( xb[ (bitwShiftR(i,2) + 1)] ,  ((i%%4)*8+4)), 0xF);
      o = paste0(o, charAt(hex,idx) );
    idx = 1 + bitwAnd( bitwShiftR( xb[ (bitwShiftR(i,2) + 1)] ,  ((i%%4)*8)), 0xF);
      o = paste0(o, charAt(hex,idx) );
    }
  o = setAttribute("xb", xb, o);
  o;
  }


