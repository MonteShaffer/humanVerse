
# source("C:/_git_/github/MonteShaffer/humanVerse/humanVerse/inst/R/functions-md5_.R");
# source("https://raw.githubusercontent.com/MonteShaffer/humanVerse/main/humanVerse/inst/R/functions-md5_.R");
# source( system.file() );
# system.file("inst/R/functions-md5_.R", package = "humanVerse", lib.loc = NULL, mustWork = FALSE)



##################################################
#'
#' md5_
#'
#' This is a univariate function ...
#'
#' @param s a string as an input
#'
#' @return an md5-string output
#' @export
#'
#' @examples
#' md5_("");
#' md5_("The quick brown fox jumps over the lazy dog");
#' md5_("alex 22");
md5_ = function(s)
	{
	# http://md5.mshaffer.com/				 ... circa 2005 ???
	# http://md5.mshaffer.com/md5.js
	# https://tools.ietf.org/html/rfc1321 ... ported from c -> javascript -> R
	
	
	
	
	is.negative = function(a, tol = DEFAULT_TOLERANCE)
		{
		a < ( -1 * tol );
		}
	bitShiftR = function(x, bits, unsigned=FALSE)
		{
		# # https://stackoverflow.com/questions/64839024/using-r-how-to-do-bitwise-shifting-for-signed-negative-integers
		if(!is.negative(x) | unsigned) { return( bitwShiftR(x,bits) ); }
		-bitwShiftR(-x,bits) - 1; 
		}
		
		
	bitShiftL = function(x, bits, unsigned=FALSE)
		{
		if(!is.negative(x) | unsigned)
			{
			tmp = suppressWarnings( bitwShiftL(x,bits) );				
			if(is.na(tmp)) { tmp = -2^31; }	# 0x80 << 24
			return( tmp );
			}
		tmp = suppressWarnings( -bitwShiftL(-x,bits) ); 
		if(is.na(tmp))
			{
			tmp = 2^31;
			if(is.negative(x)) { tmp = -1 * tmp; }
			}
		tmp;
		}


	bitOr = function(a, b)
		{
		if(!is.negative(a) && ( b <= -1 * 2^31) )
			{
			return (a + b);
			}
		if(!is.negative(b) && ( a <= -1 * 2^31) )
			{
			return (a + b);
			}
		bitwOr(a,b);
		}

	charAt = function(str,idx)
		{
		substr(str,idx,idx);
		}

	lastChar = function(str)
		{
		s.len = strlen(str);
		charAt(str, s.len);
		}

	charCodeAt = function(str,idx)
		{
		charCode ( charAt(str,idx) ); 
		}

	charCode = function(s)
		{
		utf8ToInt(s);
		}

	
	
	
	
	
	
	
	
	
	
	
	
	
	

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
	nx = bitOr( x[idx], bitShiftL( 0x80, (w %% 32), unsigned=TRUE) );	# prevent some overflow


	x[idx] = nx;
	x[L] = w;

############### .INTERNAL FUNCTIONS ###############
	X = function (xx,yy)
		{
		l = bitwAnd(xx, 0xFFFF) + bitwAnd(yy, 0xFFFF);
		m = bitShiftR(xx,16) + bitShiftR(yy,16) + bitShiftR(l,16);
		bitwOr( bitShiftL(m,16),	bitwAnd(l, 0xFFFF) ); ## will this overflow?
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

			a= A(a,b,c,d,x[i],		7, -680876936);
			d= A(d,a,b,c,x[i+1], 12, -389564586);
			c= A(c,d,a,b,x[i+2], 17,	606105819);
			b= A(b,c,d,a,x[i+3], 22, -1044525330);

		a=A(a,b,c,d,x[i+4],		7, -176418897);
		d=A(d,a,b,c,x[i+5],	 12,	1200080426);
		c=A(c,d,a,b,x[i+6],	 17, -1473231341);
		b=A(b,c,d,a,x[i+7],	 22, -45705983);

			a=A(a,b,c,d,x[i+8],	7,	1770035416);
			d=A(d,a,b,c,x[i+9], 12, -1958414417);

			c=A(c,d,a,b,x[i+10],17, -42063);
			b=A(b,c,d,a,x[i+11],22, -1990404162);

		a=A(a,b,c,d,x[i+12],	 7,	1804603682);
		d=A(d,a,b,c,x[i+13],	12, -40341101);
		c=A(c,d,a,b,x[i+14],	17, -1502002290);
		b=A(b,c,d,a,x[i+15],	22,	1236535329);

			a=B(a,b,c,d,x[i+1],	5, -165796510);
			d=B(d,a,b,c,x[i+6],	9, -1069501632);
			c=B(c,d,a,b,x[i+11],14,	643717713);
			b=B(b,c,d,a,x[i],	 20, -373897302);

		a=B(a,b,c,d,x[i+5],		5, -701558691);
		d=B(d,a,b,c,x[i+10],	 9,	38016083);
		c=B(c,d,a,b,x[i+15],	14, -660478335);
		b=B(b,c,d,a,x[i+4],	 20, -405537848);

			a=B(a,b,c,d,x[i+9],	5,	568446438);
			d=B(d,a,b,c,x[i+14], 9, -1019803690);
			c=B(c,d,a,b,x[i+3], 14, -187363961);
			b=B(b,c,d,a,x[i+8], 20,	1163531501);

		a=B(a,b,c,d,x[i+13],	 5, -1444681467);
		d=B(d,a,b,c,x[i+2],		9, -51403784);
		c=B(c,d,a,b,x[i+7],	 14,	1735328473);
		b=B(b,c,d,a,x[i+12],	20, -1926607734);

			a=C(a,b,c,d,x[i+5],	4, -378558);
			d=C(d,a,b,c,x[i+8], 11, -2022574463);
			c=C(c,d,a,b,x[i+11],16,	1839030562);
			b=C(b,c,d,a,x[i+14],23, -35309556);

		a=C(a,b,c,d,x[i+1],		4, -1530992060);
		d=C(d,a,b,c,x[i+4],	 11,	1272893353);
		c=C(c,d,a,b,x[i+7],	 16, -155497632);
		b=C(b,c,d,a,x[i+10],	23, -1094730640);

			a=C(a,b,c,d,x[i+13], 4,	681279174);
			d=C(d,a,b,c,x[i],	 11, -358537222);
			c=C(c,d,a,b,x[i+3], 16, -722521979);
			b=C(b,c,d,a,x[i+6], 23,	76029189);

		a=C(a,b,c,d,x[i+9],		4, -640364487);
		d=C(d,a,b,c,x[i+12],	11, -421815835);
		c=C(c,d,a,b,x[i+15],	16,	530742520);
		b=C(b,c,d,a,x[i+2],	 23, -995338651);

			a=D(a,b,c,d,x[i],		6, -198630844);
			d=D(d,a,b,c,x[i+7], 10,	1126891415);
			c=D(c,d,a,b,x[i+14],15, -1416354905);
			b=D(b,c,d,a,x[i+5], 21, -57434055);

		a=D(a,b,c,d,x[i+12],	 6,	1700485571);
		d=D(d,a,b,c,x[i+3],	 10, -1894986606);
		c=D(c,d,a,b,x[i+10],	15, -1051523);
		b=D(b,c,d,a,x[i+1],	 21, -2054922799);

			a= D(a,b,c,d,x[i+8],	6,	1873313359);
			d=D(d,a,b,c,x[i+15],10, -30611744);
			c=D(c,d,a,b,x[i+6], 15, -1560198380);
			b=D(b,c,d,a,x[i+13],21,	1309151649);

		a=D(a,b,c,d,x[i+4],		6, -145523070);
		d=D(d,a,b,c,x[i+11],	10, -1120210379);
		c=D(c,d,a,b,x[i+2],	 15,	718787259);
		b=D(b,c,d,a,x[i+9],	 21, -343485551);

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
		idx = 1 + bitwAnd( bitwShiftR( xb[ (bitwShiftR(i,2) + 1)] ,	((i%%4)*8+4)), 0xF);
			o = paste0(o, charAt(hex,idx) );
		idx = 1 + bitwAnd( bitwShiftR( xb[ (bitwShiftR(i,2) + 1)] ,	((i%%4)*8)), 0xF);
			o = paste0(o, charAt(hex,idx) );
		}
	attributes(o)[["xb"]] = xb;
	o;
	}
