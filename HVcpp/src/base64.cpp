#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <iostream>
#include <string>
#include "RApiSerializeAPI.h"

// found it, but it headers are not found ...
#include <glib-2.0**> 


#include <fstream>
// https://github.com/opencv/opencv/blob/4.x/include/opencv2/opencv.hpp
// https://deepayan.github.io/rip/opencv-intro.html
// https://cran.r-project.org/web/packages/Rcpp/vignettes/Rcpp-FAQ.pdf
/* NOT FOUND ARG !!JKLJ$) */

/* /// found in Rtools
#include <opencv4/opencv2/opencv.hpp>
#include <opencv4/opencv2/imgproc/imgproc.hpp>
#include <opencv4/opencv2/core/core.hpp>
#include <opencv4/opencv2/core/version.hpp>
#include <opencv4/opencv2/opencv_modules.hpp>
*/

using namespace cv;
*/

// https://www.cryptopp.com/wiki/Base64Encoder

using namespace Rcpp;

// https://lists.r-forge.r-project.org/pipermail/rcpp-devel/2010-October/001179.html


// this works on strings, but not RAW data structures 
// how to put a C script into Rccp ?
// http://web.mit.edu/freebsd/head/contrib/wpa/src/utils/base64.c
// https://stackoverflow.com/questions/54000015/r-package-with-both-c-and-cpp-files-with-rcpp


static const char* B64chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

static const int B64index[256] =
{
		0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
		0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
		0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	62, 63, 62, 62, 63,
		52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 0,	0,	0,	0,	0,	0,
		0,	0,	1,	2,	3,	4,	5,	6,	7,	8,	9,	10, 11, 12, 13, 14,
		15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 0,	0,	0,	0,	63,
		0,	26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
		41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51
};


const std::string b64_enc_(const void* data, const size_t &len)
{
	std::string result((len + 2) / 3 * 4, '=');
	unsigned char *p = (unsigned	char*) data;
	char *str = &result[0];
	size_t j = 0, pad = len % 3;
	const size_t last = len - pad;

	for (size_t i = 0; i < last; i += 3)
		{
		int n = int(p[i]) << 16 | int(p[i + 1]) << 8 | p[i + 2];
		str[j++] = B64chars[n >> 18];
		str[j++] = B64chars[n >> 12 & 0x3F];
		str[j++] = B64chars[n >> 6 & 0x3F];
		str[j++] = B64chars[n & 0x3F];
		}
		
	if (pad)	/// Set padding
		{
		int n = --pad ? int(p[last]) << 8 | p[last + 1] : p[last];
		str[j++] = B64chars[pad ? n >> 10 & 0x3F : n >> 2];
		str[j++] = B64chars[pad ? n >> 4 & 0x03F : n << 4 & 0x3F];
		str[j++] = pad ? B64chars[n << 2 & 0x3F] : '=';
		}
	return result;
}


const std::string b64_dec_(const void* data, const size_t &len)
{
	if (len == 0) return "";

	unsigned char *p = (unsigned char*) data;
	size_t j = 0,
			pad1 = len % 4 || p[len - 1] == '=',
			pad2 = pad1 && (len % 4 > 2 || p[len - 2] != '=');
	const size_t last = (len - pad1) / 4 << 2;
	std::string result(last / 4 * 3 + pad1 + pad2, '\0');
	unsigned char *str = (unsigned char*) &result[0];

	for (size_t i = 0; i < last; i += 4)
		{
		int n = B64index[p[i]] << 18 | B64index[p[i + 1]] << 12 | B64index[p[i + 2]] << 6 | B64index[p[i + 3]];
		str[j++] = n >> 16;
		str[j++] = n >> 8 & 0xFF;
		str[j++] = n & 0xFF;
		}
	
	if (pad1)
		{
		int n = B64index[p[last]] << 18 | B64index[p[last + 1]] << 12;
		str[j++] = n >> 16;
			if (pad2)
			{
			n |= B64index[p[last + 2]] << 6;
			str[j++] = n >> 8 & 0xFF;
			}
		}
	return result;
}




// maybe port http://c.mshaffer.com/js/monte/base64.js
// then it will be equivalent ...
// b64_enc_js_ ... maybe this is my internal R-base version ...



// https://dirk.eddelbuettel.com/code/rcpp/Rcpp-sugar.pdf
// https://rcpp-devel.r-forge.r-project.narkive.com/joEUKvxE/convert-to-unsigned-char-variable
// > demangle( "RawVector::stored_type" )
// [1] "unsigned char"
// [[Rcpp::export]]
std::string s_b64_enc_str(const std::string str)
{
	return b64_enc_(str.c_str(), str.size());
}


// [[Rcpp::export]]
CharacterVector cpp_b64_enc_str(const std::vector<std::string> str)
{
	CharacterVector r{};
	for (auto& element : str) 
		{
		//std::string res = b64_enc_(element.c_str(), element.size());
		std::string res = s_b64_enc_str(element);
		r.push_back(res);
		}
	return r;
}



// [[Rcpp::export]]
std::string s_b64_dec_str(const std::string str64)
{
	return b64_dec_(str64.c_str(), str64.size());
}


// [[Rcpp::export]]
CharacterVector cpp_b64_dec_str(const std::vector<std::string> str64)
{
	CharacterVector r{};
	for (auto& element : str64) 
		{
		//std::string res = b64_dec_(element.c_str(), element.size());
		std::string res = s_b64_dec_str(element);
		r.push_back(res);
		}
	return r;
}



/*
std::string foo( "blabla" ) ;
		 RawVector res( foo.size() ) ;
		 std::copy( foo.begin(), foo.end(), res.begin() ) ;
		 return res ;
*/	 

// serializeToRaw ... https://github.com/eddelbuettel/rcppredis/blob/master/src/Redis.cpp
// unserializeFromRaw(res);
// https://github.com/eddelbuettel/rapiserialize/tree/master/inst/include
// 
	 


// [[Rcpp::export]] 
std::string s_b64_enc_raw(RawVector raw)
{
	const char *c = reinterpret_cast<char*>(raw.begin());	
	std::string inp = std::string(c);
	std::string res = b64_enc_(inp.c_str(), raw.size());
	return res;
}


// [[Rcpp::export]]
CharacterVector cpp_b64_enc_raw(Rcpp::List raws)
{
	CharacterVector r{};
	for (auto& element : raws) 
		{
		std::string res = s_b64_enc_raw(element);		
		r.push_back(res);
		}
	return r;
}



// [[Rcpp::export]]
RawVector s_b64_dec_raw(const std::string str64)
{
	std::string res = b64_dec_(str64.c_str(), str64.size());
	RawVector raw( res.size() ) ;
	std::copy( res.begin(), res.end(), raw.begin() ) ;
	return raw ;
}



// [[Rcpp::export]]
List cpp_b64_dec_raw(const std::vector<std::string> str64)
{
	List r{};
	for (auto& element : str64) 
		{
		RawVector res = s_b64_dec_raw(element);		
		r.push_back(res);
		}
	return r;
}

// https://stackoverflow.com/questions/10167534/how-to-find-out-what-type-of-a-mat-object-is-with-mattype-in-opencv
// https://docs.opencv.org/3.4/d4/da8/group__imgcodecs.html#ga461f9ac09887e47797a54567df3b8b63
/*
// [[Rcpp::export]] 
std::string s_b64_enc_img(std::string filename, std::string imgtype="png", bool append=true)
{
	Mat img = cv::imread(filename, 0); // it doesn't know the type?
	vector<uchar> buf;
	cv::imencode( ("."+imgtype), img, buf);  // jpeg vs jpg ?
	
	std::string imgstr;
	auto imgstr = reinterpret_cast<const unsigned char*>(buf.data());
	
	std::string res = b64_enc_(imgstr, buf.size());
	if(append) { res = "data:image/" + imgtype + ";base64," + res; }
	return res;
}
*/

// https://png-pixel.com/
// https://stackoverflow.com/a/56973683/184614
// s_b64_enc_file("c:/_git_/transparent.gif")
// s_b64_enc_file("c:/_git_/transparent.png")
// [[Rcpp::export]] 
std::string s_b64_enc_file(std::string filename)
{
	std::string line;
	std::ifstream input(filename, std::ios::in | std::ios::binary);
	std::string output;
	
	if (input.is_open()) 
		{
		while (getline(input, line)) 
			{
			std::string encoded = b64_enc_(reinterpret_cast<const unsigned char*>(line.c_str()), line.length());

			// output << encoded;
			output += encoded;
			}
		input.close();
		}
		
	return output;
}

/* 
TODO ... dec_file ... enc/dec_img (dataURI)


		enc/dec obj (iris) ... multivariate ...
*/

// COMPILES, doesn't work ... ANOTHER day ...
// s_b64_enc_obj(iris);
// [[Rcpp::export]] 
std::string s_b64_enc_obj(SEXP s)
{
	// Rcpp::RawVector raw = (TYPEOF(s) == RAWSXP) ? s : serializeToRaw(s);
	Rcpp::RawVector raw = serializeToRaw(s);
	std::string res = s_b64_enc_raw(raw);
	return res;
}




/* Multivariate LISTS of RawVector, CharVector of others */







// open cv ... image 2 uri 







/*

x = c("hello friend", "The quick brown fox")
y = cpp_b64_enc_str(x)
z = cpp_b64_dec_str(y)
identical(x,z);

xx = list(as.raw(1:10), as.raw(1:20))
# as.numeric(as.raw(1:10))

xx = charToRaw(x);


x = as.raw(1:10); y = charToRaw("hello friend");
z = cpp_b64_enc_raw(list(x,y))
a = cpp_b64_dec_raw(z);

identical(a, list(x,y));



xx = charToRaw(x[1]);
yy = s_b64_enc_raw(xx);
zz = s_b64_dec_raw(yy);
identical(xx,zz);


xxx = serialize(iris, NULL);
yyy = s_b64_enc_raw(xxx);
zzz = s_b64_dec_raw(yyy);
length(zzz) == length(xxx);  # TRUE 
identical(xxx,zzz);  # FALSE 
iris2 = unserialize(zzz);  # Error in unserialize(zzz) : 
  cannot read workspace version 1964159 written by R 0.0.0; need R 0.0.0 or newer




xxxx = JSON.stringify(iris);
yyyy = s_b64_enc_str(xxxx);
zzzz = s_b64_dec_str(yyyy);
identical(xxxx,zzzz);  # FALSE 
xxxx == zzzz;  #TRUE (one is chr ... other is 'json' chr )





*/









/*
## NA_INTEGER : -2147483648
## NA_STRING  : 0x7fa860003e00
## NA_LOGICAL : -2147483648
## NA_REAL    : nan
*/










// [[Rcpp::export]]
RawVector say_hello(std::string msg = "hello friend")
{
	RawVector raw( msg.size() ) ;
	std::copy( msg.begin(), msg.end(), raw.begin() ) ;
	return raw ;
}


// [[Rcpp::export]]
std::string print_hello(RawVector raw)
{	
	const char *c = reinterpret_cast<char*>(raw.begin());
	std::string res = std::string(c);
	return res ;
}





/*

#YEAH!!!
x = as.raw(1:10);
y = s_b64_enc_raw(x);
z = s_b64_dec_raw(y);

x;y;z;
identical(x,z);





*/



/*
Rcpp::cppFunction("std::string r2c(RawVector x) { \
       const char *c = reinterpret_cast<char*>(x.begin()); \
       return std::string(c); }")
	   */

/*

U+00db  ... Unicode Character “Û” (U+00DB)
U+00ce  ... Unicode Character “Î” (U+00CE)
U+0002  ... start of text 

> r = (charToRaw("The quick brown fox"))
> r
 [1] 54 68 65 20 71 75 69 63 6b 20 62 72 6f 77 6e 20 66 6f 78
> r2c(r)
[1] "The quick brown fox"
> r2c(charToRaw("The quick brown fox"))
[1] "The quick brown fox\xd2\xce\002"

# Likely just a console issue?

g++ -std=gnu++11  -I"C:/PROGRA~1/R/R-42~1.1/include" -DNDEBUG   -I"C:/Users/......./AppData/Local/R/WIN-LI~1/4.2/Rcpp/include" -I"C:/_git_/github/......./src" -I"C:/_git_/github/......./inst/include"   -I"C:/rtools42/x86_64-w64-mingw32.static.posix/include"     -O2 -Wall  -mfpmath=sse -msse2 -mstackrealign  -c base64.cpp -o base64.o


R_RTOOLS42_PATH                             C:\rtools42/x86_64-w64-mingw32.static.posix/bin;C:\rtools42/usr/bin

"pkg.version(Rcpp)"
[1] ‘1.0.9’


> R.version
               _                                
platform       x86_64-w64-mingw32               
arch           x86_64                           
os             mingw32                          
crt            ucrt                             
system         x86_64, mingw32                  
status                                          
major          4                                
minor          2.1                              
year           2022                             
month          06                               
day            23                               
svn rev        82513                            
language       R                                
version.string R version 4.2.1 (2022-06-23 ucrt)
nickname       Funny-Looking Kid  
*/
	   