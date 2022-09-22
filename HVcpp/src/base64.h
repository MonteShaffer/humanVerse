// https://web.mit.edu/freebsd/head/contrib/wpa/src/utils/base64.h
// See also Rcurl 

#ifndef BASE64_H
#define BASE64_H

#include <Rdefines.h>

extern size_t CURL_base64_decode(const char *src, unsigned char **outptr);
extern size_t CURL_base64_encode(const char *inp, size_t insize, char **outptr);
#endif

size_t C_base64_decode(const char *src, unsigned char **outptr);
size_t C_base64_encode(const char *inp, size_t insize, char **outptr);



#ifndef BASE64_H
#define BASE64_H

#endif /* BASE64_H */