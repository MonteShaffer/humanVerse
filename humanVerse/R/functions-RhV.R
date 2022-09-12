
# this is the parser for .RhV files 

# BETWEEN comments [generally called multiline] can be 
# <-- //--> ... or simply <-- --> ... maybe <!-- -->
# /* .... */
# /** USER/SYSTEM ... starts a documentation block ...
# we will source me ... replace .RhV style to a similar file with similar line numbers but with COMMENTS # ... as an .R file 
# allow // as single line comments as well ...
# obviously "#" is the single line comment ...
# take care of "\cite" issues ...
# I have to know if a string literal is started to see if "#" is inside a string ...

# R renders in standard " [dq] format 
# allows single/double ...
# doesn't allow the dreaded \ without \" or \n or \t or \u or \x ...
# eval.c could just add a \\ to avoid the issue
# > alex = 'boss \says "hi" to me'
# Error: '\s' is an unrecognized escape in character string starting "'boss \s"
## not eval.c ... gram.c  line 5186
# /* A Bison parser, made by GNU Bison 3.0.4.  */

# /* Bison implementation for Yacc-like parsers in C





str = "monte says \cite p";
# Error: '\c' is an unrecognized escape in character string starting ""monte says \c"


str = "monte says "hi" after watching the game";
# Error: unexpected symbol in "str = "monte says "hi"



str = "monte says#";


str = "monte says /*    */";


{/** USER 




**/}



/** USER 
{
#
#
}
**/





/** USER 
{
str = monte says hi " ' ";
str = */ hiellowj %  ... REGEX CODE HERE ...
}
**/



str = "hello"; /* here is a comment */   str2 = "again";

In the above case, here is a comment works, str2 has to go on a new line ... 

# https://www.cs.utah.edu/~germain/PPS/Topics/commenting.html
# https://www.cs.utah.edu/~germain/PPS/Topics/programming_style.html
# https://www.cs.utah.edu/~germain/PPS/Topics/random_numbers.html
# https://www.cs.utah.edu/~germain/PPS/Topics/semicolons.html
# https://www.cs.utah.edu/~germain/PPS/Topics/variables.html
# Good programs are "Chopped" into small self contained sections (called functions) much like a good novel is broken into chapters, and a good chapter is broken into paragraphs, etc. A variable that is seen and used in one function is NOT available in another section. This allows us to reuse variable names, such as age. In one function 'age' could refer to the age of a student, and in another function 'age' could refer to the vintage of a fine wine.

#https://www.cs.utah.edu/~germain/PPS/Topics/development_cycle.html
# https://www.cs.utah.edu/~germain/PPS/Topics/functions.html
# https://en.wikipedia.org/wiki/API#History_of_the_term
# expose what you need like library ... in humanVerse form ...

#?Escape
#?Special
#?Symbols
#?Operators
#?Syntax

# http://mathcenter.oxford.emory.edu/site/math117/stringsInR/


% are matlab ... 

R 
C are fortran 


allow user to define single and multiline comment codes ... look for those ...



case 'a':
		    c = '\a';
		    break;
		case 'b':
		    c = '\b';
		    break;
		case 'f':
		    c = '\f';
		    break;
		case 'n':
		    c = '\n';
		    break;
		case 'r':
		    c = '\r';
		    break;
		case 't':
		    c = '\t';
		    break;
		case 'v':
		    c = '\v';
		    break;
		case '\\':
		    c = '\\';
		    break;
		case '"':
		case '\'':
		case '`':
		case ' ':
		case '\n':
		    break;
		default:
		    *ct = '\0';



> x = "monte \c"
Error: '\c' is an unrecognized escape in character string starting ""monte \c"
> x = "monte \b"
> x = "monte \v"
