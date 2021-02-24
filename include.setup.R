# this is the one file that I need to call remotely
# with this file, I have the functions in the core library
# that will enable me to include files, and so on.
 
# these functions are found and documented in:
#       - functions-file.R
#       - functions-parse.R
#
# Can I build a protocol that inserts these functions
# here programmatically?  So when they update this updates?
# Maybe a script called "update.include.me.R"?

github.monte = "https://raw.githubusercontent.com/MonteShaffer/";
	include.me = paste0(github.monte, "humanVerse/main/humanVerse/R/functions-file.R");  
source(include.me); # includeGithubFolder
	include.me = paste0(github.monte, "humanVerse/main/humanVerse/R/functions-str.R");  
source(include.me); # trimMe
	include.me = paste0(github.monte, "humanVerse/main/humanVerse/R/functions-parse.R");  
source(include.me); # folderizeURL ... 





