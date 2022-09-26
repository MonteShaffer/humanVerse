

# maybe have a copy/paste file ...

# maybe have openSesame, but this is generally me copying PATH from WINDOWZ into R ...
pathFromClipboard = function(trailing = TRUE)
	{
	x = readClipboard();
	y = str.replace("\\", "/", x);
	if(trailing) { y = paste0(y, "/"); y = str.replace("//", "/", y);}
	minvisible(y, display=print, key="PATH");
	}





file.init = function()
	{
	# grab settings from MEMORY if exists ... 
	
	}







#' writeToPipe
#'
#' This is the inverse of 'readFromPipe'
#'
#' @param df dataframe to be stored
#' @param file filename and path
#' @param header whether or not to add a header
#' @param quote whether or not to add quotes
#' @param sep "pipe" means sep="|" but you could change
#' @param row.names whether or not to include row names
#'
#' @return NOTHING, it writes
#' @export
#'
#' @aliases storeToPipe 
writeToPipe = function(df, filename, header=TRUE, quote="", sep="|", 
									prepend.meta = TRUE, 
									meta.content="",
									meta.sep = "^",
									row.names=FALSE, ...)
  {
  if(quote == "") { quote = FALSE; }
  if(!prepend.meta)
	{
	utils::write.table(df, file=filename, quote=quote, col.names=header, row.names=row.names, sep=sep);
	return(TRUE);
	}
  
  if(meta.content == "")
	{
	meta.content = property.get("meta", df); 
	if(is.null(meta.content))
		{
		types = df.getColumnTypes(df);
		if(row.names) { types = c("row.names", types); }
		types.line = paste0("# ", paste0(types, collapse=meta.sep), " #");
		h.length = strlen(types.line);
		
		# meta.content = 
		# msg = paste0("\n\n", 
			# str.commentWrapper(paste0("\n\n", "Welcome to the {humanVerse}", "\n\n") ), 
			# "\n\n",
					# "You could try installing the package: ", "\n\n",
					# str.commentWrapper( pkg, r.tag = "-", s.pad=15), "\n");
					
		# str.commentWrapper
		}
	}
	
  m.a = property.get("meta", df); 
  ## if we have meta , str.trim(charAt(meta,1)) ... all = "#", just append and go ...
  #types = df.getColumnTypes(df);
  #types.line = paste0("# ", paste0(y, collapse="^"), " #"); # if row.names == TRUE, append - or something?
  #h.length = strlen(types.line); # use for custom header ... 
  #meta = paste0("# fdlskjf #", "\n");
  
	conn = file(filename, "rt");
	on.exit(close(conn));
  writeLines(meta);
  utils::write.table(df, conn, quote=quote, col.names=header, row.names=row.names, sep=sep);
  
  
  #fp = file(filename, open="wt");
  # writeLines("# comments #");
  # write.csv(df, fp);
  # close(fp);
  # open connection, write # comment header #, nearest data, write 
  # date types ... if row.names = TRUE, do what ... if they wanted row.names, make it a column
  # so I restore types ... as-is on dataframe 
  utils::write.table(df, file=filename, quote=quote, col.names=header, row.names=row.names, sep=sep);
  }

#' @rdname file.writeToPipe
#' @export
file.writeToPipe = writeToPipe;


#' readFromPipe
#'
#' This is the inverse of 'writeToPipe'
#'
#' @param file filename and path
#' @param header whether or not to add a header
#' @param quote whether or not to add quotes
#' @param sep "pipe" means sep="|" but you could change
#'
#' @return a dataframe
#' @export
readFromPipe = function() {}  
								# , as.is=TRUE
								# comment.char="#" ... hexdata
readFromPipe = function(filename, header=TRUE, quote="", sep="|",
								row.names = FALSE,
								meta.content = TRUE, 
								meta.skip="#", 
								stop.at=100, 
								meta.sep="^", ...)
  {
  if(!meta.content) 
	{
	df = utils::read.csv(filename, header=header, row.names=row.names,
										quote=quote, sep=sep, ...);
	return(df);
	}
  
  if(meta.skip == "") { stop("meta.skip must have a value, ala comment.char = '#'"); }
  # loop over readlines, grab the "header" content before the variable names ...
  # comment.char doesn't stop at header ... e.g., hexcolor data ...
  
  i = 0;
  hstr = character(0);
  conn = file(filename, "rt");
	on.exit(close(conn));
  while ( i < stop.at ) 
	{
    line = readLines(conn, n = 1);
	line_ = str.trim(line);
	if(length(line_) == 0) { stop("after n=i lines, we reached end of line without finidng"); }
	if(charAt(line_, 1) == meta.skip) 
		{ 
		hstr = c(hstr, line);
		i = 1 + i;
		print(i);
		} else {
				# we are stopping 
				cat("\n non-skip # found on line i \n");
				break;
				}
	}
	
	df = utils::read.csv(filename, header=header, quote=quote, sep=sep, skip=i, ...);
	df = property.set("meta", df, hstr);
	# meta.sep="^" ... let's find it ... and convert df.setColumnsType(df, types);
	return(df);	
  }
  
  
  
#' @rdname file.readFromPipe
#' @export
file.readFromPipe = readFromPipe;









#' @rdname fopen
#' @export
# https://www.php.net/manual/en/function.fopen.php
# EXPOSING the library:  https://www.tutorialspoint.com/c_standard_library/c_function_fopen.htm
# line 773 of connections.c in R:::source ... 
# 	fp = R_fopen(name, con->mode);
# https://www.php.net/manual/en/function.fgets.php
# fopen — Opens file or URL
# fopen() binds a named resource, specified by filename, to a stream.
# fp = fopen 
# fp as file-pointer 
fopen = function(filename, mode="rb", use.include.path = FALSE, ... )
	{
	# maybe do smart filename if use.include.path 
	if(!file.exists(filename)) { return(FALSE); }
	fp = file( description=filename, open=mode, ...);
	return(fp); 
	} 
	

#' @rdname file.open
#' @export
file.open = fopen;


# # close returns either NULL or an integer status, invisibly. The status is from when the connection was last closed and is available only for some types of connections (e.g., pipes, files and fifos): typically zero values indicate success. Negative values will result in a warning; if writing, these may indicate write failures and should not be ignored.
# The file pointed to by stream is closed.
# fclose — Closes an open file pointer 
# PHP https://www.php.net/manual/en/function.fclose.php
fclose = function(fp)
	{
	status = close(fp);
	if(status == 0) { return(TRUE); }
	res = FALSE;
	res = property.set("status", res, status);
	res;
	}
	
	

#' @rdname file.close
#' @export
file.close = fclose;



fread = function(fp)
	{
	## read in bytes by default, get to read lines ...
	## bypass whatever readLines is doing ... 
	
	}
	
# SEEK_END = "end";
# One of "start", "current", "end": see ‘Details’.
# Use of seek on Windows is discouraged. We have found so many errors in the Windows implementation of file positioning that users are advised to use it only at their own risk, and asked not to waste the R developers' time with bug reports on Windows' deficiencies.
# keep it binary 
fseek = function(fp, pos, origin="current")
	{	
	seek(fp, -1 * pos * buffer, origin = SEEK_END);
	}
	
	
# skip.lines has to be >= 0 as integer 
freadlines = function(filename, howmany=Inf, direction="FORWARD", skip.lines=0)
	{
	# could do some voodoo on howmany +/- and skip.lines +/- to remove direction ... voodoo is not generally human readable
	if(skip.lines < 0) { skip.lines = 0; }
	skip.lines = as.integer(skip.lines);
	 
	DIR = prep.arg(direction, n=1);
	SEEK = "end";
	if(DIR == "f") { SEEK = "start"; }
	
	buffer = 1024;
	file.size = file.info(filename)$size;
	if (file.size < buffer) 
		{
		buffer = file.size;
		}
	fp = file(filename, "rb");  # we have to read in binary 
		on.exit(close(fp));
		
	pos = 1;  # count of buffers ... 
	n 	= 1;  # count of lines ... 
	
	EOL = "\r\n";
	
	
	
	
	SEEK_END = "end";
	pos = 1;
	fragment = "";
	out = NULL;

# can I while loop this like fseek on PHP	
	fs = seek(fp, -1 * pos * buffer, origin = SEEK_END);
	info = readChar(fp, nchars = buffer);
	}
	


#' writeRDS
#'
#' The opposite of readRDS is writeRDS, make it so.
#'
#' @param obj The object to be stored
#' @param filename The file to store the object
#'
#' @return
#' @export
#'
writeRDS = function(obj, filename)
	{
	saveRDS(obj, file=filename);
	}



#' @rdname file.writeRDS
#' @export
file.writeRDS = writeRDS;


#' @rdname file.readRDS
#' @export
file.readRDS = readRDS;









##  cat(stri_info(short = TRUE))
## file:///C:/Users/Monte%20J.%20Shaffer/Desktop/v103i02.pdf
## https://stackoverflow.com/questions/7779032/validate-a-character-as-a-file-path

testme = "C:\\_git_\\github\\MonteShaffer\\humanVerse\\humanVerse\\R\\functions-HC.R";

# file.exists(testme);
# dir.exists( dirname(testme) );


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' file.init;
#'
#' @param my.path
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
#' WARNING:  OneDrive, DropBox may have file-lock ... CACHE, DATA, CODE are separate


#  /humanVerse/ SETUP ... SANDBOX ... CODE/NOTEBOOKS ... SYSTEM_LOGS ... DATA ... SECRET

# may I suggest for now ... C:/_R_/  or /home/_R_/ or /Users/_R_/ ... would not suggest ~


# SETUP ... log-level ... 
# $conf = new Config();
# $root = $conf->parseConfig("/etc/apache2/httpd.conf", "apache");
# php.ini 
# use my custom format ...
# $settings = @parse_ini_file($configfile, TRUE);
# everyone starts with output 
# https://github.com/austinhyde/IniParser/blob/master/src/IniParser.php
# https://stackoverflow.com/a/2120481/184614
# under the hood, it is written in C ... portable, eventually ...
# let's write an R-base version ...
 

file.init = function(
	CONFIG = paste0(Sys.getenv("R_USER"),"/R/humanVerse/"),
	CACHE = paste0(Sys.getenv("TEMP"),"/R/humanVerse/"),
	DATA = "C:/_R-DATA_/",
	CODE = "C:/_git_/github/MonteShaffer/humanVerse/notebooks/-functions-/",
	base.path = getwd(),
	verbose = TRUE)
	{


	}


# file.init = function(path.data, path.code, path.humanVerse, base.path="", verbose = TRUE)
# file.init = function(my.path = "C:/_R-TEMP_/", verbose = TRUE,
	# SECRET = ~/.ssh/R/ 
	# CONFIG = paste0(Sys.getenv("R_USER"),"/R/humanVerse/"),
	# SANDBOX = paste0(Sys.getenv("TEMP"),"/R/humanVerse/"),
	# DATA = "C:/_R-DATA_/",
	# below will have include.paths to search, this is the first 
	
	# CODE = "C:/_git_/github/MonteShaffer/humanVerse/notebooks/-functions-/",
	# base.path = getwd()
# ;
						# CACHE="/humanVerse/CACHE/", append.cache = TRUE,
						# DATA="C:/_R-DATA_/", 			append.data  = TRUE,
						# CODE="/-CODE-/",			append.code  = TRUE )
  
file.init = function()  
  {
	my.path = normalizePath(my.path);

	msg = list();
	msg[["INIT"]] = paste0(" INITIALIZING FILESYSTEM WITH _PATH_ ", "\n\t\t\t", my.path, "\n\n");
	msg[["STORING"]] = paste0(" STORING _PATH ", "\n\t\t\t", my.path, "\n\n");

	if(verbose) { cat(msg$INIT); }

	

	dir.createDirectoryRecursive(my.path);

	if(verbose) { cat(msg$STORING); }

	path.init(); # sets wd
	path.set("_PATH_", my.path);

	# file.path("E:", "DATA", "example.csv")

	my.pathCACHE = if(append.cache) { normalizePath( paste0(my.path, CACHE), mustWork=FALSE ); } else { normalizePath( CACHE, mustWork=TRUE ); }

	dir.createDirectoryRecursive(my.pathCACHE);
	path.set("_CACHE_", my.pathCACHE);

	my.pathDATA = if(append.data) { normalizePath( paste0(my.path, DATA), mustWork=FALSE ); } else { normalizePath( DATA, mustWork=TRUE ); }

	dir.createDirectoryRecursive(my.pathDATA);
	path.set("_DATA_", my.pathDATA);

	my.pathCODE = normalizePath( paste0(my.path, CODE), mustWork=FALSE );
	my.pathCODE = if(append.code) { normalizePath( paste0(my.path, CODE), mustWork=FALSE ); } else { normalizePath( CODE, mustWork=TRUE ); }

	dir.createDirectoryRecursive(my.pathCODE);
	path.set("_CODE_", my.pathCODE);
  }






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' file.readFrom;
#'
#' @param file
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
#' WARNING:  OneDrive, DropBox may have file-lock ... CACHE, DATA, CODE are separate
# add basic save/load ... save.image 
file.readFrom = function(filename, ..., method="stringi")
	{
	mmm = prep.arg(method, 3);

	if(mmm == "csv" || mmm == "pip")
		{
		# PIPE / CSV with allowed comments
		# not missing, but exists, see MD5 ===> is.missing 
		if( !exists("header", inherits = FALSE ) ) { header = TRUE; }
		if( !exists("quote", inherits = FALSE ) ) { quote = ""; }
		if( !exists("sep", inherits = FALSE ) ) { sep = "|"; }
		if( !exists("comment.char", inherits = FALSE ) ) { comment.char = "#"; }
						
		# read.csv is a fairly thin wrapper around read.table;
		return( utils::read.csv(filename, header=header, sep=sep, quote=quote, 
									comment.char=comment.char, ...) );
		}

	if(mmm == "tab")  # table
		{
		return( utils::read.table(filename, ...) );
		}

	if(mmm == "rds")
		{
		return( readRDS(filename) );
		}

	if(mmm == "jso")  # JSON
		{
		# maybe call this function again with "str" to get the stringi form.
		# json 	= rjson::fromJSON(json_str = readChar(filename, file.info(filename)$size), ...);
		# switch to jsonlite ???
		return( jsonlite::read_json(file=filename, ...) );
		}

	# readChar is one long string; readLines is a vector broken on "\n"

	if(mmm == "str")  # stringi
		{
		# file:///C:/Users/Monte%20J.%20Shaffer/Desktop/v103i02.pdf
		x = stringi::stri_read_raw(filename);
		if(!is.set(from))
			{
			y = stringi::stri_enc_detect(x);
			from = y[[1]][1,]$Encoding;  # most probable
			}
		if(!is.set(to)) { to = "UTF-8"; }
		z = stringi::stri_encode(x, from = from, to = to);
		return (z);

		## also has a readLines ...  stri_read_lines("ES_latin1.txt", encoding = "ISO-8859-1")
		}


	if(mmm == "cha")  # readChar
		{
		return( readChar(filename, file.info(filename)$size) );
		}

	if(mmm == "lin")  # readLines
		{
		if( !exists("n", inherits = FALSE ) ) { n = 10^5;} # guessing [pass in the value]
		return( readLines(filename, n) );
		}

	if(mmm == "bin")  # binary
		{
		if( !exists("what", inherits = FALSE ) )		{ what = "raw";}
		if( !exists("n", inherits = FALSE ) ) { n = 10^5;} # guessing [pass in the value]
		return( readBin(filename, what, n=n, ...) );
		}

	if(mmm == "dcf" || mmm =="deb")  # debian
		{
		return( read.dcf(filename, ...) );
		}

	stop(paste0("Appropriate Method [",method,"] was not found!"));
	# SPSS, SAV, STATA, MINITAB ... 

	}



file.readTailPipe = function( filename,
								n.end = 1,
								return = "string",
								adaptive = TRUE)
	{
	buffer = 1024;
	file.size = file.info(filename)$size;
	if (file.size < buffer) 
		{
		buffer = file.size;
		}
	fp = file(filename, "rb");  # we have to read in binary 
		on.exit(close(fp));
		
	SEEK_END = "end";
	pos = 1;
	fragment = "";
	out = NULL;

# can I while loop this like fseek on PHP	
	fs = seek(fp, -1 * pos * buffer, origin = SEEK_END);
	info = readChar(fp, nchars = buffer);
	lines = str.explode("\r\n", info);
		n.lines = length(lines);
		n.pipes = str.count(lines, what="|");
		n.mode = stats.mode(n.pipes);
		n.bad = which(n.pipes != n.mode);
	if (length(n.bad) > 0) 
		{
		fragment = lines[n.bad[1]];
		lines = lines[-c(n.bad[1])];
		}
		
		out = c(out, lines);
		n.out = length(out);
		if (n.out > n.end) 
			{
			return(out[(n.out - n.end + 1):n.out]);
			}
			
			
	stop("monte :: TODO")
	pos = 1 + pos
	fs2 = seek(fp, -1 * pos * buffer, origin = SEEK_END)
	info2 = readChar(fp, nchars = buffer)
	lines2 = str.explode("\r\n", info2)
	n.lines2 = length(lines2)
	n.pipes = str.count(lines2, what="|")
	n.bad = which(n.pipes != n.mode)
	
	}	

