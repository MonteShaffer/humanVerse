

# maybe have a copy/paste file ...
 
# maybe have openSesame, but this is generally me copying PATH from WINDOWZ into R ...
pathFromClipboard = function(trailing = TRUE)
	{
	x = readClipboard();
	prep.dir(x);	# prep.dir cleanses .. check.dir verifies it 
	}

# > y = pathFromClipboard()
# > y
# [1] "C:/_git_/github/MonteShaffer/humanVerse/humanVerse/inst/R/"
# > openSesame(y)

openSesame = function() {}
openSesame = function(path=getwd())
	{
	d = check.dir(path);  # could be a file or directory 
	utils::browseURL(path);
	}


file.init = function()
	{
	# grab settings from MEMORY if exists ... 
	
	# prompt for at least a root 
	
	if(!is.null(ROOT))
		{
	LIST = list("SYSTEM" = paste0(ROOT, "humanVerse/SYSTEM/"),
				"CONFIG" = paste0(ROOT, "humanVerse/CONFIG/"),
				"SECRET" = paste0(ROOT, "humanVerse/SECRET/"),
				"SANDBOX" = paste0(ROOT, "humanVerse/SANDBOX/"),
				"DATA" = paste0(ROOT, "humanVerse/DATA/"),
				"WORKSPACE" = paste0(ROOT, "humanVerse/WORKSPACE/")
				);
		}
	
	# TEMP 
	ini.file = "C:/_git_/github/MonteShaffer/humanVerse/humanVerse/inst/R/sample.ini";
	inistr = readChars(ini.file, 9999);
	lines = str.explode("\r\n", inistr);
	info = parse.ini(lines);
	names(info);
	
	x = info[["[PATH][laptop]"]];
	xnames = names(x);
	n = length(x);
	for(i in 1:n)
		{
		xname = xnames[i];
		xpath = as.character(x[i]);
cat("\n\n Checking directory [",xname,"] at ", xpath, "\n\n");
		# maybe a wrapper function to be verbose 
		dir.create(xpath, showWarnings=FALSE, recursive=TRUE);
		}
		
	# copy 
	# file.copy("C:/_git_/github/MonteShaffer/humanVerse/humanVerse/inst/R/sample.ini", "C:/_R_/humanVerse/CONFIG/humanVerse.ini");
		
	}

parse.pipeMeta = function() {}
parse.pipeMeta = function(meta.content, meta.sep = VSEP, meta.skip=COMMENT_CHAR,
					keys=c("NAMES:","TYPES:","FACTOR:","with LEVELS:")
				)
	{
	
		nam = str.contains(keys[1], meta.content);
	tmp = meta.content[nam]; 
	tmp2 = str.explode(keys[1], tmp);
	names = str.trim( str.explode(meta.sep, tmp2[2]) );
		typ = str.contains(keys[2], meta.content);
	tmp = meta.content[typ]; 
	tmp2 = str.explode(keys[2], tmp);
	types = str.trim( str.explode(meta.sep, tmp2[2]) );
		fac = str.contains(keys[3], meta.content);
	tmp = meta.content[fac]; 
	tmp2 = str.explode(keys[3], tmp);
	tmp3 = str.explode(keys[4], tmp2[2]);
	# this might be multivariate ...
	# may have +1 cases or 0 cases on this and above names/types 
	fkeys = str.trim(tmp3[1]);
	fvals = str.trim(str.explode(meta.sep, tmp3[2]));
	
	## TODO, when multivariate or zero variate 
	############## list(fkeys = fvals); # name => levels 

	slen = str.len(meta.content);
	description = "";  # lines between TOP and BOTTOM without 1 len ...
	idx = v.which(slen, 1);  imin = min(idx); imax = max(idx);
	missing = set.diff(imin:imax, idx);
	description = str.trim(str.trimFromFixed( meta.content[missing], meta.skip, "LEFT"));
	description = paste0(description, collapse="\n");
	
	meta = list("names" = names, "types" = types, "factors" = list("keys" = fkeys, "values" = fvals), "description" = description );
	meta;
	}


dir.setSeparator = function(force=NULL)
	{
	if(!is.defined(DIR_SEPARATOR)) { constants.default(); }
	if(is.null(force))
		{	
		DIR_SEPARATOR = DIR_LINUX;
		if(is.windows()) { DIR_SEPARATOR = DIR_WINDOZE; }
		} else { DIR_SEPARATOR = force; } # manually force ... 
	
	# this needs to be modified to NAMESPACE of library
	# keep ls() clean ... 
	# %NAMESPACE% could also be functions ...
	# DIR_SEPARATOR %NAMESPACE%.   #(. would be humanVerse, but any allowed)
	DIR_SEPARATOR %GLOBAL%.;   
	DIR_SEPARATOR;  # should be mute ... seems like R internally handles LINUX forms on windows ... 
	}

tmp.dir = function()
	{
	d = prep.dir( tempdir(check=TRUE) );
	d;
	}
	
tmp.file = function(stem = "humanVerse.txt")
	{
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.STEM = check.type(stem);
	if(!ct.STEM || !is.character(stem))	
		{ stem = deparse(substitute(stem)); } 
##########################################################

	# allow lazy loading ... 
	d = prep.dir( tempdir(check=TRUE) );
	f = paste0(d, stem);
	f;
	}
	
# path.info("C:/garba/dkfj/")
# path.info(tmp.file("sldsfeep.txt"))
# exists from writeToPipe(sleep, tmp.file("sleep.txt"));
# path.info(tmp.file("sleep.txt"));  
# path.info(getwd())

path.info = function(path, trailing = TRUE, create=FALSE)
	{
	# is.file and is.dir fails on path=getwd() ... not a file 
	# fopen(path)  cannot open file 'C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R': Permission denied
	# ergo, its a path ?
	pf = check.file(path, trailing=trailing, create=create)
	pd 	= prep.dir(path, trailing=trailing); 
	cd =  check.dir(path, trailing=trailing, create=create);
	d1 = prep.dir(dirname(pd), trailing=trailing);
	e = check.ext(path);
	b = basename(path);
	d = dirname(path);
	if(trailing) { d = paste0(d, DIR_LINUX); }
	# not the best logic, but all I got, I think ... 
	ext.test = "file"; if(is.null(e)) { ext.test = "dir"; }
	trailing.test = "file"; if(pd != pf) { trailing.test = "dir"; } 
	if_ = is.file(pf);  # reserved word 
	id_ = is.dir(pf);
	is_ = c("-UNKNOWN-", "!exists");
	is__ = FALSE;
		if(if_ && !id_) { is_ = c("file", "exists"); is__ = TRUE;}
		if(if_ && id_) { is_ = c("dir", "exists");  is__ = TRUE;}
	
	# subtract pd - cd ... if it contains a SLASH
	di = pd %-% d;
	
	status = "file";
	if(str.contains(SLASH, di)) { status = "dir"; }
	
	info = list("type" 				= status,
				"exists" 			= is__,
				"exists.info" 		= is_,
				"stem" 				= b,
				"ext" 				= e,
				"path.as.dirname"	= d,
				"path.as.dir" 		= pd,
				"path.diff" 		= di,
				"path.as.file" 		= pf,
				"ext.test" 			= ext.test,
				"dir.test" 			= trailing.test,
				
				"is.file" = if_,  # file.stat means has r/w perms?
				"is.dir" = id_		# chmod ... run as ADMIN
				);
	print(str(info));
	invisible(info);
	}
	
# create as in create.DIRECTORY
check.file = function(path, trailing = TRUE, create=TRUE)  
	{
	d = check.dir(path, create=create, trailing=trailing);
	stem = basename(path);  # not filename()	
	f = paste0(d, stem);
	f;
	}	
	
# library(help = "datasets")
prep.dir = function(x, trailing = TRUE, force.trailing=FALSE)
	{
	z = check.ext(x);
	y = str.replace(DIR_WINDOZE, DIR_LINUX, x);
	# you may want to create a directory with stem
	# force.trailing ... DATA PROVENANCE ... 
	# "C:/.../Temp/Rtmp2XXr6l/iris.txt" => "C:/.../Temp/Rtmp2XXr6l/iris.txt/"
	if((trailing && is.null(z)) || force.trailing) 
		{ 
		y = paste0(y, DIR_LINUX); 
		y = str.replace(DOUBLE_SLASH, DIR_LINUX, y);
		}
	y = str.replace(DOUBLE_SLASH, DIR_LINUX, y);  # ONE more, just in CASE 
	# minvisible(y, display=print, key="DIR");	
	# Error in eval(parse(text = objstr)) :  trying to get slot "original" from an object of a basic class ("list") with no slots

	y;
	}
	
check.ext = function(x, dotless=TRUE)
	{
	stem = basename(x);  # not filename()
	s = str.pos(EXT, stem);
	if(is.null(s)) { return(NULL); }
	lenstem = str.len(stem);
	slen = len(s);
	
	dot = 1;
	if(!dotless) { dot = 0; }
	
	substring(stem, dot + s[slen], lenstem);	
	}
	
# tempdir()
check.dir = function(path, trailing = TRUE, create=TRUE)
	{
	# if NOT LOCAL, download to TMP location
	# update filename using %TO% ?  parent.frame(1)
	d = dirname(path);
	d = prep.dir(d, trailing=trailing);
	
	if(create)
		{
		dir.create( d, 
					showWarnings = FALSE, 
					recursive = TRUE
				);
		}
	d;
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
writeToPipe = function() {}
# sleep has two factors 
# writeToPipe(sleep, tmp.file("sleep.txt"));
# filename = "C:/_R_/humanVerse/SANDBOX/data/iris.txt"; 
writeToPipe = function(df=iris, filename=tmp.file("iris.txt"), header = TRUE, quote=EMTPY, sep=PIPE, 
									prepend.meta = TRUE, 
									meta.content = EMPTY,
									meta.msg = "This data is about",
									meta.sep = VSEP,
									meta.skip = COMMENT_CHAR,
									row.names = FALSE, ...)
	{
	filename = check.file(filename);
cat("\n\n", filename, "\n\n");  openSesame(filename); 
	df.name = substitute(df);
	quote_ = quote;
	if(quote == "") { quote = FALSE; } else { quote = TRUE; }
	if(!prepend.meta)
		{
		# warning about HEADERS, weird?
		suppressWarnings( 
			utils::write.table(df, file=filename, 
				sep=sep, quote=quote, 
				col.names=header, row.names=row.names, ...)
						);
		return(TRUE);
		}
	
	# we can load our data ... write ...
	# then call write.table (append=TRUE)
	## IF we have, GOOD TO GO, otherwise, build it ...
	################### BUILD META #################
	{
	names = colnames(df);		
	types = df.getColumnTypes(df);	
	
	# extra rows for factors?
	idx = v.which(types, "factor");	
	factor.lines = NULL;
	if(!is.null(idx))
		{ 
		n = length(idx);
		factor.lines = character(n);
		for(i in 1:n)
			{
			id = idx[i]; 
			cname = colnames(df)[id];
			# fact = paste0("FACTOR: ", quote_, cname, quote_, " with LEVELS: ", paste0(quote_,paste0( levels(df[[cname]]), collapse = paste0(quote_,meta.sep,quote_)),quote_));
			factor.lines[i] = paste0(meta.skip, " " , "FACTOR: ", cname, " with LEVELS: ", paste0( levels(df[[cname]]), collapse = meta.sep));					
			}				
		}
		
		if(row.names) { names = c("row.names", names); }
			names.line = paste0(names, collapse=meta.sep);
		if(row.names) { types = c("row.names", types); }
			types.line = paste0(types, collapse=meta.sep);
	
			# maybe build ASCII art WELCOME to HUMAN VERSE 
			meta.content = str.pipeHeader(meta.msg, ctag=meta.skip);
				minfo = property.get("more", meta.content);
			meta.content %.=% ("\n" %.% meta.skip %.% " DATA:  " %.% df.name %.% "\n");
			meta.content %.=% ("\n" %.% meta.skip %.% " NAMES:  " %.% names.line %.% "\n");
			meta.content %.=% (meta.skip %.%" TYPES:  " %.% types.line %.% "\n");
			if(!is.null(factor.lines)) 
				{
				meta.content %.=% (paste0(factor.lines, collapse="\n") %.% "\n");
				}
			meta.content %.=% (minfo[["cline"]] %.% "\n");
			
	cat(meta.content, sep=EMPTY, file=filename, append=FALSE);
	# writeLines(meta.content, sep="\r\n");
	}
	
	# warning about HEADERS, weird?
	suppressWarnings( 
			utils::write.table(df, file=filename, 
				sep=sep, quote=quote, 
				col.names=header, row.names=row.names, 
				append=TRUE, ...)
						);
						
	return(TRUE);
	}




















































readFromPipe = function() {}
# filename = "C:/_R_/humanVerse/SANDBOX/data/iris.txt";
readFromPipe = function(filename=tmp.file("iris.txt"), header = TRUE, 
									quote=EMPTY, sep=PIPE, 
									append.meta = TRUE, 
									meta.sep = VSEP,
									meta.skip = COMMENT_CHAR,
									...)
	{
	check.dir(filename);
	# quote is not TRUE/FALSE here, it is a string
	# if the TEXT file has #336699 (hexcolors) in TABLE, the 
	# regular import with read.table/csv will fail ...
	if(!append.meta)
		{
		df = utils::read.table(filename, 
					header=header, comment.char=meta.skip,
					quote=quote, sep=sep, ...)
		return(df);
		}
	
	# can I pipe a stream to read.table ...
	# make two files in TEMP ... 
	# or just build it all myself ...
	lines = str.explode("\r\n", readTextFile(filename) );
		meta.content = character();
	n = length(lines); idx = 1;
	for(i in 1:n)
		{
		line = lines[i]; first = charAt(line,1);
		if(first == meta.skip)
			{
			meta.content[idx] = line; idx %++%.; 
			} else { break; }		
		}				
		meta = parse.pipeMeta(meta.content, 
							meta.sep=meta.sep, meta.skip=meta.skip);
	data = lines[i:n];
		filenameTMP = paste0(filename, "TMP");
	cat(data, file = filenameTMP, sep="\n");
	
	df = utils::read.table(filenameTMP, 
					header=header, comment.char=meta.skip,
					quote=quote, sep=sep, ...)
		unlink(filenameTMP);
		
	types = df.getColumnTypes(df);
	names = names(df);
	
	# meta may have rownames 
	# if types != ... let's update that ...
	logic = (types != meta$types);
	# get type, with factor what is the best way ...
	
	# append meta object to df ... with meta.raw.content 
	meta$raw = meta.content;
	
	# before appending meta, goal is to get 
	# identical(df, iris)
	# df[["Species"]] = factor(df[["Species"]], levels = meta$factors$values)
	# > identical(df,iris)
	#[1] TRUE

	df = property.set("meta", df, meta);
	df;
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
readFromPipe222 = function() {}	
								# , as.is=TRUE
								# comment.char="#" ... hexdata
readFromPipe222 = function(filename, header=TRUE, quote="", sep="|",
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
# EXPOSING the library:	https://www.tutorialspoint.com/c_standard_library/c_function_fopen.htm
# line 773 of connections.c in R:::source ... 
# 	fp = R_fopen(name, con->mode);
# https://www.php.net/manual/en/function.fgets.php
# fopen — Opens file or URL
# fopen() binds a named resource, specified by filename, to a stream.
# fp = fopen 
# fp as file-pointer 
fopen = function(filename, mode="rb", use.include.path = FALSE, ... )
	{
	if(!is.defined(SEEK_CURRENT)) { constants.default(); }
 
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
fseek = function(fp, pos, buffer, origin=SEEK_CURRENT)
	{	
	seek(fp, pos * buffer, origin = origin);
	} 

	
prep.data = function(df, sep=PIPE, quote=EMPTY, row.names = FALSE)
	{
	# factors as characters?
	n = nrow(df);	
	if(row.names) 
		{
		df = cbind(rownames(df), df);		
		}
	str = character(n);
	for(i in 1:n)
		{
		row = paste0(df[i, ], collapse=sep);
		# encase in "quote"[data]"quote"
		rstr = str.replace(sep, paste0(quote,sep,quote), row);
		str[i] = paste0(quote, rstr, quote);
		}
	str;	
	}

writePipeDelimitedFile = function() {}
writePipeDelimitedFile = function (df, 
				filename, 
				header=TRUE, 
				sep="|",
				quote="",
				row.names = FALSE,
				
				meta.attach = TRUE, 
				meta.comment="#",
				meta.sep="^",
				
				meta.content = defaultPipeHeader()
					)
	{
	# just use cat?
	
	
	
	}
	
readPipeDelimitedFile = function() {}
readPipeDelimitedFile = function(
				filename, 
				header=TRUE, 
				sep="|",
				quote="",
				
				meta.attach = TRUE, 
				meta.comment="#",
				meta.sep="^",
				row.names = FALSE
								)
	{
	lines = str.explode("\r\n", readTextFile(filename) );
	
	
	}
	
# identical(readChar(filename, file.info(filename)$size), readTextFile(filename));
# maybe set default to a test file in /inst/ ... read.system ... 
# colors would be a good choice ... # tag in FIELDS 
readTextFile = function(filename, buffer=BUFFER)
	{
	fp = file(filename, "rb");	# we have to read in binary 
		on.exit(close(fp));
	file.size = file.info(filename)$size;
	if (file.size < buffer) 
		{
		buffer = file.size;
		}
		
	str = "";
	pos = 0;
	csize = 0;
	while(csize < file.size)
		{
		fs 		= seek(fp, pos*buffer, origin = SEEK_START);
		stream 	= readChar(fp, nchars = buffer);
		str 	= paste0(str, stream);
		pos %++%.;
		csize %+=% buffer;
		}	
	str;
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
	fp = file(filename, "rb");	# we have to read in binary 
		on.exit(close(fp));
		
	pos = 1;	# count of buffers ... 
	n 	= 1;	# count of lines ... 
	
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









##	cat(stri_info(short = TRUE))
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
#' WARNING:	OneDrive, DropBox may have file-lock ... CACHE, DATA, CODE are separate


#	/humanVerse/ SETUP ... SANDBOX ... CODE/NOTEBOOKS ... SYSTEM_LOGS ... DATA ... SECRET

# may I suggest for now ... C:/_R_/	or /home/_R_/ or /Users/_R_/ ... would not suggest ~


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
						# DATA="C:/_R-DATA_/", 			append.data	= TRUE,
						# CODE="/-CODE-/",			append.code	= TRUE )
	
dfsafile.init = function()	
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
#' WARNING:	OneDrive, DropBox may have file-lock ... CACHE, DATA, CODE are separate
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

	if(mmm == "tab")	# table
		{
		return( utils::read.table(filename, ...) );
		}

	if(mmm == "rds")
		{
		return( readRDS(filename) );
		}

	if(mmm == "jso")	# JSON
		{
		# maybe call this function again with "str" to get the stringi form.
		# json 	= rjson::fromJSON(json_str = readChar(filename, file.info(filename)$size), ...);
		# switch to jsonlite ???
		return( jsonlite::read_json(file=filename, ...) );
		}

	# readChar is one long string; readLines is a vector broken on "\n"

	if(mmm == "str")	# stringi
		{
		# file:///C:/Users/Monte%20J.%20Shaffer/Desktop/v103i02.pdf
		x = stringi::stri_read_raw(filename);
		if(!is.set(from))
			{
			y = stringi::stri_enc_detect(x);
			from = y[[1]][1,]$Encoding;	# most probable
			}
		if(!is.set(to)) { to = "UTF-8"; }
		z = stringi::stri_encode(x, from = from, to = to);
		return (z);

		## also has a readLines ...	stri_read_lines("ES_latin1.txt", encoding = "ISO-8859-1")
		}


	if(mmm == "cha")	# readChar
		{
		return( readChar(filename, file.info(filename)$size) );
		}

	if(mmm == "lin")	# readLines
		{
		if( !exists("n", inherits = FALSE ) ) { n = 10^5;} # guessing [pass in the value]
		return( readLines(filename, n) );
		}

	if(mmm == "bin")	# binary
		{
		if( !exists("what", inherits = FALSE ) )		{ what = "raw";}
		if( !exists("n", inherits = FALSE ) ) { n = 10^5;} # guessing [pass in the value]
		return( readBin(filename, what, n=n, ...) );
		}

	if(mmm == "dcf" || mmm =="deb")	# debian
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
	fp = file(filename, "rb");	# we have to read in binary 
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
		n.pipes = str.count(what="|", lines);
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
	n.pipes = str.count(what="|", lines)
	n.bad = which(n.pipes != n.mode)
	
	}	

