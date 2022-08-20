
# library(humanVerse);
# source( github/humanVerse/-ONE- );
# github.install("humanVerse");  # this is hardcoded as first, HVcpp also
## TODO: add timers 

# n = 433333; n = -3933333; 
## file.fR = date.generateProlepticGregorian(433333);
## file.bR = date.generateProlepticGregorian(-3933333); 
# fR = forwardPAPAL; bR = backwardPAPAL;
# ==> PAPAL becomes PAPAL 
file.fR = date.generateProlepticGregorian(9914); 
file.bR = date.generateProlepticGregorian(-9914); 

df.fR = readFromPipe(file.fR);
df.bR = readFromPipe(file.bR);

range(df.fR$YYYY);
range(df.bR$YYYY);

LOWER = 1560;  # we looped on days = 9914
UPPER = 1605;  # let's truncate the date to have nice START/END in RANGE

## LOWER = -7575;
## UPPER = 2525;

df.bRn = subset(df.bR, YYYY >= LOWER); head(df.bRn); tail(df.bRn);
df.fRn = subset(df.fR, YYYY <= UPPER); head(df.fRn); tail(df.fRn);

df.bfRn = rbind(df.bRn, df.fRn);  dim(df.bfRn);
# df.sortBy() may be easier ... 
df.bfRns = df.bfRn[with(df.bfRn, order(-YYYY, -MM, -DD)), ];
	rownames(df.bfRns) = 1:dim(df.bfRns)[1];
	head(df.bfRns); tail(df.bfRns);  

# MY DUPLICATE is just one element, has the same IDX of 0
# df = df.bfRns[!duplicated(df.bfRns), ]; # or unique()  # SLOW on large datasets, WHY?
idxs = which(df.bfRns$IDX == 0);
# df.removeRows() may be easier ... 
df = df.bfRns[-c(idxs[1]), ]; dim(df);

df.printHead(df, 8, 1);
df.printHead(df, 8, 999999999);

search = which( df$YYYY == 1600 & df$MM == 8 & df$DD == 5 );   
# offset, PAPAL => RUTH ... 6507 days ... 
df.printHead(df, 8, search[1]);  # should only have one, JIK

# string subtraction on filenames 
# save to TXT and RDS
# saveRDS(		df, "PAPAL_(1575,1625).rds");
writeToPipe(	df, "PAPAL_(1575,1625).txt");
Sys.sleep(1);
zip("PAPAL_(1575,1625).zip", "PAPAL_(1575,1625).txt");
##saveRDS(	df, "PAPAL_(-7575,2525).rds");
##writeToPipe(df, "PAPAL_(-7575,2525).txt");

# do proleptic number checks ... this was JULIAN PROLEPTIC

df$GPN = as.integer( date.toGregorianProlepticNumber(df$YYYY, df$MM, df$DD) );

res = date.fromGregorianProlepticNumber(df$GPN);
df$gyear = res$gyear;  		# how to make this happen as a function 
df$gmonth = res$gmonth;		# names unique in df, or append .1 
df$gday = res$gday; 		# check lengths, return NULL on mismatch

# identical ?
# identical(df$DD, df$gday);
idxs = which( (df$YYYY != df$gyear) & (df$MM != df$gmonth) & (df$DD != df$gday) );

## debugging
# df$diff = df$DD - df$gday;
# search = which(df$diff > 0);
# df.printHead(df, 8, search[1]);
## current code has error in FEB 29th ...
## cleap ==> cleapdays

# QED


