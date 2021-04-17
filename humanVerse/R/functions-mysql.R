# mysql.memory = list();  # global scope ... ## add to .humanVerse


#' mysql.secretConnectionSQL
#'
#' This allows you to copy/paste database credentials in the console.
#'
#' For example,
#'
#' Sys.setenv(WSU_SANDBOX_HOST = "db.example.com");
#' Sys.setenv(WSU_SANDBOX_DATABASE = "my_db_name");
#' Sys.setenv(WSU_SANDBOX_USER = "my_db_user");
#' Sys.setenv(WSU_SANDBOX_PASSWD = "mySuperSecretPassword");
#'
#' @param str character string, default from above example would be ""WSU_SANDBOX_"
#' @param timeout.secs integer, 60 is default
#' @param save key to cache the db connection credential into .GlobalEnv$.humanVerse[["sql"]]

#'
#' @return mysql.connection `conn` if successful
#' @export
mysql.secretConnectionSQL = function(str="WSU_SANDBOX_", timeout.secs = 60, save=FALSE)
  {
  # RMySQL is being rebranded RMariaDB with better features
  # this approach is deprecated, but works in R
  # in the real world, lookup "pdo" ... prevents MySQL injection
  timeout = Sys.getenv( paste0(str,"TIMEOUT") );
  if(timeout == "") { timeout = timeout.secs; }
  conn = RMariaDB::dbConnect(RMariaDB::MariaDB(),
                      timeout = timeout,
                      user = Sys.getenv( paste0(str,"USER") ),
                      password = Sys.getenv( paste0(str,"PASSWD") ),
                      dbname = Sys.getenv( paste0(str,"DATABASE") ),
                      host = Sys.getenv( paste0(str,"HOST") )
                      );
  if(!isFALSE(save))
    {
    info = list("timeout" = timeout,
              "user" = Sys.getenv( paste0(str,"USER") ),
              "password" = Sys.getenv( paste0(str,"PASSWD") ),
              "dbname" = Sys.getenv( paste0(str,"DATABASE") ),
              "host" = Sys.getenv( paste0(str,"HOST") ) );

    .GlobalEnv$.humanVerse[["sql"]][[save]] = serialize(info, connection=NULL);
    }
  conn;
  }


#' mysql.dbConnect
#'
#' @param user character string
#' @param password character string
#' @param dbname character string
#' @param host character string
#' @param timeout.secs integer, 60 is default
#' @param save key to cache the db connection credential into .GlobalEnv$.humanVerse[["sql"]]
#'
#' @return
#' @export
mysql.dbConnect = function(user=user,password=password,dbname=dbname,host=host,timeout.secs=60,save=FALSE)
  {
  conn = RMariaDB::dbConnect(RMariaDB::MariaDB(),
                      timeout = timeout.secs,
                      user = user,
                      password = password,
                      dbname = dbname,
                      host = host
                      );

  if(!isFALSE(save))
    {
    info = list("timeout" = timeout.secs,
              "user" = user,
              "password" = password,
              "dbname" = dbname,
              "host" = host );
    .GlobalEnv$.humanVerse[["sql"]][[save]] = serialize(info, connection=NULL);
    }
  conn;
  }

#' mysql.dbConnectFromMemory
#'
#' @param save single character string, key to save the db info into .GlobalEnv$.humanVerse[["sql"]]
#'
#' @return FALSE if fails, a `conn` if succeeds
#' @export
mysql.dbConnectFromMemory = function(save="")
  {
  # if not known, we will grab first element ...
  if(save == "") { save = 1; }
  db = tryCatch(
        {
        info = unserialize(.GlobalEnv$.humanVerse[["sql"]][[save]]);
        },
        warning = function(w)
            {
              warning(paste0("dbConnectFromMemory throws a warning",w));
              info; # let's still return the value ...
            },
            error = function(e)
            {
              warning(paste0("dbConnectFromMemory throws an error",e));
              return (FALSE);
            },
            finally =
              {

              }
            );

  if(length(db)==5)
  	{
  	conn = RMariaDB::dbConnect(RMariaDB::MariaDB(),
  						  timeout = db$timeout,
  						  user = db$user,
  						  password = db$password,
  						  dbname = db$dbname,
  						  host = db$host
  						  );
  	return (conn);
  	}
  FALSE;
  }


#' mysql.checkServerStatusSQL
#'
#' @param conn mysql.connection object (technically MariaDB)
#' @param sql character string of SQL query
#'
#' @return FALSE if not good, the CURRENT_TIMESTAMP (by default)
#' @export
mysql.checkServerStatusSQL = function(conn, sql = "SELECT CURRENT_TIMESTAMP;")
  {
  # sql = "SELECT CURTIME(4);"
  status = tryCatch(
        {
        res = RMariaDB::dbGetQuery(conn,sql);
        },
        warning = function(w)
            {
              warning(paste0("mysql.checkServerStatusSQL throws a warning",w));
              res; # let's still return the value ...
            },
            error = function(e)
            {
              warning(paste0("mysql.checkServerStatusSQL throws an error",e));
              return (FALSE);
            },
            finally =
              {

              }
            );
  unlist(status);
  }


#' fetchAllSQL
#'
#' @param conn mysql.connection object (technically MariaDB)
#' @param sql character string of SQL query
#' @param save name of `conn` object that can be saved for reconnection
#'
#' @return dataframe of results; possibly FALSE or an error ... it tries to grab
#'  the query, and if it fails, it tries to reconnect ... if it fails twice, it fails.
#' @export
mysql.fetchAllSQL = function(conn, sql, save="")
  {
  # this may break for large datasets ...
  # dbGetInfo(mysql.connection);
  result = tryCatch(
        {
        res = RMariaDB::dbGetQuery(conn, sql);
        },
        warning = function(w)
            {
              warning(paste0("fetchAllSQL throws a warning",w));
              res; # let's still return the value ...
            },
            error = function(e)
            {
              warning(paste0("fetchAllSQL throws an error",e));
              FALSE;
            },
            finally =
              {

              }
            );

  if(isFALSE(result))
    {
    conn = mysql.dbConnectFromMemory(save);
    # second try
    result = RMariaDB::dbGetQuery(conn, sql); # do I need a try/catch here?
    }
  result;
  }

mysql.parseTemplateFromList = function(sql, obj)
  {
  keys = names(obj);
  nsql = sql;
  n.keys = length(keys);
  for(i in 1:n.keys)
    {
    mykey = paste0("{", keys[i], "}");
    nsql = gsub(mykey, obj[[mykey]], nsql, fixed=TRUE);
    }
  nsql;
  }
  
  
#' mysql.parseTemplateKeysVals
#'
#' @param sql character string as sql.template
#' @param keys keys to replace in the sql.template
#' @param vals values to replace keys in the sql.template
#'
#' @return updated sql character string
#' @export
#'
#' @examples
#' sql.template = "SELECT * FROM {tablename} WHERE zipcode = '{zipcode}';";
#'         keys = c("tablename", "zipcode");
#'         vals = c("zipcodes", 99163);
#' parseTemplateSQL(sql.template, keys, vals);
#'
mysql.parseTemplateKeysVals = function(sql, keys, vals)
  {
  # similiar to pdo but for readablity and parsing (variadic)
  nsql = sql;
  n.keys = length(keys);
  n.vals = length(vals);
  if(n.keys != n.vals)
    {
    warning("Something wrong in parseTemplateSQL ... keys and vals are of different lengths");
    return (NA);
    }
  for(i in 1:n.keys)
    {
    mykey = paste0("{", keys[i], "}");
    nsql = gsub(mykey, vals[i], nsql, fixed=TRUE);
    }
  nsql;
  }





# library(readxl);
# xlsfile = "Q:/_git_/github/DataWar/CRAN/misc/CRANv2.xlsx";
# outpath = "Q:/_git_/github/DataWar/CRAN/v2020/SQL/"; createDirectoryRecursive(outpath);
# mysql.XLStoInsert(xlsfile, outpath);
mysql.XLStoInsert = function(xlsfile, outpath="SQL")
	{
	if(dir.exists(outpath)) 
		{ 
		basedir = outpath; 
		} else 	{ 
				basedir = paste0( dirname(xlsfile), "/", outpath);				
				}
				
				
		dir.schema 		= cleanup.local( paste0(basedir, "/", "01-database-schema", "/") );
							createDirectoryRecursive(dir.schema);
		dir.insert 		= cleanup.local( paste0(basedir, "/", "02-database-insert", "/") );
							createDirectoryRecursive(dir.insert);
		dir.copy 		= cleanup.local( paste0(basedir, "/", "02-database-copy", "/") );
							createDirectoryRecursive(dir.copy);
		dir.indexing 	= cleanup.local( paste0(basedir, "/", "03-database-indexing", "/") );
							createDirectoryRecursive(dir.indexing);
							
		all.schema 		= paste0(dir.schema, "-ALL-.sql");
		all.insert 		= paste0(dir.insert, "-ALL-.sql");
		all.indexing 	= paste0(dir.indexing, "-ALL-.sql");
			

	templates = list();
	templates$create 	= '
						CREATE TABLE IF NOT EXISTS `{mytable}` (
																{my.columns}
																);';

	templates$column 	= '
						`{mycolumn}` {mytype} {myauto} {mysigned} {mynull} {mydefault} {mycomma}
							';
							
	
	templates$alter  	= '
						ALTER TABLE `{mytable}`
												{my.alters}
						COMMIT; ';
						
	templates$index  	= '
						ADD KEY `{index.key}` (`{index.key}`) ';

	templates$primary  	= '
						ADD PRIMARY KEY (`{primary.key}`) ';
						
		
	tabs = readxl::excel_sheets(xlsfile);
	for(tab in tabs)
		{
		my.sheet = as.data.frame(  readxl::read_excel(xlsfile, sheet=tab) );
		
		
		
		
		
		
		rinfo = my.sheet[1, ];
		if(is.null(rinfo$variable)) { break; }
		printPaste0(" -------- TAB :: ", tab, "\n");
		
		
		create = templates$create;
			create = str_replace("{mytable}", tab, create);
			
		alter  = templates$alter;
			alter = str_replace("{mytable}", tab, alter);
		
		mycols = "";
		myalts = "";
		
		nr = nrow(my.sheet);
		for(i in 1:nr)
		  {
		  rinfo = my.sheet[i, ];
			
			mycol = templates$column;
				# `{mycolumn}` {mytype} {myauto} {mysigned} {mynull} {mydefault} {mycomma}
				mycol = str_replace("{mycolumn}", 	rinfo$variable, mycol);
				mycol = str_replace("{mytype}", 	rinfo$type, mycol);
					myauto = ""; if(!is.null(rinfo$auto)) { myauto = "AUTO_INCREMENT"; }
				mycol = str_replace("{myauto}", 	myauto, mycol);
					mysigned = ""; if(!is.null(rinfo$signed)) { mysigned = "UNSIGNED"; }
				mycol = str_replace("{mysigned}", 	mysigned, mycol);
					mynull = "NOT NULL"; if(!is.null(rinfo$null)) { mynull = "NULL"; }
				mycol = str_replace("{mynull}", 	mynull, mycol);
					mydefault = ""; if(!is.null(rinfo$default)) { mydefault = paste0("DEFAULT ", rinfo$default); }  # ZEROFILL
				mycol = str_replace("{mydefault}", 	mydefault, mycol);
					mycomma = ""; if(i != nr) { mycomma = " , "; }  
				mycol = str_replace("{mycomma}", 	mycomma, mycol);
				
			mycols = paste0( mycols, mycol, "\n");	

			myalt = "";
			if(!is.na(rinfo$key))
				{
				myalt = templates$primary;
				myalt = str_replace("{primary.key}", rinfo$variable, myalt);
				} else	{
						if(!is.na(rinfo$key))
							{
							myalt = templates$index;
							myalt = str_replace("{index.key}", rinfo$variable, myalt);
							}
						}
			
			myalts = paste0( myalts, myalt, "\n");
			
		  }
		  
		create = str_replace("{my.columns}", mycols, create);
		file.schema = paste0(dir.schema, tab, ".sql");
		
		writeLine(create, file=file.schema, append = FALSE);
		writeLine(create, file=all.schema,  append = TRUE);
		
		
		alter = str_replace("{my.alters}", myalts, alter);
		file.indexing = paste0(dir.indexing, tab, ".sql");
		
		if(trimMe(myalts) != "")
			{		
			writeLine(alter, file=file.indexing, append = FALSE);
			writeLine(alter, file=all.indexing,  append = TRUE);
			}
		  
		}
	# inserts to be done later ...
	
	}
	
	
	
	
	



