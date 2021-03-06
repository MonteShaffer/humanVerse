% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions-mysql.R
\name{mysql.secretConnectionSQL}
\alias{mysql.secretConnectionSQL}
\title{mysql.secretConnectionSQL}
\usage{
mysql.secretConnectionSQL(
  str = "WSU_SANDBOX_",
  timeout.secs = 60,
  save = FALSE
)
}
\arguments{
\item{str}{character string, default from above example would be ""WSU_SANDBOX_"}

\item{timeout.secs}{integer, 60 is default}

\item{save}{key to cache the db connection credential into .GlobalEnv$.humanVerse[["sql"]]}
}
\value{
mysql.connection `conn` if successful
}
\description{
This allows you to copy/paste database credentials in the console.
}
\details{
For example,

Sys.setenv(WSU_SANDBOX_HOST = "db.example.com");
Sys.setenv(WSU_SANDBOX_DATABASE = "my_db_name");
Sys.setenv(WSU_SANDBOX_USER = "my_db_user");
Sys.setenv(WSU_SANDBOX_PASSWD = "mySuperSecretPassword");
}
