
# https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html
.onLoad = function(libname, pkgname)
  {
  memory.init();
  memory.smartRestore();
  invisible();
  }


.onUnload = function(libname, pkgname)
  {
  invisible();
  }


