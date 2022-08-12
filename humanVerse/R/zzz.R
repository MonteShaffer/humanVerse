
# https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html
.onLoad = function(libname, pkgname)
  {
  init(); 
  invisible();
  }


.onUnload = function(libname, pkgname)
  {
  unload();
  invisible();
  }


