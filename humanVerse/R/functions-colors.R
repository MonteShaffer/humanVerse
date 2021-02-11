#' rgb2col
#'
#' Reverse the built-in grDevices::col2rgb function
#' [ See grDevices::convertColor or grDevices::make.rgb ]
#'
#' @param x vector of colors
#'
#' @return vector of colors in RGB hex format
#' @export
#'
#' @examples
#'
#' rgb2col( col2rgb("red") );
#' rgb2col( col2rgb("red", alpha=TRUE) );
#' rgb2col( col2rgb("#FF0000FF", alpha=TRUE) );
#' rgb2col( col2rgb("#FF000033", alpha=TRUE) );
#'
rgb2col = function(x)
  {
  # reverses col2rgb function
  x.n = dim(x)[1];
  if(x.n == 4)
    {
    x.rgb = t(x[1:4,]) /255;
    grDevices::rgb(   as.numeric(x.rgb[,1]),
                      as.numeric(x.rgb[,2]),
                      as.numeric(x.rgb[,3]),
                      as.numeric(x.rgb[,4]),
      names=rownames(x.rgb) );
    } else {
            x.rgb = t(x[1:3,]) /255;
            grDevices::rgb( x.rgb, names=rownames(x.rgb) );
            }
  }


#' colorsInGradient
#'
#' @param n Number of colors to return
#' @param colvec Vector of color names "red" or RGB "#FF0000" or RGBa "#4169E1FF"
#' @param alpha Pass transparency filter "alpha" as TRUE or FALSE
#'
#' @return vector of colors in RGB or RGBa form (depending on alpha)
#' @export
#'
#' @examples
#'
#' colorsInGradient(4, c("red", "royalblue"));
#' colorsInGradient(4, c("#FF000000", "#FF0000FF"), TRUE);  # red through alphas
#' colorsInGradient(4, c("#FF000000", "#4169E1FF"), TRUE);
#'
colorsInGradient = function(n, colvec=c("red","royalblue"), alpha=FALSE)
  {
  # colorsInGradient(4, c("red", "royalblue"));
  # colorsInGradient(4, c("#FF000000", "#FF0000FF"), TRUE);  # red through alphas
  # colorsInGradient(4, c("#FF000000", "#4169E1FF"), TRUE);  # red->royalblue through alphas
  # rgb2col( col2rgb("royalblue") );

  # alpha doesn't seem to work as expected ... unless I pass in RGBa?
  grDevices::colorRampPalette(colvec, alpha=alpha)(n);
  }










