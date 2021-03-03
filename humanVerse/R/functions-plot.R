
# https://onertipaday.blogspot.com/2007/04/highlight-overlapping-area-between-two.html
# This "shades" between two lines or other data, specificy a color in ...
plot.polygonsBetweenTwoData = function(x1, y1, x2, y2, ...)
  {
  polygon (  x = c(x1, rev(x2), x1[1]),  # x1, reverse on x2, and reconnect to first point of x1
             y = c(y1, rev(y2), y1[1]),
            ...
          );
  }


# plot.buildAxis ... axes=F in "plot" ...
# axis(1, lwd.tick=0, labels=FALSE)


