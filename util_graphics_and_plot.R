
########################################
## Graphics output wrappers
## For easy one-liners, like:
## dopdf("tmp.pdf",width=5,height=5,cmd=plot(x,y))

util$dopdf <- function(filename,..., cmd) {
  pdf(filename, ...)
  eval(cmd)
  dev.off()
  if (exists('OPEN') && OPEN)
    system(sprintf("open %s", filename))
}
attr(util$dopdf, "help") <- "Command: dopdf('tmp.pdf',width=5,height=5,cmd=plot(x,y))"

util$dopng <- function(filename,..., cmd) {
  png(filename, ...)
  par(mar = c(0,0,0,10))
  eval(cmd)
  dev.off()
  if ((exists('OPEN') && OPEN))
    system(sprintf("open %s", filename))
}

util$dosink <- function(filename,cmd, open=NULL) {
  # like capture.output() but follows open/OPEN conventions here
  sink(filename)
  eval(cmd)
  sink(NULL)
  if (?coalesce(open, exists('OPEN') && OPEN))
    system(sprintf("open %s", filename))
}

util$dosvg <- function(filename, ..., cmd, open=NULL) {
  library("RSvgDevice")
  devSVG(filename, ...)
  eval(cmd)
  dev.off()
  if (prio_check(open, exists('OPEN') && OPEN))
    system(sprintf("open %s", filename))
}


########################################
## Base R Plotting routines

util$linelight <- function(x,y, lty='dashed', col='lightgray', ...) {
  # highlight a point with lines running to the axes.
  left = par('usr')[1]
  bot = par('usr')[3]
  segments(left,y, x,y, lty=lty, col=col, ...)
  segments(x,bot,  x,y, lty=lty, col=col, ...)
}
attr(util$linelight, "help") <- "Highlight point(s) in a plot with dashed lines running to the x,y-axes."

util$hintonplot <- function(mat, max_value=max(abs(mat)), mid_value=0, ...) {
  # Plots a matrix/dataframe/table as colored, size-varying boxes
  # I dunno who started calling this a "Hinton plot", but anyways

  # Example:
  # hintonplot(matrix(rnorm(100),10))

  # Example, for counts:
  # table(cyl=mtcars$cyl, mpg=cut(mtcars$mpg,3))
  #    mpg
  # cyl (10.4,18.2] (18.2,26.1] (26.1,33.9]
  #   4           0           6           5
  #   6           2           5           0
  #   8          12           2           0
  # hintonplot(table(cyl=mtcars$cyl, mpg=cut(mtcars$mpg,3)))

  plot.new()
  plot.window(xlim=c(0.5,ncol(mat)+0.5), ylim=c(0.5,nrow(mat)+0.5))

  x_mid = 1:ncol(mat)
  y_mid = 1:nrow(mat)

  area = abs(mat) / max_value
  side = sqrt(area)

  for (x in 1:ncol(mat)) {
    for (y in nrow(mat):1) {
      # ym = (nrow(mat):1)[y]
      ym = y
      d = side[ym,x] / 2
      rect(x-d, y-d, x+d, y+d, col=if (mat[ym,x]>0) 'darkblue' else 'darkred')
    }
  }

  axis(1, 1:ncol(mat), labels=colnames(mat))
  # axis(2, nrow(mat):1, labels=row.names(mat))
  axis(2, 1:nrow(mat), labels=row.names(mat))
  title(xlab=names(dimnames(mat))[2], ylab=names(dimnames(mat))[1], ...)
}
attr(util$hintonplot, "help") <- " Plots a matrix/dataframe/table as colored, size-varying boxes (simpler than a heatmap)"