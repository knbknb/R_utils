#
# table.graph.r
#
# Created by David Ruau on 2011-06-23.
# Department of Pediatrics/Systems Medicine,
# Stanford University.
#
#
##################### USAGE #########################
# Tufte table-graphic shown in the Tufte book for paired numeric data
# "the visal display of quantitative information" p. 158
#
# Column names and row names will be used to label the plot
# Depending on the length of your rownames the margin might have to be adjusted
#
# df: data frame with 2 column
# line.col: vector length 2 with colors for the lines. Default to grey for
# value going up and black for value going down
# label.cex: magnification for x and y labels from 0 to 1
# title.cex: magnificatoin for titles from 0 to 1
# ...: supplementary arguments supplied to par usually margin
#
# EXAMPLE:
# table.graph(cars, label.cex=0.7, mar=c(5, 5, 1, 5))
#
#####################################################

table.graph <- function(df, line.col=c("grey", "black"), label.cex=1, title.cex=1, ...) {
  xmin <- min(df)
  xmax <- max(df)
  X1 <- as.numeric(as.vector(df[,1]))
  X2 <- as.numeric(as.vector(df[,2]))
  # original settings
  old.par <- par(no.readonly = TRUE)
  # par settings usually margins
  par(...)
  # left
  plot(rep(0, nrow(df)), X1, xlim=c(0,1), ylim=c(xmin, xmax),
    axes=FALSE, xlab='', ylab='', type='n')
  mtext(text=paste(rownames(df), X1, sep=' '), side=2, at=X1, las=1, cex=label.cex)
  par(new=TRUE)
  # right
  plot(rep(1, nrow(df)), X2, xlim=c(0,1), ylim=c(xmin, xmax),
    axes=FALSE, xlab='', ylab='', type='n')
  mtext(text=paste(X2, rownames(df), sep=' '), side=4, at=X2, las=1, cex=label.cex)
  # class label
  mtext(colnames(df)[1], side=3, at=0, cex=title.cex)
  mtext(colnames(df)[2], side=3, at=1, cex=title.cex)
  # lines
  segments(x0 = rep(0, nrow(df)), y0 = X1, x1 = rep(1, nrow(df)), y1 = X2,
   col=ifelse({X1 - X2} < 0, line.col[1], line.col[2]))
  # restore original settings
  par(old.par)
}



#
# slopegraph.r
#
# Created by David Ruau on 2011-07-18.
# 2011 Dept. of Pediatrics/Div. Systems Medicine
# Stanford University.
#
#
##################### USAGE #########################
# data: data.frame in the same shape as the slopegraph is wanted
# label.cex: magnification for numeric line labels from 0 to 1
# axis.cex: magnificatoin for axis titles from 0 to 1
# digits: number of significant digits to report
# rounding.method: can be NULL, round or signif
# ...: supplementary arguments supplied to par, usually margins
#
# EXAMPLE:
# source('slopegraph.r')
# pdf('slopegraph.pdf', height=7, width=8)
# slopegraph(data = t(WorldPhones[,1:3]), mymain = "YEARS", mar=c(2, 5, 5, 5), label.cex=0.8, axis.cex=0.9)
# dev.off()
#
# Tips: when values overlap try first to extend the height of you plot and if this does not work
# round your value using the option rounding.method = 'round' and digits=0
#
#####################################################

slopegraph <- function(data, label.cex=0.8, axis.cex=0.9, digits = 2, rounding.method = NULL, mymain = "slopegraph", ...) {
  require(plotrix)
  if(!is.data.frame(data)){
    data <- as.data.frame(data)
  }
  
  if(!is.null(rounding.method)){
    data.temp <- .rd.method(rounding.method, width, digits)
    data.temp <- as.numeric(sprintf(fmt, as.matrix(data)))
    data <- as.data.frame(matrix(data.temp, nrow=nrow(data), ncol=ncol(data), dimnames=list(rownames(data), colnames(data))))
  }
  
  old.par <- par(no.readonly = TRUE)
  par(...)
  matplot(t(data), type='b', pch=NA, axes=FALSE, xlab='', ylab='', lty='solid', col="grey", ...)
  for(i in 1:ncol(data)){
    for(j in 1:nrow(data)){
      boxed.labels(i, data[j,i], labels=data[j,i], bg='white', border = FALSE, cex=label.cex)
    }
  }
  mtext(text = rownames(data), side = 2, at=data[,1], line = 0.5, las=1, cex=axis.cex)
  mtext(text = colnames(data), side = 3, at=1:ncol(data), line = 1, cex=axis.cex)
  mtext(text = rownames(data), side = 4, at=data[,ncol(data)], line = 0.5, las=1, cex=axis.cex)
  title(main = mymain, line=3)
  par(old.par)
}


.rd.method <- function(rounding.method, width, digits){
  rounding.character <- switch(match(rounding.method, c("round", "signif")), "f", "g")
  fmt = paste("%.", digits, rounding.character, sep = "")
  return(fmt)
}
