
########################################
##  Other data manipulation routines
########################################

#from : https://github.com/bobthecat/codebox/blob/master/list2DF.r
util$listOfVec2DF <- function(list){
  l <- as.vector(unlist(lapply(list, length)))
  if(length(unique(l))>1) stop("Elements in vectors are not same length. Cannot transform to data.frame")
  df <- as.data.frame(matrix(as.vector(unlist(list)), ncol=l[1]))
  return(df)
}
attr(util$listOfVec2DF, "help") <- "Transform a list of numerical vectors of the same length to a data.frame."

util$merge.lists <- function(x,y,only.new.y=FALSE,append=FALSE,...) {
  # http://tolstoy.newcastle.edu.au/R/devel/04/11/1469.html
  out=x
  ystructure = names(c(y,recursive=TRUE))
  xstructure = names(c(x,recursive=TRUE))
  yunique = ystructure[! ystructure %in% xstructure]

  ystructure = sapply(ystructure,FUN=function(element)   strsplit(element,"\\."))
  xstructure = sapply(xstructure,FUN=function(element)   strsplit(element,"\\."))
  yunique = sapply(yunique,FUN=function(element) strsplit(element,"\\."))

  if (only.new.y)
    lapply(yunique, FUN=function(index) out[[index]]<<-y[[index]])
  else {
    if (!append) {
      lapply(ystructure, FUN=function(index) out[[index]]<<-y[[index]])
    }
    else lapply(ystructure, FUN=function(index) out[[index]]<<-c(out[[index]],y[[index]]))
  }
  return(out)
}
attr(util$merge.lists, "help") <- "several ways to 'merge' two different lists. Removes duplicates (append, only.new.y"


util$tapply2 <- function(x, ...) {
  if (is.factor(x)) {
    r = factor(tapply(as.character(x), ...), levels=levels(x))
  } else {
    r = tapply(x, ...)
  }
  r
}
attr(util$tapply2, "help") <- "like tapply(), but preserves factors."


util$inject <- function(collection, start, fn) {
  # like lisp reduce.  (named after ruby)
  acc = start
  for (x in collection)
    acc = fn(acc, x)
  acc
}
attr(util$inject, "help") <- "Iteratively apply a function on elements of a collection. like lisp reduce.  (named after ruby)"

util$xprod <- function(xs,ys) {
  # Set cross-product
  ret = list()
  i=0
  for (x in xs)  for (y in ys) {
    i = i+1
    ret[[i]] = list(x=x,y=y)
  }
  ret
}


util$multi_xprod <- function(args) {
  # Set cross-product
  pair_xprod <- function(xs,ys) {
    ret = list()
    i=0
    for (x in xs)  for (y in ys) {
      i = i+1
      ret[[i]] = c(x,y)
    }
    ret
  }
  ret = list(NA)
  for (i in 1:length(args)) {
    ret = pair_xprod(ret, args[[i]])
  }
  lapply(ret, function(x)  x[2:length(x)])
}
