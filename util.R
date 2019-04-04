# util.R:
# Utilities to make R a happier place
# Brendan O'Connor, brenocon.com/code - see bottom of file

# call "docstrings" with describe(), e,g, describe(read.tsv)
# describe() is also mentioned in this file

########################################
## Put everything into an environment, to not pollute global namespace by default

util = new.env()

#util$describe <- function(obj) attr(obj, "help")

########################################
##  Misc small routines

## A helper function that tests whether an object is either NULL _or_
## a list of NULLs
util$is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))

## Recursively step down into list, removing all such objects
util$rmNullObs <- function(x) {
        x <- Filter(Negate(is.NullOb), x)
        lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
}

util$as.c <- as.character


util$nna <- function(...) !is.na(...)   # i type this a lot, i think its worth 3 characters + shift key



util$rbern <- function(n, p=0.5)  rbinom(n, size=1, prob=p)

util$boot_binom <- function(n, p)   rbinom(1,n,p)/n

util$shuffle <- function(...) UseMethod("shuffle")

util$shuffle.default <- function(x)  x[order(runif(length(x)))]

util$shuffle.data.frame <- function(x)  x[order(runif(nrow(x))),]

util$sample_df <- function(d, size=10, ...)  {
  samp = sample(1:nrow(d), size=size, ...)
  d[samp,]
}

util$present_levels <- function(x) intersect(levels(x), x)

util$trim_levels <- function(...) UseMethod("trim_levels")

util$trim_levels.factor <- function(x)  factor(x, levels=present_levels(x))

util$trim_levels.data.frame <- function(x) {
  for (n in names(x))
    if (is.factor(x[,n]))
      x[,n] = trim_levels(x[,n])
  x
}


util$coalesce = function(...) {
  vars = list(...)
  for (i in 1:length(vars)) {
    if (!is.null(vars[[i]]) && !is.na(vars[[i]]))
      return(vars[[i]])
  }
  FALSE
}
attr(util$coalesce, "help") <- "coalesce(): return the first value of a vector that is not NA or NULL"


util$grid_points <- function(min=1,max) {
  x = min
  ret = NULL
  while(x <= max) {
    ret = c(ret, x, x*2, x*5)
    x = x * 10
  }
  ret[ret <= max]
}
attr(util$grid_points, "help") <- "grid_points(1, 40) returns 1,2,5,10,20,50 ... kinda-exponential scaling, nice for grid search"

util$age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
        suppressMessages(library(lubridate))
        calc.age = interval(dob, age.day) / duration(num = 1, units = units)
        if (floor) return(as.integer(floor(calc.age)))
        return(calc.age)
}
attr(util$age, "help") <- "age(date_of_birth) returns age of  a person in years. Requires lubridate pkg."


########################################
## Printing, viewing
## see also:  str()
util$printf <- function(...) cat(sprintf(...))

util$msg <- function(...)  cat(..., "\n", file=stderr())


util$ppy <- function(x, column.major=FALSE, ...) {
  library(yaml)
  cat(as.yaml(x, column.major=column.major), ...)
  cat("\n", ...)
}
attr(util$ppy, "help") <- "pretty-print as YAML. Intended for rows with big textual cells. similar to mysql's '\\G' operator"




########################################
##  Workspace management
suppressMessages(require("stringr"))
small_str <- function(x) {
  out = capture.output(
    str(x, max.level=0, give.attr=F, give.head=F, width=info_width, strict.width='cut')
  )
  out = str_c(out,collapse=' ')
  out = cutoff(str_replace(out,"\n"," "))
  if (str_detect(out, "^List of"))
    out = str_c("[Names] $ ", str_c(names(x),collapse=' '))
  cutoff(out)
}

cutoff <- function(s) {
  if (str_length(s) >= info_width) {
    str_c(str_sub(s,1,info_width-2),'..')
  } else {
    s
  }
}


# improved list of objects
# http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
util$list_objects = function (pos = 1, pattern = ".") {


    napply <- function(names, fn) {
      sapply(names, function(x)
        fn(get(x, pos = pos)))
    }
    names <- ls(pos = pos, pattern = pattern)
    obj_class <- napply(names, function(x) as.character(class(x))[1])
    obj_mode <- napply(names, mode)
    obj_type <- ifelse(is.na(obj_class), obj_mode, obj_class)
    obj_prettysize <- napply(names, function(x) {
                           capture.output(print(object.size(x),
                                                units = "auto"))
    })
    obj_size = napply(names, object.size)
    obj_prettysize[obj_size < 1e6] = ""

    obj_length = napply(names, function(x) length(x))
    obj_dim = t(napply(names, function(x)
                        as.numeric(dim(x))[1:2]))

    is_flat = is.na(obj_dim)[, 1]
    is_vector = napply(names, function(x) is.vector(x) & class(x) != 'list')

    info_width = max(20, options('width')$width - 60)



    out <- data.frame(
      Type = obj_type,
      Size = obj_prettysize,
      Dim = ifelse(is_vector | is_flat, obj_length,
        sprintf("(%s, %s)", obj_dim[,1], obj_dim[,2])),
      Value = napply(names, function(x)
        if (class(x) %in% c('data.frame','list') && !is.null(names(x)))
          cutoff(str_c("[Names] $ ",str_c(names(x), collapse=' ')))
        else small_str(x)
        ),
      stringsAsFactors=F)
    row.names(out) = names
    out$Dim = sprintf(" %s", out$Dim)
    out$Value = sprintf(str_c(" %-", info_width, "s"), out$Value)

    out = rbind(subset(out, Type!='function'), subset(out, Type=='function'))
    out
}
attr(util$list_objects, "help") <- "like ls(), but improved listing of objects as a dataframe."

# shorthand
util$lsos = function(pos=10, ...) {
  d = util$list_objects(...)
  d$name = row.names(d)
  d = subset(d, name != 'util')
  row.names(d)=d$name
  d$name=NULL
  d
}
attr(util$lsos, "help") <- "like ls(), but improved listing of objects as a dataframe."

########################################
## For performance optimization and long-running jobs

util$timeit <- function(expr, name=NULL) {
  # print how long the expression takes, and return its value too.
  # So you can interpose   timeit({ blabla })   around any chunk of code "blabla".
  start = Sys.time()
  ret = eval(expr)
  finish = Sys.time()
  if (!is.null(name)) cat(name,": ")
  print(finish-start)
  invisible(ret)
}

util$dotprogress <- function(callback, interval=10) {
  # intended to wrap the anonymous callback for sapply() or somesuch.
  # ALTERNATIVE: plyr *ply(.progress='text')
  # ALternative: utils::txtProgressBar,
  # see https://ryouready.wordpress.com/2009/03/16/r-monitor-function-progress-with-a-progress-bar/
  count = 0
  return(function(...) {
    if ((count <<- count+1) %% interval == 0)
      cat(".")
    callback(...)
  })
}




########################################
## Has to be last in file.
# After executing this, function is part of global namespace,
# so we do no longer need to fully qualify package name.

while("util" %in% search())
  detach("util")
attach(util)

