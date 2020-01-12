
util_knb <- new.env()
util_knb$describe <- function(obj) attr(obj, "help")
#########################################
# functions below added by knb 2011-2017+
# call these aliases as functions.
#
# call pseudo-docstrings like this (example)
# describe(all_datasets)


util_knb$all_datasets <- function() data(package = .packages(all.available = TRUE))
attr(util_knb$all_datasets, "help") <- "all_datasets(): Find data sets available in your R installation"

util_knb$sysenv_search <- function(pat = ".") {
  grep(pat, names(Sys.getenv()), perl = TRUE, value = TRUE)
}
attr(util_knb$sysenv_search, "help") <- "sysenv_search(): Perform a simple grep-search on environment variables, return keys only"



util_knb$sysenv_get <- function(pat = ".") {
  varnames <- grep(pat, names(Sys.getenv()), perl = TRUE, value = TRUE)
  Sys.getenv(varnames)
}
attr(util_knb$sysenv_get, "help") <- "sysenv_get(): Perform a simple grep-search on environment variables, return keys _and_ values"

util_knb$tryCatch.W.E <- function(expr) {
  W <- NULL
  w.handler <- function(w) { # warning handler
    W <<- w
    invokeRestart("muffleWarning")
  }
  list(
    value = withCallingHandlers(
      tryCatch(expr, error = function(e) e),
      warning = w.handler
    ),
    warning = W
  )
}
attr(util_knb$tryCatch.W.E, "help") <- "tryCatch.W.E(): tryCatch.Warning.Error: From demo(error.catching): 1) catch all errors and warnings (and continue), 2) store BOTH the error and/or warning messages."


util_knb$'%nin%' <- Negate('%in%')

util_knb$ht <- function(d) rbind(head(d, 6), tail(d, 6))
util_knb$s <- function() base::summary
util_knb$pwd <- function() base::getwd
util_knb$pkgs <- function() {
  as.data.frame(installed.packages()[, c(1, 3)], row.names = FALSE)
}

# see  http://stackoverflow.com/questions/14260340/function-to-clear-the-console-in-r.
util_knb$cls <- function() {
  cat("\014")
}
attr(util_knb$cls, "help") <- "cls(): alias to clear Rstudio console. CTRL-L also works."

util_knb$v <- function() {
  R.version$version.string
}
attr(util_knb$v, "help") <- "v(): R version string"

########################################
## Has to be last in file
while("util_knb" %in% search())
        detach("util_knb")
attach(util_knb)
