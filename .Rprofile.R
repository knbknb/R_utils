# ## Knut Behrends .Rprofile

# aliases
## If you ever delete all objects in your global environment,
## then the aliases above will also be deleted.
## You can prevent that by hiding these in an environment
.startup <- new.env()
attach(.startup)

options(papersize = "a4")
# options(showWarnCalls=TRUE, showErrorCalls=TRUE)
options(repos = c(CRAN = "https://cloud.r-project.org"))
options("pdfviewer" = "evince")
options(prompt = paste0("R> "), digits = 4, show.signif.stars = TRUE)
options(width = 120)

# options(tibble.width = Inf)
# options(tibble.print_max = Inf)
# options("max.print" = 600) # does this work?
# options(width = 65, digits = 5)
# options(show.signif.stars = FALSE)

# will turn on completion in library and require calls
# so that when I type library(su and hit tab, it becomes library(survival.
#utils::rc.settings(ipck=TRUE)
#.libPaths(c(.libPaths(), "~/R/x86_64-pc-linux-gnu-library/3.3"))
#.libPaths(c("~/R/x86_64-pc-linux-gnu-library/3.3"))
tryCatch({
        if (interactive()) {
                #library(lookup)
                library(colorout) # Colorize R output in terminal
                library(tibble) # for rstudio clipboard
                library(clipr) # for rstudio clipboard
                library(knitr) # important for rstudio
                options(colorout.anyterm = TRUE)
                q <- function(save="no", ...) {
                  quit(save = save, ...)
                }
                #attr(knbknb$q, "help") <- "Override q() to not save by default. Same as saying q('no')"
                
        }
        #options(rstudio.markdownToHTML =
        #  function(inputFile, outputFile) {
        #        require(markdown)
        #        css=paste0(Sys.getenv("HOME"),'.Rcustom.css')
        #        markdownToHTML(inputFile, outputFile, stylesheet=css)
        #  }
        #)
        # library(rj) #  RJava, for Eclipse
        # http://stackoverflow.com/questions/19024873/why-does-r-hang-when-using-ngramtokenizer?rq=1
        # library(rJava)
        # .jinit(parameters="-Xmx4g")
        # Sets the default number of threads to use
        # options(mc.cores=1)

}) #end try

# Set proxy=privoxy for all web requests
# Sys.setenv(http_proxy = "http://127.0.0.1:8118/")
# Sys.setenv(LD_LIBRARY_PATH = paste0(Sys.getenv("LD_LIBRARY_PATH"), ":", "/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/amd64/server/"))#:/usr/lib/R/site-library/rJava/jri/:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/amd64/server:/usr/lib/R/lib

setHook(
  packageEvent("grDevices", "onLoad"),
  function(...) grDevices::ps.options(horizontal = FALSE)
)
# set.seed(874)
Sys.setenv(R_HISTSIZE = "100000")
# If no R_HISTFILE environment variable, set default
#if (Sys.getenv("R_HISTFILE") == "") {
Sys.setenv(R_HISTFILE = file.path("/home/knb/.Rhistory"))
#}

{
  sourcefiles <- c(
    "/home/knb/code/git/_my/R_utils/util.R",
    "/home/knb/code/git/_my/R_utils/util_knb.R",
    "/home/knb/code/git/_my/R_utils/googleapiUtils.R"
  )
  for (fn in sourcefiles) {
    if (file.exists(fn)) {
      source(fn)
    }
  }
  rm(fn)
  rm(sourcefiles)
}



# these messages can interfere with the package installation mechanism
#.First <- function() cat(paste0("\n #  Useful aliases are in env '.startup', Functions in env 'util'.\n\n"))
#.Last <- function() cat("\n   Goodbye!\n\n")