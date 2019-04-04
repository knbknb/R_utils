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
options(jags.moddir = "/usr/lib/x86_64-linux-gnu/JAGS/modules-4/")

# graphical representation. in the app, press CTRL-F3
options(shiny.reactlog = TRUE)

# (needs processx package)
# whenever you edit and save a source file,
# Hugo will automatically navigate to the page
# corresponding to this file,
# no matter which page you are currently on.
# Note: if you see a 404 error in RStudio Viewer,
# just refresh the viewer.
options(servr.daemon = TRUE,
        blogdown.author = "Knut Behrends",
        blogdown.generator.server = TRUE,
        blogdown.hugo.server = c('-D', '-F', '--navigateToChanged'),
        blogdown.title_case = TRUE)

# options(tibble.width = Inf)
# options(tibble.print_max = Inf)

# will turn on completion in library and require calls
# so that when I type library(su and hit tab, it becomes library(survival.
#utils::rc.settings(ipck=TRUE)
#.libPaths(c(.libPaths(), "~/R/x86_64-pc-linux-gnu-library/3.5"))

tryCatch({
        if (interactive()) {
                library(colorout) # Colorize R output in terminal
                options(colorout.anyterm = TRUE)
                q <- function(save="no", ...) {
                  quit(save = save, ...)
                }
        }
        #library(rJava)
        #.jinit(parameters="-Xmx8g")
        # Sets the default number of threads to use
        # options(mc.cores=parallel::detectCores())
}) #end try

# Set proxy=privoxy for all web requests
# Sys.setenv(http_proxy = "http://127.0.0.1:8118/")
# Sys.setenv(LD_LIBRARY_PATH = paste0(Sys.getenv("LD_LIBRARY_PATH"), ":", "/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/amd64/server/"))#:/usr/lib/R/site-library/rJava/jri/:/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/amd64/server:/usr/lib/R/lib

setHook(
  packageEvent("grDevices", "onLoad"),
  function(...) grDevices::ps.options(horizontal = FALSE)
)
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
  tryCatch(source(fn),
    error = function(e) {
      message(sprintf("Startup: Cannot source '%s':\n%s", fn, conditionMessage(e)))
      #rm(fn)
    }
  )
 }
 rm(sourcefiles)
}

# these messages can interfere with the package installation mechanism
#.First <- function() cat(paste0("\n #  Useful aliases are in env '.startup', Functions in env 'util'.\n\n"))
#.Last <- function() cat("\n   Goodbye!\n\n")
