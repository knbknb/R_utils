# ## Knut Behrends .Rprofile

## If you ever delete all objects in your global environment,
## then the aliases above will also be deleted.
## You can prevent that by hiding these in an environment
.startup <- new.env()
attach(.startup)

options(papersize = "a4")
# options(showWarnCalls=TRUE, showErrorCalls=TRUE)
options(repos = c(CRAN = "https://cloud.r-project.org"),#
        "pdfviewer" = "evince",
        prompt = paste0("R> "),
        digits = 6L,
        digits.secs = 6L,
        show.signif.stars = TRUE,
        mc.cores = 4L,
        NCpus = 4,
        useFancyQuotes = FALSE,
        width = 200, # terminal width
        jags.moddir = "/usr/lib/x86_64-linux-gnu/JAGS/modules-4/",
        shiny.reactlog = TRUE) # graphical representation. in the app, press CTRL-F3
#
# https://rud.is/b/2020/01/03/writing-frictionless-r-package-wrappers-building-a-basic-r-package/
options(
  usethis.description = list(
     `Authors@R` = 'person("Knut", "Behrends", email = "knb@tnp-online.de", role = c("aut", "cre"),
    comment = c(ORCID = "https://orcid.org/0000-0003-0594-2892"))',
    License = "MIT + file LICENSE"
   )
  )
#
#Sys.setenv(TZ="Europe/Berlin")
Sys.setenv(R_HISTSIZE = "100000")
# If no R_HISTFILE environment variable, set default
#if (Sys.getenv("R_HISTFILE") == "") {
Sys.setenv(R_HISTFILE = file.path(Sys.getenv("HOME"), ".Rhistory"))

#Sys.setenv(RETICULATE_PYTHON = "/usr/bin/python3")

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

#.libPaths(c(.libPaths(), "some_path"))
if (requireNamespace("magrittr", quietly = TRUE)) {
  `%>%` <- magrittr::`%>%`
}

tryCatch({
        if (interactive()) {

          options(menu.graphics = FALSE)

          if (is.na(Sys.getenv("RSTUDIO", NA)))
            options(
              setWidthOnResize = TRUE
          )

          if( requireNamespace("colorout", quietly = TRUE) ){
            options(colorout.anyterm = TRUE)  # Colorize R output in terminal
          }

          if( requireNamespace("dang", quietly = TRUE) ){
            message(unlist(ifelse(stats::runif(1) > 0.5, dang::demotivate(), dang::motivate())))
          }
          if( requireNamespace("prompt", quietly = TRUE) ){
            prompt::set_prompt(prompt::prompt_git)

            if(dang::getGitRoot() == ""){
              options(prompt = "R> ")
            }
          }
          q <- function(save="no", ...) {
            quit(save = save, ...)
          }
        } else { # not interactive
          # warn on partial matches
          options(
            warnPartialMatchArgs = TRUE,
            warnPartialMatchAttr = TRUE,
            warnPartialMatchDollar = TRUE
          )

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

# aliases and a few utility functions
{
  if(dir.exists("/home/knb/code/git/_my/R_utils/")){
    sourcefiles <- c(
      # "/home/knb/code/git/_my/R_utils/util.R",
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

}

