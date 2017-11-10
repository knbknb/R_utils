library(ggplot2)
library(reshape2)
library(scales) #pretty_breaks
library(gridExtra) # several plots on one page

knbknb <- new.env()

######################################### 
# functions below added by knb 2011-2017+

knbknb$save.xlsx <- function (file, ...)
{
  require(xlsx, quietly = TRUE)
  objects <- list(...)
  fargs <- as.list(match.call(expand.dots = TRUE))
  objnames <- as.character(fargs)[-c(1, 2)]
  nobjects <- length(objects)
  for (i in 1:nobjects) {
    if (i == 1)
      write.xlsx(objects[[i]], file, sheetName = objnames[i])
    else write.xlsx(objects[[i]], file, sheetName = objnames[i],
                    append = TRUE)
  }
  print(paste("Workbook", file, "has", nobjects, "worksheets."))
}


# knbknb$ggtheme <- function (){
#         theme(panel.background =        element_rect(fill=        "#FFFFFF"),
#               panel.grid.major.x = element_blank(),
#               panel.grid.major.y = element_line(colour= "grey",size=0.1),
#               panel.grid.minor   = element_line(colour="grey",size=0.1))
# }

knbknb$sysenv_search <- function(pat="."){
  grep(pat, names(Sys.getenv()),perl=TRUE, value=TRUE)
}
attr(knbknb$sysenv_search, "help") <- "Perform a simple grep-search on environment variables, return keys only"



knbknb$sysenv_get <- function(pat="."){
  varnames <- grep(pat, names(Sys.getenv()),perl=TRUE, value=TRUE)
  Sys.getenv(varnames)
}
attr(knbknb$sysenv_get, "help") <- "Perform a simple grep-search on environment variables, return keys _and_ values"

knbknb$tryCatch.W.E <- function(expr){
  W <- NULL
  w.handler <- function(w){ # warning handler
    W <<- w
    invokeRestart("muffleWarning")
  }
  list(value = withCallingHandlers(
    tryCatch(expr, error = function(e) e),
    warning = w.handler),
    warning = W)
}
attr(knbknb$tryCatch.W.E, "help") <- "from demo(error.catching): 1) catch all errors and warnings (and continue), 2) store the error or warning messages."



knbknb$unshorten_url <- function(uri, timeoutsecs=5){
  if(require(RCurl)){
    uri <- as.character(uri)
    if(RCurl::url.exists(uri)){
      # from listCurlOptions()
      #do not stop if requests time out
      resolved_list <- tryCatch.W.E (eval({
        opts <- list(
          timeout = timeoutsecs,
          maxredirs = 5,
          followlocation = TRUE,  # resolve redirects
          ssl.verifyhost = FALSE, # suppress certain SSL errors
          ssl.verifypeer = FALSE,
          nobody = TRUE, # perform HEAD request - not guaranteed to return something
          verbose = FALSE
        )
        curlhandle = getCurlHandle(.opts = opts)
        getURL(uri, curl = curlhandle)
        info <- getCurlInfo(curlhandle)
        rm(curlhandle)  # release the curlhandle!
        as.character(info$effective.url)
      }))
      return(resolved_list)
      #                 resolved <- ! is.null(resolved_list[["warning"]])  &
      #                                 ! is.null(resolved_list[["value"]][["message"]])
      #                 if(resolved){
      #                         resolved_list[["value"]]
      #                 } else {
      #                         #mesg <- resolved_list["warning"]["message"]
      #                         warning(paste("cannot resolve url:", uri, "\n"))
      #                         uri
      #                 }
    } else {
      # just return the url as-is
      warning(paste0("url invalid, or 404 error: ", uri, "\n"))
      uri
    }
  } else {
    # just return the url as-is
    printf(paste0("cannot load package RCurl, returning as-is:", uri, "\n"))
    uri
  }
}
attr(knbknb$unshorten_url, "help") <- "Resolve an anonymous URL given by an URL shortener"

knbknb$extractURL<-function(x, s=".") {
  if(grepl(pattern = s,x = x, perl=TRUE, ignore.case = TRUE)){
    m <- gregexpr('https?\\S+',x, perl = TRUE, ignore.case = TRUE)
    return(regmatches(x, m))
  }
  x
}

knbknb$removeURL <- function(x) gsub('https?://\\S+'," ",x, perl = TRUE)

knbknb$removeStrangeMarkup <- function(x) {
  # remove ed><a0><bc><ed><be><89 and similar
  x <- gsub('(?:<\\w\\w>)+',"",x, perl = TRUE)
  x <- gsub('(?:\\w\\w><\\w\\w) ?',"",x, perl = TRUE)
  x
}

knbknb$removeFirstChars <- function(x, pat="^#|^@|\\s#|\\s@") {
  # remove #hashmarks and @mentions
  gsub(pat," ",x, perl = TRUE)
}

knbknb$'%nin%' <- Negate('%in%')

knbknb$skipn <- function(fn, marker="*/"){
  con <- file(zf,open="r")
  lines <- readLines(con)
  skipn <- match(marker, lines) #gets the row index of the close comment
  skipn
}

#knbknb$h", utils::head, env=.startup)
#knbknb$n", base::names, env=.startup)
# same as my bash function i()
knbknb$ht <- function(d) rbind(head(d,6),tail(d,6))
knbknb$s <-function()  base::summary
knbknb$pwd <-function() base::getwd
#knbknb$cd <-function() base::setwd
#knbknb$last <- function(x) { rbind(tail(x, n = 1)) }, env=.startup)
knbknb$pkgs <- function(){as.data.frame(installed.packages()[,c(1,3)],row.names=F)}
knbknb$ucfirst <- function (str) {  minlen = 3; paste(sapply(strsplit(as.character(str), '\\s', NULL), FUN=function(str){ifelse(nchar(str) > (minlen-1), paste(toupper(substring(str, 1, 1)), tolower(substring(str, 2)), sep = ""), str)}), collapse = " ")}
#knbknb$describe <- function(obj) {attr(obj, "help")}, env=.startup)

# alias to clear console. see  http://stackoverflow.com/questions/14260340/function-to-clear-the-console-in-r. CTRL-L also works.
knbknb$cls <- function(){cat("\014")}
# Override q() to not save by default.
# Same as saying q("no")
#knbknb$q <- function (save="no", ...) { quit(save=save, ...) }, env=.startup)

knbknb$datasets <- function(){ data(package = .packages(all.available = TRUE))}
knbknb$download <- function(){ 
  download.file(url,destfile=filename,method="curl", extra="-L")
}
attr(knbknb$download, "help") <- "download a file via https, follow redirects"

knbknb$v <- function() {R.version$version.string}

knbknb$readXlsx <- function(fn){

        data <- knbknb$readFile(fn)
        data

}

knbknb$cleanXlsx <- function(data, badcols){

        #data[is.na(data)] <- 0

        data <- knbknb$removeBadCols(data, badcols)
        datecol <- which(names(data) %in% c("LOG_DATE"))
        data[,datecol] <- ymd_hms(data[,datecol], tz = "GMT")
        data
}


# write to pdf. useful when many X11 devices are open
knbknb$writePDF <- function(d,fn, onefile = TRUE){
        pdf1 <- file.path(d,fn)
        print("Abspath PDF Outfile:")
        print(pdf1)

        dev.copy2pdf(device = x11, file= ifelse(onefile, pdf1, paste0(pdf1, "%03d" , ".pdf")))
        #return(pdf1)

}

############# func defs st come before func body

knbknb$readFile <- function(fn){
        #http://cran.r-project.org/web/packages/openxlsx/vignettes/formatting.pdf
        # errmsg <- sprintf("Cannot read data file '%s'.", fn)
        getOption("openxlsx.datetimeFormat", "yyyy/mm/dd hh:mm:ss")
        data <-  read.xlsx(fn, sheet=1, colNames = TRUE)
        return(data)
}

knbknb$removeBadCols <- function(data, badcol=c("EXPEDITION","SITE", "n")){
        print(contents(data))
        print(data[1:2,])
        #if( as.logical("EXPEDITION" %in% names(data))){
        badcolidx <- which(names(data) %in% badcol)
        print(sprintf("badcol: '%s'", badcol))
        tryCatch({
                data <- data[, -badcolidx]
        }, error=function(e){})
        data
        #}
}

## is not robust enough
knbknb$plotFile <- function(data, wd, convertstr=FALSE, fn="ketzindata.pdf"){
        # begin processing data
        names(data) <- make.names(names(data))
        #print(contents(data))
        # almost properly convert excel date to datetime
        # fct from openxlsx
        #data2[,1] <- convertToDate(data[,1])

        # convert wrongly parsed characters to numerics
        data[1:2,]
        (classes <- sapply(data[2:5, ], class))
        clsidx <- grep("character", classes)

        #sapply(dataliste[], function(x)gsub(",", ".", x)
        #plot(data[,1], data[,25])
        #data[,clsidx] <- gsub("n.a.", NA, data[,clsidx])
        #data[,clsidx] <- as.data.frame(lapply(data[,clsidx],function(x) if(is.character(x)|is.factor(x)) gsub("n.a|''",0,x) else x))
        #data[,clsidx] <- as.data.frame(data[,clsidx], stringsAsFactors = TRUE)
        data2 = data
        if(convertstr == TRUE){
                for (i in clsidx[1:(length(clsidx))]){
                        data2[,clsidx[i]] <- as.numeric( data[,clsidx[i]])
                        #print(i)
                }
        }

        data2[is.na(data2)] <- 0

        dim(data2)
        #Remove empty columns
        #data2 <- Filter(function(x) {! all(is.na(x), data)}, data2)
        #after
        #dim(data)

        # set remaining NAs to 0
        #data[is.na(data)] <- 0
        #names(data)
        #
        #end <- length(names(data))
        #end
        #head(data[,c(1,end)])

        #par(mfrow=c(1,1))
        # plots not inside RStudio
        options(device = "X11")
        #options(device = "RStudioGD")
        #options(device = "pdf")
        dev.new()
        par(mfrow = c(1,1))
        #par(mfrow = c(3,3))
        #data <- data2
        #rm(data2)
        for (x in names(data)){
                tryCatch({
                        xx <- ifelse(!is.na(x),x,"???") #, xlab=as.character(year(data[1,1]))
                        #plot( data[,1], data[,x], ylab="", xlim=c(ymd("2008-01-01"),ymd(today())), ylim=c(0,1000), ymin=0, ymax=1000 ,type="l", main=xx)
                        qplot(  data[,1], data[,x], ymin=0, ymax=1000 , main=xx) #, ylab="", geom="l",
                }, error=function(e){print(paste0("error ", str(e)))}, finally=print(paste0("done col:", xx)))
        }
        outf <- fn
        knbknb$writePDF (wd,outf)
        #dev.off()

}
knbknb$wrapString <-  function(vector_of_strings,width){
        #http://stackoverflow.com/questions/7367138/text-wrap-for-plot-titles-in-r
        sapply(vector_of_strings,FUN=function(x){paste (strwrap(x,width=width), collapse="\n")
                                                 })
}

knbknb$findYearStr <- function(v){
        # example:
        # return "2012 - 2013" if min and max are different,
        # return "2012" if they aren't
        mi <- min(year(v))
        ma <- max(year(v))
        if(mi == ma){
                mi
        } else {
                paste0(mi, " - ",  ma)
        }

}

knbknb$ggplotFile <- function(data, fn=""){
        errmsg <- sprintf("Cannot plot data file '%s'.", fn)
        data2 <- melt(data,id.vars = names(data)[1], variable.names=names(data)[-1])
        p <- list()
        #first column shows x-axis
        for (x in names(data)[-1]){

                tryCatch({
                        #xx <- ifelse(!is.na(x),x,0) #, xlab=as.character(year(data[1,1]))
                        #plot( data[,1], data[,x], ylab="", xlim=c(ymd("2008-01-01"),ymd(today())), ylim=c(0,1000), ymin=0, ymax=1000 ,type="l", main=xx)
                        #qplot(  data[,1], data[,x], ymin=0, ymax=1000 , main=xx) #, ylab="", geom="l",
                        #scale_y_continuous(breaks = round(seq(min(data3$x), max(dat$x), by = 0.5),1)) +
                        #x = "KTZI201_WF_DRUCK"
                        data3 <- data2[data2$variable == x,]

                        #data3["n.a." ==  data3$value,"value"] <- NA
                        data3$value <- as.numeric(data3$value)
                        xlabel <- knbknb$findYearStr(data3[,1])
                        ptitle <- paste0(x, "\n", knbknb$wrapString(fn, nchar(fn)/5))
                        pbr <- pretty_breaks(n=10)
                        if(! all(is.na(data3$value))){
                                p[x] <-  list(ggplot(data = data3, aes(x = LOG_DATE, y = value)) +
                                                      geom_point(alpha = 1/3, na.rm = TRUE)  +
                                         labs(title = ptitle) +
                                         labs(x = xlabel) +
                                         theme_bw(base_family = "", base_size = 10) +
                                                scale_y_continuous(breaks = pbr) +

                                        facet_grid(variable ~ .))
                        }
                }, error=function(e){print(paste0("error ", str(e))); stop()},
                finally=print(paste0("done col:", x)))
        }
        #unique(data3$variable)
        p["nrow"] = length(p)
        p


}

# Here is a simple R function that will find time series outliers
# (and optionally show them in a plot).
# It will handle seasonal and non-seasonal time series.
# by Rob Hyndman ()
# https://stats.stackexchange.com/a/1153/20107
knbknb$tsoutliers <- function(x,plot=FALSE)
{
        x <- as.ts(x)
        if(frequency(x)>1)
                resid <- stl(x,s.window="periodic",robust=TRUE)$time.series[,3]
        else
        {
                tt <- 1:length(x)
                resid <- residuals(loess(x ~ tt))
        }
        resid.q <- quantile(resid,prob=c(0.25,0.75))
        iqr <- diff(resid.q)
        limits <- resid.q + 1.5*iqr*c(-1,1)
        score <- abs(pmin((resid-limits[1])/iqr,0) + pmax((resid - limits[2])/iqr,0))
        if(plot)
        {
                plot(x)
                x2 <- ts(rep(NA,length(x)))
                x2[score>0] <- x[score>0]
                tsp(x2) <- tsp(x)
                points(x2,pch=19,col="red")
                return(invisible(score))
        }
        else
                return(score)
}


knbknb$download_13fs <-  function(cik = "0001079114"){
  all_13FS_overview <- sprintf('https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=%s&type=13F-HR&dateb=&owner=include&count=400', cik)
  all_13FS_overview_html <- read_html(all_13FS_overview)
  all_13FS_overview_urls <- html_attr(html_nodes(all_13FS_overview_html, "#documentsbutton"), "href")
  all_13FS_overview_urls <- paste0("https://www.sec.gov", all_13FS_overview_urls)
  all_13FS_overview_urls_html <- map(all_13FS_overview_urls, read_html)
  all_13FS_overview_urls_html
  
}

knbknb$filingdates_of_13fs <- function(cik = "0001079114"){
  all_13FS_overview <- sprintf('https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=%s&type=13F-HR&dateb=&owner=include&count=400', cik)
  all_13FS_overview_html <- read_html(all_13FS_overview)
  all_13FS_overview_years <- html_table(all_13FS_overview_html)[[3]][, c(3,4)]
  all_13FS_overview_years[,1] <- gsub("-", "", stri_extract_last_regex(str = all_13FS_overview_years[,1],
                                                                       pattern="\\d+-\\d+-\\d+"))
  all_13FS_overview_years
  
}

knbknb$persistent_obj <- function(outdir = ".", outfile = ""){
  outfile <- paste0(outdir, outfile)
  dir.create(outdir, showWarnings = FALSE)
  
  all_13FS_overview_urls_html <- download_13fs(outfile)
  all_13FS_overview_urls_html_chr <- map(all_13FS_overview_urls_html,
                                         as, "character")
  
  saveRDS(object = all_13FS_overview_urls_html_chr, file=outfile)
  outfile
}




########################################
## Has to be last in file
while("knbknb" %in% search())
        detach("knbknb")
attach(knbknb)
