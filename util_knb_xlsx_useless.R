
util_knb <- new.env()

#########################################
# functions below added by knb 2011-2017+



# describe(save.xlsx)
util_knb$save.xlsx <- function (file, ...)
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
attr(util_knb$save.xlsx, "help") <- "(filename) -> void // Save objects to several worksheets in an xlsx file."


util_knb$unshorten_url <- function(uri, timeoutsecs = 5) {
  if (require(RCurl)) {
    uri <- as.character(uri)
    if (RCurl::url.exists(uri)) {
      # from listCurlOptions()
      # do not stop if requests time out
      resolved_list <- tryCatch.W.E(eval({
        opts <- list(
          timeout = timeoutsecs,
          maxredirs = 5,
          followlocation = TRUE, # resolve redirects
          ssl.verifyhost = FALSE, # suppress certain SSL errors
          ssl.verifypeer = FALSE,
          nobody = TRUE, # perform HEAD request - not guaranteed to return something
          verbose = FALSE
        )
        curlhandle <- getCurlHandle(.opts = opts)
        getURL(uri, curl = curlhandle)
        info <- getCurlInfo(curlhandle)
        rm(curlhandle) # release the curlhandle!
        as.character(info$effective.url)
      }))
      return(resolved_list)
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
attr(util_knb$unshorten_url, "help") <- "Resolve an anonymous URL given by an URL shortener"

util_knb$extractURL <- function(x, s = ".") {
  if (grepl(pattern = s, x = x, perl = TRUE, ignore.case = TRUE)) {
    m <- gregexpr("https?\\S+", x, perl = TRUE, ignore.case = TRUE)
    return(regmatches(x, m))
  }
  x
}



util_knb$unshorten_url <- function(uri, timeoutsecs = 5) {
  if (require(RCurl)) {
    uri <- as.character(uri)
    if (RCurl::url.exists(uri)) {
      # from listCurlOptions()
      # do not stop if requests time out
      resolved_list <- tryCatch.W.E(eval({
        opts <- list(
          timeout = timeoutsecs,
          maxredirs = 5,
          followlocation = TRUE, # resolve redirects
          ssl.verifyhost = FALSE, # suppress certain SSL errors
          ssl.verifypeer = FALSE,
          nobody = TRUE, # perform HEAD request - not guaranteed to return something
          verbose = FALSE
        )
        curlhandle <- getCurlHandle(.opts = opts)
        getURL(uri, curl = curlhandle)
        info <- getCurlInfo(curlhandle)
        rm(curlhandle) # release the curlhandle!
        as.character(info$effective.url)
      }))
      return(resolved_list)
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
attr(util_knb$unshorten_url, "help") <- "Resolve an anonymous URL given by an URL shortener"

util_knb$extractURL <- function(x, s = ".") {
  if (grepl(pattern = s, x = x, perl = TRUE, ignore.case = TRUE)) {
    m <- gregexpr("https?\\S+", x, perl = TRUE, ignore.case = TRUE)
    return(regmatches(x, m))
  }
  x
}
attr(util_knb$extractURL, "help") <- "Split string into a list of URLS (must be whitespace-free)"

util_knb$removeURL <- function(x) gsub("https?://\\S+", " ", x, perl = TRUE)
attr(util_knb$removeURL, "help") <- "Remove https://something prefix from string (must be whitespace-free)"

util_knb$removeStrangeMarkup <- function(x) {
  # remove ed><a0><bc><ed><be><89 and similar
  x <- gsub("(?:<\\w\\w>)+", "", x, perl = TRUE)
  x <- gsub("(?:\\w\\w><\\w\\w) ?", "", x, perl = TRUE)
  x
}

util_knb$removeFirstChars <- function(x, pat = "^#|^@|\\s#|\\s@") {
  # remove #hashmarks and @mentions
  gsub(pat, " ", x, perl = TRUE)
}



util_knb$cleanXlsx <- function(data, badcols){
  #data[is.na(data)] <- 0
  data <- util_knb$removeBadCols(data, badcols)
  datecol <- which(names(data) %in% c("LOG_DATE"))
  data[,datecol] <- ymd_hms(data[,datecol], tz = "GMT")
  data
}


# write to pdf. useful when many X11 devices are open
util_knb$writePDF <- function(d, fn, onefile = TRUE) {
  pdf1 <- file.path(d, fn)
  print("Abspath PDF Outfile:")
  print(pdf1)
  dev.copy2pdf(device = x11, file = ifelse(onefile, pdf1, paste0(pdf1, "%03d", ".pdf")))
  # return(pdf1)
}
############# func defs st come before func body

util_knb$readFile <- function(fn){
  #http://cran.r-project.org/web/packages/openxlsx/vignettes/formatting.pdf
  # errmsg <- sprintf("Cannot read data file '%s'.", fn)
  getOption("openxlsx.datetimeFormat", "yyyy/mm/dd hh:mm:ss")
  data <-  read.xlsx(fn, sheet=1, colNames = TRUE)
  return(data)
}

util_knb$removeBadCols <- function(data, badcol=c("EXPEDITION","SITE", "n")){
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
util_knb$plotFile <- function(data, wd, convertstr=FALSE, fn="ketzindata.pdf"){
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
  util_knb$writePDF (wd,outf)
  #dev.off()

}

util_knb$wrapString <-  function(vector_of_strings,width){
  #http://stackoverflow.com/questions/7367138/text-wrap-for-plot-titles-in-r
  sapply(vector_of_strings,FUN=function(x){paste (strwrap(x,width=width), collapse="\n")
  })
}

util_knb$findYearStr <- function(v){
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

util_knb$ggplotFile <- function(data, fn = "") {
  require(ggplot2, quietly = TRUE)
  require(reshape2, quietly = TRUE)
  require(scales, quietly = TRUE) # pretty_breaks
  require(gridExtra, quietly = TRUE) # several plots on one page

  # errmsg <- sprintf("Cannot plot data file '%s'.", fn)
  data2 <- melt(data, id.vars = names(data)[1], variable.names = names(data)[-1])
  p <- list()
  # first column shows x-axis
  for (x in names(data)[-1]) {
    tryCatch({
      # xx <- ifelse(!is.na(x),x,0) #, xlab=as.character(year(data[1,1]))
      # plot( data[,1], data[,x], ylab="", xlim=c(ymd("2008-01-01"),ymd(today())), ylim=c(0,1000), ymin=0, ymax=1000 ,type="l", main=xx)
      # qplot(  data[,1], data[,x], ymin=0, ymax=1000 , main=xx) #, ylab="", geom="l",
      # scale_y_continuous(breaks = round(seq(min(data3$x), max(dat$x), by = 0.5),1)) +
      # x = "KTZI201_WF_DRUCK"
      data3 <- data2[data2$variable == x, ]

      # data3["n.a." ==  data3$value,"value"] <- NA
      data3$value <- as.numeric(data3$value)
      xlabel <- util_knb$findYearStr(data3[, 1])
      ptitle <- paste0(x, "\n", util_knb$wrapString(fn, nchar(fn) / 5))
      pbr <- pretty_breaks(n = 10)
      if (!all(is.na(data3$value))) {
        p[x] <- list(ggplot(data = data3, aes(x = LOG_DATE, y = value)) +
          geom_point(alpha = 1 / 3, na.rm = TRUE) +
          labs(title = ptitle) +
          labs(x = xlabel) +
          theme_bw(base_family = "", base_size = 10) +
          scale_y_continuous(breaks = pbr) +

          facet_grid(variable ~ .))
      }
    },
    error = function(e) {
      message(paste0("error ", str(e)))
      stop()
    },
    finally = print(paste0("done col:", x))
    )
  }
  # unique(data3$variable)
  p["nrow"] <- length(p)
  p
}


########################################
## Has to be last in file
while("util_knb" %in% search())
        detach("util_knb")
attach(util_knb)
