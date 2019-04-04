
util_knb <- new.env()

#########################################
# functions below added by knb 2011-2017+

# Here is a simple R function that will find time series outliers
# (and optionally show them in a plot).
# It will handle seasonal and non-seasonal time series.
# by Rob Hyndman ()
# https://stats.stackexchange.com/a/1153/20107
util_knb$tsoutliers <- function(x,plot=FALSE)
{
  x <- as.ts(x)
  if(frequency(x)>1)
    resid <- stl(x,s.window="periodic",robust=TRUE)$time.series[,3]
  else
  {
    tt <- 1:length(x)
    tt <- tt + 0
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


util_knb$download_13fs <-  function(cik = "0001079114"){
  require(xml2, quietly = TRUE)
  all_13FS_overview <- sprintf('https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=%s&type=13F-HR&dateb=&owner=include&count=400', cik)
  all_13FS_overview_html <- read_html(all_13FS_overview)
  all_13FS_overview_urls <- html_attr(html_nodes(all_13FS_overview_html, "#documentsbutton"), "href")
  all_13FS_overview_urls <- paste0("https://www.sec.gov", all_13FS_overview_urls)
  all_13FS_overview_urls_html <- map(all_13FS_overview_urls, read_html)
  all_13FS_overview_urls_html

}

util_knb$filingdates_of_13fs <- function(cik = "0001079114"){
  require(xml2, quietly = TRUE)
  all_13FS_overview <- sprintf('https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=%s&type=13F-HR&dateb=&owner=include&count=400', cik)
  all_13FS_overview_html <- read_html(all_13FS_overview)
  all_13FS_overview_years <- html_table(all_13FS_overview_html)[[3]][, c(3,4)]
  all_13FS_overview_years[,1] <- gsub("-", "", str_extract(string = all_13FS_overview_years[,1],
                                                           pattern=regex("\\d+-\\d+-\\d+")))
  all_13FS_overview_years

}

util_knb$persistent_obj <- function(outdir = ".", outfile = ""){
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
while("util_knb" %in% search())
        detach("util_knb")
attach(util_knb)
