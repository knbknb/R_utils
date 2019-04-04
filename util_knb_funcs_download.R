
util_knb <- new.env()
### some functions related to downloadding files

util_knb$download <- function() {
  download.file(url, destfile = filename, method = "curl", extra = "-L")
}
attr(util_knb$download, "help") <- "curl-download a file via https, follow redirects"



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


