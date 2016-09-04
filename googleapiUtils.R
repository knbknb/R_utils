library(jsonlite)
# knb 20160829

googleapi <- new.env()


googleapi$kgapi_call_str <- function(query,
                           apikey,
                           templatestr="https://kgsearch.googleapis.com/v1/entities:search?key=%s&limit=1&indent=True&query=%s"){
        knowledgeapi <- sprintf(fmt = templatestr, apikey, url_escape(query))
        knowledgeapi
}


googleapi$kg_api_call <- function(api_call_str, extracolumn=NA){
        #warning(api_call_str)
        json <- jsonlite::fromJSON(api_call_str)
        if(is.data.frame(json$itemListElement)) {
                json.result <- jsonlite::flatten(json$itemListElement)
                colnames(json.result) <- make.names(colnames(json.result) )
                json.result$name.of.issuer <- extracolumn
                json.result
        }

}


googleapi$kgapi_call_data <- function(api_call_str, extracolumn=NA){
        extracolumn_shortened <- gsub('\\s+\\w+$', '', extracolumn, perl=TRUE)
        extracolumn_shortened.2 <- gsub('\\s+\\w+$', '', extracolumn_shortened, perl=TRUE)
        json <- kg_api_call(api_call_str, extracolumn)
        if(!is.null(json)){
                return(json)
        }
        # Query unsuccessful try shortened company-name,
        if (stri_length(extracolumn_shortened) > 0){
                message(sprintf("cannot resolve - 2nd try:\n%s\n%s\n\n", extracolumn, extracolumn_shortened))
                api_call_str <- kgapi_call_str(query=extracolumn_shortened, apikey=apikey)
                json <- kg_api_call(api_call_str, extracolumn)

                if(!is.null(json)){
                        return(json)
                }
        }

        if(is.null(json) & stri_length(extracolumn_shortened.2) > 0) {
                message(sprintf("cannot resolve - 3rd try:\n%s\n%s\n\n", extracolumn, extracolumn_shortened.2))
                api_call_str <- kgapi_call_str(query=extracolumn_shortened.2, apikey=apikey)
                json <- kg_api_call(api_call_str, extracolumn)
        }
        else {
                warning(sprintf("cannot resolve: \n%s\n%s\n\n", extracolumn, extracolumn_shortened))
        }

}

googleapi$kgapi_lookup <- function(lookup_str, apikey) {
        dat <- kgapi_call_data(api_call_str=kgapi_call_str(query=lookup_str, apikey=apikey), extracolumn = lookup_str)
        dat
}


download_13fs <-  function(cik = "0001079114"){
        all_13FS_overview <- sprintf('https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=%s&type=13F-HR&dateb=&owner=include&count=400', cik)
        all_13FS_overview_html <- read_html(all_13FS_overview)
        all_13FS_overview_urls <- html_attr(html_nodes(all_13FS_overview_html, "#documentsbutton"), "href")
        all_13FS_overview_urls <- paste0("https://www.sec.gov", all_13FS_overview_urls)
        all_13FS_overview_urls_html <- map(all_13FS_overview_urls, read_html)
        all_13FS_overview_urls_html

}

filingdates_of_13fs <- function(cik = "0001079114"){
        all_13FS_overview <- sprintf('https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=%s&type=13F-HR&dateb=&owner=include&count=400', cik)
        all_13FS_overview_html <- read_html(all_13FS_overview)
        all_13FS_overview_years <- html_table(all_13FS_overview_html)[[3]][, c(3,4)]
        all_13FS_overview_years[,1] <- gsub("-", "", stri_extract_last_regex(str = all_13FS_overview_years[,1],
                                                                             pattern="\\d+-\\d+-\\d+"))
        all_13FS_overview_years

}


persistent_obj <- function(outdir = ".", outfile = ""){
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
while("googleapi" %in% search())
        detach("googleapi")
attach(googleapi)
