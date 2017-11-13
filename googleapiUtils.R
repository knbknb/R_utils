#library(jsonlite)
# knb 20160829

googleapi <- new.env()


googleapi$kgapi_call_str <- function(query,
                                     apikey=Sys.getenv("GOO_KGR_KEY"),
                           templatestr="https://kgsearch.googleapis.com/v1/entities:search?key=%s&limit=1&indent=True&query=%s"){
        knowledgeapi <- sprintf(fmt = templatestr, apikey, xml2::url_escape(query))
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
        # Query unsuccessful, try shortened company-name,
        if (stri_length(extracolumn_shortened) > 0){
                message(sprintf("cannot resolve -> 2nd try:\n%s\n%s\n\n", extracolumn, extracolumn_shortened))
                api_call_str <- kgapi_call_str(query=extracolumn_shortened, apikey=apikey)
                json <- kg_api_call(api_call_str, extracolumn)

                if(!is.null(json)){
                        return(json)
                }
        }

        if(is.null(json) & stri_length(extracolumn_shortened.2) > 0) {
                message(sprintf("cannot resolve -> 3rd try:\n%s\n%s\n\n", extracolumn, extracolumn_shortened.2))
                api_call_str <- kgapi_call_str(query=extracolumn_shortened.2, apikey=apikey)
                json <- kg_api_call(api_call_str, extracolumn)
        }
        else {
                warning(sprintf("cannot resolve (tried 3): \n%s\n%s\n\n", extracolumn, extracolumn_shortened))
        }

}

googleapi$kgapi_lookup <- function(lookup_str, apikey=Sys.getenv("GOO_KGR_KEY")) {
        dat <- kgapi_call_data(api_call_str=kgapi_call_str(query=lookup_str, apikey=apikey), extracolumn = lookup_str)
        dat
}
attr(googleapi$kgapi_lookup, "help") <- "Perform a query on the Google Knowledge Graph API"


googleapi$kgapi_lookup_kv <- function(term, apikey=Sys.getenv("GOO_KGR_KEY")) {
        tidyr::gather(googleapi$kgapi_lookup(term, apikey))
}

########################################
## Has to be last in file
while("googleapi" %in% search())
        detach("googleapi")
attach(googleapi)
