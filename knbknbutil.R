library(ggplot2)
library(reshape2)
library(scales) #pretty_breaks
library(gridExtra) # several plots on one page

knbknb <- new.env()

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


########################################
## Has to be last in file
while("knbknb" %in% search())
        detach("knbknb")
attach(knbknb)
