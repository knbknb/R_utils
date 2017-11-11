########################################
## Put everything into an environment, to not pollute global namespace

twitterUtil <- new.env()

# this function patches a problem with the lookup function
# ignore for now
twitterUtil$patchLookupUsers <- function(users, ...)
  {
    batches <- split(users, ceiling(seq_along(users)/100))
    results <- lapply(batches, function(batch) {
      params <- twitteR:::parseUsers(batch)
      twitteR:::twInterfaceObj$doAPICall(paste("users", "lookup", sep = "/"),
                                         params = params, ...)
    })
    out <- sapply(do.call(c, results), twitteR:::buildUser)
    out
  }

# get a user's followers and their location
twitterUtil$getFollowers <- function(user, nMax=1000, ...)
  {
    followers=patchLookupUsers(user$getFollowerIDs(n=nMax))
    followersLocation = sapply(followers,location)
    list(users=followers, location=followersLocation)
  }

twitterUtil$getFollowersList <- function(user, nMax=1000, ...)
{
        followers=patchLookupUsers(user$getFollowerIDs(n=nMax))
        list(followers=followers, user=user)
        #}

}

# get latitude and longitude from a location name
twitterUtil$findLatLon <- function(loc)
  {
    latlon = NA
    cont = NA

  # Asia = 1, Africa = 2, North America = 3, South America = 4, Australia/New Zealand = 5, Europe = 6
  # hand coding the continents, not pretty
    continents = matrix(NA,nrow=length(unique(world.cities[,2])),ncol=2)
    continents[,1] = unique(world.cities[,2])
    continents[1:10,2] = c(1,1,1,2,1,1,1,1,1,1)
    continents[11:20,2]= c(1,1,2,1,1,2,1,2,2,2)
    continents[21:30,2] = c(2,1,6,6,6,6,6,6,6,6)
    continents[31:40,2] = c(6,6,6,6,2,4,4,1,2,1)
    continents[41:50,2] = c(4,6,1,4,6,1,3,1,6,6)
    continents[51:60,2] = c(3,2,4,2,6,1,6,1,3,2)
    continents[61:70,2] = c(1,2,2,2,3,6,3,3,6,6)
    continents[71:80,2] = c(1,1,2,6,3,4,3,4,6,1)
    continents[81:90,2] = c(3,3,3,2,2,6,6,6,6,4)
    continents[91:100,2] = c(2,5,2,2,3,1,1,1,1,1)
    continents[101:110,2] = c(1,2,1,1,1,3,2,5,1,6)
    continents[111:120,2] = c(1,6,1,1,2,6,1,1,6,2)
    continents[121:130,2] = c(6,6,6,1,1,3,4,3,4,2)
    continents[131:140,2] = c(6,6,2,2,1,1,1,4,1,1)
    continents[141:150,2] = c(1,2,2,1,1,1,4,6,6,2)
    continents[151:160,2] = c(4,1,1,1,1,2,4,6,2,2)
    continents[161:170,2] = c(1,2,2,1,6,2,1,1,6,1)
    continents[171:180,2] = c(1,1,1,2,6,2,2,6,1,1)
    continents[181:190,2] = c(2,6,2,1,6,6,3,3,3,3)
    continents[191:200,2] = c(2,2,2,2,3,2,3,2,3,1)
    continents[201:210,2] = c(3,2,2,2,2,2,2,1,6,2)
    continents[211:220,2] = c(1,3,1,6,2,4,3,6,3,4)
    continents[221:230,2] = c(1,1,1,3,2,3,3,6,1,6)
    continents[231:232,2] = c(2,1)


  # Get the first element of the location
 # firstElement = strsplit(loc,"[^[:alnum:]]")[[1]][1]
    firstElement = strsplit(loc,",")[[1]][1]
    if(is.na(firstElement)){firstElement="zzzzzzzzz"}

  # See if it is a city
    tmp = grep(firstElement,world.cities[,1],fixed=TRUE)
    tmp2 = grep(firstElement,state.name,fixed=TRUE)
    tmp3 = grep(firstElement,world.cities[,2],fixed=TRUE)

    if(length(tmp) == 1){
      latlon = world.cities[tmp,c(5,4)]
      cont = continents[which(world.cities[tmp,2]==continents[,1]),2]
    }else if(length(tmp) > 1){
      tmpCities = world.cities[tmp,]
      latlon = tmpCities[which.max(tmpCities$pop),c(5,4)]
      cont = continents[which(tmpCities[which.max(tmpCities$pop),2]==continents[,1]),2]
    }else if(length(tmp2) == 1){
      latlon = c(state.center$x[tmp2],state.center$y[tmp2])
      cont = 3
    }else if(length(tmp3) > 0){
      tmpCities = world.cities[tmp3,]
      latlon = tmpCities[which.max(tmpCities$pop),c(5,4)]
      cont = continents[which(tmpCities[which.max(tmpCities$pop),2]==continents[,1]),2]
    }

    return(list(latlon=latlon,cont=as.numeric(cont)))

  }

# get points along the great circle between two locations
twitterUtil$getGreatCircle <- function(userLL,relationLL)
  {
    tmpCircle = greatCircle(userLL,relationLL)
    start = which.min(abs(tmpCircle[,1] - userLL[1,1]))
    end = which.min(abs(tmpCircle[,1] - relationLL[1]))
    greatC = tmpCircle[start:end,]
    return(greatC)
  }

# can be performed with tm::stripWhitespace
#twitterUtil$trim <- function (x) gsub("^\\s+|\\s+$", "", x)

twitterUtil$commonReader <- function() {
        return(readTabular(mapping=
                    list(id="id", author="screenName",description="id", content="text",
                         retweetCount="retweetCount", datetimestamp="created")))
}
attr(twitterUtil$commonReader, "help") <- "# return a reader, which will be used in creation of Corpus from tweets.df"

twitterUtil$setHeading <-function(x){
        #input_list <- as.list(substitute(list(...)))[-1L]
        #nwords <-  ifelse(input_list[["n"]] > 0, n, 3)
        #x <- input_list[[1]]
        nwords <- 3
        w <- paste0(head(unlist(strsplit(content(x), split = "\\s+")),nwords), collapse = " ")
        meta(x, tag="heading") <- w
        x
}
attr(twitterUtil$setHeading, "help") <- "for tm_map(): first 3 words become 'heading' metadata entity"

twitterUtil$setId <-function(x){
                w <- paste0(c(meta(x, tag="description"), meta(x, tag="author")),  collapse = "-")
                meta(x, tag="id") <- w
                x
}
attr(twitterUtil$setId, "help") <- "for tm_map(): 'id' metadata entity becomes 'description' plus 'screenName' - easier for DTMs"

twitterUtil$setDatestr <- function(x){
        #w <- as.POSIXct(meta(x, tag="datetimestamp"),origin = "1970-01-01",tz = "CET")
        w <- as.character(as.POSIXct(meta(x, tag="datetimestamp"),origin = "1970-01-01"))
        meta(x, tag="datetime") <- w
        x
}
attr(twitterUtil$setDatestr, "help") <- "for tm_map(): add new 'datetime' metadata entity displaying the datetime value in humanreadable form"

twitterUtil$author <- function(x){
        meta(x, tag="author")
}
attr(twitterUtil$author, "help") <- "show only author tag of a tm::PlainTextDocument"

twitterUtil$ratelimits <- function(){
        rate.limit <- getCurRateLimitInfo()
        # print out all metrics that have been changed from their default values
        rate.limit[rate.limit$limit != rate.limit$remaining,]
}
attr(twitterUtil$ratelimits, "help") <- "twitter API endpoints: show which values are different from the default rate limit"

twitterUtil$append2SQLite <- function(dfr, db.name, table.name){
        tryCatch({
                conn <- dbConnect(SQLite(), dbname = db.name)
                dbBegin(conn)
                dbWriteTable(conn, name=table.name, append=TRUE,
                             value=dfr)
                dbCommit(conn)
                }, error=function(e){warning(e); return("cannot append table:")},
                finally = function(){


                        dbDisconnect(conn)
                })
}
attr(twitterUtil$append2SQLite, "help") <- "append a a data frame to a table in an SQLite database"

twitterUtil$makeUserTableUnique <- function( db.name, table.name = "xx"){
        #username.table <- paste0(query.name,"_userinfo")
        conn <- dbConnect(SQLite(), dbname = db.name)


        table.name.unique <- paste0(table.name,  "_unique_", "_", as.numeric(as.POSIXct(Sys.time(), format="%Y-%m-%d"))*100000)
        RSQLite::dbBegin(conn)
        try({
                sql <- paste0("drop table ", table.name.unique)
                dbSendQuery(conn, sql)

        }, silent = TRUE)
        sql <- paste0('CREATE TABLE ', table.name.unique ,
                      '( "description" TEXT,
                        "statusesCount" REAL,
                        "followersCount" REAL,
                        "favoritesCount" REAL,
                        "friendsCount" REAL,
                        "url" TEXT,
                        "name" TEXT,
                        "created" REAL,
                        "protected" INTEGER,
                        "verified" INTEGER,
                        "screenName" TEXT,
                        "location" TEXT,
                        "lang" TEXT,
                        "id" TEXT,
                        "listedCount" REAL,
                        "followRequestSent" INTEGER,
                        "profileImageUrl" TEXT
                      )
                  ')
        dbSendQuery(conn, sql)

        sql = paste0("insert into ", table.name.unique, " select distinct * from ", table.name)
        dbSendQuery(conn, sql)

        sql = paste0("delete from ", table.name)
        dbSendQuery(conn, sql)

        sql = paste0("insert into ", table.name, " select  * from ", table.name.unique)
        dbSendQuery(conn, sql)

        sql = paste0("drop table ", table.name.unique)
        #sql = paste0("delete from ", table.name.unique)

        dbSendQuery(conn, sql)
        dbCommit(conn)

        dbDisconnect(conn)
}
attr(twitterUtil$makeUserTableUnique, "help") <- "only keep unique records in the userinfo-table"

twitterUtil$makeTweetsTableUnique <- function( db.name, table.name = "xx"){
        #username.table <- paste0(query.name,"_userinfo")
        conn <- dbConnect(SQLite(), dbname = db.name)


        table.name.unique <- paste0(table.name,  "_unique_", "_", as.numeric(as.POSIXct(Sys.time(), format="%Y-%m-%d"))*100000)
        RSQLite::dbBegin(conn)
        try({
                sql <- paste0("drop table ", table.name.unique)
                dbSendQuery(conn, sql)

                }, silent = TRUE)
        sql <- paste0('CREATE TABLE ', table.name.unique ,
'( "text" TEXT,
	"favorited" INTEGER,
	"favoriteCount" REAL,
	"replyToSN" INTEGER,
	"created" REAL,
	"truncated" INTEGER,
	"replyToSID" INTEGER,
	"id" TEXT,
	"replyToUID" INTEGER,
	"statusSource" TEXT,
	"screenName" TEXT,
	"retweetCount" REAL,
	"isRetweet" INTEGER,
	"retweeted" INTEGER,
	"longitude" TEXT,
	"latitude" TEXT
)')
        dbSendQuery(conn, sql)

        sql = paste0("insert into ", table.name.unique, " select distinct * from ", table.name)
        dbSendQuery(conn, sql)

        sql = paste0("delete from ", table.name)
        dbSendQuery(conn, sql)

        sql = paste0("insert into ", table.name, " select  * from ", table.name.unique)
        dbSendQuery(conn, sql)

        sql = paste0("drop table ", table.name.unique)
        #sql = paste0("delete from ", table.name.unique)

        dbSendQuery(conn, sql)
        dbCommit(conn)

        dbDisconnect(conn)
}
attr(twitterUtil$makeTweetsTableUnique, "help") <- "only keep unique records in the tweets-table"

twitterUtil$makeTableUnique <- function( db.name, table.name = "xx" ){
        conn <- dbConnect(SQLite(), dbname = db.name)
        table.name.unique <- paste0(table.name,  "_unique_", "_", format(Sys.time(), "%Y_%m_%d__%I_%M"))
        # does not need to be part of transaction
        silent = TRUE

        RSQLite::dbBegin(conn)

        try({
                sql <- paste0("drop table ", table.name.unique)
                dbSendQuery(conn, sql)

        }, silent = silent)

        try({
                sql <- paste0('CREATE TABLE ', table.name.unique ,' as
                      select * from ', table.name, ' where 1 <> 1 ')
                warning(sql)
                dbSendQuery(conn, sql)
        }, silent = silent)

        try({
                sql = paste0("insert into ", table.name.unique, " select distinct * from ", table.name, " ")
                dbSendQuery(conn, sql)
        }, silent = silent)

        try({
                sql = paste0("delete from ", table.name, " ")
                dbSendQuery(conn, sql)
        }, silent = silent)

        try({
                sql = paste0("insert into ", table.name, " select  * from ", table.name.unique)
                dbSendQuery(conn, sql)
        }, silent = silent)

        try({
                sql = paste0("drop table ", table.name.unique)
                dbSendQuery(conn, sql)
        }, silent = silent)


        dbCommit(conn)

        dbDisconnect(conn)
}
attr(twitterUtil$makeTableUnique, "help") <- "Pass in any table name (as function parameter), only keep unique records in any table: Copy into temptable, delete, copy back, drop temptable."



twitterUtil$createViewThenRemoveDuplicateCount <- function( db.name, table.name = "xx", colname="favoriteCount" ){
        conn <- dbConnect(SQLite(), dbname = db.name)

        try({
                view.name <- paste0(table.name, "_", colname, "_dup")
                # create a view with duplicates, same tweet with incremental
                sql = paste0("CREATE VIEW ", view.name ," as select  id, max(", colname ,") as ", colname,", count(id) as cnt from ", table.name ,
                             " t1 group by  id   having  count(id) > 1")
                print(sql)
                dbSendQuery(conn, sql)
        }, silent = TRUE)
        # get ids
        sql <- paste0("SELECT distinct id, ", colname, " FROM ", view.name, " ORDER BY id, ", colname)
        res <- dbSendQuery(conn, sql)
        res.df <- dbFetch(res)
        dbClearResult(res)
        for (dup in 1:nrow(res.df)){
                RSQLite::dbBegin(conn)

        try({
                # delete these duplicates
                sql = paste0("delete from ", table.name, " where id=" ,res.df[dup, "id"], " and ", colname, " != ",
                             res.df[dup, colname] )
                #print(sql)
                dbSendQuery(conn, sql)
        }, silent = TRUE)
        dbCommit(conn)
        }

        dbDisconnect(conn)
}
attr(twitterUtil$createViewThenRemoveDuplicateCount, "help") <- "Pass in any table name (as function parameter), only keep unique records in any table: Copy into temptable, delete, copy back, drop temptable."

twitterUtil$addAugmentedTable <- function( db.name, table.name = "_augmented", table.main.name="_tabname"){
        conn <- dbConnect(SQLite(), dbname = db.name)
        RSQLite::dbBegin(conn)
        try({
                sql <- paste0('CREATE TABLE ', table.name ,
                              '( "id" TEXT,
                      "createdDateHR" TEXT,
                      "followersPerWeek" REAL,
                      "tweetsPerWeek" REAL
                )')
                dbSendQuery(conn, sql)

        }, silent = TRUE)
        # also create a view linking the two tables
        # todo: factor this out
        try({
                sql <- paste0('CREATE VIEW "', table.name, '_view" AS
        select *
                from ', table.name,'
        INNER JOIN ', table.main.name,'
        ON
        ',table.name,'.id = ',table.main.name,'.id
        ')
                dbSendQuery(conn, sql)

        }, silent = TRUE)


        dbCommit(conn)

        dbDisconnect(conn)
}
attr(twitterUtil$addAugmentedTable, "help") <- "Add an augmented table which holds custom attributes, and a view lining the two tables"

twitterUtil$addAugmentedTweetsTableAndView <- function( db.name, table.name = "_augmented", table.main.name="_tabname"){
        conn <- dbConnect(SQLite(), dbname = db.name)
        RSQLite::dbBegin(conn)
        try({
                sql <- paste0('CREATE TABLE ', table.name ,
                              '( "id" TEXT,
                              "language" TEXT,
                              "datetimestr" TEXT,
                                "sentiment" VARCHAR(50)

                )')
                dbSendQuery(conn, sql)

        }, silent = TRUE)
        # also create a view linking the two tables
        # todo: factor this out
        try({
                sql <- paste0('CREATE VIEW "', table.name, '_view" AS
        select *
                from ', table.name,'
        INNER JOIN ', table.main.name,'
        ON
        ',table.name,'.id = ',table.main.name,'.id
        ')
                dbSendQuery(conn, sql)

        }, silent = TRUE)


        dbCommit(conn)

        dbDisconnect(conn)
}
attr(twitterUtil$addAugmentedTweetsTableAndView, "help") <- "Add an augmented table which holds custom attributes for tweets, and a view linking the two tables. Ignore error if either table or views exists"

twitterUtil$createView4DuplicateIDs <- function( db.name, table.name){
        conn <- dbConnect(SQLite(), dbname = db.name)
        RSQLite::dbBegin(conn)
        try({
                sql <- paste0('CREATE VIEW "', table.name, '_duplicates_view" AS
                SELECT  id, screenName,
                        max(followersCount) as foll,
                        max(favoritesCount)   as fav,
                        max(friendsCount) as frie
                FROM ', table.name, '
                group by id, screenName
                having
                        count(followersCount) > 1 or
                        count(favoritesCount)  > 1 or
                        count(friendsCount) > 1
                              ' )
                printf(sql)
                dbSendQuery(conn, sql)

        }, silent = TRUE)
#         duplicates <- try({
#                 dbFetch(res)
#
#         }, silent = TRUE)
#         ids <- paste(duplicates$id, collapse = ", ")
#         lapply(ids, function({
#                 sql <- paste0(
#                         'delete from ', table.name, ' where  id in (', ids,')')
#                 printf(sql)
#                 dbSendQuery(conn, sql)
#         }, silent = TRUE)
        dbCommit(conn)
        dbDisconnect(conn)
        #duplicates
}
attr(twitterUtil$createView4DuplicateIDs, "help") <- "Create view that shows duplicate ids for Accounts that change frequently"

twitterUtil$insertIntoView4AugmentedTweets <- function(tweets.df2, db.name, table.name.augmented, table.name){
        conn <- dbConnect(SQLite(), dbname = db.name)
        RSQLite::dbBegin(conn)
        sapply(1:nrow(tweets.df2), function(x){
                try({
                        sql = paste0("insert into ", table.name.augmented, " (language, datetimestr,id, sentiment) values('",
                                     tweets.df2[x, "language"],"', '",tweets.df2[x, "datetimestr"], "', ", tweets.df2[x, "id"], ", '", tweets.df2[x, "sentiment"],"')")
                        #                 sql = paste0("update ", table.name.augmented, " set language='", tweets.df2[x, "language"],"', datetimestr='",
                        #                              tweets.df2[x, "datetimestr"], "' where id=", tweets.df2[x, "id"])
                        dbSendQuery(conn, sql)
                        print(sql)
                }, silent = FALSE)
        })
        dbCommit(conn)
        dbDisconnect(conn)
}
attr(twitterUtil$insertIntoView4AugmentedTweets, "help") <- "Create view that shows augmented tweets table"

#delete from  qry_wikileaks_augmented where sentiment = 'NA' and id in (select  id  from qry_wikileaks_augmented  group by  id   having  count(id) > 1)
twitterUtil$getFromDb <- function( db.name, view.name = "qry_potsdam_augmented_view"){
        conn <- dbConnect(SQLite(), dbname = db.name)

        sql <- paste0("SELECT * FROM ", view.name)
        res <- dbSendQuery(conn, sql)
        res.df <- dbFetch(res, n = 100000)
        dbDisconnect(conn)
        res.df
}
attr(twitterUtil$getFromDb, "help") <- "Get full datatable or view from sqlite database."

########################################
## Has to be last in file.
# After executing this, funciton is part of global namespace,
# so we do no longer need to fully qualify package name.

while("twitterUtil" %in% search())
        detach("twitterUtil")
attach(twitterUtil)