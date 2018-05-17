#######################################################
## Set some necessary options
options(java.parameters = "-Xmx8g") # set java stack size to 8G (default 512MB)

setwd("~/projects/Modeling")

library("RJDBC")
library("jsonlite")

credentials <- fromJSON("~/credentials.json")

#######################################################
## Establish JDBC driver
# set this to the location of the teradata JDBC .jar files
JAR_DIR <- path.expand("~/drivers/teradataJDBC/")
.jinit()
.jaddClassPath(paste(JAR_DIR, "terajdbc4.jar", sep=""))
.jaddClassPath(paste(JAR_DIR, "tdgssconfig.jar", sep=""))
drv <- JDBC(driverClass="com.teradata.jdbc.TeraDriver")

MODELTABLE <- "TWM_SANDBOX.TE_US_MODEL_DATASET"

tconn <- dbConnect(drv, "jdbc:teradata://143.30.195.74/LOGMECH=LDAP",credentials$teradata$username,credentials$teradata$password)

tableRows <- dbGetQuery(tconn, paste("SELECT COUNT(*) AS cnt FROM",MODELTABLE))$cnt
tableColumns <- dbGetQuery(tconn, paste("SELECT COLUMNNAME FROM DBC.COLUMNS WHERE DATABASENAME = 'TWM_SANDBOX' AND TABLENAME = 'TE_US_MODEL_DATASET'"))

################################################
## Do a check for empty columns ----------------

# # Grab first 1000 rows
# topsample <- dbGetQuery(tconn, paste("SELECT TOP 1000 * FROM ",MODELTABLE))
# topsample[trimws(topsample) == ''] <- NA
# 
# #See if any are completely empty
# possiblyEmpty <- names(topsample)[sapply(topsample, function(x) all(is.na(x)))] # first  100 rows were null/empty
# emptyColumns <- c()
# 
# dropThreshold <- floor(tableRows * .0001) # if not even 0.01% of the rows are populated, count as an empty column
# message(paste("Drop Threshold: ",dropThreshold))
# for(col in possiblyEmpty){
#   cnt <- dbGetQuery(tconn, paste0("SELECT COUNT(*) as cnt FROM ",MODELTABLE," WHERE TRIM(",col,") <> '' OR ",col," IS NOT NULL"))$cnt
#   message(paste("col: ",col,"; Not Null Count:",cnt,ifelse(cnt < dropThreshold," -- Dropping","")))
#   if(cnt < dropThreshold) emptyColumns <- c(emptyColumns, col)
# }

#colsToKeep <- names(topsample)[!names(topsample) %in% emptyColumns]

#selectString <- paste("Select SUM(1) OVER( rows unbounded preceding) as id,",paste(colsToKeep,collapse=", "),"FROM",MODELTABLE)
tmpTable <- "TWM_SANDBOX._IMPUTE_TMP"
#sql <- paste("CREATE TABLE",tmpTable,"AS (
#             SELECT SUM(1) OVER ( rows unbounded preceding) as id, MATCHD_CNSMR_PRSNA_ID FROM",MODELTABLE," m
#             ) WITH DATA
#             PRIMARY INDEX (id)")
#dummy <- dbSendQuery(tconn, sql)


appendMe <- FALSE
col.names <- TRUE
dataFile <- 'bigdatafiletest.csv'
interval <- 100000
batches <- ceiling(tableRows/interval)
start <- 1
end <- start + interval - 1
for(i in 1:batches){
    message("#########################################")
    loopconn <- dbConnect(drv, "jdbc:teradata://143.30.195.74/LOGMECH=LDAP",credentials$teradata$username,credentials$teradata$password)
    message(paste("Batch:",i,"of",batches,"..."))
    start <- ((i-1) * interval) + 1
    end <- start + interval - 1
    sql <- paste("SELECT m.* FROM",MODELTABLE," m INNER JOIN",tmpTable,"t on m.MATCHD_CNSMR_PRSNA_ID = t.MATCHD_CNSMR_PRSNA_ID WHERE t.id BETWEEN",format(start, scientific = FALSE),"AND",format(end, scientific = FALSE))
    message(sql)
    res <- dbGetQuery(loopconn,sql)
    message("Query Complete.  Writing to csv File")
    write.table(res, file=dataFile, append=appendMe, col.names=col.names)
    message(paste("Batch",i,"Complete."))
    appendMe <<- TRUE
    col.names <<- FALSE
    dbDisconnect(loopconn)
    res <- NULL
    gc()
}


fetchCount <- 0


dbDisconnect(tconn) #close the connection
