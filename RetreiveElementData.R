## load package for ODBC
#install.packages("RODBC")
library(RODBC)
## connect to element and get data
channel <- odbcConnect("element")
## get the names of all the tables in the database
TableNames<- sqlTables(channel,errors = FALSE)
## test example query
#tmp.test <- sqlFetch(channel, "dbo.Repo_Result", stringsAsFactors=FALSE, max=20)

#Build the query for 2014 PSP Work Orders (A note about "Work_Order": When samples are delivered to the lab, 
# they are logged by assigned a Work Order number.  
# The first two digits of the Work Orders number refer to the last two digits of the year that the samples were received, 
# the third and fourth digits of the Work Orders number refer to the month that the samples were received, 
# and the last two digits of the Work Orders number refer to the order that the samples were received by the lab (i.e. arbitrary).
# Therefore, one way to search for all the samples received by the lab in one year, e.g. 2014, is to search for Work Orders numbers 
# that start with "140" and "141".)

##create the empty query
myQuery <- c()
##populate the query
#for (i in 1:length(station.list)) {
#qry <- paste0("SELECT * FROM dbo.Repo_Result WHERE  Station_ID ='",station.list[i],"'  AND Client LIKE '%Pesticide%' AND Project LIKE '%Pudding%' AND (Work_Order LIKE '%130%' OR Work_Order LIKE '%131%') ")

## This line retreives all the pesticide samples received by the lab in 2014.  The query language is written in SQL.
qry <- paste0("SELECT * FROM dbo.Repo_Result WHERE  Client LIKE '%Pesticide%' AND 
              (Work_Order LIKE '%140%' OR Work_Order LIKE '%141%' OR Work_Order LIKE '%150%' OR Work_Order LIKE '%151%') ")
## This line adds the query language to the empty query.
myQuery <- append(myQuery, qry)
#}

## Retrieve data.
for(i in 1:length(myQuery)) {
  print(myQuery[i])
  data <- sqlQuery(channel,myQuery[i],stringsAsFactors = FALSE, na.strings = "NA")
  ifelse(i==1,mydata <- data, mydata <- rbind(mydata,data))
  rm(data)
}
