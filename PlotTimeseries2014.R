#This script should: Point to data source.  Format data. Compare data to standard. Graph the timeseries.
#Basin and data range
#authorship
####

####This function is two years old and based in Dan's python world.  Simplify me!
get.cases <- function(chk.values) {
  ## Checks for non-numeric values in the vector "chk.values", which should
  ## be a character vector. A data.frame is returned with the non-numeric
  ## values (cases) and the number of occurrences for each case. If there
  ## are olnly numeric values in the input vectore, the entries in the 
  ## data.frame returned are "No non-numeric values found" for the case
  ## and NA for the count
  ## Created by Kevin Brannan
  ## Version 1.0.0.09.20.2012
  tmp.cases <- chk.values[grep("[^0-9.]",chk.values)][!duplicated(chk.values[grep("[^0-9.]",chk.values)])]
  if(length(tmp.cases) > 0){
    tmp.cases.report <- data.frame(Case = tmp.cases,Count=as.numeric(NA))
    for(ii in 1:length(tmp.cases)){
      tmp.cases.report$Count[ii] <- length(grep(tmp.cases.report$Case[ii],chk.values))
    }
  } else{
    tmp.cases.report <- data.frame("No non-numeric values found",NA)
    names(tmp.cases.report) <- c("Case","Count")
  }
  return(tmp.cases.report)
}

## load package for ODBC
library(RODBC)
## connect to element and get data
channel <- odbcConnect("element")
## get the names of all the tables in the database
TableNames<- sqlTables(channel,errors = FALSE)
## test example query
tmp.test <- sqlFetch(channel, "dbo.Repo_Result", stringsAsFactors=FALSE, max=20)

#Pudding PSP stations 2013 
#station.list <- c(10000, 11516, 31875, 10646, 10917)

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
              (Work_Order LIKE '%140%' OR Work_Order LIKE '%141%') ")
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

unique(mydata$Work_Order)
unique(mydata$Project)

oldpath <-"\\\\Deqhq1\\PSP\\Rscripts\\2014\\old\\20140612\\"
oldfile <- "State_2014_mydata_clean_noV.csv"
old.data <- read.csv(paste0(oldpath, oldfile), colClasses = "character")
old.data.n  <- nrow(old.data)#$Station_Number)
new.data.n <- nrow(mydata)
if(new.data.n > old.data.n) print("NEW DATA! NEW DATA! NEW DATA! NEW DATA! NEW DATA AVAILABLE!")

library(stringr)
Analyte <- mydata$OrigAnalyte
Station_Description <- mydata$Station_Description
Station_Number <- as.numeric(mydata$Station_ID)
new.folder <- dir.create(paste("\\\\Deqhq1\\PSP\\Rscripts\\2014\\",Sys.Date(), sep="")) 
outpath.plot.points <- paste("\\\\Deqhq1\\PSP\\Rscripts\\2014\\",Sys.Date(), "\\", sep="") 
setwd(outpath.plot.points)
log.scale <- ""
Units <- mydata$Units
SampleType <- mydata$SampleType
Basin <- mydata$Project

RESULT <- str_trim(mydata$Result)
report <- get.cases(RESULT)
report

####To the report above, add a column called "Sub" and populate with substituted values. This is the value that will be substituted.
####Also, create a column called "RESULT_clean".  Populate column with the substituted values.
####Check the report$Sub for unacceptable substitutions.  MANUAL clean up with final values.
report$Sub <- gsub("[^0-9.]","",report$Case)
report$SubFinal  <- report$Sub #Create a copy of the Sub field
RESULT_clean <- gsub("[^0-9.]","",RESULT)

#report[report$Case == '-0.1','SubFinal'] <- -0.1
#mydata[mydata$RESULT == '-0.1','Result_clean'] <- -0.1
#View(mydata[mydata$RESULT == '-0.1',])

####turns empty fields into NAs.
report$SubFinal <- as.numeric(report$SubFinal) 
RESULT_clean <- as.numeric(RESULT_clean) 
report

####convert dates from characters to dates
#date <- as.Date(strptime(mydata$Sampled, format="%d-%b-%y %H:%M:%s")) #still lost the hours/minutes
date <- as.Date(strptime(mydata$Sampled_Date, format="%d %b %Y")) 

####Create new table with only wanted columns
mydata_clean <- data.frame(Basin, Station_Number, Station_Description, date, Analyte, RESULT, Units, SampleType, RESULT_clean, "RESULT_clean.ug.l"=NA, "RESULT_clean.ug.l.subND"=NA, stringsAsFactors = FALSE)
mydata_clean$RESULT_clean.ug.l <- as.numeric(mydata_clean$RESULT_clean.ug.l)
mydata_clean$RESULT_clean.ug.l.subND <- as.numeric(mydata_clean$RESULT_clean.ug.l.subND)

####Subset and set aside the field duplicates ----
unique(mydata_clean$SampleType) 

FD <- subset(mydata_clean, SampleType %in% c("Field Duplicate", "Field Duplicate::FD") & RESULT_clean != "NA") #dataframe of all detected FDs
unique(FD$RESULT_clean)

####Make new subset without the Voids, Field Dupes and Blanks.
sort(unique(mydata_clean$RESULT)) #verify that the names in quotes are the names being used in the datatable
mydata_clean <- subset(mydata_clean, RESULT != "Void" & RESULT != "Cancelled")
unique(mydata_clean$SampleType) #verify that the names in quotes are the names being used in the datatable
mydata_clean <- subset(mydata_clean, Station_Number != 10000)
mydata_clean <- subset(mydata_clean, SampleType != "Field Duplicate")
mydata_clean <- subset(mydata_clean, SampleType != "Field Duplicate::FD")

####Find out which field duplicates are larger than the field primaries. Make a table
df <- NULL
df.fp <- NULL
for(i in 1:nrow(FD)){
  FD2 <- FD[i,]
  FP <- subset(mydata_clean, Analyte == FD$Analyte[i] & date == FD$date[i] & Station_Number == FD$Station_Number[i] & SampleType %in% c("Field Primary", "Field Primary::FP")) #find the matching field primary
  if(nrow(FP) > 1) print("more than one match") #check that getting one match only
  FP.result <- FP$RESULT_clean #get just the number
  if(is.na(FP.result) == TRUE){
    df <- rbind(df, FD2)    
    df.fp <- rbind(df.fp, FP)
  }else{  
    if(FD$RESULT_clean[i] > FP.result){
      df <- rbind(df, FD2)
      df.fp <- rbind(df.fp, FP)
    }
  }
}


####Remove Field Primaries that are less than Field Duplicates
index <- row.names(df.fp) #row.names of the field primaries in order
for(aa in index){
  mydata_clean[as.character(aa),] <- NA #NA out the field primaries
}
mydata_clean <- subset(mydata_clean, is.na(Station_Number) == FALSE)

####Add Field Duplicates that are more than Field Primaries
mydata_clean <- rbind(df,mydata_clean)


####Subset out not needed data
station.list <- unique(mydata_clean$Station_Number) #list of stations
unique(mydata_clean$Analyte) #list of lab analytes
#Deleting the weird [2C] tag from ELEMENT
mydata_clean$Analyte <- gsub(" \\[2C\\]$","",mydata_clean$Analyte)

detections <- subset(mydata_clean, RESULT != "ND" ) #subset out the NDs 
#detections <- subset(mydata_clean, is.na(RESULT_clean) == FALSE ) #subset out the NDs 
analytes <- unique(detections$Analyte) #list of detected analytes
analytes
####obtain the sampling dates
sort(unique(mydata_clean$date))

####Establish Benchmarks and Exceedances

#### pre-processing minimum criteria with Excel for now:----
criteria.file <- "Pesticide Benchmarks and Criteria_Feb 04 2014.csv"
criteria <- read.csv(file=paste0("\\\\Deqhq1\\PSP\\Rscripts\\Wasco\\Wasco2013\\", criteria.file), row.names=1, colClasses = "character")

min.DEQ.criteria <- as.numeric(criteria$min.AL.DEQ.WQS)
min.EPA.criteria <- as.numeric(criteria$min.AL.EPA.benchmark)
min.criteria <- data.frame(criteria$Pollutant, min.DEQ.criteria, min.EPA.criteria, criteria$minimum.criteria.benchmark.value, stringsAsFactors = FALSE)

#matching the analytes name with the min.criteria name----
#recursive until Has.min.criteria is all TRUE 
criteria.pollutant.list <- unique(min.criteria$criteria.Pollutant)
Has.min.criteria <- analytes %in% criteria.pollutant.list #Caution!!"analytes" comes from the detections subset only - so NOT all the available criteria will be populated into later datasets!! It WILL skip mismatched (between LEAD analyte name and criteria name) nondetects!!
check <- data.frame(Has.min.criteria, analytes)
check  #no minimum criteria/benchmarks exist for Total Solids or DEET or Pronamide or 2,6-BAM
#end recursion
min.criteria[criteria$Pollutant == 'aminomethyl phosphoric acid (AMPA) Glyphosate degradate','criteria.Pollutant'] <- "Aminomethylphosphonic acid (AMPA)" #example for substitutions (first is old name in criteria list, second is new analyte name)
min.criteria[criteria$Pollutant == '2,6-Dichlorobenzamide (BAM)','criteria.Pollutant'] <- "2,6-Dichlorobenzamide" #example for substitutions (first is old name in criteria list, second is new analyte name)
min.criteria[criteria$Pollutant == '4,4`-DDD','criteria.Pollutant'] <- "4,4´-DDD" #example for substitutions (first is old name in criteria list, second is new analyte name)
min.criteria[criteria$Pollutant == '4,4`-DDE','criteria.Pollutant'] <- "4,4´-DDE" #example for substitutions (first is old name in criteria list, second is new analyte name)
#change min.criteria table - replace criteria value for 2,4-D with 2,4-D acids and salts
aaa <- as.numeric(min.criteria[min.criteria$criteria.Pollutant == "2,4-D acids and salts",'criteria.minimum.criteria.benchmark.value'])#benchmark for 2,4-D acids and salts
min.criteria[criteria$Pollutant == '2,4-D','criteria.minimum.criteria.benchmark.value'] <- aaa 
min.criteria <- min.criteria[-(382), ] #delete repeated imidacloprid

######################################
min.criteria0 <- min.criteria
min.criteria <- subset(min.criteria0, (min.criteria0$criteria.minimum.criteria.benchmark.value) != "")
for(i in 1:nrow(min.criteria)){
  if(min.criteria$criteria.Pollutant[i] == "Chlorpyrifos"){  #Chlorpyrifos is only standard where we draw both lines
    min.criteria$label[i] <- (paste0("\nAcute WQS = 0.083 ug/L\nChronic WQS = 0.041 ug/L"))
  }else{
    if(min.criteria$criteria.Pollutant[i] != "Chlorpyrifos" & is.na(min.criteria$min.DEQ.criteria[i])==FALSE){  #if there is a DEQ criteria
      min.criteria$label[i] <- (paste0("\nDEQ WQS = ", min.criteria$min.DEQ.criteria[i], " ug/L"))
    }else{
      if(is.na(min.criteria$min.DEQ.criteria[i])==TRUE & is.na(min.criteria$min.EPA.criteria[i])==FALSE){  #if there is no DEQ criteria and there is an EPA benchmark, 
        min.criteria$label[i] <- (paste0("\nEPA benchmark = ", min.criteria$min.EPA.criteria[i], " ug/L"))
      }else{
        if(is.na(min.criteria$min.DEQ.criteria[i])==TRUE & is.na(min.criteria$min.EPA.criteria[i])==TRUE){  #if there is no DEQ criteria or EPA benchmark
          min.criteria$label[i] <- (paste0("\nNo benchmark available")) 
        }
      }
    }
  }
}
######################################

####duplicate dataset.
mydata_clean_noV <- mydata_clean

####fill out ug/L column----
unique(mydata_clean_noV$Units)
#[1] "ng/L" "mg/L" "µg/L"
#[1] "ng/L" "mg/L" "µg/L"

for(i in 1:nrow(mydata_clean_noV)){
  if(mydata_clean_noV$Units[i] == "mg/L"){ 
    mydata_clean_noV$RESULT_clean.ug.l[i] <- mydata_clean_noV$RESULT_clean[i]*1000 #mg to ug
  } else {
    if(mydata_clean_noV$Units[i] == "ng/L"){
      mydata_clean_noV$RESULT_clean.ug.l[i] <- mydata_clean_noV$RESULT_clean[i]/1000 #ng to ug
    } else {
      if(mydata_clean_noV$Units[i] == "µg/L"){
        mydata_clean_noV$RESULT_clean.ug.l[i] <- mydata_clean_noV$RESULT_clean[i] #µg to ug
      } else {
        mydata_clean_noV$RESULT_clean.ug.l[i] <- NA #some other units (e.g. conductivity)
      }
    }
  }
}

####Substitute the NDs for zeroes in a new column
for(i in 1:nrow(mydata_clean_noV)){
  if(mydata_clean_noV$RESULT[i] == "ND"){
    mydata_clean_noV$RESULT_clean.ug.l.subND[i] <- 0  
  }else{
    if(mydata_clean_noV$RESULT[i] !="ND"){
      mydata_clean_noV$RESULT_clean.ug.l.subND[i] <- mydata_clean_noV$RESULT_clean.ug.l[i]
    }
  }
}

####Determine minimum benchmark exceedances
for(i in 1:nrow(mydata_clean_noV)){
  ccc <- mydata_clean_noV$Analyte[i]
  ddd <- match(ccc, min.criteria$criteria.Pollutant)
  mydata_clean_noV$benchmark.DEQ[i] <- as.numeric(min.criteria$min.DEQ.criteria[ddd])   #make a column of appropriate benchmark
  mydata_clean_noV$benchmark.EPA[i] <- as.numeric(min.criteria$min.EPA.criteria[ddd])   #make a column of appropriate benchmark  
  mydata_clean_noV$relevant.AL.benchmark[i] <- as.numeric(min.criteria$criteria.minimum.criteria.benchmark.value[ddd])   #make a column of appropriate benchmark
  mydata_clean_noV$final_digress[i] <- ifelse(mydata_clean_noV$RESULT_clean.ug.l[i] > mydata_clean_noV$relevant.AL.benchmark[i], 1,0) #make column with digression stations (T/F)
}
digressions <- (mydata_clean_noV[is.na(mydata_clean_noV$final_digress) == FALSE & mydata_clean_noV$final_digress == 1,])
index <-  (order(digressions$Basin))
digressions <- digressions[(index),]
digressions

#### Determine percent digression of criteria
mydata_clean_noV$percent.benchmark <- mydata_clean_noV$RESULT_clean.ug.l/mydata_clean_noV$relevant.AL.benchmark
mydata_clean_noV$exceed.type <- NA

for(i in 1:nrow(mydata_clean_noV)){
  if(is.na(mydata_clean_noV$RESULT_clean[i]) == FALSE & is.na(mydata_clean_noV$relevant.AL.benchmark[i]) == TRUE){ #result is a detection AND benchmark does NOT exist
    mydata_clean_noV$exceed.type[i] <- "no benchmark available"
  }else{
    if(is.na(mydata_clean_noV$percent.benchmark[i])==FALSE){ #percent.benchmark is a real number
      if(mydata_clean_noV$percent.benchmark[i] < 0.1){
        mydata_clean_noV$exceed.type[i] <- "less than ten percent of benchmark"  
      }else{
        if(mydata_clean_noV$percent.benchmark[i] >= 0.1 & mydata_clean_noV$percent.benchmark[i] < 0.5){
          mydata_clean_noV$exceed.type[i] <- "between ten and fifty percent of benchmark"
        }else{
          if(mydata_clean_noV$percent.benchmark[i] >= 0.5 & mydata_clean_noV$percent.benchmark[i] < 1.0){
            mydata_clean_noV$exceed.type[i] <- "between fifty and 100 percent of benchmark"
          }else{
            if(mydata_clean_noV$percent.benchmark[i] > 1.0){
              mydata_clean_noV$exceed.type[i] <- "greater than 100 percent of benchmark"
            }
          }
        }
      }
    }
  }
}

####check that these analytes truly do NOT have a benchmark value
aaa <- (mydata_clean_noV[mydata_clean_noV$exceed.type == "no benchmark available",])
unique(aaa$Analyte) #confirmed, no criteria for TS, and DEET, BAM, pronamide, 44DDD, 44DDE, chlorpropham, acetamiprid
#changed criteria for 2,4-D

rm(mydata)

##re-make detections subset with the new columns
detections <- subset(mydata_clean_noV, is.na(RESULT_clean) == FALSE) #subset out the NDs 
index <-  (order(detections$Basin))
detections <- detections[(index),]

####Output a summary table 
Det.freq.table <- data.frame("Station"=NA, "Station.Description"=NA, "Parameter"=NA,"Average"=NA, "Max"=NA, "criteria"=NA, "ALR"=NA, "N Samples" = NA, "percent.det.freq"=NA, "exceed.type"=NA, stringsAsFactors=FALSE)

####Four
for(ii in analytes){
  subset.points0 <- subset(mydata_clean_noV, Analyte == ii)#aaa
  tot.n <- nrow(subset.points0)#bbb
  for(i in station.list){
    subset.points <- subset(subset.points0, Station_Number == i)#ccc
    if(length(subset.points$RESULT_clean)>0){
      
      detects.n <- nrow(subset(subset.points, is.na(RESULT_clean) == FALSE))
      type.n <- nrow(subset.points)#ddd
      percent.det.freq <- (detects.n/type.n)*100
      
      Station <- min(subset.points$Station_Number)
      Station.Description <- min(subset.points$Station_Description)
      Analyte <- min(subset.points$Analyte)
      Average <- mean(subset.points$RESULT_clean.ug.l.subND)
      Max <- max(subset.points$RESULT_clean.ug.l.subND)
      matchup <- match(Analyte, min.criteria$criteria.Pollutant)
      criteria <- as.numeric(min.criteria$criteria.minimum.criteria.benchmark.value[matchup])
      ALR <- Max/criteria
      
      df1 <- data.frame("Station"=Station, "Station.Description"=Station.Description, "Parameter"=Analyte,"Average"=Average, "Max"=Max, "criteria"=criteria, "ALR"="Not Calculated", "N Samples" = type.n, "percent.det.freq"=percent.det.freq, "exceed.type"="Not Calculated", stringsAsFactors=FALSE)
      Det.freq.table <- rbind(df1, Det.freq.table)
    }
  }
}

####Two and Three
#Aggregate Basin wide statistics
ii <- "Hexazinone"
for(ii in analytes){
  subset.points <- subset(mydata_clean_noV, Analyte == ii)
  if(length(subset.points$RESULT_clean)>0){
    
    tot.n <- nrow(subset.points)
    detects <- subset(subset.points, is.na(exceed.type) == FALSE)
    det.n <- nrow(detects)
    percent.det.freq <- (det.n/tot.n)*100
    
    Station <- "Basin aggregate"
    Station.Description <- "Basin aggregate"
    Analyte <- min(subset.points$Analyte)
    Average <- mean(subset.points$RESULT_clean.ug.l.subND)
    Max <- max(subset.points$RESULT_clean.ug.l.subND)
    matchup <- match(Analyte, min.criteria$criteria.Pollutant)
    criteria <- as.numeric(min.criteria$criteria.minimum.criteria.benchmark.value[matchup])
    ALR <- Max/criteria
    
    df1 <- data.frame("Station"=Station, "Station.Description"=Station.Description, "Parameter"=Analyte,"Average"=Average, "Max"=Max, "criteria"=criteria, "ALR"=ALR, "N Samples" = tot.n, "percent.det.freq"=percent.det.freq, "exceed.type"="Total Detection Freq", stringsAsFactors=FALSE)
    Det.freq.table <- rbind(df1, Det.freq.table)
  }
}

####One
for(ii in analytes){
  subset.points0 <- subset(mydata_clean_noV, Analyte == ii)#aaa
  n.tot <- nrow(subset.points0)#bbb
  for(iii in unique(mydata_clean_noV$exceed.type)){
    subset.points <- subset(subset.points0, exceed.type == iii)#ccc
    if(length(subset.points$RESULT_clean)>0){
      
      n.exceed.type <- nrow(subset.points)#ddd
      percent.det.freq <- (n.exceed.type/n.tot)*100
      
      Basin <- min(subset.points$Basin)
      Station <- min(subset.points$Station_Number)
      Station.Description <- min(subset.points$Station_Description)
      Analyte <- min(subset.points$Analyte)
      Average <- mean(subset.points$RESULT_clean.ug.l.subND)
      Max <- max(subset.points$RESULT_clean.ug.l.subND)
      matchup <- match(Analyte, min.criteria$criteria.Pollutant)
      criteria <- as.numeric(min.criteria$criteria.minimum.criteria.benchmark.value[matchup])
      ALR <- Max/criteria
      
      df1 <- data.frame("Station"=Basin, "Station.Description"=Basin, "Parameter"=Analyte,"Average"=Average, "Max"=Max, "criteria"=criteria, "ALR"="Not Calculated", "N Samples" = n.exceed.type, "percent.det.freq"=percent.det.freq, "exceed.type"=iii, stringsAsFactors=FALSE)
      Det.freq.table <- rbind(df1, Det.freq.table)
    }
  }
}

#Det.freq.table <- subset(Det.freq.table, percent.det.freq>0) #subset for parameters with detections

unique(mydata_clean_noV$Station_Description)
Station_labels <- c("Lenz Creek at mouth",
                    "Wagner Creek at Valley View Road (Talent)",
                    "Mill Creek at Wright Road",
                    "Hood River at footbridge downstream of I-84",
                    "Willow Creek inflow",
                    "Amazon Creek at Bond Road",
                    "Rock Creek at Stony Brook Court",
                    "Zollner Creek at Dominic Road",
                    "Threemile Creek at Hwy 197",
                    "West Fork Palmer at Webfoot Road Bridge",
                    "West Fork Palmer Creek at SE Palmer Creek Road",
                    "West Fork Palmer at SE Lafayette Hwy",
                    "Amazon Creek at 29th Street Gaging Station",
                    "Odell Creek upstream of Odell WWTP outfall",
                    "Rogue River @ Hwy. 18/Salmon River Hwy",
                    "Upper Neal Creek, downstream of EFIC",
                    "Mud Springs Creek at Mouth",
                    "Mill Creek at 2nd Street, The Dalles",
                    "Neal Creek at mouth",
                    "Amazon Creek at Beltline Road",
                    "Walla Walla River at Pepper's Bridge",
                    "Little Walla Walla River, west branch/Crocket",
                    "Trout Creek US of Mud Springs Creek",
                    "Amazon Creek at High Pass Road",
                    "Gold Creek at Gold Creek RD",
                    "Campbell Creek at Mouth",
                    "West Prong Little Walla Walla River south of Stateline Road",
                    "Coleman Creek at Greenway Bridge",
                    "Agency CR at SW Grand Ronde RD",
                    "A1 Channel at Awbrey Lane",
                    "Pudding River at Hwy 99E (Aurora)",
                    "Fifteenmile Creek Above Seufert Falls",
                    "Little Walla Walla River Mid West Prong",
                    "Griffin Creek at Greenway Bridge",
                    "Little Walla Walla River at The Frog",
                    "Middle Cozine at Old Sheridan Road",
                    "Sieben Creek at Hwy 212",
                    "Lower Cozine Creek at Davis Street Bridge",
                    "Little Pudding River at Rambler Road",
                    "Noyer Creek at Hwy 212",
                    "North Fork Deep Creek at Hwy 212",
                    "Jackson Creek at Dean Creek Bridge")

mydata_clean_noV$Station_labels <- factor(mydata_clean_noV$Station_Description, levels=unique(mydata_clean_noV$Station_Description), labels=Station_labels)


write.csv(Det.freq.table, paste0(outpath.plot.points,"State_2014_detection_frequencies_savedon", Sys.Date(),".csv")) 

write.csv(mydata_clean_noV, paste0(outpath.plot.points,"State_2014_mydata_clean_noV_savedon", Sys.Date(),".csv")) 













#########################################################################################################
####loop through analyte list and graph multiple stations



install.packages("ggplot2")
library(ggplot2)
library(scales)
#ggplot 
#single plots
#stations sorted by shape

B <- "Hood River"
ii <- "Deisopropylatrazine"
ii <- "Total Solids"
B <- "Wasco"
ii <- "Chlorpyrifos"
B <- "Yamhill"
ii <- "Bifenthrin"
ii <- "Conductivity"

###########################
#outpath.plot.points <- ("\\\\Deqhq1\\PSP\\Rscripts\\2014\\")
###########################

for(B in unique(mydata_clean_noV$Basin)){
  subset.B <- subset(mydata_clean_noV, Basin == B)
  for(ii in analytes){
    subset.ii <- subset(subset.B, Analyte == ii)
    subset.ii <- subset(subset.ii, is.na(subset.ii$RESULT_clean.ug.l) == FALSE)
    #if(all(is.na(subset.ii$RESULT_clean.ug.l)==FALSE)){
    print(paste0(ii, ": n=", length(subset.ii$RESULT)))
    if(length(subset.ii$RESULT) > 0 & is.na(sum(subset.ii$RESULT_clean.ug.l))==FALSE){
      numeric.criterion.graph <- as.numeric(min.criteria[min.criteria$criteria.Pollutant == ii,'criteria.minimum.criteria.benchmark.value']) #find the lowest EPA AL benchmark
      numeric.criterion.label <- min.criteria[min.criteria$criteria.Pollutant == ii,'label'] #find the lowest DEQ AL benchmark
      a <- ggplot(data = subset.ii, #data source is the subset of Basin and analyte
                  aes(x = date, #x axis is dates
                      y = RESULT_clean.ug.l, #y axis is numeric result
                      group=Station_labels,
                      shape=Station_labels, #change point shapes by station
                      color=Station_labels)) #change point colors by station
      a <- a + geom_point(size = 5) #set the point size
      a <- a + xlab("") + ylab(("ug/L")) #write the labels
      a <- a + scale_x_date(breaks=unique(subset.B$date), labels=format(unique(subset.B$date), format="%m/%d"))
      a <- a + theme(panel.grid.minor.x = element_blank()) #remove minor grid lines
      a <- a + theme_bw() #blackandwhite theme
      a <- a + coord_cartesian(xlim=c(min(subset.B$date)-1, max(subset.B$date)+1)) #add a day to beginning and end
      a <- a + ylim(c(0, max(subset.ii$RESULT_clean.ug.l*1.8))) #set the y range from zero to some multiplier of the max result to increase the head space
      #benchmarks lines and labels  
      if(length(numeric.criterion.graph)==0){  #if there is NO DEQ criteria or EPA benchmark
        title <- (paste0(B, " 2014\n", ii, "\nNo benchmark available")) 
      }else{
        if(ii != "Chlorpyrifos" & ii != "2,4-D" & length(numeric.criterion.graph)>0){  #if there IS DEQ criteria or EPA benchmark
          a <- a + geom_hline(yintercept=numeric.criterion.graph)  #draw it
          title <- (paste0(B, " 2014\n", ii, numeric.criterion.label))
        }else{
          if(ii == "Chlorpyrifos"){  #Chlorpyrifos is only standard where we draw both lines
            a <- a + geom_hline(yintercept=0.083, linetype=2)  #draw Acute Chlorpyrifos WQS (only graph with two WQS)
            a <- a + geom_hline(yintercept=0.041, linetype=1)  #draw Chronic Chlorpyrifos WQS (only graph with two WQS)
            title <- (paste0(B, " 2014\n", ii, numeric.criterion.label))
          }else{
            if(ii == "2,4-D"){  #Using the "2,4-D Acids and Salts"  
              a <- a + geom_hline(yintercept=13.1)  
              title <- (paste0(B, " 2014\n", ii, "\nEPA benchmark = 13.1 ug/L "))
            }
          }
        }
      }
      a <- a + ggtitle(title) #write the title and subtitle
      a <- a + guides(shape = guide_legend(ncol = 2))
      a <- a + theme(legend.position="bottom")
      a <- a + theme(legend.direction="vertical")
      a <- a + theme(legend.text=element_text(size=10))
      a <- a + theme(legend.title=element_blank()) #remove title from legend
      a      
      a <- arrangeGrob((a), sub = textGrob(paste0("prepared by Julia Crown ", Sys.Date()), 
                                           x = 0, hjust = -0.1, vjust=0.1,
                                           gp = gpar(fontface = "italic", fontsize = 8))) 
      ggsave(filename = paste0(outpath.plot.points, B, "_", ii, "_2014_savedon", Sys.Date(),".jpg"), plot = a)
    #}
    }
  }
}

#################################################################################
#ggplot 
#multiplot
#stations sorted by shape and color

B <- "Hood River"

for(B in unique(mydata_clean_noV$Basin)){
  subset.B <- subset(mydata_clean_noV, Basin == B)
  subset.B <- subset(subset.B, is.na(subset.B$RESULT_clean.ug.l) == FALSE)
  print(paste0(B, ": n=", length(subset.B$RESULT)))
  if(sum(subset.B$RESULT_clean.ug.l.subND) > 0){
    a <- ggplot(data = subset.B, #data source is the subset of Basin and analyte
                aes(x = date, #x axis dates
                    y = RESULT_clean.ug.l, #y axis is numeric result
                    shape=Station_labels, #change point shapes by station
                    color=Station_labels)) #change point colors by station
    a <- a + geom_point(size = 4) #set the point size
    a <- a + xlab("") + ylab(paste0("ug/L")) + ggtitle(paste0(B, " 2014")) #write the labels
    a <- a + facet_wrap(~Analyte, drop=TRUE, scales = "free_y")
    a <- a + scale_x_date(breaks=unique(subset.B$date), labels=format(unique(subset.B$date), format="%m/%d"))
    a <- a + theme(axis.text.x  = element_text(angle=90, vjust=0.5, color="black", size=10))
    a <- a + theme(axis.text.y  = element_text(color="black", size=10))
    a <- a + guides(shape = guide_legend(ncol = 2)) #legend in two columns
    a <- a + theme(legend.position="bottom", legend.direction="vertical", legend.text=element_text(size=10), #move legend
                   legend.title=element_blank()) #remove title from legend
    a
    a <- arrangeGrob((a), sub = textGrob(paste0("prepared by Julia Crown ", Sys.Date()), 
                                         x = 0, hjust = -0.1, vjust=0.1,
                                         gp = gpar(fontface = "italic", fontsize = 8))) 
    ggsave(filename = paste0(outpath.plot.points, "multiplot_", B, "_2014_savedon", Sys.Date(),".jpg"), plot = a, scale=1.5)
  }
}


##################################################################################

##################################################################################

####For each station, graph all analytes
#detections0 <- subset(detections, Station_Number == 36179)

herbicides <- c("Atrazine", "Desethylatrazine", "Deisopropylatrazine", "Sulfometuron-methyl", "Pendimethalin", "Diuron", "Simazine", "Metolachlor", "Dichlobenil", "2,6-Dichlorobenzamide", "2,4-D", "Imazapyr", "Hexazinone", "Metribuzin", "Dicamba", "Dimethenamid", "Fluridone", "Trifluralin", "Chlorpropham", "Prometon", "Bromacil", "Triclopyr", "Acetochlor", "Oxyfluorfen", "Napropamide", "Pronamide", "Norflurazon", "Terbacil", "EPTC")


insecticides <- c( "Carbaryl", "Imidacloprid", "Carbofuran", "Chlorpyrifos", "Ethoprop", "Methomyl", "Oxamyl", "Acetamiprid", "Dimethoate", "Diazinon", "Methiocarb", "4,4´-DDD", "4,4´-DDE")


fungicides <- c("Propiconazole", "Pyraclostrobin")

wood.preservative <- "Pentachlorophenol"
PCP <- "DEET"

xicides <- herbicides     

#i <- 32068
for(i in station.list){
  subset.points <- subset(detections, Station_Number == i)
  subset.points <- subset(subset.points, Analyte != "Total Solids")
  analyte0 <- unique(subset.points$Analyte)
  
  #   col.v <- seq(1, 31)
  col.v <- rep("antiquewhite4", 31)
  pch.v <- c(seq(15, 25), seq(1, 14))
  #   ddd <- numeric(0)
  #   for(ii in analytes){
  #     bbb <- subset(subset.points, Analyte == ii)
  #     ccc <- length(bbb$Analyte)
  #     ddd <- c(ddd, ccc)
  #   }
  #   pch.v <- numeric(0)
  #   for(d in 1:31){
  #     aaa <- rep(d, ddd[d])
  #     pch.v <- c(pch.v, aaa)
  #   }
  
  
  if(length(subset.points$RESULT_clean)>0){
    x.min <- min(subset.points$date) #min of subset date
    x.max <- max(subset.points$date) #max of subset date
    x.lim <- c("2013/03/25", "2013/11/20")
    x.lim <- if(length(subset.points$RESULT_clean) >= 1){
      as.Date(x.lim, "%Y/%m/%d")  
    }else{
      c(x.min, x.max) ####define the data domain for graph
    }
    y.min <- 0
    y.max <- max(subset.points$RESULT_clean.ug.l) #max of data for graph
    #    if(ii == "Chlorpyrifos") y.max <- 0.083 #exception to accomodate chlorpyrifos secondary WQS
    y.lim <- c(y.min,y.max + (0.1*y.max)) ####define the data range
    x.lab <- "2013"
    y.lab <- "ug/L"
    title <- paste0("Number of analytes", length(analyte0))
    file.name.ts <- paste0(i, "_herbicides", "_timeseries.png")
    
    png(filename=file.name.ts ,width = 700, height = 400) ####create a png with the station name in the filepath specified above
    par(xpd=NA,oma=c(0,0,4,0), mar=c(5.1,4.1,1.1,2.1)) 
    plot(subset.points$date, subset.points$RESULT_clean.ug.l, pch=NA, xlim=x.lim, ylim=y.lim, xlab=x.lab, ylab=y.lab, cex.axis=1.2, cex.lab=1.2, bty="L", log=log.scale) ####plot the outline of the points  
    
    for(p in 1:(length(xicides))){
      subset.points.i <- subset(subset.points, Analyte == xicides[p])
      points(subset.points.i$date, subset.points.i$RESULT_clean.ug.l, col=col.v[p], pch=pch.v[p], cex=1.8)
      exceeds.points.i <- subset.points.i[subset.points.i$final_digress == 1,]   
      points(exceeds.points.i$date, exceeds.points.i$RESULT_clean.ug.l, col="red", bg="red", pch=pch.v[p], cex=1.8) ####plot the exceedances
    }
    
    
    #numeric.criterion.DEQ <- as.numeric(min.criteria[min.criteria$criteria.Pollutant == ii,'min.DEQ.criteria'])
    #numeric.criterion.EPA <- as.numeric(min.criteria[min.criteria$criteria.Pollutant == ii,'min.EPA.criteria'])
    
    #     for(ii in analytes){
    #       subset.points.i <- subset(subset.points, Analyte == ii)
    #       points(subset.points.i$date, subset.points.i$RESULT_clean.ug.l, col=col.v, pch=pch.v)
    #     }
    #     
    # ii <- "Bifenthrin"
    # ii <- "Chlorpyrifos"
    # 
    #     for(ii in analytes){
    #       if(ii %in% unique(digressions$Analyte)){
    #         exceeds.points.i <- subset.points.i[subset.points.i$final_digress == 1,]   
    #         points(exceeds.points.i$date, exceeds.points.i$RESULT_clean.ug.l, col="red", bg="red", pch=pch.v) ####plot the exceedances
    #       }
    #     }
    
    
    #     ###legend for displaying on charts
    #        legend("topright", 
    #                  legend=xicides, 
    #                  col= col.v, 
    #                  pch=pch.v, 
    #                  ncol=3,
    #                  xjust=0, yjust=0, box.lty=0, cex=1.2, pt.cex=1.8, horiz=FALSE, 
    #           )
    
    dev.off() ####write the .png
  }else{print(paste0("non-detect_", ii))}
}
