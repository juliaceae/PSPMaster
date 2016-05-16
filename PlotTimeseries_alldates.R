#This script should: Point to data source.  Format data. Compare data to standard. (Graph the timeseries moved to later script).
#Basin and data range
#authorship
####

library(stringr)
library(plyr) # New addition to the preamble

#new function from Jason Law via Peter Bryant
#if multiple results in one day, then take the lowest MRL or highest concentration
resolveMRLs <- function(ids, dnd, results){
  
  dnd.sum <- ave(dnd, ids, FUN = sum)
  cases   <- findInterval(dnd.sum, c(0, 1, 2))
  
  id.max <- ave(results, ids, FUN = max)
  id.min <- ave(results, ids, FUN = min)
  
  i0 <- cases == 1 & id.min == results
  i1 <- cases == 2 & dnd == 1
  i2 <- cases == 3 & id.max == results
  
  return(i0 | i1 | i2)
}

#function by Peter Bryant
remove.dups <- function(tname) {
  no.dups <- aggregate(RESULT_MRL ~ code, data = tname, FUN = max)
  tname <- tname[!duplicated(tname$code),]
  tname <- merge(no.dups, tname, by = 'code')
  #tname$tResult <- round(tname$tResult.x, 2)
  tname$tResult <- tname$RESULT_MRL.x
  tname <- within(tname, rm(RESULT_MRL.x, RESULT_MRL.y))
}


####Run "P:\Rscripts\Criteria\ToxicsCriteriaPSP.R" first.
source('P:/Rscripts/Criteria/ToxicsCriteriaPSP.R', encoding = 'UTF-8')
#these lines deprecated by changes in Sourced code above
#outpath.criteria <- paste("\\\\Deqhq1\\PSP\\Rscripts\\Criteria\\",Sys.Date(), "\\", sep="") 
#load(paste0(outpath.criteria,"min.Aquatic.Life.criteria.values_savedon", Sys.Date(),".Rdata"))

#Create and point to new directories
new.folder <- dir.create(paste("\\\\Deqhq1\\PSP\\Rscripts\\Alldates\\",Sys.Date(), sep="")) 
outpath.plot.points <- paste("\\\\Deqhq1\\PSP\\Rscripts\\Alldates\\",Sys.Date(), "\\", sep="") 
setwd(outpath.plot.points)

#### Load LASAR file ####
#
#lasar <- read.csv('//deqhq1/psp/rscripts/datapullfromlead/psp2005to2011compile20140121Version2.csv')
#lasar dump provided by Brian Boling 20150313
lasar <- read.csv('\\\\deqlab1\\wqm\\PSP\\Data\\LASARDataPull1995Current\\PSPDataLASAR_1995onDatawMethodCode.csv', stringsAsFactors=FALSE)

#Remove unit from parameter.name
#lasar$PARAMETER.NAME <- str_trim(gsub('\\(.*','',lasar$PARAMETER.NAME))

#Convert to consistent Project names
# Project Names aren't matching with the correct basin
# revalue_vector <- c()
# for (i in 1:length(levels(lasar$Sampling.Subproject.Name))) {
#   revalue_vector <- c(revalue_vector, strsplit(levels(lasar$Sampling.Subproject.Name), " P")[[i]][1])
#   names(revalue_vector)[i] <- levels(lasar$Sampling.Subproject.Name)[i]
# }
# lasar$Sampling.Subproject.Name <- revalue(lasar$Sampling.Subproject.Name, replace = revalue_vector)
# levels(lasar$Sampling.Subproject.Name) <- gsub(' Basin','',levels(lasar$Sampling.Subproject.Name))


#Match the Element column names Basin, Station_Number, Station_Description, date, Analyte, RESULT, MRL, Units, SampleType, RESULT_clean, "RESULT_clean.ug.l"=NA, "RESULT_clean.ug.l.subND"=NA
#lasar$Client <- 'Pesticide Stewardship Partnerships'
#lasar$Project <- ""
lasar <- rename(lasar, c('Sampling_Event' = 'Work_Order', 
                         #'Sampling.Subproject.Name' = 'Project',
                         'Station' = 'Station_ID',
                         'Station_Description' = 'Station_Description',
                         'Sampling_Date' = 'Sampled_Date',
                         'AnalyteName' = 'OrigAnalyte',
                         'method_reporting_limit' = 'MRL',
                         'RESULT' = 'Result',
                         'UNIT' = 'Units',
                         'Sample_Type' = 'SampleType',
                         'STATUS' = 'DQL',
                         'Sample_Matrix' = 'Matrix',
                         'method_CODE' = 'method_CODE'))
lasar <- lasar[,c("Work_Order", 
                  #"Project", 
                  "Station_ID", 
                  "Station_Description", 
                  "Sampled_Date", 
                  "OrigAnalyte", 
                  "MRL", 
                  "Result", 
                  "Units", 
                  "SampleType", 
                  "DQL", 
                  "Matrix"#, 
                  #"method_CODE"
                  )]
#Convert lasar datetime
lasar$Sampled_Date <- as.Date(strptime(lasar$Sampled_Date, format="%m/%d/%Y")) 
#lasar$Sample.Date.Time <- as.POSIXct(strptime(lasar$Sample.Date.Time, format = '%m/%d/%Y %H:%M'))

#delete samples that came from a duplicate method
# methods2 <- c('Alachlor', 
#               'Atrazine', 
#               'Azinphos-methyl', 
#               'Metolachlor', 
#               'Metribuzin', 
#               'Propazine', 
#               'Simazine')

#add the Basin to each sample point 
basins <- read.csv('P:\\GIS\\PSP_basins\\PSP_Basins_20150427\\PSPDataLASAR_1995onUniqueStations2.csv', stringsAsFactors=FALSE) #This file came from a GIS join. 
basins <- rename(basins, c('PSP_Name' = 'Project', 
                           'Station' = 'Station_ID')) 
basins[basins$Project == "Hood", "Project"] <- "Hood River"
basins[basins$Project == "Molalla-Pudding", "Project"] <- "Pudding"

lasar.basin <- merge(basins, lasar, by.x="Station_ID", by.y="Station_ID", all.x=TRUE)
lasar.basin[lasar.basin$Station_Description == "Cow Creek at mouth", "Project"] <- "South Umpqua"
lasar.basin[lasar.basin$Station_Description == "Doane Creek at Hwy 212", "Project"] <- "Clackamas"
lasar.basin[lasar.basin$Station_Description == "East Little Walla Walla River north of Stateline Road", "Project"] <- "Walla Walla"
lasar.basin <- lasar.basin[lasar.basin$Project != " ", ]


lasar.basin <- lasar.basin[lasar.basin$Station_Description != "Overstreet Drain (OWYDRN001)", ]
lasar.basin <- lasar.basin[lasar.basin$Station_Description != "Rose Creek upstream of Sieben Parkway", ]
#[4] "Fletcher Drain (OWYDRN002)"                           
#[5] "Overstreet Drain (OWYDRN001)"                         
#[6] "Rose Creek upstream of Sieben Parkway"    



require(plyr)

## load package for ODBC
#install.packages("RODBC")
library(RODBC)
## connect to element and get data
channel <- odbcConnect("element")
## get the names of all the tables in the database
TableNames<- sqlTables(channel,errors = FALSE)
## test example query
#tmp.test <- sqlFetch(channel, "dbo.Repo_Result", stringsAsFactors=FALSE, max=20)
#looking at Qualifier Code Notes. 
#tmp.test <- sqlQuery(channel, "SELECT * FROM Repo_Result_Qualifers WHERE Work_Order = 1503095 AND Client like '%Pesticide%'", stringsAsFactors=FALSE)
#View(tmp.test)
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

## This line retreives all the pesticide samples received by the lab since 2012 (everything in element).  The query language is written in SQL.
qry <- paste0("SELECT * FROM dbo.Repo_Result WHERE  Client LIKE '%Pesticide%' ")
qry <- paste0("SELECT * FROM dbo.Repo_Result WHERE  Analyte LIKE '%Linuron%' ")
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
element <- mydata

sort(unique(mydata$Work_Order))
unique(mydata$Project)
# ####check for new data
# oldpath <-"\\\\Deqhq1\\PSP\\Rscripts\\2014\\old\\20140612\\"
# oldfile <- "State_2014_mydata_clean_noV.csv"
# old.data <- read.csv(paste0(oldpath, oldfile), colClasses = "character")
# old.data.n  <- nrow(old.data)#$Station_Number)
# new.data.n <- nrow(mydata)
# if(new.data.n > old.data.n) print("NEW DATA! NEW DATA! NEW DATA! NEW DATA! NEW DATA AVAILABLE!")
# ####

library(stringr)


#before the tables are joined, reformat the element dates (which were in a different format than the lasar dump dates)
mydata$Sampled_Date <- as.Date(strptime(mydata$Sampled_Date, format="%d %b %Y")) 

mydata <- rbind(mydata[,c('Project', 'Station_ID', 'Station_Description', 'Sampled_Date', 'OrigAnalyte', 'Result', 'MRL', 'Units', 'SampleType', 'DQL', 'Matrix')], 
                lasar.basin[,c('Project', 'Station_ID', 'Station_Description', 'Sampled_Date', 'OrigAnalyte', 'Result', 'MRL', 'Units', 'SampleType', 'DQL', 'Matrix')])

mydata_clean <- rename(mydata, 
                 c('Project' = 'Basin', 
                   'Station_ID' = 'Station_Number', 
                   'Station_Description' = 'Station_Description', 
                   'Sampled_Date' = 'date', 
                   'OrigAnalyte' = 'Analyte', 
                   'Result' = 'RESULT', 
                   'MRL' = 'MRL', 
                   'Units' = 'Units', 
                   'SampleType' = 'SampleType', 
                   'DQL' = 'DQL', 
                   'Matrix' = 'Matrix')) 
                
#manipulations to the combined LASAR/element set
mydata_clean$Station_Number <- as.numeric(mydata_clean$Station_Number)
mydata_clean$RESULT.raw <- mydata_clean$RESULT
mydata_clean$RESULT <- str_trim(mydata_clean$RESULT)
mydata_clean$RESULT_clean.ug.l <- as.numeric(NA)
mydata_clean$RESULT_clean.ug.l.subND <- as.numeric(NA)
mydata_clean$Analyte <- str_trim(mydata_clean$Analyte)

#Deleting the 2nd column [2C] tag from ELEMENT
mydata_clean$Analyte <- gsub(" \\[2C\\]$","",mydata_clean$Analyte)

####This function is two years old and based in Dan's python world.  Simplify me!
get.cases <- function(chk.values) {
  ## Checks for non-numeric values in the vector "chk.values", which should
  ## be a character vector. A data.frame is returned with the non-numeric
  ## values (cases) and the number of occurrences for each case. If there
  ## are only numeric values in the input vectore, the entries in the 
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

sub.cases <- function(data.in,sub.table){
  ## Replaces non-numeric values of data.in with the correspoinding elements
  ## in sub.table. The sub.table dataframe should be generated using the 
  ## get.cases function
  ## Created by Kevin Brannan
  ## Version 1.0.0.09.20.2012
  for(ii in 1:length(sub.table$Sub)){
    sub.index <- data.in == sub.table$Case[ii]  #grep(sub.table$Case[ii],data.in, fixed = TRUE)
   # print(paste("Number of sub for ", sub.table$Case[ii], " is ",sub.table$Sub[ii],sep=""))
    if(length(sub.index)> 0){
      data.in[data.in == sub.table$Case[ii]] <- as.character(sub.table$Sub[ii])
      rm(sub.index)
    }
  }
  return(data.in)
}

report <- get.cases(mydata_clean$RESULT)
#report

####To the report above, add a column called "Sub" and populate with substituted values. This is the value that will be substituted.
lst.split <- strsplit(as.character(report$Case), split = ' ')
for (i in 1:length(lst.split)){
  report$Sub[i]  <- ifelse(substr(lst.split[[i]][1],1,1) == '<',"ND",lst.split[[i]][1])
}

#reorder the report and view a subset by row number
#report[order(report$Case),][1000:1001,]
####Check the report$Sub for unacceptable substitutions.  MANUAL clean up with final values.
report[report$Case == "0.01Est", "Sub"] <-  0.01
report[report$Case == "None detected", "Sub"] <-  "ND"
report[report$Case == "Co-elution", "Sub"] <-  "Void"

####Also, create a column called "RESULT".  Populate column with the substituted values.
mydata_clean$RESULT <- sub.cases(mydata_clean$RESULT, report) #just use the report object created in step 02_LASAR_clean.R

####turns empty fields into NAs.
#report$SubFinal <- as.numeric(report$SubFinal) 
mydata_clean$RESULT_clean <- as.numeric(mydata_clean$RESULT) 

##############################################################################################################################
###############################################################
#Need to convert units to common unit
table(mydata_clean$Matrix)
mydata_clean <- mydata_clean[!mydata_clean$Matrix %in% c('Ditch/Pond/Culvert/Drain','Groundwater','Municipal Effluent'),]
mydata_clean <- mydata_clean[!mydata_clean$RESULT.raw %in% c('Co-elution'),]
# Clean up data
sort(unique(mydata_clean$RESULT)) #verify that the names in quotes in the command are the names being used in the datatable
mydata_clean <- subset(mydata_clean, RESULT != "Void" & RESULT != "Cancelled")
unique(mydata_clean$SampleType) #verify that the names in quotes in the command are the names being used in the datatable
mydata_clean <- subset(mydata_clean, Station_Number != 10000)
mydata_clean$MRL_raw <- mydata_clean$MRL
#if no MRL, use the raw Result
mydata_clean[is.na(mydata_clean$MRL),'MRL'] <- gsub("<","",mydata_clean[is.na(mydata_clean$MRL),'RESULT.raw'])
mydata_clean$MRL <- as.numeric(mydata_clean$MRL)

mydata_clean$RESULT_MRL <- ifelse(is.na(mydata_clean$RESULT_clean),mydata_clean$MRL,mydata_clean$RESULT_clean)

#If multiple samples or methods used on one station on one day for one analyte, 
#take the lowest MRL or highest detected concentration. 
mydata_clean$code <- paste(mydata_clean$Station_Number,mydata_clean$date,mydata_clean$Analyte)
mydata_clean$dnd <- ifelse(mydata_clean$RESULT == 'ND',0,1)
sub <- with(mydata_clean, resolveMRLs(code, dnd, RESULT_MRL))
mydata.wo.dup.MRLs <- mydata_clean[sub,]
mydata.wo.dups <- remove.dups(mydata.wo.dup.MRLs)


# back to the mydata_clean convention
mydata_clean_bu <- mydata_clean
mydata_clean <- mydata.wo.dups
####################################################################

####Subset and set aside the field duplicates ----
unique(mydata_clean$SampleType) 

####deleting "Other::Ot" from SampleType (1/20/15 JC: right now, the Others are ONLY associated with Project/Basin"Lab Spike POCIS/SPMD", reaffirmed 5/28/15 JC.)
#mydata_clean <- mydata_clean[mydata_clean$SampleType != "Other::Ot",]

#Separate water grab and composite samples
unique(mydata_clean$Matrix) 
Sediment <- mydata_clean[mydata_clean$Matrix == "Sediment",]
POCIS <- mydata_clean[mydata_clean$Matrix == "POCIS" | mydata_clean$Matrix == "POCIS - Surface Water", ]
SPMD <- mydata_clean[mydata_clean$Matrix == "SPMD" | mydata_clean$Matrix == "SPMD::LAB", ]
mydata_clean <- mydata_clean[mydata_clean$Matrix == "River/Stream" | mydata_clean$Matrix == "Surface water", ]


####Subset out not needed data
station.list <- unique(mydata_clean$Station_Number) #list of stations
sort(unique(mydata_clean$Analyte)) #list of lab analytes

######################################
Names.match <- read.csv("\\\\deqhq1\\PSP\\Rscripts\\Criteria\\1998-2015NameMatchUp.csv", stringsAsFactors=FALSE)
mydata_clean <- merge(x= mydata_clean, y= Names.match, by.x= "Analyte", by.y= "Analyte.Name", all.x=TRUE)
mydata_clean$Analyte <- ifelse(mydata_clean$Element.Name == "", mydata_clean$Analyte, mydata_clean$Element.Name) 

detections <- mydata_clean[mydata_clean$dnd == 1,]
analytes <- unique(detections$Analyte) #list of detected analytes
######################################

# subset detections
# detections <- mydata_clean[mydata_clean$dnd == 1,]
# analytes <- unique(detections$Analyte) #list of detected analytes
# analytes <- unique(mmm$Analyte) #list of detected analytes
# sort(analytes)

####Establish Benchmarks and Exceedances
####Currently doing this step in "ToxicsCriteriaPSP.R"  
min.criteria <- min.AQL.1 
min.criteria <- rename(min.criteria , replace = c('Pollutant' = 'criteria.Pollutant',
                                                  'min.state.AQL' = 'min.DEQ.criteria', 
                                                  'min.other.AQL' = 'min.EPA.criteria', 
                                                  'min.AQL.0' = 'criteria.minimum.criteria.benchmark.value'))
######################################
#matching the analytes name with the min.criteria name----
#recursive until Has.min.criteria is all TRUE 

criteria.pollutant.list <- unique(min.criteria$criteria.Pollutant)
Has.min.criteria <- analytes %in% criteria.pollutant.list #Caution!!"analytes" comes from the detections subset only - so NOT all the available criteria will be populated into later datasets!! It WILL skip mismatched (between LEAD analyte name and criteria name) nondetects!!
check <- data.frame(Has.min.criteria, analytes)
check <- check[order(analytes),]   #no minimum criteria/benchmarks exist for Total Solids or DEET or Pronamide or 2,6-BAM, Etridiazole, Mexacarbate, Triadimefon, 3,5-Dichlorobenzoic acid
check#end recursion 


######################################
#2,4-D #Addessed in ToxicsCriteriaPSP.R
#Rest of benchmark name mismatches are addressed here or commented out if there is no benchmark (currently)
#2,4,5-TP (Silvex)
min.criteria[min.criteria$criteria.Pollutant == '2,6-Dichlorobenzamide (BAM)','criteria.Pollutant'] <- "2,6-Dichlorobenzamide" #example for substitutions (first is old name in criteria list, second is new analyte name)
#3,5-Dichlorobenzoic acid
min.criteria[min.criteria$criteria.Pollutant == '4,4`-DDD','criteria.Pollutant'] <- "4,4´-DDD" #example for substitutions (first is old name in criteria list, second is new analyte name)
min.criteria[min.criteria$criteria.Pollutant == '4,4`-DDE','criteria.Pollutant'] <- "4,4´-DDE" #example for substitutions (first is old name in criteria list, second is new analyte name)
min.criteria[min.criteria$criteria.Pollutant == '4,4`-DDT','criteria.Pollutant'] <- "4,4´-DDT" #example for substitutions (first is old name in criteria list, second is new analyte name)
min.criteria[min.criteria$criteria.Pollutant == 'Acifluorfen (Sodium)','criteria.Pollutant'] <- "Acifluorfen" #example for substitutions (first is old name in criteria list, second is new analyte name)
min.criteria[min.criteria$criteria.Pollutant == "aminomethyl phosphoric acid (AMPA) Glyphosate degradate", "criteria.Pollutant"] <- "Aminomethylphosphonic acid (AMPA)" #example for substitutions (first is old name in criteria list, second is new analyte name)
min.criteria[min.criteria$criteria.Pollutant == "Azinphos methyl", "criteria.Pollutant"] <- "Azinphos-methyl (Guthion)" #example for substitutions (first is old name in criteria list, second is new analyte name)
#Azinphos Methyl Oxygen Analog
min.criteria[min.criteria$criteria.Pollutant == "Propoxur", "criteria.Pollutant"] <- "Baygon (Propoxur)" #example for substitutions (first is old name in criteria list, second is new analyte name)
#Chlorpyrifos oxon
#cis-Chlordane
#Conductivity
#DCPA acid metabolites
#DCPA acid metabolites(a)
#DEET
min.criteria[min.criteria$criteria.Pollutant == '2,6-Dichlorobenzamide (BAM)','criteria.Pollutant'] <- "2,6-Dichlorobenzamide" #example for substitutions (first is old name in criteria list, second is new analyte name)
deltaBHC <- min.criteria[1,] #Table 31 guidance value
deltaBHC[,1] <- "delta-BHC"
deltaBHC[,2] <- as.numeric()
deltaBHC[,3] <- as.numeric()
deltaBHC[,4] <- 100
min.criteria <- rbind(min.criteria, deltaBHC)
#Dissolved Oxygen
#Dissolved Oxygen, Saturation
min.criteria[min.criteria$criteria.Pollutant == 'Endosulfan Sulfate','criteria.Pollutant'] <- "Endosulfan sulfate" #example for substitutions (first is old name in criteria list, second is new analyte name)
#Endrin aldehyde
#Etridiazole
min.criteria[min.criteria$criteria.Pollutant == "Esfenvalerate", "criteria.Pollutant"] <- "Fenvalerate+Esfenvalerate" #example for substitutions (first is old name in criteria list, second is new analyte name)
min.criteria[min.criteria$criteria.Pollutant == "Lindane", "criteria.Pollutant"] <- "gamma-BHC (Lindane)" #example for substitutions (first is old name in criteria list, second is new analyte name)
min.criteria[min.criteria$criteria.Pollutant == "Heptachlor Epoxide", "criteria.Pollutant"] <- "Heptachlor epoxide" #example for substitutions (first is old name in criteria list, second is new analyte name)
#Malathion Oxygen Analog
min.criteria[min.criteria$criteria.Pollutant == 'MCPA EHE','criteria.Pollutant'] <- "MCPA" #example for substitutions (first is old name in criteria list, second is new analyte name)
min.criteria[min.criteria$criteria.Pollutant == 'MCPP-p DMAS','criteria.Pollutant'] <- "MCPP" #example for substitutions (first is old name in criteria list, second is new analyte name)
min.criteria[min.criteria$criteria.Pollutant == 'Methyl parathion','criteria.Pollutant'] <- "Methylparathion" #example for substitutions (first is old name in criteria list, second is new analyte name)
min.criteria[min.criteria$criteria.Pollutant == 'Metsulfuron','criteria.Pollutant'] <- "Metsulfuron Methyl" #example for substitutions (first is old name in criteria list, second is new analyte name)
#Mexacarbate
#Phosmet Oxygen Analog
#Pronamide
#Simetryn
#Temperature
#Terbutryn (Prebane)
min.criteria[min.criteria$criteria.Pollutant == 'Terbuthylazine','criteria.Pollutant'] <- "Terbutylazine" #example for substitutions (first is old name in criteria list, second is new analyte name)
#Total Solids
#Turbidity


#min.criteria[min.criteria$criteria.Pollutant == 'Triazine DIA degredate','criteria.Pollutant'] <- "Deisopropylatrazine" #example for substitutions (first is old name in criteria list, second is new analyte name)
#min.criteria[min.criteria$criteria.Pollutant == 'Triazine DEA degredate','criteria.Pollutant'] <- "Desethylatrazine" #example for substitutions (first is old name in criteria list, second is new analyte name)
#change min.criteria table - replace criteria value for 2,4-D with 2,4-D acids and salts
#aaa <- as.numeric(min.criteria[min.criteria$criteria.Pollutant == "2,4-D acids and salts",'criteria.minimum.criteria.benchmark.value'])#benchmark for 2,4-D acids and salts
#min.criteria[criteria$Pollutant == '2,4-D','criteria.minimum.criteria.benchmark.value'] <- aaa 
#min.criteria <- min.criteria[-(446), ] #delete repeated imidacloprid



######################################
min.criteria <- subset(min.criteria, (min.criteria$criteria.minimum.criteria.benchmark.value) != "")

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

min.criteria[min.criteria$criteria.Pollutant == "Pentachlorophenol", "label"] <- (paste0("\nDEQ WQS = ", min.criteria[min.criteria$criteria.Pollutant == "Pentachlorophenol", "min.DEQ.criteria"], " ug/L at assumed pH 7.8")) #Pentachlorophenol Freshwater WQS criteria are pH dependent
min.criteria[min.criteria$criteria.Pollutant=="4,4´-DDE", "label"] <- paste0("\nlowest DEQ WQS= 0.001 ug/L\n*criterion applies to DDT and its metabolites")
min.criteria[min.criteria$criteria.Pollutant=="4,4´-DDD", "label"] <- paste0("\nlowest DEQ WQS= 0.001 ug/L\n*criterion applies to DDT and its metabolites")
min.criteria[min.criteria$criteria.Pollutant=="Atrazine", "label"] <- paste0("\nEPA benchmark = 1 ug/L\nproposed EPA benchmark = 0.001 ug/L")
min.criteria[min.criteria$criteria.Pollutant=="Simazine", "label"] <- paste0("\nEPA benchmark = 36 ug/L\nproposed EPA benchmark = 2.24 ug/L")

####duplicate dataset.
mydata_clean_noV <- mydata_clean
#rm(mydata_clean)

####fill out ug/L column----
unique(mydata_clean_noV$Units)
#[1] "µg/L"     "ppb"      "ng/L"     "mg/L"     "µmhos/cm" "%"       
#[7] "°C"       "NTU"   
mydata_clean_noV[mydata_clean_noV$Units == "ppb", "Units"] <- "µg/L"
mydata_clean_noV[mydata_clean_noV$Units == "mg/L", "RESULT_clean.ug.l"] <-mydata_clean_noV[mydata_clean_noV$Units == "mg/L", ]$RESULT_clean*1000
mydata_clean_noV[mydata_clean_noV$Units == "ng/L", "RESULT_clean.ug.l"] <- mydata_clean_noV[mydata_clean_noV$Units == "ng/L", ]$RESULT_clean/1000
mydata_clean_noV[mydata_clean_noV$Units == "µg/L", "RESULT_clean.ug.l"] <- mydata_clean_noV[mydata_clean_noV$Units == "µg/L", ]$RESULT_clean
mydata_clean_noV <- mydata_clean_noV[mydata_clean_noV$Units %in% c("mg/L", "ng/L", "µg/L"),]

####Substitute the NDs for zeroes in a new column
mydata_clean_noV[mydata_clean_noV$dnd == 0, "RESULT_clean.ug.l.subND"] <- 0
mydata_clean_noV[mydata_clean_noV$dnd == 1, "RESULT_clean.ug.l.subND"] <- mydata_clean_noV[mydata_clean_noV$RESULT != "ND", ]$RESULT_clean.ug.l

####Determine minimum benchmark exceedances

########
  ddd <- match(mydata_clean_noV$Analyte, min.criteria$criteria.Pollutant)
  mydata_clean_noV$benchmark.DEQ <- as.numeric(min.criteria$min.DEQ.criteria[ddd])   #make a column of appropriate benchmark
  mydata_clean_noV$benchmark.EPA <- as.numeric(min.criteria$min.EPA.criteria[ddd])   #make a column of appropriate benchmark  
  mydata_clean_noV$relevant.AL.benchmark <- as.numeric(min.criteria$criteria.minimum.criteria.benchmark.value[ddd])   #make a column of appropriate benchmark
  mydata_clean_noV$final_digress <- ifelse(mydata_clean_noV$RESULT_clean.ug.l > mydata_clean_noV$relevant.AL.benchmark, 1,0) #make column with digression stations (T/F)

# for(i in 1:nrow(mydata_clean_noV)){
#   ccc <- mydata_clean_noV$Analyte[i]
#   ddd <- match(ccc, min.criteria$criteria.Pollutant)
#   mydata_clean_noV$benchmark.DEQ[i] <- as.numeric(min.criteria$min.DEQ.criteria[ddd])   #make a column of appropriate benchmark
#   mydata_clean_noV$benchmark.EPA[i] <- as.numeric(min.criteria$min.EPA.criteria[ddd])   #make a column of appropriate benchmark  
#   mydata_clean_noV$relevant.AL.benchmark[i] <- as.numeric(min.criteria$criteria.minimum.criteria.benchmark.value[ddd])   #make a column of appropriate benchmark
#   mydata_clean_noV$final_digress[i] <- ifelse(mydata_clean_noV$RESULT_clean.ug.l[i] > mydata_clean_noV$relevant.AL.benchmark[i], 1,0) #make column with digression stations (T/F)
# }
digressions <- (mydata_clean_noV[is.na(mydata_clean_noV$final_digress) == FALSE & mydata_clean_noV$final_digress == 1,])
index <-  (order(digressions$Basin))
digressions <- digressions[(index),]
#View(digressions)

#### Determine percent digression of criteria
mydata_clean_noV$percent.benchmark <- mydata_clean_noV$RESULT_clean.ug.l/mydata_clean_noV$relevant.AL.benchmark
mydata_clean_noV$exceed.type <- NA
# 
#     mydata_clean_noV[is.na(mydata_clean_noV$RESULT_clean) == FALSE & is.na(mydata_clean_noV$relevant.AL.benchmark) == TRUE, "exceed.type"] <- "no benchmark available"#result is a detection AND benchmark does NOT exist
#       #percent.benchmark is a real number
#     mydata_clean_noV[is.na(mydata_clean_noV$percent.benchmark)==FALSE) & 
#         if(mydata_clean_noV$percent.benchmark < 0.1){
#           mydata_clean_noV$exceed.type <- "less than ten percent of benchmark"  
#         }else{
#     mydata_clean_noV[is.na(mydata_clean_noV$percent.benchmark)==FALSE) &
#if(mydata_clean_noV$percent.benchmark >= 0.1 & mydata_clean_noV$percent.benchmark < 0.5){
#             mydata_clean_noV$exceed.type <- "between ten and fifty percent of benchmark"
#           }else{
#mydata_clean_noV[is.na(mydata_clean_noV$percent.benchmark)==FALSE) &
#  if(mydata_clean_noV$percent.benchmark >= 0.5 & mydata_clean_noV$percent.benchmark < 1.0){
#               mydata_clean_noV$exceed.type <- "between fifty and 100 percent of benchmark"
#             }else{
# mydata_clean_noV[is.na(mydata_clean_noV$percent.benchmark)==FALSE) &
#    if(mydata_clean_noV$percent.benchmark > 1.0){
#                 mydata_clean_noV$exceed.type <- "greater than 100 percent of benchmark"
#               }
#             }
#           }
#         }
#       }
#     }
#   }
 
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
unique(aaa$Analyte) #confirmed, no criteria for TS, and DEET, 2,6-BAM, pronamide, chlorpropham, acetamiprid, mexacarbate, etridiazole, Triadimefon
#changed criteria for 2,4-D

#rm(mydata)

##re-make detections subset with the new columns
detections <- subset(mydata_clean_noV, is.na(RESULT_clean) == FALSE) #subset out the NDs 
index <-  (order(detections$Basin))
detections <- detections[(index),]


###########################
#mydata_clean_noV[mydata_clean_noV$dnd == 0, "RESULT_clean.ug.l.neg"] <- -999
#mydata_clean_noV[mydata_clean_noV$dnd == 1, "RESULT_clean.ug.l.neg"] <- mydata_clean_noV$RESULT_clean.ug.l
mydata_clean_noV$RESULT_clean.ug.l.neg <- ifelse(mydata_clean_noV$dnd == 0, -999, mydata_clean_noV$RESULT_clean.ug.l)

# for(i in 1:nrow(mydata_clean_noV)){
#   if(is.na(mydata_clean_noV$RESULT_clean.ug.l[i]) == TRUE){
#     mydata_clean_noV$RESULT_clean.ug.l.neg[i] <- -999  
#   }else{
#     if(is.na(mydata_clean_noV$RESULT_clean.ug.l[i]) == FALSE){
#       mydata_clean_noV$RESULT_clean.ug.l.neg[i] <- mydata_clean_noV$RESULT_clean.ug.l[i]
#     }
#   }
# }

###########################
mydata_clean_noV[mydata_clean_noV$Station_Number == 32010, "Station_Description"] <- "West Prong Little Walla Walla River south of Stateline Rd"  

# for(i in 1:nrow(mydata_clean_noV)){
#   if(mydata_clean_noV$Station_Number[i] == 32010){
#     mydata_clean_noV$Station_Description[i] <- "West Prong Little Walla Walla River south of Stateline Rd"  
#   }
# }
#fix station name from "Little Walla Walla River at The Frog     "  to delete the spaces
mydata_clean_noV[mydata_clean_noV$Station_Number == 32012, "Station_Description"] <- "Little Walla Walla River at The Frog"  

# for(i in 1:nrow(mydata_clean_noV)){
#   if(mydata_clean_noV$Station_Number[i] == 32012){
#     mydata_clean_noV$Station_Description[i] <- "Little Walla Walla River at The Frog"  
#   }
# }

#subset the Basin "Hood River POCIS/SPMD" and assign new name
mydata_clean_noV[mydata_clean_noV$Basin == "Hood River POCIS/SPMD", "Basin"] <- "Hood River POCIS_SPMD"

#change Clackamas basin station names, because too long on the graphs
#df[is.na(df$col),"col"] <- 0
mydata_clean_noV[mydata_clean_noV$Station_Description == "North Fork Deep Creek at Hwy 212 (upstream of Boring)", "Station_Description"] <- "North Fork Deep Creek at Hwy 212"
mydata_clean_noV[mydata_clean_noV$Station_Description == "Rock Creek at 172nd, Stony Brook Court", "Station_Description"] <- "Rock Creek at 172nd"
mydata_clean_noV[mydata_clean_noV$Station_Description == "Noyer Creek at Hwy 212, St. Paul Lutheran Church (North Fork, Deep Creek, Clackamas)", "Station_Description"] <- "Noyer Creek at Hwy 212"
mydata_clean_noV[mydata_clean_noV$Station_Description == "Sieben Creek at Hwy 212 (Clackamas)" , "Station_Description"] <- "Sieben Creek at Hwy 212" 


#### Adding year column to mydata_clean_noV
mydata_clean_noV$year<-as.integer(substr(mydata_clean_noV$date,1,4))
mydata_clean_noV[mydata_clean_noV$Analyte == "Aminomethylphosphonic acid (AMPA)", "Analyte"] <- "AMPA"
#### South Coast and South Umpqua pilot season spans years 2014-2015
mydata_clean_noV[mydata_clean_noV$Basin %in% c("South Coast", "South Umpqua") & mydata_clean_noV$year %in% c(2014, 2015),"year"] <- as.integer(201415)

#### New strategy to generate statistics for "mini-basins" (a subset of stations within a PSP Basin). Duplicate data for mini-basins ####
miniWFPalmer <- mydata_clean_noV[mydata_clean_noV$Station_Description %in% c("West Fork Palmer at SE Lafayette Hwy", 
                                                                    "West Fork Palmer Creek at SE Palmer Creek Road", 
                                                                    "West Fork Palmer at Webfoot Road Bridge"),]
miniWFPalmer$Basin <- "West Fork Palmer"
miniCozine <- mydata_clean_noV[mydata_clean_noV$Station_Description %in% c("Lower Cozine Creek at Davis Street Bridge", 
                                                                           "Middle Cozine at Old Sheridan Road"),]
miniCozine$Basin <- "Cozine Creek"
mydata_clean_noV <- rbind(mydata_clean_noV, miniWFPalmer, miniCozine)
