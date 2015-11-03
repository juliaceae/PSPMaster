#Stormwater MS4
#Julia Crown, Joey Peters
#Combine MS4 data

library(stringr)
library(plyr) 

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
new.folder <- dir.create(paste("\\\\Deqhq1\\PSP\\Rscripts\\2014\\",Sys.Date(), sep="")) 
outpath.plot.points <- paste("\\\\Deqhq1\\PSP\\Rscripts\\2014\\",Sys.Date(), "\\", sep="") 
setwd(outpath.plot.points)

#Read MS4 pesticide data and Rename column headers
#Permittees provided data as requested by DEQ (see raw data in folder in line below). Data combined by Joey Peters, DEQ.  
ms4 <- read.csv('//deqhq1/STORMWATER/Muni Stormwater Program/Program Planning & Admin/4 Science & Technology/Pesticide Monitoring/MS4_Pesticide_Data/Dataframe_Pest_MS4.csv', stringsAsFactors=FALSE)
# ms4 <- rename(ms4, c(
#                          #'Sampling_Event' = 'Work_Order', 
#                          'Permittee' = 'Project',
#                          'Station_ID' = 'Station_ID',
#                          'Station.Name' = 'Station_Description',
#                          'Sample.Date' = 'Sampled_Date',
#                          'Analyte' = 'Analyte',
#                          'QL' = 'MRL',
#                          'Result' = 'Result',
#                          'Unit' = 'Units',
#                          #'Sample_Type' = 'SampleType',
#                          'DL' = 'DQL',
#                          #'Sample_Matrix' = 'Matrix',
#                          'Analysis' = 'method_CODE'))
ms4 <- rename(ms4, c(
                         'Permittee' = 'Basin', 
                         'Station_ID' = 'Station_Number', 
                         'Station.Name' = 'Station_Description', 
                         'Sample.Date' = 'date', 
                         'Analyte' = 'Analyte', 
                         'Result' = 'RESULT', 
                         'QL' = 'MRL', 
                         'Unit' = 'Units', 
                         #'SampleType' = 'SampleType', 
                         'DL' = 'DQL'#, 
                         #'Matrix' = 'Matrix'
                         )) 
# Add column describing Field Duplicate vs. Field Primary
ms4$Sample_Type <- as.character("Field Primary")
ms4[ms4$Station_Description %in% c("Hilfiker DUP", "Electric DUP", "Salem Industrial Dup"), ]$Sample_Type <- "Field Duplicate"
# Use the MRL first.  If no MRL, use the DQL
ms4$ReportLimit <- as.numeric(ms4$MRL)
ms4[is.na(ms4$MRL),]$ReportLimit <- ms4[is.na(ms4$MRL),]$DQL
# Format date
ms4$date <- as.Date(strptime(ms4$date, format="%m/%d/%Y")) 

mydata_clean <- ms4
#manipulations to the combined LASAR/element/MS4 set
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
#report[order(report$Case),][1:23,]
####Check the report$Sub for unacceptable substitutions.  MANUAL clean up with final values.
#report[report$Case == "0.01Est", "Sub"] <-  0.01
#report[report$Case == "None detected", "Sub"] <-  "ND"

####Also, create a column called "RESULT".  Populate column with the substituted values.
mydata_clean$RESULT <- sub.cases(mydata_clean$RESULT.raw, report) #just use the report object created in step 02_LASAR_clean.R

####turns empty fields into NAs.
#report$SubFinal <- as.numeric(report$SubFinal) 
mydata_clean$RESULT_clean <- as.numeric(mydata_clean$RESULT) 

# Clean up data
sort(unique(mydata_clean$RESULT)) #verify that the names in quotes in the command are the names being used in the datatable
unique(mydata_clean$Sample_Type) #verify that the names in quotes in the command are the names being used in the datatable
#[1] "Field Primary"   "Field Duplicate"
#exclude field and transfer blanks
mydata_clean <- subset(mydata_clean, Station_Number != 10000)

# If no MRL or DQL, use the Result
#mydata_clean[is.na(mydata_clean$MRL),'MRL'] <- gsub("<","",mydata_clean[is.na(mydata_clean$MRL),'RESULT.raw'])
mydata_clean[is.na(mydata_clean$ReportLimit),'ReportLimit'] <- mydata_clean[is.na(mydata_clean$ReportLimit),]$'RESULT'
mydata_clean$ReportLimit <- as.numeric(mydata_clean$ReportLimit)
# If the ND, use the MRL (or DQL if no MRL), otherwise, use the result
mydata_clean$RESULT_MRL <- ifelse(is.na(mydata_clean$RESULT_clean),mydata_clean$ReportLimit,mydata_clean$RESULT_clean)

#If multiple samples or methods used on one station on one day for one analyte, take the lowest MRL or highest detected concentration. 
mydata_clean$code <- paste(mydata_clean$Station_Number,mydata_clean$date,mydata_clean$Analyte)
mydata_clean$dnd <- ifelse(is.na(mydata_clean$RESULT_clean), 0, 
                           ifelse(mydata_clean$RESULT_clean >= mydata_clean$ReportLimit, 1, 0))
sub <- with(mydata_clean, resolveMRLs(code, dnd, RESULT_MRL))
mydata.wo.dup.MRLs <- mydata_clean[sub,]
mydata.wo.dups <- remove.dups(mydata.wo.dup.MRLs)

# back to the mydata_clean convention
mydata_clean_bu <- mydata_clean
mydata_clean <- mydata.wo.dups

####Subset out not needed data
station.list <- unique(mydata_clean$Station_Number) #list of stations
sort(unique(mydata_clean$Analyte)) #list of lab analytes
#Deleting the 2nd column [2C] tag from ELEMENT
#mydata_clean$Analyte <- gsub(" \\[2C\\]$","",mydata_clean$Analyte)

# subset detections
detections <- mydata_clean[mydata_clean$dnd == 1,]
analytes <- unique(detections$Analyte) #list of detected analytes
analytes
####obtain the sampling dates
sort(unique(mydata_clean$date))

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
check <- data.frame(Has.min.criteria, (analytes))
check <- check[order(analytes),] #no minimum criteria/benchmarks exist for Total Solids or DEET or Pronamide or 2,6-BAM, Etridiazole, Mexacarbate, Triadimefon, 3,5-Dichlorobenzoic acid, Endrin Ketone
check #end recursion 
noBench <- c("Total Solids", "DEET", "Pronamide", "2,4,5-T", "Benzo(ghi)perylene", "DCBP", "DCPAA", "delta-BHC", "Di-n-octyl phthalate", "Endrin Ketone", "m,p-Xylene", "Naphthalene", "o-Xylene")

#min.criteria <- replace(min.criteria$criteria.Pollutant, min.criteria$criteria.Pollutant == "4,4´-DDE", "4,4'-DDE")
min.criteria[min.criteria$criteria.Pollutant == "4,4`-DDE",'criteria.Pollutant'] <- "4,4'-DDE" #example for substitutions (first is old name in criteria list, second is new analyte name)
min.criteria[min.criteria$criteria.Pollutant == "4,4`-DDD", "criteria.Pollutant"] <- "4,4'-DDD" #example for substitutions (first is old name in criteria list, second is new analyte name)
min.criteria[min.criteria$criteria.Pollutant == "4,4`-DDT", "criteria.Pollutant"] <- "4,4'-DDT" #example for substitutions (first is old name in criteria list, second is new analyte name)
min.criteria[min.criteria$criteria.Pollutant == "Lindane", "criteria.Pollutant"] <- "gamma-BHC (Lindane)" #example for substitutions (first is old name in criteria list, second is new analyte name)
min.criteria[min.criteria$criteria.Pollutant == "Acenaphthene", "criteria.Pollutant"] <- "Acenaphthylene" #example for substitutions (first is old name in criteria list, second is new analyte name)
min.criteria[min.criteria$criteria.Pollutant == "Benzo[b]fluoranthene", "criteria.Pollutant"] <- "Benzo(b)fluoranthene" #example for substitutions (first is old name in criteria list, second is new analyte name)
min.criteria[min.criteria$criteria.Pollutant == "Dibenz[a,h]anthracene", "criteria.Pollutant"] <- "Dibenzo(a,h)anthracene" #example for substitutions (first is old name in criteria list, second is new analyte name)
min.criteria[min.criteria$criteria.Pollutant == 'MCPA EHE','criteria.Pollutant'] <- "MCPA" #example for substitutions (first is old name in criteria list, second is new analyte name)
min.criteria[min.criteria$criteria.Pollutant == 'MCPP-p','criteria.Pollutant'] <- "MCPP" #example for substitutions (first is old name in criteria list, second is new analyte name)

# min.criteria[min.criteria$criteria.Pollutant == 'MCPP-p DMAS','criteria.Pollutant'] <- "MCPP" #example for substitutions (first is old name in criteria list, second is new analyte name)
# min.criteria[min.criteria$criteria.Pollutant == "aminomethyl phosphoric acid (AMPA) Glyphosate degradate", "criteria.Pollutant"] <- "Aminomethylphosphonic acid (AMPA)" #example for substitutions (first is old name in criteria list, second is new analyte name)
# min.criteria[min.criteria$criteria.Pollutant == '2,6-Dichlorobenzamide (BAM)','criteria.Pollutant'] <- "2,6-Dichlorobenzamide" #example for substitutions (first is old name in criteria list, second is new analyte name)
# min.criteria[min.criteria$criteria.Pollutant == 'Endosulfan Sulfate','criteria.Pollutant'] <- "Endosulfan sulfate" #example for substitutions (first is old name in criteria list, second is new analyte name)
# min.criteria[min.criteria$criteria.Pollutant == 'Propoxur','criteria.Pollutant'] <- "Baygon (Propoxur)" #example for substitutions (first is old name in criteria list, second is new analyte name)
# min.criteria[min.criteria$criteria.Pollutant == 'Acifluorfen (Sodium)','criteria.Pollutant'] <- "Acifluorfen" #example for substitutions (first is old name in criteria list, second is new analyte name)
# min.criteria[min.criteria$criteria.Pollutant == 'Metsulfuron','criteria.Pollutant'] <- "Metsulfuron Methyl" #example for substitutions (first is old name in criteria list, second is new analyte name)

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

min.criteria[min.criteria$criteria.Pollutant=="4,4´-DDE", "label"] <- paste0("\nlowest DEQ WQS= 0.001 ug/L\n*criterion applies to DDT and its metabolites")
min.criteria[min.criteria$criteria.Pollutant=="4,4´-DDD", "label"] <- paste0("\nlowest DEQ WQS= 0.001 ug/L\n*criterion applies to DDT and its metabolites")
min.criteria[min.criteria$criteria.Pollutant=="Atrazine", "label"] <- paste0("\nEPA benchmark = 1 ug/L\nproposed EPA benchmark = 0.001 ug/L")
min.criteria[min.criteria$criteria.Pollutant=="Simazine", "label"] <- paste0("\nEPA benchmark = 36 ug/L\nproposed EPA benchmark = 2.24 ug/L")

####duplicate dataset.
mydata_clean_noV <- mydata_clean
#rm(mydata_clean)

####fill out ug/L column----
unique(mydata_clean_noV$Units)
#[1] "ng/L" "mg/L" "µg/L"
#[1] "ng/L" "µg/L" "mg/L" "ug/l" "ug/L"
mydata_clean_noV[mydata_clean_noV$Units == "ug/l", "Units"] <- "µg/L"
mydata_clean_noV[mydata_clean_noV$Units == "ug/L", "Units"] <- "µg/L"
mydata_clean_noV[mydata_clean_noV$Units == "mg/L", "RESULT_clean.ug.l"] <-mydata_clean_noV[mydata_clean_noV$Units == "mg/L", ]$RESULT_clean*1000
mydata_clean_noV[mydata_clean_noV$Units == "ng/L", "RESULT_clean.ug.l"] <- mydata_clean_noV[mydata_clean_noV$Units == "ng/L", ]$RESULT_clean/1000
mydata_clean_noV[mydata_clean_noV$Units == "µg/L", "RESULT_clean.ug.l"] <- mydata_clean_noV[mydata_clean_noV$Units == "µg/L", ]$RESULT_clean

####Substitute the NDs for zeroes in a new column

mydata_clean_noV[mydata_clean_noV$RESULT == "ND", "RESULT_clean.ug.l.subND"] <- 0
mydata_clean_noV[mydata_clean_noV$RESULT != "ND", "RESULT_clean.ug.l.subND"] <- mydata_clean_noV[mydata_clean_noV$RESULT != "ND", ]$RESULT_clean.ug.l

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
#digressions

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
            if(mydata_clean_noV$percent.benchmark[i] >= 1.0){
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

detections <- subset(mydata_clean_noV, is.na(RESULT_clean) == FALSE) #subset out the NDs 
index <-  (order(detections$Basin))
detections <- detections[(index),]



###########################
####Output a summary table 
Det.freq.table <- data.frame("Basin"=NA,
                             "Station"=NA, 
                             "Station.Description"=NA,
                             "Parameter"=NA,
                             "Median"=NA,
                             "Average"=NA, 
                             "Max"=NA, 
                             "criteria"=NA, 
                             "ALR"=NA, 
                             "N Samples" = NA, 
                             "percent.det.freq"=NA, 
                             "exceed.type"=NA, 
                             stringsAsFactors=FALSE)


#### Adding year column to mydata_clean_noV
mydata_clean_noV$year<-as.integer(substr(mydata_clean_noV$date,1,4))

#### Setting up lists by year
mydata_clean_noV_list<-list()

for (y in unique(mydata_clean_noV$year)){
  mydata_clean_noV_list[[y]]<-subset(mydata_clean_noV,year == y)
}

Det.freq.table_list<-list()

for (y in unique(mydata_clean_noV$year)){
  Det.freq.table_list[[y]]<-Det.freq.table
}

####Add the Basin loop and Analyte loop wrapper
#### NEW (Old is below); I have not got this to work yet
#### Idea is to subset the mydata_clean_noV dataframe into a list composed of
#### individual years, run the loop, and then compile the lists into one dataframe
#### There's probably a cleaner way to do this, but it should work.

for (y in unique(mydata_clean_noV$year)){
  for(B in unique(mydata_clean_noV_list[[y]]$Basin)){
    subset.pointsB <- mydata_clean_noV_list[[y]][mydata_clean_noV_list[[y]]$Basin == B,]
    print(B)
    
    for(ii in analytes){
      subset.points0 <- subset(subset.pointsB, Analyte == ii)#aaa
      print(ii)
      
      if((B=="Walla Walla"| B=="Wasco" | B=="Hood River") & (ii == "Chlorpyrifos")){
        subset.points0 <- subset.points0[subset.points0$date <= "2014-04-30",]#Early Spring chlorpyrifos in WW, Wasco, Hood
        print(paste0(B, ii, " Early spring chlorpyrifos"))
      }else{
        if((B=="Walla Walla"| B=="Wasco" | B=="Hood River") & (ii == "Azinphos-methyl (Guthion)" | ii == "Malathion")){
          subset.points0 <- subset.points0[subset.points0$date >= "2014-05-01",]#Late Spring guthion and malathion in WW, Wasco, Hood
          print(paste0(B, ii, " Late spring guthion and malathion"))
        }}
      
      ####Walla Walla distributaries
      if(B=="Walla Walla"){
        subset.points <- subset.points0[subset.points0$Station_Number %in% c(32010, 33083, 33084), ] 
        if(length(subset.points$RESULT_clean)>0){
          detects.n <- nrow(subset(subset.points, is.na(RESULT_clean) == FALSE))
          stn.n <- nrow(subset.points)#ddd
          percent.det.freq <- (detects.n/stn.n)*100
          matchup <- match(min(subset.points$Analyte), min.criteria$criteria.Pollutant)
          criteria <- as.numeric(min.criteria$criteria.minimum.criteria.benchmark.value[matchup])
          
          df1 <- data.frame("Basin"=B,
                            "Station"="Walla Walla distributaries", 
                            "Station.Description"="Walla Walla distributaries", 
                            "Parameter"=min(subset.points$Analyte),
                            "Median"=median(subset.points$RESULT_clean.ug.l, na.rm=TRUE),
                            "Average"=mean(subset.points$RESULT_clean.ug.l.subND), 
                            "Max"=max(subset.points$RESULT_clean.ug.l.subND), 
                            "criteria"=criteria, 
                            "ALR"="Not Calculated", 
                            "N Samples" = stn.n, 
                            "percent.det.freq"=percent.det.freq, 
                            "exceed.type"="Not Calculated", 
                            stringsAsFactors=FALSE)
          Det.freq.table_list[[y]] <- rbind(df1, Det.freq.table_list[[y]])
        }
      }
      
      ####Four (by station summary statistics)
      for(i in station.list){
        subset.points <- subset(subset.points0, Station_Number == i)#ccc
        if(length(subset.points$RESULT_clean)>0){
          
          detects.n <- nrow(subset(subset.points, is.na(RESULT_clean) == FALSE))
          stn.n <- nrow(subset.points)#ddd
          percent.det.freq <- (detects.n/stn.n)*100
          
          Station <- min(subset.points$Station_Number)
          Station.Description <- min(subset.points$Station_Description)
          Analyte <- min(subset.points$Analyte)
          Median <- median(subset.points$RESULT_clean.ug.l, na.rm=TRUE)
          Average <- mean(subset.points$RESULT_clean.ug.l.subND)
          Max <- max(subset.points$RESULT_clean.ug.l.subND)
          matchup <- match(Analyte, min.criteria$criteria.Pollutant)
          criteria <- as.numeric(min.criteria$criteria.minimum.criteria.benchmark.value[matchup])
          ALR <- Max/criteria
          
          df1 <- data.frame("Basin"=B, 
                            "Station"=Station, 
                            "Station.Description"=Station.Description, 
                            "Parameter"=Analyte,
                            "Median"=Median,
                            "Average"=Average, 
                            "Max"=Max, 
                            "criteria"=criteria, 
                            "ALR"="Not Calculated", 
                            "N Samples" = stn.n, 
                            "percent.det.freq"=percent.det.freq, 
                            "exceed.type"="Not Calculated", 
                            stringsAsFactors=FALSE)
          Det.freq.table_list[[y]] <- rbind(df1, Det.freq.table_list[[y]])
        }
      }
      
      ####Two and Three (by Basin and Analyte)
      #Aggregate Basin wide statistics
      subset.points <- subset.points0
      if(length(subset.points$RESULT_clean)>0){
        
        tot.n <- nrow(subset.points)
        detects <- subset(subset.points, is.na(exceed.type) == FALSE)
        det.n <- nrow(detects)
        percent.det.freq <- (det.n/tot.n)*100
        
        Station <- "Basin aggregate"
        Station.Description <- "Basin aggregate"
        Analyte <- min(subset.points$Analyte)
        Median <- median(subset.points$RESULT_clean.ug.l, na.rm=TRUE)
        Average <- mean(subset.points$RESULT_clean.ug.l.subND)
        Max <- max(subset.points$RESULT_clean.ug.l.subND)
        matchup <- match(Analyte, min.criteria$criteria.Pollutant)
        criteria <- as.numeric(min.criteria$criteria.minimum.criteria.benchmark.value[matchup])
        ALR <- Max/criteria
        
        df1 <- data.frame("Basin"=B, 
                          "Station"="Basin aggregate", 
                          "Station.Description"="Basin aggregate", 
                          "Parameter"=Analyte,
                          "Median"=Median,
                          "Average"=Average, 
                          "Max"=Max, 
                          "criteria"=criteria, 
                          "ALR"=ALR, 
                          "N Samples" = tot.n, 
                          "percent.det.freq"=percent.det.freq, 
                          "exceed.type"="Total Detection Freq", 
                          stringsAsFactors=FALSE)
        Det.freq.table_list[[y]] <- rbind(df1, Det.freq.table_list[[y]])
      }
      
      ####One (By Basin and Analyte and Exceedance Type)
      n.tot <- nrow(subset.points0)#count by Basin and Analyte #bbb
      for(iii in unique(mydata_clean_noV_list[[y]]$exceed.type)){
        subset.points <- subset(subset.points0, exceed.type == iii)#ccc
        if(length(subset.points$RESULT_clean)>0){
          
          n.exceed.type <- nrow(subset.points)#count by Exceedance Type #ddd
          percent.det.freq <- (n.exceed.type/n.tot)*100
          
          Basin <- min(subset.points$Basin)
          Station <- min(subset.points$Station_Number)
          Station.Description <- min(subset.points$Station_Description)
          Analyte <- min(subset.points$Analyte)
          Median <- median(subset.points$RESULT_clean.ug.l, na.rm=TRUE)
          Average <- mean(subset.points$RESULT_clean.ug.l.subND)
          Max <- max(subset.points$RESULT_clean.ug.l.subND)
          matchup <- match(Analyte, min.criteria$criteria.Pollutant)
          criteria <- as.numeric(min.criteria$criteria.minimum.criteria.benchmark.value[matchup])
          ALR <- Max/criteria
          
          df1 <- data.frame("Basin"=B, 
                            "Station"=Basin, 
                            "Station.Description"=Basin, 
                            "Parameter"=Analyte,
                            "Median"="by exceed type",
                            "Average"="by exceed type", 
                            "Max"=Max, 
                            "criteria"=criteria, 
                            "ALR"="Not Calculated", 
                            "N Samples" = n.exceed.type, 
                            "percent.det.freq"=percent.det.freq, 
                            "exceed.type"=iii, 
                            stringsAsFactors=FALSE)
          Det.freq.table_list[[y]] <- rbind(df1, Det.freq.table_list[[y]])
        }
      }
    }
  }
}

### Adding a column for year
for (y in unique(mydata_clean_noV$year)){
  Det.freq.table_list[[y]]$Year<-y
}

### Unlisting to dataframe with plyr package
Det.freq.table.new<-ldply(Det.freq.table_list,data.frame)

head(Det.freq.table.new)
tail(Det.freq.table.new)

### STOPPED HERE; note that you will have to change the Det.freq.table to Det.freq.table.new below
Det.freq.table <- Det.freq.table.new

##### Clean up files----

Det.freq.table <- subset(Det.freq.table, percent.det.freq>0) #subset for parameters with detections

write.csv(Det.freq.table, paste0(outpath.plot.points,"State_alldates_detection_frequencies_savedon", Sys.Date(),".csv")) 

write.csv(mydata_clean_noV, paste0(outpath.plot.points,"State_alldates_mydata_clean_noV_savedon", Sys.Date(),".csv")) 


###########################
#########################################################################################################
####loop through analyte list and graph multiple stations



#install.packages("ggplot2")
#install.packages("gridExtra")
library(ggplot2)
#library(scales)
library(gridExtra)

#ggplot 
#single plots
#stations sorted by shape

B <- "Hood River"
ii <- "Deisopropylatrazine"
ii <- "Total Solids"
B <- "Amazon"
ii <- "Chlorpyrifos"
B <- "Yamhill"
B <- "Middle Rogue"
ii <- "Bifenthrin"
ii <- "Conductivity"
ii <- "4,4´-DDE"


#20141023: Delete Walla Walla at the Frog results (because no detections) #32012
View(mydata_clean_noV[mydata_clean_noV$Station_Number == 32012 & mydata_clean_noV$RESULT != "ND",])



for (B in unique(mydata_clean_noV$Basin)) {
  subset.B <- mydata_clean_noV[mydata_clean_noV$Station_Number != 32012,] #20141023 to fix the number of stations on WWatTheFrog.  
  subset.B <- subset(subset.B, Basin == B)
  for (ii in analytes){
    subset.ii <- subset(subset.B, Analyte == ii)
    if(length(subset.ii$RESULT > 0) & any(subset.ii$RESULT_clean.ug.l.neg > 0)){
      print(paste0(B, " ", ii, ": n=", length(subset.ii$RESULT), " sum=", sum(subset.ii$RESULT_clean.ug.l.subND)))
      
      numeric.criterion.graph <- as.numeric(min.criteria[min.criteria$criteria.Pollutant == ii,'criteria.minimum.criteria.benchmark.value']) #find the lowest EPA AL benchmark
      numeric.criterion.label <- min.criteria[min.criteria$criteria.Pollutant == ii,'label'] #find the lowest DEQ AL benchmark
      a <- ggplot(data = subset.ii, #data source is the subset of Basin and analyte
                  aes(x = date, #x axis is dates
                      y = RESULT_clean.ug.l.neg, #y axis is numeric result
                      group=Station_Description,
                      shape=Station_Description, #change point shapes by station
                      color=Station_Description)) #change point colors by station
      a <- a + geom_point(size = 5) #set the point size
      a <- a + xlab("") + ylab(("ug/L")) #write the labels
      a <- a + scale_x_date(breaks=unique(subset.B$date), labels=format(unique(subset.B$date), format="%m/%d"))
      a <- a + coord_cartesian(xlim=c(min(subset.B$date)-1, max(subset.B$date)+1)) #add a day to beginning and end
      a <- a + theme(aspect.ratio=1)
      a <- a + theme_bw() #blackandwhite theme
      a <- a + ylim(c(0, max(subset.ii$RESULT_clean.ug.l.subND*1.8))) #set the y range from zero to some multiplier of the max result to increase the head space
      a <- a + theme(panel.grid.minor.x = element_blank()) #remove minor grid lines
      #benchmarks lines and labels  
      if(length(numeric.criterion.graph)==0){  #if there is NO DEQ criteria or EPA benchmark
        title <- (paste0(B, " 2014\n", ii, "\nNo benchmark available")) 
      }else{
        if(ii != "Chlorpyrifos" 
           & ii != "2,4-D" 
           & ii != "Atrazine" 
           & ii != "Simazine" 
           #& ii != "Deisopropylatrazine" 
           #& ii != "Desethylatrazine" 
           & length(numeric.criterion.graph)>0){  #list of names of the exceptions#if there IS DEQ criteria or EPA benchmark
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
            }else{
              if(ii == "Atrazine"){  #Proposed EPA benchmarks 12/17/14  
                a <- a + geom_hline(yintercept=1.0, linetype=1)  #draw solid last year's EPA benchmark
                a <- a + geom_hline(yintercept=0.001, linetype=2)  #draw dashed proposed EPA benchmark
                title <- (paste0(B, " 2014\n", ii, numeric.criterion.label))
              }else{
                if(ii == "Simazine"){  #Proposed EPA benchmarks 12/17/14  
                  a <- a + geom_hline(yintercept=36, linetype=1)  #draw solid line last year's EPA benchmark
                  a <- a + geom_hline(yintercept=2.24, linetype=2)  #draw dashed line proposed EPA benchmark
                  title <- (paste0(B, " 2014\n", ii, numeric.criterion.label))
                }else{
                  if(ii == "Deisopropylatrazine"){  #Proposed EPA benchmarks 12/17/14  
                    a <- a + geom_hline(yintercept=numeric.criterion.graph)  #draw it
                    title <- (paste0(B, " 2014\n", "Triazine DIA degradate", numeric.criterion.label))
                  }else{
                    if(ii == "Desethylatrazine"){  #Proposed EPA benchmarks 12/17/14  
                      a <- a + geom_hline(yintercept=numeric.criterion.graph)  #draw it
                      title <- (paste0(B, " 2014\n", "Triazine DEA degradate", numeric.criterion.label))
                    }
                  }
                }
              }
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
      a <- arrangeGrob((a), sub = textGrob(paste0("prepared by Julia Crown, ODEQ, ", Sys.Date()), 
                                           x = 0, hjust = -0.1, vjust=0.1,
                                           gp = gpar(fontface = "italic", fontsize = 8))) 
      ggsave(filename = paste0(outpath.plot.points, B, "_", ii, "_2014_savedon", Sys.Date(),".jpg"), plot = a)
    }
  }
}


#################################################################################
#ggplot 
#multiplot
#stations sorted by shape and color

B <- "Hood River"

for(B in unique(mydata_clean_noV$Basin)){
  subset.B <- mydata_clean_noV[mydata_clean_noV$Station_Number != 32012,] #20141023 to fix the number of stations on WWatTheFrog.  
  subset.B <- subset(subset.B, Basin == B)
  subset.B <- subset(subset.B, is.na(subset.B$RESULT_clean.ug.l) == FALSE)
  print(paste0(B, ": n=", length(subset.B$RESULT)))
  if(sum(subset.B$RESULT_clean.ug.l.subND) > 0){
    a <- ggplot(data = subset.B, #data source is the subset of Basin and analyte
                aes(x = date, #x axis dates
                    y = RESULT_clean.ug.l, #y axis is numeric result
                    shape=Station_Description, #change point shapes by station
                    color=Station_Description)) #change point colors by station
    a <- a + geom_point(size = 4) #set the point size
    a <- a + theme(aspect.ratio=1/2)
    a <- a + xlab("") + ylab(paste0("ug/L")) + ggtitle(paste0(B, " 2014")) #write the labels and title
    a <- a + theme(panel.grid.minor.x = element_blank()) #remove minor grid lines
    a <- a + facet_wrap(~Analyte, drop=TRUE, scales = "free_y")
    a <- a + scale_x_date(breaks=unique(subset.B$date), labels=format(unique(subset.B$date), format="%m/%d"))
    a <- a + coord_cartesian(xlim=c(min(subset.B$date)-5, max(subset.B$date)+5)) #add 5 day to beginning and end
    a <- a + theme(axis.text.x  = element_text(angle=90, vjust=0.5, color="black", size=10))
    a <- a + theme(axis.text.y  = element_text(color="black", size=10))
    a <- a + guides(shape = guide_legend(ncol = 2)) #legend in two columns
    a <- a + theme(legend.position="bottom", legend.direction="vertical", legend.text=element_text(size=10), #move legend
                   legend.title=element_blank()) #remove title from legend
    a
    a <- arrangeGrob((a), sub = textGrob(paste0("prepared by Julia Crown, ODEQ, ", Sys.Date()), 
                                         x = 0, hjust = -0.1, vjust=0.1,
                                         gp = gpar(fontface = "italic", fontsize = 8))) 
    ggsave(filename = paste0(outpath.plot.points, "multiplot_", B, "_2014_savedon", Sys.Date(),".jpg"), plot = a, scale=1.5)
  }
}


##################################################################################

#
