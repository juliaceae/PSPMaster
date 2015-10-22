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
ms4$Sampled_Date <- as.Date(strptime(ms4$date, format="%m/%d/%Y")) 

mydata_clean <- ms4
#manipulations to the combined LASAR/element/MS4 set
mydata_clean$Station_Number <- as.numeric(mydata_clean$Station_Number)
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
#df[is.na(df$col),’col’] <- 0
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
#digressions
