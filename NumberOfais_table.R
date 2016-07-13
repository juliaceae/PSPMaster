# number of a.i.s detected at each station
# maximum number of a.i.s in a mixture by station 
# number of detections of pesticides at each station (detection frequency as an absolute number)
# number of detections for each a.i. by year
#install.packages("dplyr")
#install.packages("plyr")
#library("dplyr")
library("plyr")

#source("\\\\Deqhq1\\PSP\\Rscripts\\Criteria\\ToxicsCriteriaPSP.R")
#source("\\\\Deqhq1\\PSP\\Rscripts\\PSPMaster\\RetreiveElementData.R")
#source("\\\\Deqhq1\\PSP\\Rscripts\\PSPMaster\\PSPformatData.R")
##################################################################################
#Create station list with Lat/Longs
LatLong <- element[,c("Station_Description", 
                     "Station_ID", 
                     "DECIMAL_LAT", 
                     "DECIMAL_LONG")]
#get unique stations
LatLong <- LatLong[!duplicated(LatLong), ]
write.csv(LatLong, paste0("\\\\deqhq1\\PSP\\GIS\\DisplayMaps\\Statewide\\","State_Unique_SitesLL_savedon", Sys.Date(),".csv")) 
write.csv(LatLong, paste0("\\\\deqhq1\\PSP\\GIS\\DisplayMaps\\Statewide\\","State_Unique_SitesLL_savedon", ".csv")) 



#Peter, I selected for this year's analysis.  I would like a table to compare multiple years, but it's not time sensitive.

#by year
currentYear <- 2012
mydata_clean_noVY <- mydata_clean_noV[mydata_clean_noV$year == currentYear,]

#filter out detections only 
detects <- mydata_clean_noVY[mydata_clean_noVY$dnd == 1,]

#Remove non-pesticide Analyte results
#sort(unique(detects$Analyte))
source("\\\\Deqhq1\\PSP\\Rscripts\\PSPMaster\\PestNameGroups.R")
detects <- detects[!detects$Analyte %in% Non.Pest,]

#count of Number of samples taken 
#this line uses "summarise" function is from the plyr package and does not use the "summarize" function from the HMisc package (loaded in sourced code).
#Number of analytes analyzed (detects and nondetects) by station.
N.bystation <- ddply(mydata_clean_noVY, .(Station_Number), summarise, N.Samples = length(dnd)) 
#N.Detects: count of Results detections 
N.detects.bystation <- ddply(detects, .(Station_Number), summarise, N.Detects = sum(dnd))

#Steve Riley's table
#N.detects.bystation.byAnalyte  <- table(detects$Station_Number,  detects$Analyte)
#refine Steve's table to count of 
#number of ais detected by station
N.Analytes.bystation <- ddply(detects, .(Station_Number), summarise, N.Analytes = length(unique(Analyte)))

#Number of analytes detected by station and date
by.st.date <- ddply(detects, .(Station_Number, date), summarise, AI.in.Mixture = sum(dnd))
#Date of Max number of analytes detected by site
max.by.st.date <- ddply(by.st.date, .(Station_Number),function(x) {x[which.max(x$AI.in.Mixture),c('AI.in.Mixture', 'date')]}) 
max.by.st.date <- rename(max.by.st.date, c('AI.in.Mixture' = 'Max.AI.in.Mixture'))

#merge the tables together
N.ai.bystation.LL <- merge(LatLong, N.bystation, by.x="Station_ID", by.y="Station_Number")
N.ai.bystation.LL <- merge(N.ai.bystation.LL, N.detects.bystation, by.x="Station_ID", by.y="Station_Number")
N.ai.bystation.LL <- merge(N.ai.bystation.LL, N.Analytes.bystation, by.x="Station_ID", by.y="Station_Number")
N.ai.bystation.LL <- merge(N.ai.bystation.LL, max.by.st.date, by.x="Station_ID", by.y="Station_Number")
N.ai.bystation.LL$percent.det.freq <- N.ai.bystation.LL$N.Detects/N.ai.bystation.LL$N.Samples
N.ai.bystation.LL <- merge(N.ai.bystation.LL, unique(mydata_clean_noVY[, c(3,4)]), by.x="Station_ID", by.y="Station_Number") #includes all basins in the year
N.ai.bystation.LL <- N.ai.bystation.LL[order(N.ai.bystation.LL$Station_Description),]
head(N.ai.bystation.LL)

write.csv(N.ai.bystation.LL, paste0("\\\\deqhq1\\PSP\\GIS\\DisplayMaps\\Statewide\\","State_N.Ai_", currentYear, "_savedon", Sys.Date(),".csv")) 
write.csv(N.ai.bystation.LL, paste0("\\\\deqhq1\\PSP\\GIS\\DisplayMaps\\Statewide\\","State_N.Ai_savedon", currentYear, ".csv")) 
write.csv(N.ai.bystation.LL, paste0("\\\\deqhq1\\PSP\\Rscripts\\Alldates\\", Sys.Date(), "\\State_N.Ai_", currentYear, "_savedon", Sys.Date(),".csv")) 

#################################################################################
#Write a shapefile
##################################################################################

################################################
#Statewide summaries for Sarah Whitney at Long Tom WC
library("plyr")
#install.packages("dplyr")
library("dplyr")
#install.packages("reshape")
library("reshape")

max.Conc <- summarise(group_by(mydata_clean_noV[mydata_clean_noV$dnd == 1 & mydata_clean_noV$Analyte != "Total Solids",], 
                     Basin, Analyte),
            max.Conc = max((RESULT_clean.ug.l)))


#tapply(ffm$value, ffm$treatment, sum)
#tapply(mydata_clean_noV[is.na(mydata_clean_noV$RESULT_clean.ug.l)==FALSE,]$RESULT_clean.ug.l, mydata_clean_noV[is.na(mydata_clean_noV$RESULT_clean.ug.l)==FALSE,]$Analyte, (length))
#tapply(mydata_clean_noV[is.na(mydata_clean_noV$RESULT_clean.ug.l)==FALSE,'RESULT_clean.ug.l'], 
#       mydata_clean_noV[is.na(mydata_clean_noV$RESULT_clean.ug.l)==FALSE,'Analyte'], 
#       (max))


options(digits = 3)
options(scipen=999)

mmm <- mydata_clean_noV[ ,c("Analyte", "code", "Basin", "Station_Description", "RESULT_clean.ug.l")]
mmm <- mydata_clean_noV[ ,c("Analyte", "code", "Basin", "Station_Description", "RESULT_clean.ug.l", "year")]
mmm$year <- as.factor(mmm$year)
mmm <- melt(mmm)
#Maximum concentration (ug/L) by basin by analyte in 2015.
StateCompare.max <- cast(mmm[mmm$year == 2015,],  Analyte ~ Basin, function(x) max(x, na.rm=TRUE), margins = "grand_col")
SCm <- melt(StateCompare.max)
SCm[SCm$value == -Inf, "value"] <- ""
StateCompare.max <- cast(SCm, Analyte~Basin)
StateCompare.max <- StateCompare.max[,!(names(StateCompare.max) %in% "Hood River POCIS_SPMD")]
write.csv(StateCompare.max, paste0("//deqhq1/PSP/AnnualReports/Statewide_2015_max_values_savedon", Sys.Date(), ".csv")) 

#StateCompare.max <- cast(mmm,  Analyte ~ Basin, function(x) max(x, na.rm=TRUE))
#StateCompare <- cast(mmm,  Analyte ~ ., function(x) c(n = length(x), max = max(x, na.rm=TRUE)))
#StateCompare.nsample <- cast(mmm, Analyte ~ Basin, function(x) n.samples = length((na.omit(x))))
#StateCompare.n.ai <- cast(mmm, Basin ~ ., function(x) n.analytes = length((na.omit(x))))
N.Analytes.byBasin <- ddply(mydata_clean_noV[mydata_clean_noV$dnd == 1 & mydata_clean_noV$year == 2015,], .(Basin), summarise, N.Analytes = length(unique(Analyte)))
N.Analytes.byBasin <- N.Analytes.byBasin[order(N.Analytes.byBasin$N.Analytes, decreasing = TRUE),]

###########################
##Amazon Basin info request: How many detects in each sample? 

library("plyr")

N.Analytes.byDateStation <- ddply(mydata_clean_noV[mydata_clean_noV$Basin == "Amazon" & mydata_clean_noV$Analyte != "Total Solids", ], c("Station_Description", "date"), summarise, N.Analytes = sum(dnd))
N.Analytes.byDateStation <- N.Analytes.byDateStation[order(N.Analytes.byDateStation$N.Analytes),]

View(N.Analytes.byDateStation)
write.csv(N.Analytes.byDateStation, paste0("//deqhq1/PSP/Amazon/2015Amazon/Amazon_2011-2015_N.analytes.byDateStation_savedon", Sys.Date(), ".csv")) 

N.Analytes.byDate <- ddply(mydata_clean_noV[mydata_clean_noV$Basin == "Amazon" & mydata_clean_noV$Analyte != "Total Solids", ], c("date"), summarise, N.Analytes = sum(dnd))
N.Analytes.byDate <- N.Analytes.byDate[order(N.Analytes.byDate$N.Analytes),]

View(N.Analytes.byDate)

write.csv(N.Analytes.byDate, paste0("//deqhq1/PSP/Amazon/2015Amazon/Amazon_2011-2015_N.analytes.byDate_savedon", Sys.Date(), ".csv")) 


# oha <- criteria.values.melted.applicable[criteria.values.melted.applicable$variable %in% c('OHA Maximum Contaminant Levels','OHA.MCL'),]
# oha.casted <- dcast(oha)
# oha.casted <- dcast(oha, Pollutant ~ variable, value.var = "value")
