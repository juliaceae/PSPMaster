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

#mydata_clean_noV$dnd <- ifelse(as.numeric(mydata_clean_noV$RESULT) < mydata_clean_noV$MRL,0,1)
detects <- mydata_clean_noV[mydata_clean_noV$dnd == 1,]

#Remove non-pesticide Analyte results
#sort(unique(detects$Analyte))
source("\\\\Deqhq1\\PSP\\Rscripts\\PSPMaster\\PestNameGroups.R")
detects <- detects[!detects$Analyte %in% Non.Pest,]

#count of Number of samples taken 
#summarise function is from the plyr package, not the summarize function from the HMisc package.
N.bystation <- ddply(mydata_clean_noV, .(Station_Number), summarise, N.Samples = length(dnd)) 
#count of Results detections 
N.detects.bystation <- ddply(detects, .(Station_Number), summarise, N.Detects = sum(dnd))

#Steve's table
#N.detects.bystation.byAnalyte  <- table(detects$Station_Number,  detects$Analyte)
#refine Steve's table to count of ais by station
N.Analytes.bystation <- ddply(detects, .(Station_Number), summarise, N.Analytes = length(unique(Analyte)))

#Detections by station and date
by.st.date <- ddply(detects, .(Station_Number, date), summarise, AI.in.Mixture = sum(dnd))
#Max detections to site
#by.st.date <- ddply(by.st.date, .(Station_Number), summarize, Max.n.AI.in.Mixture = max(AI.in.Mixture), date = )
max.by.st.date <- ddply(by.st.date, .(Station_Number),function(x) {x[which.max(x$AI.in.Mixture),c('AI.in.Mixture', 'date')]})
max.by.st.date <- rename(max.by.st.date, c('AI.in.Mixture' = 'Max.AI.in.Mixture'))

#merge the tables together
N.ai.bystation.LL <- merge(LatLong, N.bystation, by.x="Station_ID", by.y="Station_Number")
N.ai.bystation.LL <- merge(N.ai.bystation.LL, N.detects.bystation, by.x="Station_ID", by.y="Station_Number")
N.ai.bystation.LL <- merge(N.ai.bystation.LL, N.Analytes.bystation, by.x="Station_ID", by.y="Station_Number")
N.ai.bystation.LL <- merge(N.ai.bystation.LL, max.by.st.date, by.x="Station_ID", by.y="Station_Number")
head(N.ai.bystation.LL)

write.csv(N.ai.bystation.LL, paste0("\\\\deqhq1\\PSP\\GIS\\DisplayMaps\\Statewide\\","State_N.Ai_savedon", Sys.Date(),".csv")) 
write.csv(N.ai.bystation.LL, paste0("\\\\deqhq1\\PSP\\GIS\\DisplayMaps\\Statewide\\","State_N.Ai_savedon", ".csv")) 

#################################################################################
#Write a shapefile
##################################################################################
