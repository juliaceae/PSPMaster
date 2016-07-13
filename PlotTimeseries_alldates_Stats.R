#This script should: Table calculated detection freqs, max, ave, other stats.  Need to source the data retreival and cleaning script first
#Basin and data range
#Julia Crown

#source('//deqhq1/PSP/Rscripts/PSPMaster/PlotTimeseries_alldates.R', encoding = 'UTF-8')

###########################
##########################
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

# B <- "Walla Walla"
# B <- "Wasco"
# B <- "Hood River"
# ii <- "Atrazine"
# ii <- "Hexazinone"
# ii <- "Malathion"
# B <- "Pudding"
# ii <- "Ametryn"
# y <- as.integer(2015)

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
      # print(B)

    for(ii in analytes){
      subset.points0 <- subset(subset.pointsB, Analyte == ii)#aaa
      # print(ii)
      
      if((B=="Walla Walla"| B=="Wasco" | B=="Hood River") & (ii == "Chlorpyrifos")){
        subset.points0 <- subset.points0[subset.points0$date <= paste0(y, "-04-30"), ]#Early Spring chlorpyrifos in WW, Wasco, Hood
              #print(paste0(B, ii, " Early spring chlorpyrifos"))
      }else{
        if((B=="Walla Walla"| B=="Wasco" | B=="Hood River") & (ii == "Azinphos-methyl (Guthion)" | ii == "Malathion")){
          subset.points0 <- subset.points0[subset.points0$date >= paste0(y, "-05-01"), ]#Late Spring guthion and malathion in WW, Wasco, Hood
                  #print(paste0(B, ii, " Late spring guthion and malathion"))
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
      for(i in unique(mydata_clean_noV$Station_Number)){
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
#          if(B == "Cozine Creek") print(paste0(B, y, ii))}}}
    
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

Det.freq.table.new <- Det.freq.table.new[!is.na(Det.freq.table.new$Basin),]

### STOPPED HERE; note that you will have to change the Det.freq.table to Det.freq.table.new below
Det.freq.table <- Det.freq.table.new
##### Clean up files----

#Det.freq.table <- subset(Det.freq.table, percent.det.freq>0) #subset for parameters with detections

write.csv(Det.freq.table, paste0("\\\\Deqhq1\\PSP\\Rscripts\\Alldates\\", Sys.Date(), "\\State_alldates_detection_frequencies_savedon", Sys.Date(),".csv")) 
#write.csv(Det.freq.table, paste0("//deqhq1/PSP/Yamhill/2015Yamhill/",stn.title,"_detection_frequencies_savedon", Sys.Date(),".csv")) 

write.csv(mydata_clean_noV, paste0(outpath.plot.points,"State_alldates_mydata_clean_noV_savedon", Sys.Date(),".csv")) 
#write.csv(mydata_clean_noV, paste0("//deqhq1/PSP/Yamhill/2015Yamhill/",stn.title,"_mydata_clean_noV_savedon", Sys.Date(),".csv")) 

############################################
#mydata : subset by basin and write out a separate .csv file
new.folder <- dir.create(paste("\\\\Deqhq1\\PSP\\Rscripts\\Alldates\\",Sys.Date(), "\\", Sys.Date(), "_by_Basin_alldates_datafiles", sep="")) 
for (B in unique(mydata_clean_noV$Basin)){
  subset.B <- mydata_clean_noV[mydata_clean_noV$Basin == B,]
  write.csv(subset.B, paste0("\\\\Deqhq1\\PSP\\Rscripts\\Alldates\\",Sys.Date(), "\\", Sys.Date(), "_by_Basin_alldates_datafiles\\", B, "_alldates_mydata_clean_noV_savedon", Sys.Date(),".csv")) 
}

############################################
#cross tab (updated) the exceedance types and write a .csv by basin and year
library(reshape2)

dir.create(paste0('//deqhq1/PSP/Rscripts/Alldates/', Sys.Date(), '/',Sys.Date(), '_updated_det_freq'))

#set your Basin and Year
# B <- "South Umpqua"
# B <- "South Coast"
# y <- as.integer(201415)
# B <- "Clackamas"
B <- "Pudding"
# B <- "Amazon"
# B <- "Yamhill"
y <- as.integer(2015)

aaa <- Det.freq.table.new[Det.freq.table.new$exceed.type != 'Not Calculated' 
                        & Det.freq.table.new$Station == 'Basin aggregate' 
                        & Det.freq.table.new$Basin == B 
                        & Det.freq.table.new$Year == y, ]
bbb <- Det.freq.table.new[Det.freq.table.new$exceed.type != 'Not Calculated' 
                        & Det.freq.table.new$Station == B 
                        & Det.freq.table.new$Year == y,]

bbb$exceed.type <- factor(bbb$exceed.type, levels = c('less than ten percent of benchmark',
                                                  'between ten and fifty percent of benchmark',
                                                  'between fifty and 100 percent of benchmark',
                                                  'greater than 100 percent of benchmark',
                                                  'no benchmark available'))

aa <- reshape2::dcast(aaa, Parameter ~ exceed.type, value.var = 'percent.det.freq')
bb <- reshape2::dcast(bbb, Parameter ~ exceed.type, value.var = 'percent.det.freq', drop = FALSE)

ab <- merge(aa, bb, by = 'Parameter', all = TRUE)

#Reorders the columns
ab <- ab[,c('Parameter','less than ten percent of benchmark',
            'between ten and fifty percent of benchmark',
            'between fifty and 100 percent of benchmark',
            'greater than 100 percent of benchmark',
            'no benchmark available', 'Total Detection Freq')]

ab <- ab[ab$'Total Detection Freq' > 0,]
ab <- ab[ab$Parameter != "Total Solids",]
ab <- ab[order(ab$'Total Detection Freq', decreasing = TRUE),]

write.csv(ab, row.names = FALSE, file = paste0('//deqhq1/PSP/Rscripts/Alldates/', Sys.Date(), '/',Sys.Date(), '_updated_det_freq', '/', B,'_', y, '_updated_det_freq_savedon', Sys.Date(), '.csv')) 


#########################
#Fifteenmile one off detection frequencies for this station (Fifteenmile at Seufert Falls LASARid#36179) only. 
mmm <- mydata_clean_noV[mydata_clean_noV$Station_Number == 36179 & mydata_clean_noV$year == 2015, 
                        c("Analyte", "code", "Basin", "Station_Description", "exceed.type", "dnd")]
mmm$exceed.type <- factor(mmm$exceed.type, levels = c('less than ten percent of benchmark',
                                                      'between ten and fifty percent of benchmark',
                                                      'between fifty and 100 percent of benchmark',
                                                      'greater than 100 percent of benchmark',
                                                      'no benchmark available'))
mmm <- melt(mmm)
PE <- cast(mmm,  Analyte ~ exceed.type, function(x) sum(x, na.rm=FALSE), margins = "grand_col")
PE.tot <- cast(mmm, Analyte ~ . , function(x) length(x))
PE <- merge(PEm, PE.tot, by.x = "Analyte", by.y = "Analyte", all.x = TRUE)
#PEm <- melt(PE)
PE$det.freq.PE <- PE$value/PE$`(all)`
PE <- PE[ , c(1,3,5)]
#PE3 <- ddply(PE2, c("Analyte", "exceed.type"), summarise, det.freq = )
PE <- melt(PE, measured=c("det.freq.PE"))
PE <- cast(PE, Analyte ~ exceed.type)

PE <- PE[PE$`(all)` != 0 & PE$Analyte != "Total Solids",]
PE <- PE[order(PE$`(all)`, decreasing = TRUE),]

write.csv(PE, paste0("//deqhq1/PSP/Wasco/Wasco2015/Fifteenmile_2015_percentexceeds_savedon", Sys.Date(), ".csv")) 

# PE <- ddply(mydata_clean_noV[mydata_clean_noV$Station_Number == 36179 & mydata_clean_noV$year == 2015,], 
#             c("Analyte", "exceed.type"),
#             summarise, 
#             n.detects = sum(dnd), 
#             n = length(dnd)
#             )
# PE.tot <- ddply(mydata_clean_noV[mydata_clean_noV$Station_Number == 36179 & mydata_clean_noV$year == 2015,], 
#                 c("Analyte"),
#                 summarise, 
#                 tot.n = length(dnd)
#                 )
# PE2 <- merge(PEm, PE.tot, by.x = "Analyte", by.y = "Analyte", all.x = TRUE)


############################################
#Test of faster det.freq table
mmm <- mydata_clean_noV[ , c("Analyte", "code", "Basin", "Station_Description", "exceed.type", "dnd")]
mmm$exceed.type <- factor(mmm$exceed.type, levels = c('less than ten percent of benchmark',
                                                      'between ten and fifty percent of benchmark',
                                                      'between fifty and 100 percent of benchmark',
                                                      'greater than 100 percent of benchmark',
                                                      'no benchmark available'))
mmm <- melt(mmm)
PE <- cast(mmm,  Analyte ~ exceed.type, function(x) sum(x, na.rm=FALSE), margins = "grand_col")
PE.tot <- cast(mmm, Analyte ~ . , function(x) length(x))
PE <- merge(PEm, PE.tot, by.x = "Analyte", by.y = "Analyte", all.x = TRUE)
#PEm <- melt(PE)
PE$det.freq.PE <- PE$value/PE$`(all)`
PE <- PE[ , c(1,3,5)]
#PE3 <- ddply(PE2, c("Analyte", "exceed.type"), summarise, det.freq = )
PE <- melt(PE, measured=c("det.freq.PE"))
PE <- cast(PE, Analyte ~ exceed.type)

PE <- PE[PE$`(all)` != 0 & PE$Analyte != "Total Solids",]
PE <- PE[order(PE$`(all)`, decreasing = TRUE),]

write.csv(PE, paste0("//deqhq1/PSP/Wasco/Wasco2015/Fifteenmile_2015_percentexceeds_savedon", Sys.Date(), ".csv")) 

# PE <- ddply(mydata_clean_noV[mydata_clean_noV$Station_Number == 36179 & mydata_clean_noV$year == 2015,], 
#             c("Analyte", "exceed.type"),
#             summarise, 
#             n.detects = sum(dnd), 
#             n = length(dnd)
#             )
# PE.tot <- ddply(mydata_clean_noV[mydata_clean_noV$Station_Number == 36179 & mydata_clean_noV$year == 2015,], 
#                 c("Analyte"),
#                 summarise, 
#                 tot.n = length(dnd)
#                 )
# PE2 <- merge(PEm, PE.tot, by.x = "Analyte", by.y = "Analyte", all.x = TRUE)


#########################
#Yamhill one off detection frequencies for this station (mini-basins) only. 
library(reshape)
library(reshape2)
#install.packages("plyr")
library(plyr)

year.title <- 2015
stn.title <- paste0("West Fork Palmer subbasin ", year.title)
stn <- c("West Fork Palmer at SE Lafayette Hwy", "West Fork Palmer Creek at SE Palmer Creek Road", "West Fork Palmer at Webfoot Road Bridge")
#stn <- c("Lower Cozine Creek at Davis Street Bridge", "Middle Cozine at Old Sheridan Road")

mmm <- mydata_clean_noV[mydata_clean_noV$Station_Description %in% stn 
                        & mydata_clean_noV$year %in% year.title, 
                        c("Analyte", "code", "Basin", "Station_Description", "exceed.type", "dnd")]
mmm$exceed.type <- factor(mmm$exceed.type, levels = c('less than ten percent of benchmark',
                                                      'between ten and fifty percent of benchmark',
                                                      'between fifty and 100 percent of benchmark',
                                                      'greater than 100 percent of benchmark',
                                                      'no benchmark available'))
mmm <- melt(mmm)
PE <- cast(mmm,  Analyte ~ exceed.type, function(x) sum(x, na.rm=FALSE), margins = "grand_col")
PE.tot <- cast(mmm, Analyte ~ . , function(x) length(x))
PE <- merge(PE, PE.tot, by.x = "Analyte", by.y = "Analyte", all.x = TRUE)
#PEm <- melt(PE)
PE$det.freq.PE <- PE$value/PE$`(all)`
PE <- PE[ , c(1,3,5)]
#PE3 <- ddply(PE2, c("Analyte", "exceed.type"), summarise, det.freq = )
PE <- melt(PE, measured=c("det.freq.PE"))
PE <- cast(PE, Analyte ~ exceed.type)

PE <- PE[PE$`(all)` != 0 & PE$Analyte != "Total Solids",]
PE <- PE[order(PE$`(all)`, decreasing = TRUE),]

write.csv(PE, paste0("//deqhq1/PSP/Yamhill/2015Yamhill/WFPalmer_2015_percentexceeds_savedon", Sys.Date(), ".csv")) 
