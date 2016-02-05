#This script should: Table calculated detection freqs, max, ave, other stats.  Need to source the data retreival and cleaning script first
#Basin and data range
#Julia Crown

source('//deqhq1/PSP/Rscripts/PSPMaster/PlotTimeseries_alldates.R', encoding = 'UTF-8')

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

B <- "Walla Walla"
B <- "Amazon"
B <- "Wasco"
B <- "Hood River"
ii <- "Chlorpyrifos"
ii <- "Atrazine"
ii <- "Hexazinone"
ii <- "Malathion"


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
    #  print(B)
    
    for(ii in analytes){
      subset.points0 <- subset(subset.pointsB, Analyte == ii)#aaa
      #    print(ii)
      
      if((B=="Walla Walla"| B=="Wasco" | B=="Hood River") & (ii == "Chlorpyrifos")){
        subset.points0 <- subset.points0[subset.points0$date <= paste0(y, "-04-30"), ]#Early Spring chlorpyrifos in WW, Wasco, Hood
        #      print(paste0(B, ii, " Early spring chlorpyrifos"))
      }else{
        if((B=="Walla Walla"| B=="Wasco" | B=="Hood River") & (ii == "Azinphos-methyl (Guthion)" | ii == "Malathion")){
          subset.points0 <- subset.points0[subset.points0$date >= paste0(y, "-05-01"), ]#Late Spring guthion and malathion in WW, Wasco, Hood
          #        print(paste0(B, ii, " Late spring guthion and malathion"))
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

#Det.freq.table <- subset(Det.freq.table, percent.det.freq>0) #subset for parameters with detections

write.csv(Det.freq.table, paste0(outpath.plot.points,"State_alldates_detection_frequencies_savedon", Sys.Date(),".csv")) 

write.csv(mydata_clean_noV, paste0(outpath.plot.points,"State_alldates_mydata_clean_noV_savedon", Sys.Date(),".csv")) 
