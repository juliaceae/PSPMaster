#One off script to cross between years 2014 and 2015 for two of the pilot basins (South Coast and South Umpqua)

source('//deqhq1/PSP/Rscripts/PSPMaster/PlotTimeseries_alldates.R', encoding = 'UTF-8')

SouthPilots <- mydata_clean_noV[mydata_clean_noV$Basin %in% c("South Umpqua", "South Coast") & mydata_clean_noV$year %in% c(2014, 2015),]

#From Stats script:
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


# #### Setting up lists by year
# mydata_clean_noV_list<-list()
# 
# for (y in unique(mydata_clean_noV$year)){
#   mydata_clean_noV_list[[y]]<-mydata_clean_noV[mydata_clean_noV$year %in% y, ]
# }
# 
# Det.freq.table_list<-list()
# 
# for (y in unique(mydata_clean_noV$year)){
#   Det.freq.table_list[[y]]<-Det.freq.table
# }
# 
# ####Add the Basin loop and Analyte loop wrapper
# #### NEW (Old is below); I have not got this to work yet
# #### Idea is to subset the mydata_clean_noV dataframe into a list composed of
# #### individual years, run the loop, and then compile the lists into one dataframe
# #### There's probably a cleaner way to do this, but it should work.
# 
# for (y in unique(mydata_clean_noV$year)){
  for(B in unique(SouthPilots$Basin)){
    subset.pointsB <- SouthPilots[SouthPilots$Basin == B,]
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
          Det.freq.table <- rbind(df1, Det.freq.table)
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
          Det.freq.table <- rbind(df1, Det.freq.table)
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
        Det.freq.table <- rbind(df1, Det.freq.table)
      }
      
      ####One (By Basin and Analyte and Exceedance Type)
      n.tot <- nrow(subset.points0)#count by Basin and Analyte #bbb
      for(iii in unique(SouthPilots$exceed.type)){
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
          Det.freq.table <- rbind(df1, Det.freq.table)
        }
      }
    }
  }
# }

# ### Adding a column for year
# for (y in unique(mydata_clean_noV$year)){
#   Det.freq.table_list[[y]]$Year<-y
# }

# ### Unlisting to dataframe with plyr package
# Det.freq.table.new<-ldply(Det.freq.table_list,data.frame)
# 
# head(Det.freq.table.new)
# tail(Det.freq.table.new)

# ### STOPPED HERE; note that you will have to change the Det.freq.table to Det.freq.table.new below
# Det.freq.table <- Det.freq.table.new
# ##### Clean up files----

#Det.freq.table <- subset(Det.freq.table, percent.det.freq>0) #subset for parameters with detections

write.csv(Det.freq.table, paste0("\\\\Deqhq1\\PSP\\Rscripts\\Alldates\\", Sys.Date(), "\\","SouthPilots_2014-15_detection_frequencies_savedon", Sys.Date(),".csv")) 

write.csv(SouthPilots, paste0("\\\\Deqhq1\\PSP\\Rscripts\\Alldates\\", Sys.Date(), "\\","SouthPilots_2014-15_mydata_clean_noV_savedon", Sys.Date(),".csv")) 

#From Detection Frequency Casting script:
#Create table of analyte by detection frequecy category.  

library(reshape2)

#add a by basin and by year loop?

Det.freq.table.new <- Det.freq.table #be careful with this... useful for SouthPilots script ONLY!!
Det.freq.table.new <- Det.freq.table.new[!is.na(Det.freq.table.new$Basin),]
B <- "South Coast"

x <- Det.freq.table.new[Det.freq.table.new$exceed.type != 'Not Calculated' & Det.freq.table.new$Station == 'Basin aggregate' & Det.freq.table.new$Basin == B,]
y <- Det.freq.table.new[Det.freq.table.new$exceed.type != 'Not Calculated' & Det.freq.table.new$Station == B,]

y$exceed.type <- factor(y$exceed.type, levels = c('less than ten percent of benchmark',
                                           'between ten and fifty percent of benchmark',
                                           'between fifty and 100 percent of benchmark',
                                           'greater than 100 percent of benchmark',
                                           'no benchmark available'))

x1 <- reshape2::dcast(x, Parameter ~ exceed.type, value.var = 'percent.det.freq')
y1 <- reshape2::dcast(y, Parameter ~ exceed.type, value.var = 'percent.det.freq', drop = FALSE)

xy <- merge(x1, y1, by = 'Parameter', all = TRUE)

#xy <- within(xy, rm("NA.y", "NA.x"))

#Reorders the columns
xy <- xy[,c('Parameter','less than ten percent of benchmark',
            'between ten and fifty percent of benchmark',
            'between fifty and 100 percent of benchmark',
            'greater than 100 percent of benchmark',
            'no benchmark available', 'Total Detection Freq')]

write.csv(xy, row.names = FALSE, file = paste0('//deqhq1/PSP/Rscripts/Alldates/', Sys.Date(), '/', B,  'updated_det_freq_savedon', Sys.Date(), '.csv')) 






#From Graphs script:
library(ggplot2)
library(gridExtra)

dir.create(paste("\\\\Deqhq1\\PSP\\Rscripts\\Alldates\\",Sys.Date(), "\\", Sys.Date(), "_TimeSeries", sep="")) 

# for(y in unique(mydata_clean_noV$year)){
#   subset.y <- mydata_clean_noV[mydata_clean_noV$year == y,]
y <- "2014-2015"
subset.y <- SouthPilots
subset.y[subset.y$Analyte == "Aminomethylphosponic acid (AMPA)", "Analyte"] <- "AMPA"
subset.y <- subset.y[subset.y$Station_Number != 37805,] #Remove Myrtle Creek at confluence with MFCoquilleR from graph (because too many stations), but add a note that it was there

  for (B in unique(subset.y$Basin)) {
    subset.B <- subset.y[subset.y$Station_Number != 32012,] #20141023 to fix the number of stations on WWatTheFrog.  
    subset.B <- subset.B[subset.B$Basin == B,]
    for (ii in analytes){
      subset.ii <- subset.B[subset.B$Analyte == ii,]
      if(length(subset.ii$RESULT > 0) & any(subset.ii$RESULT_clean.ug.l.neg > 0)){
        print(paste0(y, B, " ", ii, ": n=", length(subset.ii$RESULT), " sum=", sum(subset.ii$RESULT_clean.ug.l.subND)))
        
        df <- data.frame(date = unique(subset.ii$date), prior_date = as.Date(NA), display_date = as.Date(NA))
        df <- df[order(df$date),]
        df$index <- 1:nrow(df)
        df$prior_date_index <- df$index - 1
        df[2:nrow(df),'prior_date'] <- df[df$prior_date_index[-1],'date']
        df[1, 'prior_date'] <- df[1, 'date'] #assign the prior date to the same date
        df$diff <- df$date - df$prior_date
        df[which(df$diff > 2), 'display_date']  <- df[which(df$diff > 2), 'date'] 
        df$display_date[1] <- df$date[1]
        display_dates <- format(df$display_date, format="%m/%d/%y")
        
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
        a <- a + scale_x_date(breaks=df$date, labels=ifelse(is.na(display_dates), "", display_dates))
        a <- a + coord_cartesian(xlim=c(min(subset.ii$date)-1, max(subset.ii$date)+1)) #add a day to beginning and end
        a <- a + theme(aspect.ratio=1)
        a <- a + theme_bw() #blackandwhite theme
        a <- a + ylim(c(0, max(subset.ii$RESULT_clean.ug.l.subND*1.8))) #set the y range from zero to some multiplier of the max result to increase the head space
        a <- a + theme(panel.grid.minor.x = element_blank()) #remove minor grid lines
        #benchmarks lines and labels  
        if(length(numeric.criterion.graph)==0){  #if there is NO DEQ criteria or EPA benchmark
          title <- (paste0(B, " ", y," \n", ii, "\nNo benchmark available")) 
        }else{
          if(ii != "Chlorpyrifos" 
             & ii != "2,4-D" 
             & ii != "Atrazine" 
             & ii != "Simazine" 
             #& ii != "Deisopropylatrazine" 
             #& ii != "Desethylatrazine" 
             & length(numeric.criterion.graph)>0){  #list of names of the exceptions#if there IS DEQ criteria or EPA benchmark
            a <- a + geom_hline(yintercept=numeric.criterion.graph)  #draw it
            title <- (paste0(B, " ", y," \n", ii, numeric.criterion.label))
          }else{
            if(ii == "Chlorpyrifos"){  #Chlorpyrifos is only standard where we draw both lines
              a <- a + geom_hline(yintercept=0.083, linetype=2)  #draw Acute Chlorpyrifos WQS (only graph with two WQS)
              a <- a + geom_hline(yintercept=0.041, linetype=1)  #draw Chronic Chlorpyrifos WQS (only graph with two WQS)
              title <- (paste0(B, " ", y," \n", ii, numeric.criterion.label))
            }else{
              if(ii == "2,4-D"){  #Using the "2,4-D Acids and Salts"  
                a <- a + geom_hline(yintercept=13.1)  
                title <- (paste0(B, " ", y," \n", ii, "\nEPA benchmark = 13.1 ug/L "))
              }else{
                if(ii == "Atrazine"){  #Proposed EPA benchmarks 12/17/14  
                  a <- a + geom_hline(yintercept=1.0, linetype=1)  #draw solid last year's EPA benchmark
                  a <- a + geom_hline(yintercept=0.001, linetype=2)  #draw dashed proposed EPA benchmark
                  title <- (paste0(B, " ", y," \n", ii, numeric.criterion.label))
                }else{
                  if(ii == "Simazine"){  #Proposed EPA benchmarks 12/17/14  
                    a <- a + geom_hline(yintercept=36, linetype=1)  #draw solid line last year's EPA benchmark
                    a <- a + geom_hline(yintercept=2.24, linetype=2)  #draw dashed line proposed EPA benchmark
                    title <- (paste0(B, " ", y," \n", ii, numeric.criterion.label))
                  }else{
                    if(ii == "Deisopropylatrazine"){  #Proposed EPA benchmarks 12/17/14  
                      a <- a + geom_hline(yintercept=numeric.criterion.graph)  #draw it
                      title <- (paste0(B, " ", y," \n", "Triazine DIA degradate", numeric.criterion.label))
                    }else{
                      if(ii == "Desethylatrazine"){  #Proposed EPA benchmarks 12/17/14  
                        a <- a + geom_hline(yintercept=numeric.criterion.graph)  #draw it
                        title <- (paste0(B, " ", y," \n", "Triazine DEA degradate", numeric.criterion.label))
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
        #a <- a + theme(axis.text.x = element_text(angle=90, vjust=0.5, color="black", size=6))
        a <- a + theme(axis.text.x = element_text(angle=90, vjust=0.5, color="black", size=10))
        a <- grid.arrange((a), bottom= (paste0("prepared by Julia Crown, ODEQ, ", Sys.Date())))
        if(B == "South Coast") a <- grid.arrange((a), bottom= (paste0("prepared by Julia Crown, ODEQ, ", Sys.Date(), "            *Myrtle Creek sampled 9/24/14: no detections")))
        #             a <- arrangeGrob((a), sub = textGrob(paste0("prepared by Julia Crown, ODEQ, ", Sys.Date()), 
        #                                            x = 0, hjust = -0.1, vjust=0.1,
        #                                            gp = gpar(fontface = "italic", fontsize = 8))) 
        ggsave(filename = paste0("\\\\Deqhq1\\PSP\\Rscripts\\Alldates\\",Sys.Date(), "\\", Sys.Date(), "_TimeSeries\\", B, "_", ii, "_", "2014-15", "_savedon", Sys.Date(),".jpg"), plot = a)
      }                         
    }
  }
#}



#################################################################################
#ggplot 
#multiplot
#stations sorted by shape and color

dir.create(paste("\\\\Deqhq1\\PSP\\Rscripts\\Alldates\\",Sys.Date(), "\\", Sys.Date(), "_Multiplots", sep="")) 

B <- "Hood River"
#for(y in unique(mydata_clean_noV$year)){
#  subset.y <- mydata_clean_noV[mydata_clean_noV$year == y,]
  subset.y <- SouthPilots
  subset.y[subset.y$Analyte == "Aminomethylphosponic acid (AMPA)", "Analyte"] <- "AMPA"
  subset.y <- subset.y[subset.y$Station_Number != 37805,] #Remove Myrtle Creek from graph (because too many stations), but add a note that it was there
  
  for(B in unique(subset.y$Basin)){
    subset.B <- subset.y[subset.y$Station_Number != 32012,] #20141023 to fix the number of stations on WWatTheFrog.  
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
      a <- a + xlab("") + ylab(paste0("ug/L")) + ggtitle(paste0(B, " ", y)) #write the labels and title
      a <- a + theme(panel.grid.minor.x = element_blank()) #remove minor grid lines
      a <- a + facet_wrap(~Analyte, drop=TRUE, scales = "free_y")
      a <- a + scale_x_date(breaks=unique(subset.B$date), labels=format(unique(subset.B$date), format="%m/%d/%y"))
      a <- a + coord_cartesian(xlim=c(min(subset.B$date)-5, max(subset.B$date)+5)) #add 5 day to beginning and end
      a <- a + theme(axis.text.x  = element_text(angle=90, vjust=0.5, color="black", size=11))
      a <- a + theme(axis.text.y  = element_text(color="black", size=12))
      a <- a + guides(shape = guide_legend(ncol = 2)) #legend in two columns
      a <- a + theme(legend.position="bottom", legend.direction="vertical", legend.text=element_text(size=11), #move legend
                     legend.title=element_blank()) #remove title from legend
      a
      
      if(B == "South Umpqua") a <- grid.arrange((a), bottom= (paste0("prepared by Julia Crown, ODEQ, ", Sys.Date())))
      if(B == "South Coast") a <- grid.arrange((a), bottom= (paste0("prepared by Julia Crown, ODEQ, ", Sys.Date(), "        *Myrtle Creek sampled 9/24/14: no detections")))
      
      #     a <- arrangeGrob((a), sub = textGrob(paste0("prepared by Julia Crown, ODEQ, ", Sys.Date()), 
      #                                          x = 0, hjust = -0.1, vjust=0.1,
      #                                          gp = gpar(fontface = "italic", fontsize = 8))) 
#      ggsave(filename = paste0("\\\\Deqhq1\\PSP\\Rscripts\\Alldates\\",Sys.Date(), "\\", Sys.Date(), "_Multiplots\\", "multiplot_", B, "_", "2014-15", "_savedon", Sys.Date(),".jpg"), plot = a, scale=1.5)
      ggsave(filename = paste0("\\\\Deqhq1\\PSP\\Rscripts\\Alldates\\",Sys.Date(), "\\", Sys.Date(), "_Multiplots\\", "multiplot_", B, "_", "2014-15", "_savedon", Sys.Date(),".jpg"), plot = a, scale=1.7)
    }
  }
#}
