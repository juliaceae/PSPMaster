#This script should: Table calculated detection freqs, max, ave, other stats.  Need to source the data retreival and cleaning script first
#Basin and data range
#Julia Crown


#source('//deqhq1/PSP/Rscripts/PSPMaster/PlotTimeseries_alldates.R', encoding = 'UTF-8')

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

B <- "Wasco"
ii <- "Deisopropylatrazine"
ii <- "Total Solids"
B <- "Amazon"
ii <- "Chlorpyrifos"
B <- "South Coast"
B <- "Middle Rogue"
ii <- "Bifenthrin"
ii <- "Pentachlorophenol"
ii <- "4,4´-DDE"
y <- 2015
y <- as.integer(201415)

#20141023: Delete Walla Walla at the Frog results (because no detections) #32012
View(mydata_clean_noV[mydata_clean_noV$Station_Number == 32012 & mydata_clean_noV$RESULT != "ND" & mydata_clean_noV$date > as.Date("2015-01-01"),])

new.folder <- dir.create(paste("\\\\Deqhq1\\PSP\\Rscripts\\Alldates\\",Sys.Date(), "\\", Sys.Date(), "_TimeSeries", sep="")) 

for(y in unique(mydata_clean_noV$year)){
  subset.y <- mydata_clean_noV[mydata_clean_noV$year == y,]
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
        display_dates <- format(df$display_date, format="%m/%d")
        if(B %in% c("South Coast", "South Umpqua")) display_dates <- format(df$display_date, format="%m/%d/%y")
        
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
        if(y == as.integer(201415)) y <- "2014-2015"
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
        if(B == "South Coast") {
          a <- grid.arrange((a), bottom= (paste0("prepared by Julia Crown, ODEQ, ", Sys.Date(), "            *Myrtle Creek sampled 9/24/14: no detections")))
        }else{
          a <- grid.arrange((a), bottom= (paste0("prepared by Julia Crown, ODEQ, ", Sys.Date())))
          }
        #             a <- arrangeGrob((a), sub = textGrob(paste0("prepared by Julia Crown, ODEQ, ", Sys.Date()), 
        #                                            x = 0, hjust = -0.1, vjust=0.1,
        #                                            gp = gpar(fontface = "italic", fontsize = 8))) 
        ggsave(filename = paste0("\\\\Deqhq1\\PSP\\Rscripts\\Alldates\\",Sys.Date(), "\\", Sys.Date(), "_TimeSeries","\\", B, "_", ii, "_", y, "_savedon", Sys.Date(),".jpg"), plot = a)
      }
    }
  }
}


#################################################################################
#ggplot 
#multiplot
#stations sorted by shape and color

dir.create(paste("\\\\Deqhq1\\PSP\\Rscripts\\Alldates\\",Sys.Date(), "\\", Sys.Date(), "_Multiplots", sep="")) 

B <- "Hood River"
for(y in unique(mydata_clean_noV$year)){
  subset.y <- mydata_clean_noV[mydata_clean_noV$year == y,]
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
    a <- a + theme(panel.grid.minor.x = element_blank()) #remove minor grid lines
    a <- a + facet_wrap(~Analyte, drop=TRUE, scales = "free_y")
    a <- a + scale_x_date(breaks=unique(subset.B$date), labels=format(unique(subset.B$date), format="%m/%d"))
    if(B %in% c("South Coast", "South Umpqua")) a <- a + scale_x_date(breaks=unique(subset.B$date), labels=format(unique(subset.B$date), format="%m/%d/%y"))
    a <- a + coord_cartesian(xlim=c(min(subset.B$date)-5, max(subset.B$date)+5)) #add 5 day to beginning and end
    a <- a + theme(axis.text.x  = element_text(angle=90, vjust=0.5, color="black", size=10))
    a <- a + theme(axis.text.y  = element_text(color="black", size=10))
    if(y == as.integer(201415)) y <- "2014-2015"
    a <- a + xlab("") + ylab(paste0("ug/L")) + ggtitle(paste0(B, " ", y)) #write the labels and title
    a <- a + guides(shape = guide_legend(ncol = 2)) #legend in two columns
    a <- a + theme(legend.position="bottom", legend.direction="vertical", legend.text=element_text(size=12), #move legend
                   legend.title=element_blank()) #remove title from legend
    if(B == "South Coast") {
      a <- grid.arrange((a), bottom= (paste0("prepared by Julia Crown, ODEQ, ", Sys.Date(), "            *Myrtle Creek sampled 9/24/14: no detections")))
    }else{
      a <- grid.arrange((a), bottom= (paste0("prepared by Julia Crown, ODEQ, ", Sys.Date())))
    }
    ggsave(filename = paste0("\\\\Deqhq1\\PSP\\Rscripts\\Alldates\\",Sys.Date(), "\\", Sys.Date(), "_Multiplots\\", "multiplot_", B, "_", y, "_savedon", Sys.Date(),".jpg"), plot = a, scale=1.5)
  }
}
}


##################################################################################
# #one off graph for average annual detection frequency for Hood Diuron by site
# source('//deqhq1/PSP/Rscripts/PSPMaster/PlotTimeseries_alldates_Stats.R', encoding = 'UTF-8')
# Det.freq.table$Average <- as.numeric(Det.freq.table$Average)
# Det.freq.table$Year <- as.POSIXct(strptime(Det.freq.table$Year, format = '%Y'))
# 
# ii <- "Diuron"
# B <- "Hood River"
# 
#   subset.B <- Det.freq.table[Det.freq.table$Basin == B,]
#   subset.B <- subset.B[subset.B$Station != "Basin aggregate",] #20141023 to fix the number of stations on WWatTheFrog.  
#   subset.B <- subset.B[subset.B$Average != "by exceed type",] #20141023 to fix the number of stations on WWatTheFrog.  
#   subset.B <- subset.B[is.na(subset.B$Median)==FALSE,]
#   subset.B <- subset.B[is.na(subset.B$Station)==FALSE,]
#   subset.B <- subset.B[subset.B$Station.Description %in% c("Lenz Creek at mouth", "Neal Creek at mouth (upstream of bridge)"),] #20141023 to fix the number of stations on WWatTheFrog.  
#   
#     subset.ii <- subset.B[subset.B$Parameter == ii,]
#     if(ii == "Aminomethylphosponic acid (AMPA)") subset.ii[subset.ii$Parameter == "Aminomethylphosponic acid (AMPA)", "Parameter"] <- "AMPA"
#     if(length(subset.ii$Average > 0) & any(subset.ii$Average > 0)){
#       print(paste0(B, " ", ii, ": n=", length(subset.ii$Average)))
#       
#       
#       numeric.criterion.graph <- as.numeric(min.criteria[min.criteria$criteria.Pollutant == ii,'criteria.minimum.criteria.benchmark.value']) #find the lowest EPA AL benchmark
#       numeric.criterion.label <- min.criteria[min.criteria$criteria.Pollutant == ii,'label'] #find the lowest DEQ AL benchmark
#       a <- ggplot(data = subset.ii, #data source is the subset of Basin and analyte
#                   aes(x = Year, #x axis dates
#                       y = Average, #y axis is numeric result
#                       group=Station.Description, #change point shapes by station
#                       color=Station.Description)) #change point colors by station
#       a <- a + geom_point(size = 5) #set the point size
# #      a <- a + geom_line() #set the point size
#       a <- a + xlab("") + ylab(("Average Annual Concentration (ug/L)")) #write the labels
#       a <- a + scale_x_datetime(breaks=unique(subset.ii$Year), labels=format(unique(subset.ii$Year), format="%Y"))
#       #a <- a + scale_x_date(breaks=unique(subset.B$Year), labels=format(unique(subset.B$Year), format="%Y"))
#       a <- a + coord_cartesian(xlim=c(as.POSIXct(strptime(2009, format = '%Y')), max(subset.ii$Year)+1)) #add a day to beginning and end
#       a <- a + theme(aspect.ratio=1)
#       a <- a + theme_bw() #blackandwhite theme
#       a <- a + ylim(c(0, max(subset.ii$Average)*1.8)) #set the y range from zero to some multiplier of the max result to increase the head space
# #      a <- a + xlim(c(2009, max(subset.ii$Year))) #set the y range from zero to some multiplier of the max result to increase the head space
#       a <- a + theme(panel.grid.minor.x = element_blank()) #remove minor grid lines
#       #benchmarks lines and labels  
#       if(length(numeric.criterion.graph)==0){  #if there is NO DEQ criteria or EPA benchmark
#         title <- (paste0(B, " "," \n", ii, "\nNo benchmark available")) 
#       }else{
#         if(ii != "Chlorpyrifos" 
#            & ii != "2,4-D" 
#            & ii != "Atrazine" 
#            & ii != "Simazine" 
#            #& ii != "Deisopropylatrazine" 
#            #& ii != "Desethylatrazine" 
#            & length(numeric.criterion.graph)>0){  #list of names of the exceptions#if there IS DEQ criteria or EPA benchmark
#           a <- a + geom_hline(yintercept=numeric.criterion.graph)  #draw it
#           title <- (paste0(B, " "," \n", ii, numeric.criterion.label))
#         }else{
#           if(ii == "Chlorpyrifos"){  #Chlorpyrifos is only standard where we draw both lines
#             a <- a + geom_hline(yintercept=0.083, linetype=2)  #draw Acute Chlorpyrifos WQS (only graph with two WQS)
#             a <- a + geom_hline(yintercept=0.041, linetype=1)  #draw Chronic Chlorpyrifos WQS (only graph with two WQS)
#             title <- (paste0(B, " "," \n", ii, numeric.criterion.label))
#           }else{
#             if(ii == "2,4-D"){  #Using the "2,4-D Acids and Salts"  
#               a <- a + geom_hline(yintercept=13.1)  
#               title <- (paste0(B, " "," \n", ii, "\nEPA benchmark = 13.1 ug/L "))
#             }else{
#               if(ii == "Atrazine"){  #Proposed EPA benchmarks 12/17/14  
#                 a <- a + geom_hline(yintercept=1.0, linetype=1)  #draw solid last year's EPA benchmark
#                 a <- a + geom_hline(yintercept=0.001, linetype=2)  #draw dashed proposed EPA benchmark
#                 title <- (paste0(B, " ", " \n", ii, numeric.criterion.label))
#               }else{
#                 if(ii == "Simazine"){  #Proposed EPA benchmarks 12/17/14  
#                   a <- a + geom_hline(yintercept=36, linetype=1)  #draw solid line last year's EPA benchmark
#                   a <- a + geom_hline(yintercept=2.24, linetype=2)  #draw dashed line proposed EPA benchmark
#                   title <- (paste0(B, " "," \n", ii, numeric.criterion.label))
#                 }else{
#                   if(ii == "Deisopropylatrazine"){  #Proposed EPA benchmarks 12/17/14  
#                     a <- a + geom_hline(yintercept=numeric.criterion.graph)  #draw it
#                     title <- (paste0(B, " "," \n", "Triazine DIA degradate", numeric.criterion.label))
#                   }else{
#                     if(ii == "Desethylatrazine"){  #Proposed EPA benchmarks 12/17/14  
#                       a <- a + geom_hline(yintercept=numeric.criterion.graph)  #draw it
#                       title <- (paste0(B, " "," \n", "Triazine DEA degradate", numeric.criterion.label))
#                     }
#                   }
#                 }
#               }
#             }
#           }
#         }
#       }
#       
#       a <- a + ggtitle(title) #write the title and subtitle
#       a <- a + guides(shape = guide_legend(ncol = 2))
#       a <- a + theme(legend.position="bottom")
#       a <- a + theme(legend.direction="vertical")
#       a <- a + theme(legend.text=element_text(size=12))
#       a <- a + theme(legend.title=element_blank()) #remove title from legend
#       #a <- a + theme(axis.text.x = element_text(angle=90, vjust=0.5, color="black", size=10))
#       a <- a + theme(axis.text.x = element_text(angle=90, vjust=0.5, color="black", size=12))
#       a <- grid.arrange((a), bottom= (paste0("prepared by Julia Crown, ODEQ, ", Sys.Date())))
#       #             a <- arrangeGrob((a), sub = textGrob(paste0("prepared by Julia Crown, ODEQ, ", Sys.Date()), 
#       #                                            x = 0, hjust = -0.1, vjust=0.1,
#       #                                            gp = gpar(fontface = "italic", fontsize = 8))) 
#       if(ii == "Diuron") ggsave(filename = paste0(outpath.plot.points, "AverageAnnualConc", B, "LenzNeal_", ii, "_", "_savedon", Sys.Date(),"_points.jpg"), plot = a)
#     }

##########################################
#combined annual average concentration and percent detection frequency graphs
#source('//deqhq1/PSP/Rscripts/PSPMaster/PlotTimeseries_alldates_Stats.R', encoding = 'UTF-8')
    
dir.create(paste0("\\\\Deqhq1\\PSP\\Rscripts\\Alldates\\",Sys.Date())) 
dir.create(paste0("\\\\Deqhq1\\PSP\\Rscripts\\Alldates\\",Sys.Date(), "\\", Sys.Date(), "_AnnualAverageFreq")) 

ii <- "Diuron"
#ii <- "Pentachlorophenol"
B <- "Clackamas"

Det.freq.table$Average <- as.numeric(Det.freq.table$Average)
Det.freq.table$Year <- as.POSIXct(strptime(Det.freq.table$Year, format = '%Y'))

for (B in unique(Det.freq.table$Basin)) {
  subset.B <- Det.freq.table[Det.freq.table$Basin == B,]
  subset.B <- subset.B[subset.B$Station == "Basin aggregate",] 
  subset.B <- subset.B[is.na(subset.B$Station)==FALSE,]
  
  for(ii in unique(subset.B$Parameter)){
    subset.ii <- subset.B[subset.B$Parameter == ii,]
    if(ii == "Aminomethylphosponic acid (AMPA)") subset.ii[subset.ii$Parameter == "Aminomethylphosponic acid (AMPA)", "Parameter"] <- "AMPA"
    if(length(subset.ii$percent.det.freq > 0) & any(subset.ii$percent.det.freq > 0)){
      #      print(paste0(B, " ", ii, ": n=", length(subset.ii$percent.det.freq)))
      

f1 <- subset.ii 
f2 <- subset.ii 
f3 <- subset.ii 
#determine title names
f1$panel <- "Average annual concentration (ug/L)" 
f1$panel <- factor(f1$panel, levels=c("Maximum annual concentration (ug/L)", "Average annual concentration (ug/L)", "Percent Detection Frequency"))
f2$panel <- "Percent Detection Frequency" 
f2$panel <- factor(f2$panel, levels=c("Maximum annual concentration (ug/L)", "Average annual concentration (ug/L)", "Percent Detection Frequency"))
f3$panel <- "Maximum annual concentration (ug/L)" 
f3$panel <- factor(f3$panel, levels=c("Maximum annual concentration (ug/L)", "Average annual concentration (ug/L)", "Percent Detection Frequency"))
#f <- rbind(f3, f1, f2) 
#f$panel <- factor(f$panel, levels=c("Maximum annual concentration (ug/L)", "Average annual concentration (ug/L)", "Percent Detection Frequency"))

#outline the top panel
a <- ggplot(data = f1, mapping = aes(x = Year, y = Average))
#outline the bottom panel
#a <- a + facet_grid(panel~., scale="free", switch = 'y')
a <- a + facet_wrap(~panel, scale="free", ncol=1) #move the plot titles to the top of each plot
#plot the Max panel
a <- a + layer(data=f3, mapping = aes(x = Year, y = Max), geom =  "point", stat = "identity", position = "identity", params = list(na.rm = FALSE))
#a <- a + layer(data=f3, mapping = aes(x = Year, y = Max), geom =  "point", stat = "identity", position = "identity", params = list(na.rm = FALSE))
#a1 <- ggplot(data=f3, mapping = aes(x = Year, y = Max))#, geom =  "point", stat = "identity", position = "identity", params = list(na.rm = FALSE))
#a1 <- a1 + geom_point()
#plot the top panel
a <- a + layer(data=f1, mapping = aes(x = Year, y = Average), geom =  "point", stat = "identity", position = "identity", params = list(na.rm = FALSE))
#a <- a + layer(data=f1, mapping = aes(x = Year, y = Average), geom =  "point", stat = "identity", position = "identity", params = list(na.rm = FALSE))
#a2 <- ggplot(data=f1, mapping = aes(x = Year, y = Average), geom =  "point", stat = "identity", position = "identity", params = list(na.rm = FALSE))
#a2 <- a2 + geom_point()
#plot the bottom panel
a <- a + layer(data=f2, mapping = aes(x = Year, y = percent.det.freq), geom =  "point", stat = "identity", position = "identity", params = list(na.rm = FALSE))
#a <- a + layer(data=f2, mapping = aes(x = Year, y = percent.det.freq), geom =  "point", stat = "identity", position = "identity", params = list(na.rm = FALSE))
#a3 <- ggplot(data=f2, mapping = aes(x = Year, y = percent.det.freq), geom =  "point", stat = "identity", position = "identity", params = list(na.rm = FALSE))
#a3 <- a3 + geom_point()
a <- a + expand_limits(y = 0)
a <- a + scale_y_continuous(expand = c(0.005, 0))


#a <- ggplot(data=f3, aes(x = Year, y = Max)) + geom_point() 
#a <- a + ylim(c(0, max(f3$Max))) 
#a <- a + ylim(c(0, 10)) 
#a <- a + expand_limits(y = 0)
#a <- a + scale_y_continuous(expand = c(0.005, 0))

# b <- ggplot(data=f1, aes(x = Year, y = Average)) + geom_point()
# c <- ggplot(data=f2, aes(x = Year, y = percent.det.freq)) + geom_point()
# d <- grid.arrange(a, b, c, ncol=1)

numeric.criterion.graph <- as.numeric(min.criteria[min.criteria$criteria.Pollutant == ii,'criteria.minimum.criteria.benchmark.value']) #find the lowest EPA AL benchmark
numeric.criterion.label <- min.criteria[min.criteria$criteria.Pollutant == ii,'label'] #find the lowest DEQ AL benchmark

if(length(numeric.criterion.graph)==0){  #if there is NO DEQ criteria or EPA benchmark
  title <- (paste0(B, " "," \n", ii, "\nNo benchmark available")) 
}else{
  if(ii != "Chlorpyrifos" 
     & ii != "2,4-D" 
     & ii != "Atrazine" 
     & ii != "Simazine" 
     #& ii != "Deisopropylatrazine" 
     #& ii != "Desethylatrazine" 
     & length(numeric.criterion.graph)>0){  #list of names of the exceptions#if there IS DEQ criteria or EPA benchmark
    #          a <- a + geom_hline(yintercept=numeric.criterion.graph)  #draw it
    title <- (paste0(B, " "," \n", ii, numeric.criterion.label))
  }else{
    if(ii == "Chlorpyrifos"){  #Chlorpyrifos is only standard where we draw both lines
      #            a <- a + geom_hline(yintercept=0.083, linetype=2)  #draw Acute Chlorpyrifos WQS (only graph with two WQS)
      #            a <- a + geom_hline(yintercept=0.041, linetype=1)  #draw Chronic Chlorpyrifos WQS (only graph with two WQS)
      title <- (paste0(B, " "," \n", ii, numeric.criterion.label))
    }else{
      if(ii == "2,4-D"){  #Using the "2,4-D Acids and Salts"  
        #              a <- a + geom_hline(yintercept=13.1)  
        title <- (paste0(B, " "," \n", ii, "\nEPA benchmark = 13.1 ug/L "))
      }else{
        if(ii == "Atrazine"){  #Proposed EPA benchmarks 12/17/14  
          #                a <- a + geom_hline(yintercept=1.0, linetype=1)  #draw solid last year's EPA benchmark
          #                a <- a + geom_hline(yintercept=0.001, linetype=2)  #draw dashed proposed EPA benchmark
          title <- (paste0(B, " ", " \n", ii, numeric.criterion.label))
        }else{
          if(ii == "Simazine"){  #Proposed EPA benchmarks 12/17/14  
            #                  a <- a + geom_hline(yintercept=36, linetype=1)  #draw solid line last year's EPA benchmark
            #                  a <- a + geom_hline(yintercept=2.24, linetype=2)  #draw dashed line proposed EPA benchmark
            title <- (paste0(B, " "," \n", ii, numeric.criterion.label))
          }else{
            if(ii == "Deisopropylatrazine"){  #Proposed EPA benchmarks 12/17/14  
              #                    a <- a + geom_hline(yintercept=numeric.criterion.graph)  #draw it
              title <- (paste0(B, " "," \n", "Triazine DIA degradate", numeric.criterion.label))
            }else{
              if(ii == "Desethylatrazine"){  #Proposed EPA benchmarks 12/17/14  
                #                      a <- a + geom_hline(yintercept=numeric.criterion.graph)  #draw it
                title <- (paste0(B, " "," \n", "Triazine DEA degradate", numeric.criterion.label))
              }
            }
          }
        }
      }
    }
  }
}

a <- a + xlab("Year") + ylab(("")) #write the labels
a <- a + theme_bw() #blackandwhite theme
a <- a + scale_x_datetime(breaks=unique(subset.ii$Year), labels=format(unique(subset.ii$Year), format="%Y"))
a <- a + theme(axis.title.x  = element_text(size=10))
a <- a + theme()
a <- a + theme(panel.grid.minor.x = element_blank()) #remove minor grid lines
a <- a + ggtitle(title) #write the title and subtitle
a <- a + guides(shape = guide_legend(ncol = 2))
a <- a + theme(legend.position="bottom")
a <- a + theme(legend.direction="vertical")
a <- a + theme(legend.text=element_text(size=10))
a <- a + theme(legend.title=element_blank()) #remove title from legend
#a <- a + theme(axis.text.x = element_text(angle=90, vjust=0.5, color="black", size=10))
a <- a + theme(axis.text.x = element_text(angle=90, vjust=0.5, color="black", size=8))
a <- a + theme(aspect.ratio=0.25)
a <- grid.arrange((a), bottom= (paste0("prepared by Julia Crown, ODEQ, ", Sys.Date())))
#             a <- arrangeGrob((a), sub = textGrob(paste0("prepared by Julia Crown, ODEQ, ", Sys.Date()), 
#                                            x = 0, hjust = -0.1, vjust=0.1,
#                                            gp = gpar(fontface = "italic", fontsize = 8))) 
ggsave(filename = paste0("\\\\Deqhq1\\PSP\\Rscripts\\Alldates\\",Sys.Date(), "\\", Sys.Date(), "_AnnualAverageFreq\\", "AnnualAverageFrequency", B, "_", ii, "_", "_savedon", Sys.Date(),".jpg"),
       plot = a, 
       scale = 1.3)
    }
  }
}

##################################################################################
# 
# ####For each station, graph all analytes
# #detections0 <- subset(detections, Station_Number == 36179)
# 
# top.herb <- c("2,4-D",
#               "2,6-Dichlorobenzamide",
#               "Atrazine",
#               "Deisopropylatrazine",
#               "Desethylatrazine",
#               "Dichlobenil",
#               "Diuron",
#               "Simazine")
# herbicides <- c("2,4-D",
#                 "2,6-Dichlorobenzamide",
#                 "Acetochlor",
#                 "Aminomethylphosphonic acid (AMPA)",
#                 "Atrazine",
#                 "Bromacil",
#                 "Chlorpropham",
#                 "Cycloate",
#                 "Deisopropylatrazine",
#                 "Desethylatrazine",
#                 "Dicamba",
#                 "Dichlobenil",
#                 "Dimethenamid",
#                 "Diuron",
#                 "EPTC",
#                 "Fluridone",
#                 "Glyphosate",
#                 "Hexazinone",
#                 "Imazapyr",
#                 "Linuron",
#                 "Metolachlor",
#                 "Metribuzin",
#                 "Napropamide",
#                 "Norflurazon",
#                 "Oxyfluorfen",
#                 "Pendimethalin",
#                 "Prometon",
#                 "Pronamide",
#                 "Propazine",
#                 "Simazine",
#                 "Sulfometuron-methyl",
#                 "Tebuthiuron",
#                 "Terbacil",
#                 "Triclopyr",
#                 "Trifluralin")
# 
# top.insect <- c( "Carbaryl",
#                  "Chlorpyrifos",
#                  "Imidacloprid",
#                  "Malathion")
# insecticides <- c( "4,4´-DDD",
#                    "4,4´-DDE",
#                    "Acetamiprid",
#                    "Bifenthrin",
#                    "Carbaryl",
#                    "Carbofuran",
#                    "Chlorpyrifos",
#                    "Diazinon",
#                    "Dimethoate",
#                    "Ethoprop",
#                    "Imidacloprid",
#                    "Malathion",
#                    "Methiocarb",
#                    "Methomyl",
#                    "Oxamyl",
#                    "Prometon")
# 
# 
# fungicides <- c("Propiconazole", "Pyraclostrobin", "Chlorothalonil")
# 
# wood.preservative <- "Pentachlorophenol"
# PCP <- "DEET"
# 
# xicides <- list(top.herb, top.insect, fungicides)     
# 
# for(c in xicides){
#   subset.points <- mydata_clean_noV[mydata_clean_noV$Analyte %in% c,]
#   #subset.points <- subset(subset.points, Station_Number == i)
#   subset.points <- subset.points[!is.na(subset.points$RESULT_clean.ug.l),]
#   subset.points <- subset(subset.points, Analyte != "Total Solids")
#   print(c)
#   if(length(subset.points$RESULT_clean)>0){
#     
#     subset.points[is.na(subset.points$final_digress),"final_digress"] <- 0
#     subset.points[subset.points$final_digress == 0,"final_digress"] <- paste0("less than criteria or \n no criteria available")
#     subset.points[subset.points$final_digress == 1,"final_digress"] <- "more than criteria"
#     
#     a <- ggplot(subset.points, aes(date, RESULT_clean.ug.l))
#     a <- a + aes(shape = (final_digress)) +
#       geom_point(aes(colour = Analyte), size = 4) +
#       geom_point(colour="grey90", size = 1.5)
#     if(c == top.herb) title <- "Selected common herbicides"
#     if(c == top.insect) title <- "Selected common insecticides"
#     if(c == fungicides) title <- "Selected common fungicides"
#     
#     a <- a + xlab("2014") + ylab("ug/L") + ggtitle (title)
#     a <- a + theme(panel.grid.minor.x = element_blank()) #remove minor grid lines
#     a <- a + scale_x_date(breaks=unique(subset.points$date), labels=format(unique(subset.points$date), format="%m/%d"))
#     a <- a + coord_cartesian(xlim=c(min(subset.points$date)-1, max(subset.points$date)+1)) #add a day to beginning and end
#     #a <- a + ylim(c(0, max(subset.points$RESULT_clean.ug.l.subND*1.8))) #set the y range from zero to some multiplier of the max result to increase the head space
#     a <- a + theme(axis.text.x  = element_text(angle=90, vjust=0.5, color="black", size=8))
#     a <- a + theme(axis.text.y  = element_text(color="black", size=10))
#     #a <- a + guides(shape = guide_legend(ncol = 2)) #legend in two columns
#     #a <- a + guides(shape = guide_legend(ncol = 2))
#     #a <- a + theme(legend.position="bottom")
#     #a <- a + theme(legend.direction="vertical")
#     #a <- a + theme(legend.text=element_text(size=10))
#     a <- a + theme(legend.title=element_blank()) #remove title from legend
#     a <- arrangeGrob((a), sub = textGrob(paste0("prepared by Julia Crown, ODEQ, ", Sys.Date()), 
#                                          x = 0, hjust = -0.1, vjust=0.1,
#                                          gp = gpar(fontface = "italic", fontsize = 10))) 
#     ggsave(filename = paste0(outpath.plot.points, title, "_2014_savedon", Sys.Date(),".jpg"), plot = a, scale=1.5)
#     
#   }else{print(paste0("non-detect_", c))}
# }
# 
# ##################################################################################
# 
# #### For each station, graph all analytes
# #### Change the symbols to represent each analyte.  Exceedances are red/black (depending on colorblindness palette).  
# #### Points and graph size blown up for presentations.  
# herbicides <- c("2,4-D",
# #                 #"2,6-Dichlorobenzamide",
# #                 #"Acetochlor",
#                  "Aminomethylphosphonic acid (AMPA)",
# #                 #"Atrazine",
# #                 #"Bromacil",
# #                 #"Chlorpropham",
# #                 #"Cycloate",
# #                 #"Deisopropylatrazine",
# #                 #"Desethylatrazine",
# #                 #"Dicamba",
# #                 #"Dichlobenil",
#                  "Dimethenamid",
#                  "Diuron",
# #                 #"EPTC",
# #                 #"Fluridone",
#                  "Glyphosate",
#                  "Hexazinone",
# #                 #"Imazapyr",
#                  "Linuron",
#                  "Metolachlor",
#                  "Metribuzin",
# #                 #"Napropamide",
# #                 #"Norflurazon",
# #                 #"Oxyfluorfen",
#                  "Pendimethalin",
# #                 #"Prometon",
#                  "Prometryn",
# #                 #"Pronamide",
# #                 #"Propazine",
# #                 #"Simazine",
#                  "Sulfometuron-methyl",
# #                 #"Tebuthiuron",
#                  "Terbacil"#,
# #                 #"Triclopyr",
# #                 #"Trifluralin"
#                  )
# 
#  insectFung <- c(   #"4,4´-DDD",
# #                    "4,4´-DDE",
# #                    "Acetamiprid",
# #                    "Bifenthrin",
# #                    "Carbaryl",
# #                    "Carbofuran",
# #                    "Chlorpyrifos",
# #                    "Diazinon",
#                     "Dimethoate",
# #                    "Ethoprop",
# #                    "Imidacloprid",
# #                    "Malathion",
# #                    "Methiocarb",
# #                    "Methomyl",
# #                    "Oxamyl",
# #                    "Prometon",
#                     "Propiconazole", "Pyraclostrobin"#, "Chlorothalonil"
#                     )#fungicides
# xicides <- analytes     
# xicides <- herbicides     
# xicides <- insectFung     
# 
# i <- 37635 #Campbell
# i <- 37636 #Mudd
# station.list <- c(33215, 37637, 37636, 37635)
# 
# for(i in station.list){
#   subset.B <- subset(mydata_clean_noV, Station_Number == i)
#   subset.B <- subset(subset.B, is.na(subset.B$RESULT_clean.ug.l) == FALSE)
#   if(sum(subset.B$RESULT_clean.ug.l.subND) > 0){
#     subset.points <- subset(subset.B, Analyte != "Total Solids")
#     if(unique(subset.points$Station_Number) == 37636) subset.points <- subset(subset.points, Analyte != "2,4-D") #(take out the 2,4-D 'outlier')
#     analyte0 <- unique(subset.points$Analyte)
#   
#   #   col.v <- seq(1, 31)
#   col.v <- rep("#999999", 31)
#   pch.v <- c(seq(15, 25), seq(1, 14))  
#   pch.v <- c(seq(15, 25), seq(14, 0))  
#   bkgrd.col  <- "#E5E5E5"
#   exc.col <- "#000000"
#   cbPalette <- c("#999999", "#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#   
#   if(length(subset.points$RESULT_clean)>0){
#     x.min <- min(subset.points$date) #min of subset date
#     x.max <- max(subset.points$date) #max of subset date
#     x.lim <- c("2014/03/25", "2014/11/20")
#     x.lim <- if(length(subset.points$RESULT_clean) < 1){
#       c(x.min, x.max)  
#     }else{
#       c(x.min-5, x.max+5) ####define the data domain for graph
#     }
#     y.min <- 0
#     y.max <- max(subset.points$RESULT_clean.ug.l) #max of data for graph
#     #    if(ii == "Chlorpyrifos") y.max <- 0.083 #exception to accomodate chlorpyrifos secondary WQS
#     y.lim <- c(y.min,y.max + (0.1*y.max)) ####define the data range
#     x.lab <- ""
#     y.lab <- "ug/L"
#     title <- paste0(unique(subset.points$Station_Description), " 2014")
#     if(unique(subset.points$Station_Number) == 37636) {
#       title <- paste0(unique(subset.points$Station_Description), " 2014", "\n* 2,4-D detected at 5.1 ug/L on 5/7/14")
#     }
#     file.name.ts <- paste0(unique(subset.points$Station_Description), " 2014", "_all analytes", "_timeseries.png")
#     
#     png(filename=file.name.ts ,width = 950, height = 700) ####create a png with the station name in the filepath specified above
#     par(xpd=NA,oma=c(6,0,0,0), mar=c(6.1,4.1,4.1,2.1)) 
#     plot(subset.points$date, subset.points$RESULT_clean.ug.l,  pch=NA, xlim=x.lim, ylim=y.lim, xlab=x.lab, ylab=y.lab, cex.axis=1.2, cex.lab=1.2, bty="L", log=log.scale, main=title, font.main=12) ####plot the outline of the points  
#     #xaxt = 'n',
#     #axis(1, at = seq(x.min-5, x.max+5, by = 10))
#     for(p in 1:(length(xicides))){
#       subset.points.i <- subset(subset.points, Analyte == xicides[p])
#       points(subset.points.i$date, subset.points.i$RESULT_clean.ug.l, col=col.v[p], pch=pch.v[p], bg=bkgrd.col, cex=2.2)
#       exceeds.points.i <- subset.points.i[subset.points.i$final_digress == 1,]   
#       points(exceeds.points.i$date, exceeds.points.i$RESULT_clean.ug.l, col=exc.col, bg=exc.col, pch=pch.v[p], cex=2.2) ####plot the exceedances
#     }
#        
#     par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
#     plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
#     
#     ###legend for displaying on charts
#            legend("bottom", 
#                   xpd=TRUE, 
#                   inset=c(0, 0), 
#                      legend=xicides, 
#                      col= col.v, 
#                      pch=pch.v, 
#                      pt.bg=bkgrd.col,
#                      ncol=3,
#                      xjust=0, yjust=0, box.lty=0, cex=1.4, pt.cex=1.4, horiz=FALSE, 
#               )
#     
#     
#     dev.off() ####write the .png
#   }else{print(paste0("non-detect_"))}
# }
# }
####################################################
#One off graph requests for Yamhill and Pudding 20160509
library(ggplot2)
library(gridExtra)

Det.freq.table$Average <- as.numeric(Det.freq.table$Average)
#Det.freq.table$Year <- as.POSIXct(strptime(Det.freq.table$Year, format = '%Y'))
Det.freq.table[Det.freq.table$Parameter == "Aminomethylphosponic acid (AMPA)", "Parameter"] <- "AMPA"
Det.freq.table[Det.freq.table$Basin == "Molalla-Pudding", "Basin"] <- "Pudding"


#B <- "Yamhill" 
B <- "Pudding" 
#stn <- "West Fork Palmer at Webfoot Road Bridge"
stn <- "Zollner Creek at Dominic Road"
#stn <- "Little Pudding River at Rambler Road"
#stn <- "Pudding River at Hwy 99E (Aurora)"
#ppp <- c("Diuron", "Metolachlor", "Atrazine", "Simazine", "Sulfometuron-methyl") #herbicides Pudding 2012-15
ppp <- c("Chlorpyrifos", "Methomyl", "Carbaryl", "Imidacloprid", "Ethoprop") #insecticides Pudding 2012-15
#ppp <- c("Diuron", "Metolachlor", "Oxyfluorfen", "Simazine", "Sulfometuron-methyl") #herbicides WFPalmer 2012-15
#ppp <- c("Chlorpyrifos", "Endosulfan sulfate", "Diazinon", "Imidacloprid", "Ethoprop") #insecticides WFPalmer 2012-15
aaa <- Det.freq.table[Det.freq.table$Basin == B 
               & Det.freq.table$Station.Description == stn
               & Det.freq.table$Parameter %in% ppp
              # & Det.freq.table$Year %in% seq(2012, 2015, 1),] #West Fork Palmer 2012-15
               & Det.freq.table$Year %in% seq(2012, 2015, 1),] #Zollner Creek and Little Pudding at Rambler Rd

f1 <- aaa
f2 <- aaa
f3 <- aaa

f1$panel <- "Average annual concentration (ug/L)" 
f1$panel <- factor(f1$panel, levels=c("Maximum annual concentration (ug/L)", "Average annual concentration (ug/L)", "Percent Detection Frequency (%)"))
f2$panel <- "Percent Detection Frequency (%)" 
f2$panel <- factor(f2$panel, levels=c("Maximum annual concentration (ug/L)", "Average annual concentration (ug/L)", "Percent Detection Frequency (%)"))
f3$panel <- "Maximum annual concentration (ug/L)" 
f3$panel <- factor(f3$panel, levels=c("Maximum annual concentration (ug/L)", "Average annual concentration (ug/L)", "Percent Detection Frequency (%)"))

#outline the top panel
a <- ggplot(data = f1, mapping = aes(x = Year, y = Average))

#outline the bottom panel
a <- a + facet_wrap(~panel, scale="free", ncol=1) #move the plot titles to the top of each plot

#plot the Max panel
a <- a + layer(data=f3, mapping = aes(x=Year, y=Max, group=Parameter, colour=Parameter, shape=Parameter, size=5), geom="point", stat = "identity", position = "identity", params = list(na.rm = FALSE))
a <- a + layer(data=f3, mapping = aes(x = Year, y = Max, group=Parameter, colour=Parameter), geom =  "line", stat = "identity", position = "identity", params = list(na.rm = FALSE))

#plot the top panel
a <- a + layer(data=f1, mapping = aes(x = Year, y = Average, group=Parameter, colour=Parameter, shape=Parameter, size=5), geom =  "point", stat = "identity", position = "identity", params = list(na.rm = FALSE))
a <- a + layer(data=f1, mapping = aes(x = Year, y = Average, group=Parameter, colour=Parameter), geom =  "line", stat = "identity", position = "identity", params = list(na.rm = FALSE))

#plot the bottom panel
a <- a + layer(data=f2, mapping = aes(x = Year, y = percent.det.freq, group=Parameter, colour=Parameter, shape=Parameter, size=5), geom =  "point", stat = "identity", position = "identity", params = list(na.rm = FALSE))
a <- a + layer(data=f2, mapping = aes(x = Year, y = percent.det.freq, group=Parameter, colour=Parameter), geom =  "line", stat = "identity", position = "identity", params = list(na.rm = FALSE))

a <- a + ggtitle(paste0(stn))
a <- a + expand_limits(y = 0)
a <- a + scale_y_continuous(expand = c(0.005, 0))

a <- a + xlab("Year") + ylab(("")) #write the labels
a <- a + theme_bw() #blackandwhite theme
a <- a + theme(axis.title.x  = element_text(size=10))
a <- a + theme()
a <- a + theme(panel.grid.minor.x = element_blank()) #remove minor grid lines
a <- a + guides(shape = guide_legend(ncol = 2))
a <- a + theme(aspect.ratio=0.25)
a <- grid.arrange((a), bottom= (paste0("prepared by Julia Crown, ODEQ, ", Sys.Date())))
#ggsave(filename = paste0("\\\\Deqhq1\\PSP\\Yamhill\\2015Yamhill\\", B, "_", stn, "_Herbicides_savedon", Sys.Date(),".jpg"),
#ggsave(filename = paste0("\\\\Deqhq1\\PSP\\Yamhill\\2015Yamhill\\", B, "_", stn, "_Insecticides_savedon", Sys.Date(),".jpg"),
#ggsave(filename = paste0("\\\\Deqhq1\\PSP\\Pudding\\2015Pudding\\", B, "_", stn, "_Herbicides_savedon", Sys.Date(),".jpg"),
ggsave(filename = paste0("\\\\Deqhq1\\PSP\\Pudding\\2015Pudding\\", B, "_", stn, "_Insecticides_savedon", Sys.Date(),".jpg"),
       plot = a, 
       scale = 1.3)


####################################################
####################################################
#One off graph requests for Yamhill and Pudding 20160509
library(ggplot2)
library(gridExtra)

Det.freq.table$Average <- as.numeric(Det.freq.table$Average)
#Det.freq.table$Year <- as.POSIXct(strptime(Det.freq.table$Year, format = '%Y'))
Det.freq.table[Det.freq.table$Parameter == "Aminomethylphosponic acid (AMPA)", "Parameter"] <- "AMPA"
Det.freq.table[Det.freq.table$Basin == "Molalla-Pudding", "Basin"] <- "Pudding"


B <- "Yamhill" 
year.title <- 2014
year.title <- 2015
stn.title <- paste0("West Fork Palmer subbasin ", year.title)
stn <- c("West Fork Palmer at SE Lafayette Hwy", "West Fork Palmer Creek at SE Palmer Creek Road", "West Fork Palmer at Webfoot Road Bridge")
#stn <- c("Lower Cozine Creek at Davis Street Bridge", "Middle Cozine at Old Sheridan Road")
ppp <- c("Diuron", "Metolachlor", "Oxyfluorfen", "Simazine", "Sulfometuron-methyl") #herbicides Palmer aggr 2012-15
#ppp <- c("Chlorpyrifos", "Bifenthrin", "Dimethoate", "Diazinon", "Imidacloprid", "Ethoprop") #insecticides Palmer aggr 2012-15
aaa <- Det.freq.table[Det.freq.table$Basin == B 
                      & Det.freq.table$Station.Description %in% stn
                      & Det.freq.table$Parameter %in% ppp
                      & Det.freq.table$Year %in% year.title,] #Palmer 2014 or 2015
                     # & Det.freq.table$Year %in% (2015),] #Palmer 15
aaa[aaa$Station.Description == "West Fork Palmer at SE Lafayette Hwy", "Station.Description"] <- "at SE Lafayette Hwy"
aaa[aaa$Station.Description == "West Fork Palmer Creek at SE Palmer Creek Road", "Station.Description"] <- "at SE Palmer Creek Road"
aaa[aaa$Station.Description == "West Fork Palmer at Webfoot Road Bridge", "Station.Description"] <- "at Webfoot Road Bridge"

f1 <- aaa
f2 <- aaa
f3 <- aaa

f1$panel <- "Average annual concentration (ug/L)" 
f1$panel <- factor(f1$panel, levels=c("Maximum annual concentration (ug/L)", "Average annual concentration (ug/L)", "Percent Detection Frequency (%)"))
f2$panel <- "Percent Detection Frequency (%)" 
f2$panel <- factor(f2$panel, levels=c("Maximum annual concentration (ug/L)", "Average annual concentration (ug/L)", "Percent Detection Frequency (%)"))
f3$panel <- "Maximum annual concentration (ug/L)" 
f3$panel <- factor(f3$panel, levels=c("Maximum annual concentration (ug/L)", "Average annual concentration (ug/L)", "Percent Detection Frequency (%)"))

#outline the top panel
a <- ggplot(data = f1, mapping = aes(x = Station.Description, y = Average))

#outline the bottom panel
a <- a + facet_wrap(~panel, scale="free", ncol=1) #move the plot titles to the top of each plot

#plot the Max panel
a <- a + layer(data=f3, mapping = aes(x=Station.Description, y=Max, group=Parameter, colour=Parameter, shape=Parameter, size=5), geom="point", stat = "identity", position = "identity", params = list(na.rm = FALSE))
a <- a + layer(data=f3, mapping = aes(x = Station.Description, y = Max, group=Parameter, colour=Parameter), geom =  "line", stat = "identity", position = "identity", params = list(na.rm = FALSE))

#plot the top panel
a <- a + layer(data=f1, mapping = aes(x = Station.Description, y = Average, group=Parameter, colour=Parameter, shape=Parameter, size=5), geom =  "point", stat = "identity", position = "identity", params = list(na.rm = FALSE))
a <- a + layer(data=f1, mapping = aes(x = Station.Description, y = Average, group=Parameter, colour=Parameter), geom =  "line", stat = "identity", position = "identity", params = list(na.rm = FALSE))

#plot the bottom panel
a <- a + layer(data=f2, mapping = aes(x = Station.Description, y = percent.det.freq, group=Parameter, colour=Parameter, shape=Parameter, size=5), geom =  "point", stat = "identity", position = "identity", params = list(na.rm = FALSE))
a <- a + layer(data=f2, mapping = aes(x = Station.Description, y = percent.det.freq, group=Parameter, colour=Parameter), geom =  "line", stat = "identity", position = "identity", params = list(na.rm = FALSE))

a <- a + ggtitle(paste0(stn.title))
a <- a + expand_limits(y = 0)
a <- a + scale_y_continuous(expand = c(0.005, 0))

a <- a + xlab("Year") + ylab(("")) #write the labels
a <- a + theme_bw() #blackandwhite theme
a <- a + theme(axis.title.x  = element_text(size=10))
a <- a + theme()
a <- a + theme(panel.grid.minor.x = element_blank()) #remove minor grid lines
a <- a + guides(shape = guide_legend(ncol = 2))
a <- a + theme(aspect.ratio=0.25)
a <- grid.arrange((a), bottom= (paste0("prepared by Julia Crown, ODEQ, ", Sys.Date())))
ggsave(filename = paste0("\\\\Deqhq1\\PSP\\Yamhill\\2015Yamhill\\", B, "_", stn.title, "_Herbicides_savedon", Sys.Date(),".jpg"),
#ggsave(filename = paste0("\\\\Deqhq1\\PSP\\Yamhill\\2015Yamhill\\", B, "_", stn.title, "_Insecticides_savedon", Sys.Date(),".jpg"),
       plot = a, 
       scale = 1.3)

####################################################
#broken axis experiment
#BTW, current ggplot2 advice is no axis gaps.  Use a table!
install.packages("plotrix")
library(plotrix)
gap.barplot(aaa$Max, gap=c(3,12), xaxlab = paste0(aaa$Station.Description, aaa$Parameter))

dotchart(aaa$Max, labels=paste0(aaa$Parameter), xlab="Maximum ug/L in 2015", groups=as.factor(aaa$Station.Description))

#http://polisci.msu.edu/jacoby/research/dotplots/tpm/Creating%20figures/Creating%20Figure%204.R

library(ggplot2)
ggplot(aaa, aes(x = Station.Description, y = Max, colour = Parameter, size=5)) +
  geom_point()

library(ggplot2)
ggplot(Det.freq.table, aes(x = Max, y = Station.Description)) +
  geom_point()

library(ggplot2)
ggplot(mydata_clean_noV, aes(x = RESULT_clean.ug.l, y = Station.Description)) +
  geom_point()
