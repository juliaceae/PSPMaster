#This script should: Table calculated detection freqs, max, ave, other stats.  Need to source the data retreival and cleaning script first
#Basin and data range
#Julia Crown


source('//deqhq1/PSP/Rscripts/PSPMaster/PlotTimeseries_alldates.R', encoding = 'UTF-8')

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
y <- 2015

#20141023: Delete Walla Walla at the Frog results (because no detections) #32012
View(mydata_clean_noV[mydata_clean_noV$Station_Number == 32012 & mydata_clean_noV$RESULT != "ND" & mydata_clean_noV$date > as.Date("2015-01-01"),])


for(y in unique(mydata_clean_noV$year)){
  subset.y <- mydata_clean_noV[mydata_clean_noV$year == y,]
  subset.y[subset.y$Analyte == "Aminomethylphosponic acid (AMPA)", "Analyte"] <- "AMPA"
  
  for (B in unique(subset.y$Basin)) {
    subset.B <- subset.y[subset.y$Station_Number != 32012,] #20141023 to fix the number of stations on WWatTheFrog.  
    subset.B <- subset.B[subset.B$Basin == B,]
    for (ii in analytes){
      subset.ii <- subset.B[subset.B$Analyte == ii,]
      if(length(subset.ii$RESULT > 0) & any(subset.ii$RESULT_clean.ug.l.neg > 0)){
        print(paste0(y, B, " ", ii, ": n=", length(subset.ii$RESULT), " sum=", sum(subset.ii$RESULT_clean.ug.l.subND)))
        
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
        #a <- a + theme(axis.text.x = element_text(angle=90, vjust=0.5, color="black", size=10))
        a <- a + theme(axis.text.x = element_text(angle=90, vjust=0.5, color="black", size=6))
        a <- grid.arrange((a), bottom= (paste0("prepared by Julia Crown, ODEQ, ", Sys.Date())))
        #             a <- arrangeGrob((a), sub = textGrob(paste0("prepared by Julia Crown, ODEQ, ", Sys.Date()), 
        #                                            x = 0, hjust = -0.1, vjust=0.1,
        #                                            gp = gpar(fontface = "italic", fontsize = 8))) 
        ggsave(filename = paste0(outpath.plot.points, B, "_", ii, "_", y, "_savedon", Sys.Date(),".jpg"), plot = a)
      }
    }
  }
}


#################################################################################
#ggplot 
#multiplot
#stations sorted by shape and color

B <- "Hood River"
for(y in unique(mydata_clean_noV$year)){
  subset.y <- mydata_clean_noV[mydata_clean_noV$year == y,]
  subset.y[subset.y$Analyte == "Aminomethylphosponic acid (AMPA)", "Analyte"] <- "AMPA"
  
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
    a <- a + scale_x_date(breaks=unique(subset.B$date), labels=format(unique(subset.B$date), format="%m/%d"))
    a <- a + coord_cartesian(xlim=c(min(subset.B$date)-5, max(subset.B$date)+5)) #add 5 day to beginning and end
    a <- a + theme(axis.text.x  = element_text(angle=90, vjust=0.5, color="black", size=10))
    a <- a + theme(axis.text.y  = element_text(color="black", size=10))
    a <- a + guides(shape = guide_legend(ncol = 2)) #legend in two columns
    a <- a + theme(legend.position="bottom", legend.direction="vertical", legend.text=element_text(size=10), #move legend
                   legend.title=element_blank()) #remove title from legend
    a
    
    a <- grid.arrange((a), bottom= (paste0("prepared by Julia Crown, ODEQ, ", Sys.Date())))
    
    #     a <- arrangeGrob((a), sub = textGrob(paste0("prepared by Julia Crown, ODEQ, ", Sys.Date()), 
    #                                          x = 0, hjust = -0.1, vjust=0.1,
    #                                          gp = gpar(fontface = "italic", fontsize = 8))) 
    ggsave(filename = paste0(outpath.plot.points, "multiplot_", B, "_", y, "_savedon", Sys.Date(),".jpg"), plot = a, scale=1.5)
  }
}
}


##################################################################################
#average annual and percent detection frequency graphs
source('//deqhq1/PSP/Rscripts/PSPMaster/PlotTimeseries_alldates_Stats.R', encoding = 'UTF-8')
Det.freq.table$Average <- as.numeric(Det.freq.table$Average)
Det.freq.table$Year <- as.POSIXct(strptime(Det.freq.table$Year, format = '%Y'))

str(Det.freq.table$Year)

ii <- "Chlorpyrifos"
B <- "Yamhill"


for (B in unique(Det.freq.table$Basin)) {
  subset.B <- Det.freq.table[Det.freq.table$Basin == B,]
  subset.B <- subset.B[subset.B$Station == "Basin aggregate",] #20141023 to fix the number of stations on WWatTheFrog.  
  subset.B <- subset.B[is.na(subset.B$Station)==FALSE,]
  
  for(ii in unique(subset.B$Parameter)){
    subset.ii <- subset.B[subset.B$Parameter == ii,]
    if(ii == "Aminomethylphosponic acid (AMPA)") subset.ii[subset.ii$Parameter == "Aminomethylphosponic acid (AMPA)", "Parameter"] <- "AMPA"
    if(length(subset.ii$Average > 0) & any(subset.ii$Average > 0)){
      print(paste0(B, " ", ii, ": n=", length(subset.ii$Average)))
      
      
      numeric.criterion.graph <- as.numeric(min.criteria[min.criteria$criteria.Pollutant == ii,'criteria.minimum.criteria.benchmark.value']) #find the lowest EPA AL benchmark
      numeric.criterion.label <- min.criteria[min.criteria$criteria.Pollutant == ii,'label'] #find the lowest DEQ AL benchmark
      a <- ggplot(data = subset.ii, #data source is the subset of Basin and analyte
                  aes(x = Year, #x axis is dates
                      y = Average#, #y axis is numeric result
                      #                        group=Station_Description,
                      #                        shape=Station_Description, #change point shapes by station
                      #                        color=Station_Description
                  )) #change point colors by station
      a <- a + geom_point(size = 5) #set the point size
      a <- a + xlab("") + ylab(("Average Annual Concentration (ug/L)")) #write the labels
      a <- a + scale_x_datetime(breaks=unique(subset.ii$Year), labels=format(unique(subset.ii$Year), format="%Y"))
      #        a <- a + scale_x_date(breaks=unique(subset.B$Year), labels=format(unique(subset.B$Year), format="%Y"))
      a <- a + coord_cartesian(xlim=c(min(subset.B$Year)-1, max(subset.B$Year)+1)) #add a day to beginning and end
      a <- a + theme(aspect.ratio=1)
      a <- a + theme_bw() #blackandwhite theme
      a <- a + ylim(c(0, max(subset.ii$Average)*1.8)) #set the y range from zero to some multiplier of the max result to increase the head space
      a <- a + theme(panel.grid.minor.x = element_blank()) #remove minor grid lines
      #benchmarks lines and labels  
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
          a <- a + geom_hline(yintercept=numeric.criterion.graph)  #draw it
          title <- (paste0(B, " "," \n", ii, numeric.criterion.label))
        }else{
          if(ii == "Chlorpyrifos"){  #Chlorpyrifos is only standard where we draw both lines
            a <- a + geom_hline(yintercept=0.083, linetype=2)  #draw Acute Chlorpyrifos WQS (only graph with two WQS)
            a <- a + geom_hline(yintercept=0.041, linetype=1)  #draw Chronic Chlorpyrifos WQS (only graph with two WQS)
            title <- (paste0(B, " "," \n", ii, numeric.criterion.label))
          }else{
            if(ii == "2,4-D"){  #Using the "2,4-D Acids and Salts"  
              a <- a + geom_hline(yintercept=13.1)  
              title <- (paste0(B, " "," \n", ii, "\nEPA benchmark = 13.1 ug/L "))
            }else{
              if(ii == "Atrazine"){  #Proposed EPA benchmarks 12/17/14  
                a <- a + geom_hline(yintercept=1.0, linetype=1)  #draw solid last year's EPA benchmark
                a <- a + geom_hline(yintercept=0.001, linetype=2)  #draw dashed proposed EPA benchmark
                title <- (paste0(B, " ", " \n", ii, numeric.criterion.label))
              }else{
                if(ii == "Simazine"){  #Proposed EPA benchmarks 12/17/14  
                  a <- a + geom_hline(yintercept=36, linetype=1)  #draw solid line last year's EPA benchmark
                  a <- a + geom_hline(yintercept=2.24, linetype=2)  #draw dashed line proposed EPA benchmark
                  title <- (paste0(B, " "," \n", ii, numeric.criterion.label))
                }else{
                  if(ii == "Deisopropylatrazine"){  #Proposed EPA benchmarks 12/17/14  
                    a <- a + geom_hline(yintercept=numeric.criterion.graph)  #draw it
                    title <- (paste0(B, " "," \n", "Triazine DIA degradate", numeric.criterion.label))
                  }else{
                    if(ii == "Desethylatrazine"){  #Proposed EPA benchmarks 12/17/14  
                      a <- a + geom_hline(yintercept=numeric.criterion.graph)  #draw it
                      title <- (paste0(B, " "," \n", "Triazine DEA degradate", numeric.criterion.label))
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
      a <- a + theme(legend.text=element_text(size=12))
      a <- a + theme(legend.title=element_blank()) #remove title from legend
      #a <- a + theme(axis.text.x = element_text(angle=90, vjust=0.5, color="black", size=10))
      a <- a + theme(axis.text.x = element_text(angle=90, vjust=0.5, color="black", size=12))
      a <- grid.arrange((a), bottom= (paste0("prepared by Julia Crown, ODEQ, ", Sys.Date())))
      #             a <- arrangeGrob((a), sub = textGrob(paste0("prepared by Julia Crown, ODEQ, ", Sys.Date()), 
      #                                            x = 0, hjust = -0.1, vjust=0.1,
      #                                            gp = gpar(fontface = "italic", fontsize = 8))) 
      ggsave(filename = paste0(outpath.plot.points, "AverageAnnualConc", B, "_", ii, "_", "_savedon", Sys.Date(),".jpg"), plot = a)
    }
  }
}

###########


###########
#percent detection frequency (go back and facet wrap this with the average conc graphs above)
ii <- "Chlorpyrifos"
B <- "Yamhill"

for (B in unique(Det.freq.table$Basin)) {
  subset.B <- Det.freq.table[Det.freq.table$Basin == B,]
  subset.B <- subset.B[subset.B$Station == "Basin aggregate",] #20141023 to fix the number of stations on WWatTheFrog.  
  subset.B <- subset.B[is.na(subset.B$Station)==FALSE,]
  
  for(ii in unique(subset.B$Parameter)){
    subset.ii <- subset.B[subset.B$Parameter == ii,]
    if(ii == "Aminomethylphosponic acid (AMPA)") subset.ii[subset.ii$Parameter == "Aminomethylphosponic acid (AMPA)", "Parameter"] <- "AMPA"
    if(length(subset.ii$percent.det.freq > 0) & any(subset.ii$percent.det.freq > 0)){
#      print(paste0(B, " ", ii, ": n=", length(subset.ii$percent.det.freq)))
      
      numeric.criterion.graph <- as.numeric(min.criteria[min.criteria$criteria.Pollutant == ii,'criteria.minimum.criteria.benchmark.value']) #find the lowest EPA AL benchmark
      numeric.criterion.label <- min.criteria[min.criteria$criteria.Pollutant == ii,'label'] #find the lowest DEQ AL benchmark
      a <- ggplot(data = subset.ii, #data source is the subset of Basin and analyte
                  aes(x = Year, #x axis is dates
                      y = percent.det.freq#, #y axis is numeric result
                      #                        group=Station_Description,
                      #                        shape=Station_Description, #change point shapes by station
                      #                        color=Station_Description
                  )) #change point colors by station
      a <- a + geom_point(size = 5) #set the point size
      a <- a + xlab("") + ylab(("Percent Detection Frequency")) #write the labels
      a <- a + scale_x_datetime(breaks=unique(subset.ii$Year), labels=format(unique(subset.ii$Year), format="%Y"))
      a <- a + coord_cartesian(xlim=c(min(subset.B$Year)-1, max(subset.B$Year)+1)) #add a day to beginning and end
      a <- a + theme(aspect.ratio=1)
      a <- a + theme_bw() #blackandwhite theme
      a <- a + ylim(c(0, 100)) #set the y range from zero to some multiplier of the max result to increase the head space
      a <- a + theme(panel.grid.minor.x = element_blank()) #remove minor grid lines
      #benchmarks lines and labels  
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
      
      a <- a + ggtitle(title) #write the title and subtitle
      a <- a + guides(shape = guide_legend(ncol = 2))
      a <- a + theme(legend.position="bottom")
      a <- a + theme(legend.direction="vertical")
      a <- a + theme(legend.text=element_text(size=12))
      a <- a + theme(legend.title=element_blank()) #remove title from legend
      #a <- a + theme(axis.text.x = element_text(angle=90, vjust=0.5, color="black", size=10))
      a <- a + theme(axis.text.x = element_text(angle=90, vjust=0.5, color="black", size=12))
      a <- grid.arrange((a), bottom= (paste0("prepared by Julia Crown, ODEQ, ", Sys.Date())))
      #             a <- arrangeGrob((a), sub = textGrob(paste0("prepared by Julia Crown, ODEQ, ", Sys.Date()), 
      #                                            x = 0, hjust = -0.1, vjust=0.1,
      #                                            gp = gpar(fontface = "italic", fontsize = 8))) 
      ggsave(filename = paste0(outpath.plot.points, "PercentDetectionFrequency", B, "_", ii, "_", "_savedon", Sys.Date(),".jpg"), plot = a)
    }
  }
}



#####################
SCRATCH COMBINING


f1 <- subset.ii 
f2 <- subset.ii 
f1$panel <- "Average annual concentration (ug/L)" 
f2$panel <- "Percent Detection Frequency" 
f <- rbind(f1, f2) 

ff <- ggplot(data = f, mapping = aes(x = Year, y = Average))
ff <- ff + facet_grid(panel~., scale="free")
ff <- ff + layer(data=f1, mapping = aes(x = Year, y = Average), geom =  "point", stat = "identity", position = "identity", params = list(na.rm = FALSE))
ff <- ff + layer(data=f2, mapping = aes(x = Year, y = percent.det.freq), geom =  "point", stat = "identity", position = "identity", params = list(na.rm = FALSE))
ff
ff <- ff + xlab("Year") + ylab(("")) #write the labels
ff <- ff + theme_bw() #blackandwhite theme
ff <- ff + scale_x_datetime(breaks=unique(subset.ii$Year), labels=format(unique(subset.ii$Year), format="%Y"))

a <- a + geom_point(size = 5) #set the point size
a <- a + ylim(c(0, 100)) #set the y range from zero to some multiplier of the max result to increase the head space
a <- a + theme(panel.grid.minor.x = element_blank()) #remove minor grid lines



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
