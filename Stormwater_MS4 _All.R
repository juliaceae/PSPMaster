#Colin Donald and Julia Crown
#initial date started June 30, 2016
#compile electronic data voluntarily provided by MS4 permittees. 
#Make a summary table of analytes and range of Results, MDLs, MRLs

library("dplyr")
library("stringr")

source('//deqhq1/psp/rscripts/criteria/ToxicsCriteriaPSP_CD.R')

# Read in data ------------------------------------------------------------

#Eugene data
Eugene <- read.csv("//Deqhq1/STORMWATER/Muni Stormwater Program/4 Science & Technology/Pesticide Monitoring/MS4_Pesticide_Data/DEQ_Pesticides_083115_EUGENE.csv", strip.white = TRUE)
Eugene$Result.RAW <- Eugene$RESULT
Eugene$PQL.RAW <- Eugene$PQL
Eugene$AUNIT.RAW <- Eugene$AUNIT
Eugene$Result <- as.numeric(levels(Eugene$RESULT))[Eugene$RESULT]
Eugene$Col_Date <- as.Date(Eugene$Col_Date, "%m/%d/%Y")
Eugene$Year <- format(Eugene$Col_Date, "%Y")
Eugene$Analyte <- as.character(Eugene$ANALYTE)

#Salem Data
Salem_Electric <- read.csv("//Deqhq1/STORMWATER/Muni Stormwater Program/4 Science & Technology/Pesticide Monitoring/MS4_Pesticide_Data/City of Salem Pesticide Data for DEQ_Electric.csv", strip.white = TRUE)
Salem_Electric_DUP <- read.csv("//Deqhq1/STORMWATER/Muni Stormwater Program/4 Science & Technology/Pesticide Monitoring/MS4_Pesticide_Data/City of Salem Pesticide Data for DEQ_Electric_DUP.csv", strip.white = TRUE)
Salem_Hilfiker <- read.csv("//Deqhq1/STORMWATER/Muni Stormwater Program/4 Science & Technology/Pesticide Monitoring/MS4_Pesticide_Data/City of Salem Pesticide Data for DEQ_Hilfiker.csv", strip.white = TRUE)
Salem_Hilfiker_DUP <- read.csv("//Deqhq1/STORMWATER/Muni Stormwater Program/4 Science & Technology/Pesticide Monitoring/MS4_Pesticide_Data/City of Salem Pesticide Data for DEQ_Hilfiker_DUP.csv", strip.white = TRUE)
Salem_SalemInd <- read.csv("//Deqhq1/STORMWATER/Muni Stormwater Program/4 Science & Technology/Pesticide Monitoring/MS4_Pesticide_Data/City of Salem Pesticide Data for DEQ_Salem Ind.csv", strip.white = TRUE)
Salem_SalemInd_DUP <- read.csv("//Deqhq1/STORMWATER/Muni Stormwater Program/4 Science & Technology/Pesticide Monitoring/MS4_Pesticide_Data/City of Salem Pesticide Data for DEQ_Salem Ind_DUP.csv", strip.white = TRUE)

Salem <- rbind(Salem_Electric[, 1:17],
               Salem_Electric_DUP[,1:17],
               Salem_Hilfiker[, 1:17],
               Salem_Hilfiker_DUP[, 1:17],
               Salem_SalemInd[, 1:17],
               Salem_SalemInd_DUP[, 1:17])

Salem$Result <- as.numeric(levels(Salem$Result))[Salem$Result]
Salem$Sample.Date <- as.Date(Salem$Sample.Date, "%m/%d/%Y")
Salem$Year <- format(Salem$Sample.Date, "%Y")
Salem$DL <- as.numeric(Salem$DL)
Salem$Analyte <- as.character(Salem$Analyte)

#Portland Data
Portland <- read.csv("//Deqhq1/STORMWATER/Muni Stormwater Program/4 Science & Technology/Pesticide Monitoring/MS4_Pesticide_Data/Portland pesticide data.csv", strip.white = TRUE)

Portland$Result.na <- Portland$Numeric.Result
Portland$Result.na[Portland$Result.na == Portland$MRL] <- NA
Portland$Sample.Date <- as.Date(Portland$Sample.Date, "%m/%d/%Y")
Portland$Year <- format(Portland$Sample.Date, "%Y")
Portland$Analyte <- as.character(Portland$Analysis)

#Multnomah County Data
MultCo <- read.csv("//Deqhq1/STORMWATER/Muni Stormwater Program/4 Science & Technology/Pesticide Monitoring/MS4_Pesticide_Data/MultCo UIC Pesticide Data.csv", skip = 3, strip.white = TRUE)

MultCo$Result <- as.numeric(levels(MultCo$Result))[MultCo$Result]
MultCo$Sample.Date <- as.Date(MultCo$Sample.Date, "%m/%d/%Y")
MultCo$Year <- format(MultCo$Sample.Date, "%Y")
MultCo$DL <- as.numeric(MultCo$DL)
MultCo$Analyte <- as.character(MultCo$Analyte)

#Gresham Data
Gresham <- read.csv('//deqhq1/STORMWATER/Muni Stormwater Program/4 Science & Technology/Pesticide Monitoring/MS4_Pesticide_Data/City of Gresham pesticide data.csv', stringsAsFactors=FALSE, strip.white = TRUE)

Gresham$Sample.Date <- as.Date(Gresham$Sample.Date, "%m/%d/%Y")
Gresham$Year <- format(Gresham$Sample.Date, "%Y")

#Clackamas Data
Clackamas_Results <- read.csv('//deqhq1/STORMWATER/Muni Stormwater Program/4 Science & Technology/Pesticide Monitoring/MS4_Pesticide_Data/Clackamas_Results.csv', stringsAsFactors=FALSE, strip.white = TRUE)
Clackamas_MDLs <- read.csv('//deqhq1/STORMWATER/Muni Stormwater Program/4 Science & Technology/Pesticide Monitoring/MS4_Pesticide_Data/Clackamas_MDLs.csv', stringsAsFactors=FALSE, strip.white = TRUE)

Clackamas_Results <- Clackamas_Results[!is.na(Clackamas_Results$Result), ]
Clackamas <- merge(Clackamas_Results, Clackamas_MDLs, by.x = "Analyte", by.y = "Pesticide.compound.water", all.x = TRUE)


# Standardize and format data --------------------------------------------------------


#make sure all units are ug/l
unique(Eugene$AUNIT)
unique(Salem$Unit)
unique(Portland$Units)
unique(MultCo$Unit)
unique(Gresham$Unit)

#convert to ug/l
Gresham$Result <- as.numeric(Gresham$Result)
Gresham$Result.ug.l <- as.numeric(0)
Gresham$DL.ug.l <- as.numeric(0)
Gresham$QL.ug.l <- as.numeric(0)
Gresham[Gresham$Unit == "mg/L", "Result.ug.l"] <- Gresham[Gresham$Unit == "mg/L", ]$Result*1000
Gresham[Gresham$Unit == "ng/L", "Result.ug.l"] <- Gresham[Gresham$Unit == "ng/L", ]$Result/1000
Gresham[Gresham$Unit == "µg/L", "Result.ug.l"] <- Gresham[Gresham$Unit == "µg/L", ]$Result

Gresham[Gresham$Unit == "mg/L", "DL.ug.l"] <- Gresham[Gresham$Unit == "mg/L", ]$DL*1000
Gresham[Gresham$Unit == "ng/L", "DL.ug.l"] <- Gresham[Gresham$Unit == "ng/L", ]$DL/1000
Gresham[Gresham$Unit == "µg/L", "DL.ug.l"] <- Gresham[Gresham$Unit == "µg/L", ]$DL

Gresham[Gresham$Unit == "mg/L", "QL.ug.l"] <- Gresham[Gresham$Unit == "mg/L", ]$QL*1000
Gresham[Gresham$Unit == "ng/L", "QL.ug.l"] <- Gresham[Gresham$Unit == "ng/L", ]$QL/1000
Gresham[Gresham$Unit == "µg/L", "QL.ug.l"] <- Gresham[Gresham$Unit == "µg/L", ]$QL


# Apply benchmarks and create ratios --------------------------------------


#Bring in Benchmarks
Benchmarks <- merge(min.state.AQL, min.state.HH, by = "Pollutant", all.y = TRUE)
Benchmarks$min.bmark <- pmin(Benchmarks$min.HH.value, Benchmarks$min.AQL.value, na.rm=TRUE)
Benchmarks$Pollutant_lcase <- tolower(Benchmarks$Pollutant)
Benchmarks[(duplicated(Benchmarks$Pollutant)), "Pollutant"]
# Benchmarks <- Benchmarks[!(duplicated(Benchmarks$Pollutant)), ]

#merge benchmarks to city data
Eug_Bench <- merge(Eugene, Benchmarks, by.x = "Analyte", by.y = "Pollutant", all.x = TRUE, incomparables = NA)
Eug_Bench$Result <- as.numeric(Eug_Bench$Result)

Salem_Bench <- merge(Salem, Benchmarks, by.x = "Analyte", by.y = "Pollutant", all.x = TRUE)

Portland_Bench <- merge(Portland, Benchmarks, by.x = "Analyte", by.y = "Pollutant_lcase", all.x = TRUE)

MultCo_Bench <- merge(MultCo, Benchmarks, by.x = "Analyte", by.y = "Pollutant", all.x = TRUE)

Gresham_Bench <- merge(Gresham, Benchmarks, by.x = "Analyte", by.y = "Pollutant", all.x = TRUE)

Clack_Bench <- merge(Clackamas, Benchmarks, by.x = "Analyte", by.y = "Pollutant", all.x = TRUE)

#create ratio of each detection result to the AQL and the HH benchmark
Eug_Bench$AQL_Ratio <- Eug_Bench$Result/Eug_Bench$min.AQL.value
Eug_Bench$HH_Ratio <- Eug_Bench$Result/Eug_Bench$min.HH.value

Salem_Bench$AQL_Ratio <- Salem_Bench$Result/Salem_Bench$min.AQL.value
Salem_Bench$HH_Ratio <- Salem_Bench$Result/Salem_Bench$min.HH.value

Portland_Bench$AQL_Ratio <- Portland_Bench$Result.na/Portland_Bench$min.AQL.value
Portland_Bench$HH_Ratio <- Portland_Bench$Result.na/Portland_Bench$min.HH.value

MultCo_Bench$AQL_Ratio <- MultCo_Bench$Result/MultCo_Bench$min.AQL.value
MultCo_Bench$HH_Ratio <- MultCo_Bench$Result/MultCo_Bench$min.HH.value

Gresham_Bench$AQL_Ratio <- Gresham_Bench$Result.ug.l/Gresham_Bench$min.AQL.value
Gresham_Bench$HH_Ratio <- Gresham_Bench$Result.ug.l/Gresham_Bench$min.HH.value

Clack_Bench$AQL_Ratio <- Clack_Bench$Result.ug.l/Clack_Bench$min.AQL.value
Clack_Bench$HH_Ratio <- Clack_Bench$Result.ug.l/Clack_Bench$min.HH.value

# Summarize results -------------------------------------------------------


#summarize data by Analyte
Eugene_range <- Eug_Bench %>% group_by(Analyte) %>% group_by(Year, add=TRUE) %>% dplyr:::summarise(City = "Eugene",
                                                                                           Result_min = min(Result, na.rm = TRUE),
                                                                                           Result_max = max(Result, na.rm = TRUE),
                                                                                           N_Detects = sum(!is.na(Result)),
                                                                                           N_Samples = sum(!is.na(SAMPNO)),
                                                                                           Detect_Freq = N_Detects/N_Samples,
                                                                                           AQL_Value = min(min.AQL.value),
                                                                                           Over_AQL = sum(na.omit(AQL_Ratio) > 1.0),
                                                                                           "50_100_AQL" = sum(na.omit(AQL_Ratio) > 0.5 & na.omit(AQL_Ratio) <= 1.0),
                                                                                           "10_50_AQL" = sum(na.omit(AQL_Ratio) > 0.1 & na.omit(AQL_Ratio) <= 0.5),
                                                                                           "<10_AQL" = sum(na.omit(AQL_Ratio) <= 0.1),
                                                                                           Max_AQL_Ratio = max(AQL_Ratio, na.rm = TRUE),
                                                                                           HH_Value = min(min.HH.value),
                                                                                           Over_HH = sum(na.omit(HH_Ratio) > 1.0),
                                                                                           "50_100_HH" = sum(na.omit(HH_Ratio) > 0.5 & na.omit(HH_Ratio) <= 1.0),
                                                                                           "10_50_HH" = sum(na.omit(HH_Ratio) > 0.1 & na.omit(HH_Ratio) <= 0.5),
                                                                                           "<10_HH" = sum(na.omit(HH_Ratio) <= 0.1),
                                                                                           Max_HH_Ratio = max(HH_Ratio, na.rm = TRUE),
                                                                                           PQL_min = min(PQL, na.rm = TRUE),
                                                                                           PQL_max = max(PQL, na.rm = TRUE))

Salem_range <- Salem_Bench %>% group_by(Analyte) %>% group_by(Year, add=TRUE) %>% dplyr:::summarise(City = "Salem",
                                                                                            Result_min = min(Result, na.rm = TRUE),
                                                                                            Result_max = max(Result, na.rm = TRUE),
                                                                                            QL_min = min(QL, na.rm = TRUE),
                                                                                            QL_max = max(QL, na.rm = TRUE),
                                                                                            N_Detects = sum(!is.na(Result)),
                                                                                            N_Samples = sum(!is.na(Analyte)),
                                                                                            Detect_Freq = N_Detects/N_Samples,
                                                                                            AQL_Value = min(min.AQL.value),
                                                                                            Over_AQL = sum(na.omit(AQL_Ratio) > 1.0),
                                                                                            "50_100_AQL" = sum(na.omit(AQL_Ratio) > 0.5 & na.omit(AQL_Ratio) <= 1.0),
                                                                                            "10_50_AQL" = sum(na.omit(AQL_Ratio) > 0.1 & na.omit(AQL_Ratio) <= 0.5),
                                                                                            "<10_AQL" = sum(na.omit(AQL_Ratio) <= 0.1),
                                                                                            Max_AQL_Ratio = max(AQL_Ratio, na.rm = TRUE),
                                                                                            HH_Value = min(min.HH.value),
                                                                                            Over_HH = sum(na.omit(HH_Ratio) > 1.0),
                                                                                            "50_100_HH" = sum(na.omit(HH_Ratio) > 0.5 & na.omit(HH_Ratio) <= 1.0),
                                                                                            "10_50_HH" = sum(na.omit(HH_Ratio) > 0.1 & na.omit(HH_Ratio) <= 0.5),
                                                                                            "<10_HH" = sum(na.omit(HH_Ratio) <= 0.1),
                                                                                            Max_HH_Ratio = max(HH_Ratio, na.rm = TRUE))


Portland_range <- Portland_Bench %>% group_by(Analyte) %>% group_by(Year, add=TRUE) %>% dplyr:::summarise(City = "Portland",
                                                                                                   Result_min = min(Result.na, na.rm = TRUE),
                                                                                                   Result_max = max(Result.na, na.rm = TRUE),
                                                                                                   MDL_min = min(MDL, na.rm = TRUE),
                                                                                                   MDL_max = max(MDL, na.rm = TRUE),
                                                                                                   MRL_min = min(MRL, na.rm = TRUE),
                                                                                                   MRL_max = max(MRL, na.rm = TRUE),
                                                                                                   N_Detects = sum(!is.na(Result.na)),
                                                                                                   N_Samples = sum(!is.na(Analysis)),
                                                                                                   Detect_Freq = N_Detects/N_Samples,
                                                                                                   AQL_Value = min(min.AQL.value),
                                                                                                   Over_AQL = sum(na.omit(AQL_Ratio) > 1.0),
                                                                                                   "50_100_AQL" = sum(na.omit(AQL_Ratio) > 0.5 & na.omit(AQL_Ratio) <= 1.0),
                                                                                                   "10_50_AQL" = sum(na.omit(AQL_Ratio) > 0.1 & na.omit(AQL_Ratio) <= 0.5),
                                                                                                   "<10_AQL" = sum(na.omit(AQL_Ratio) <= 0.1),
                                                                                                   Max_AQL_Ratio = max(AQL_Ratio, na.rm = TRUE),
                                                                                                   HH_Value = min(min.HH.value),
                                                                                                   Over_HH = sum(na.omit(HH_Ratio) > 1.0),
                                                                                                   "50_100_HH" = sum(na.omit(HH_Ratio) > 0.5 & na.omit(HH_Ratio) <= 1.0),
                                                                                                   "10_50_HH" = sum(na.omit(HH_Ratio) > 0.1 & na.omit(HH_Ratio) <= 0.5),
                                                                                                   "<10_HH" = sum(na.omit(HH_Ratio) <= 0.1),
                                                                                                   Max_HH_Ratio = max(HH_Ratio, na.rm = TRUE))

MultCo_range <- MultCo_Bench %>% group_by(Analyte) %>% group_by(Year, add=TRUE) %>% dplyr:::summarise(City = "MultCo",
                                                                                              Result_min = min(Result, na.rm = TRUE),
                                                                                              Result_max = max(Result, na.rm = TRUE),
                                                                                              QL_min = min(QL, na.rm = TRUE),
                                                                                              QL_max = max(QL, na.rm = TRUE),
                                                                                              N_Detects = sum(!is.na(Result)),
                                                                                              N_Samples = sum(!is.na(Analyte)),
                                                                                              Detect_Freq = N_Detects/N_Samples,
                                                                                              AQL_Value = min(min.AQL.value),
                                                                                              Over_AQL = sum(na.omit(AQL_Ratio) > 1.0),
                                                                                              "50_100_AQL" = sum(na.omit(AQL_Ratio) > 0.5 & na.omit(AQL_Ratio) <= 1.0),
                                                                                              "10_50_AQL" = sum(na.omit(AQL_Ratio) > 0.1 & na.omit(AQL_Ratio) <= 0.5),
                                                                                              "<10_AQL" = sum(na.omit(AQL_Ratio) <= 0.1),
                                                                                              Max_AQL_Ratio = max(AQL_Ratio, na.rm = TRUE),
                                                                                              HH_Value = min(min.HH.value),
                                                                                              Over_HH = sum(na.omit(HH_Ratio) > 1.0),
                                                                                              "50_100_HH" = sum(na.omit(HH_Ratio) > 0.5 & na.omit(HH_Ratio) <= 1.0),
                                                                                              "10_50_HH" = sum(na.omit(HH_Ratio) > 0.1 & na.omit(HH_Ratio) <= 0.5),
                                                                                              "<10_HH" = sum(na.omit(HH_Ratio) <= 0.1),
                                                                                              Max_HH_Ratio = max(HH_Ratio, na.rm = TRUE))

Gresham_range <- Gresham_Bench %>% group_by(Analyte) %>% group_by(Year, add=TRUE) %>% dplyr:::summarise(City = "Gresham",
                                                                                                Result_min = min(Result.ug.l, na.rm = TRUE),
                                                                                                Result_max = max(Result.ug.l, na.rm = TRUE),
                                                                                                DL_min = min(DL.ug.l, na.rm = TRUE),
                                                                                                DL_max = max(DL.ug.l, na.rm = TRUE),
                                                                                                QL_min = min(QL.ug.l, na.rm = TRUE),
                                                                                                QL_max = max(QL.ug.l, na.rm = TRUE),
                                                                                                N_Detects = sum(!is.na(Result.ug.l)),
                                                                                                N_Samples = sum(!is.na(Analyte)),
                                                                                                Detect_Freq = N_Detects/N_Samples,
                                                                                                AQL_Value = min(min.AQL.value),
                                                                                                Over_AQL = sum(na.omit(AQL_Ratio) > 1.0),
                                                                                                "50_100_AQL" = sum(na.omit(AQL_Ratio) > 0.5 & na.omit(AQL_Ratio) <= 1.0),
                                                                                                "10_50_AQL" = sum(na.omit(AQL_Ratio) > 0.1 & na.omit(AQL_Ratio) <= 0.5),
                                                                                                "<10_AQL" = sum(na.omit(AQL_Ratio) <= 0.1),
                                                                                                Max_AQL_Ratio = max(AQL_Ratio, na.rm = TRUE),
                                                                                                HH_Value = min(min.HH.value),
                                                                                                Over_HH = sum(na.omit(HH_Ratio) > 1.0),
                                                                                                "50_100_HH" = sum(na.omit(HH_Ratio) > 0.5 & na.omit(HH_Ratio) <= 1.0),
                                                                                                "10_50_HH" = sum(na.omit(HH_Ratio) > 0.1 & na.omit(HH_Ratio) <= 0.5),
                                                                                                "<10_HH" = sum(na.omit(HH_Ratio) <= 0.1),
                                                                                                Max_HH_Ratio = max(HH_Ratio, na.rm = TRUE))

Clack_range <- Clack_Bench %>% group_by(Analyte) %>% group_by(Year, add=TRUE) %>% dplyr:::summarise(City = "Clackamas",
                                                                                                        Result_min = min(Result.ug.l, na.rm = TRUE),
                                                                                                        Result_max = max(Result.ug.l, na.rm = TRUE),
                                                                                                        MDL_min = min(Water.MDL.ug.l, na.rm = TRUE),
                                                                                                        MDL_max = max(Water.MDL.ug.l, na.rm = TRUE),
                                                                                                        N_Detects = sum(!is.na(Result.ug.l)),
                                                                                                        N_Samples = sum(!is.na(Analyte)),
                                                                                                        Detect_Freq = N_Detects/N_Samples,
                                                                                                        AQL_Value = min(min.AQL.value),
                                                                                                        Over_AQL = sum(na.omit(AQL_Ratio) > 1.0),
                                                                                                        "50_100_AQL" = sum(na.omit(AQL_Ratio) > 0.5 & na.omit(AQL_Ratio) <= 1.0),
                                                                                                        "10_50_AQL" = sum(na.omit(AQL_Ratio) > 0.1 & na.omit(AQL_Ratio) <= 0.5),
                                                                                                        "<10_AQL" = sum(na.omit(AQL_Ratio) <= 0.1),
                                                                                                        Max_AQL_Ratio = max(AQL_Ratio, na.rm = TRUE),
                                                                                                        HH_Value = min(min.HH.value),
                                                                                                        Over_HH = sum(na.omit(HH_Ratio) > 1.0),
                                                                                                        "50_100_HH" = sum(na.omit(HH_Ratio) > 0.5 & na.omit(HH_Ratio) <= 1.0),
                                                                                                        "10_50_HH" = sum(na.omit(HH_Ratio) > 0.1 & na.omit(HH_Ratio) <= 0.5),
                                                                                                        "<10_HH" = sum(na.omit(HH_Ratio) <= 0.1),
                                                                                                        Max_HH_Ratio = max(HH_Ratio, na.rm = TRUE))

#combine all summaries
All_range <- rbind.fill(Eugene_range,
                        Salem_range,
                        MultCo_range,
                        Portland_range,
                        Gresham_range,
                        Clack_range)

All_range$Analyte <- tolower(All_range$Analyte)

#Add criteria descriptions to summaries
All_range <- merge(x=All_range, y=Benchmarks[ , c("Pollutant_lcase", "min.AQL.criteria", "min.HH.criteria")], by.x = "Analyte", by.y = "Pollutant_lcase", all.x=TRUE, all.y = FALSE)
All_range <- plyr:::rename(All_range, replace = c("min.AQL.criteria"="AQL_text", "min.HH.criteria"="HH_text"))
All_range <- All_range[!All_range$Analyte == "",]
colnames(All_range)
All_range <- All_range[,c(1:9,31,10:15,32,16:30)]

#Compare to DEQ MRL/MDLs
Amazon <- read.csv("//Deqhq1/PSP/Rscripts/Alldates/2016-03-29_by_Basin_alldates_datafiles/Amazon_alldates_mydata_clean_noV_savedon2016-03-29.csv", strip.white = TRUE)
unique(Amazon$Units)
Amazon[Amazon$Unit == "mg/L", "MRL.ug.l"] <- Amazon[Amazon$Unit == "mg/L", ]$MRL*1000
Amazon[Amazon$Unit == "ng/L", "MRL.ug.l"] <- Amazon[Amazon$Unit == "ng/L", ]$MRL/1000
Amazon[Amazon$Unit == "µg/L", "MRL.ug.l"] <- Amazon[Amazon$Unit == "µg/L", ]$MRL
Amazon$Analyte <- as.character(tolower(Amazon$Analyte))
Amazon_MRL <- Amazon %>% group_by(Analyte) %>% dplyr:::summarise(min.DEQ.MRL = min(MRL.ug.l),
                                                         max.DEQ.MRL = max(MRL.ug.l))
All_range <- merge(All_range, Amazon_MRL, by="Analyte", all.x=TRUE, all.y=FALSE)

#Summarize only detected samples
All_range$Detect_Freq[All_range$Detect_Freq == 0] <- NA
Only_Detects <- All_range[!is.na(All_range$Detect_Freq),]

# Only_Det_Analyte_List <- Only_Detects[!duplicated(Only_Detects$Analyte),]
# Only_Det_Analyte_List$Legacy <- ""
# Only_Det_Analyte_List <- Only_Det_Analyte_List[,c("Analyte", "Legacy")]
# write.csv(Only_Det_Analyte_List, paste0("//Deqhq1/STORMWATER/Muni Stormwater Program/4 Science & Technology/Pesticide Monitoring/MS4_Pesticide_Data/Summaries/Detected Analyte List.csv"))


# Write to .csv file ------------------------------------------------------


# #save summary to folder: //Deqhq1/STORMWATER/Muni Stormwater Program/4 Science & Technology/Pesticide Monitoring/MS4_Pesticide_Data/Summaries/
# write.csv(All_range, paste0("//Deqhq1/STORMWATER/Muni Stormwater Program/4 Science & Technology/Pesticide Monitoring/MS4_Pesticide_Data/Summaries/All_MS4_ranges_savedon_", Sys.Date(), ".csv"))
# # write.csv(Eug_Bench, paste0("//Deqhq1/STORMWATER/Muni Stormwater Program/4 Science & Technology/Pesticide Monitoring/MS4_Pesticide_Data/Summaries/MS4 Data (raw) w Benchmarks/Eugene_Bench_savedon_", Sys.Date(), ".csv"))
# # write.csv(Salem_Bench, paste0("//Deqhq1/STORMWATER/Muni Stormwater Program/4 Science & Technology/Pesticide Monitoring/MS4_Pesticide_Data/Summaries/MS4 Data (raw) w Benchmarks/Salem_Bench_savedon_", Sys.Date(), ".csv"))
# # write.csv(Portland_Bench, paste0("//Deqhq1/STORMWATER/Muni Stormwater Program/4 Science & Technology/Pesticide Monitoring/MS4_Pesticide_Data/Summaries/MS4 Data (raw) w Benchmarks/Portland_Bench_savedon_", Sys.Date(), ".csv"))
# # write.csv(MultCo_Bench, paste0("//Deqhq1/STORMWATER/Muni Stormwater Program/4 Science & Technology/Pesticide Monitoring/MS4_Pesticide_Data/Summaries/MS4 Data (raw) w Benchmarks/MultCo_Bench_savedon_", Sys.Date(), ".csv"))
# # write.csv(Gresham_Bench, paste0("//Deqhq1/STORMWATER/Muni Stormwater Program/4 Science & Technology/Pesticide Monitoring/MS4_Pesticide_Data/Summaries/MS4 Data (raw) w Benchmarks/Gresham_Bench_savedon_", Sys.Date(), ".csv"))
# # write.csv(Clack_Bench, paste0("//Deqhq1/STORMWATER/Muni Stormwater Program/4 Science & Technology/Pesticide Monitoring/MS4_Pesticide_Data/Summaries/MS4 Data (raw) w Benchmarks/Clackamas_Bench_savedon_", Sys.Date(), ".csv"))
