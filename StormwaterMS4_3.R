#Stormwater MS4 permits 
#Julia Crown and Colin Donald
#initial date started June 30, 2016
#compile electronic data voluntarily provided by MS4 permittees. 
#Make a summary table of analytes and range of Results, MDLs, MRLs

Gresham <- read.csv('//deqhq1/STORMWATER/Muni Stormwater Program/4 Science & Technology/Pesticide Monitoring/MS4_Pesticide_Data/City of Gresham pesticide data.csv', stringsAsFactors=FALSE)
Gresham$Result.Raw <- Gresham$Result
Gresham$DL.Raw <- Gresham$DL
Gresham$QL.Raw <- Gresham$QL
Gresham$Unit.Raw <- Gresham$Unit
Gresham$Result <- as.numeric(Gresham$Result)
Gresham$Result.ug.l <- as.numeric(0)
Gresham$DL.ug.l <- as.numeric(0)
Gresham$QL.ug.l <- as.numeric(0)

unique(Gresham$Unit)

Gresham[Gresham$Unit == "mg/L", "Result.ug.l"] <- Gresham[Gresham$Unit == "mg/L", ]$Result*1000
Gresham[Gresham$Unit == "ng/L", "Result.ug.l"] <- Gresham[Gresham$Unit == "ng/L", ]$Result/1000
Gresham[Gresham$Unit == "µg/L", "Result.ug.l"] <- Gresham[Gresham$Unit == "µg/L", ]$Result

Gresham[Gresham$Unit == "mg/L", "DL.ug.l"] <- Gresham[Gresham$Unit == "mg/L", ]$DL*1000
Gresham[Gresham$Unit == "ng/L", "DL.ug.l"] <- Gresham[Gresham$Unit == "ng/L", ]$DL/1000
Gresham[Gresham$Unit == "µg/L", "DL.ug.l"] <- Gresham[Gresham$Unit == "µg/L", ]$DL

Gresham[Gresham$Unit == "mg/L", "QL.ug.l"] <- Gresham[Gresham$Unit == "mg/L", ]$QL*1000
Gresham[Gresham$Unit == "ng/L", "QL.ug.l"] <- Gresham[Gresham$Unit == "ng/L", ]$QL/1000
Gresham[Gresham$Unit == "µg/L", "QL.ug.l"] <- Gresham[Gresham$Unit == "µg/L", ]$QL

library("dplyr")
gresham_range <- Gresham %>% group_by(Analyte) %>% summarise(Result_min = min(Result.ug.l),
                                                               Result_max = min(Result.ug.l,
                                                               DL_min = min(DL.ug.l),
                                                               DL_max = max(DL.ug.l),
                                                               QL_min = min(QL.ug.l),
                                                               QL_max = max(QL.ug.l)
                                                               ))

dir.create(paste0("//Deqhq1/STORMWATER/Muni Stormwater Program/4 Science & Technology/Pesticide Monitoring/MS4_Pesticide_Data/Summaries")) 

write.csv(gresham_range, paste0("//Deqhq1/STORMWATER/Muni Stormwater Program/4 Science & Technology/Pesticide Monitoring/MS4_Pesticide_Data/Summaries/Gresham_MS4_ranges_savedon_", Sys.Date(), ".csv"))
