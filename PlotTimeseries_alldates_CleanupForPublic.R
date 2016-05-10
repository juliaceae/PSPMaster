#clean up mydata_clean_noV
#for PSP results
#April 14, 2016
#Julia Crown

#mydata_clean_noV <- read.csv(file = "//deqhq1/PSP/Rscripts/Alldates/2016-04-12/State_alldates_mydata_clean_noV_savedon2016-04-12.csv")
#source('//deqhq1/PSP/Rscripts/PSPMaster/PlotTimeseries_alldates.R', encoding = 'UTF-8')


public <- mydata_clean_noV[,c(
#$ code                    #: chr  "30173 2010-06-08 2,4-D" "11316 2014-09-23 2,4-D" "28491 2011-04-17 2,4-D" "12248 2015-04-14 2,4-D" ...
  "Basin"                  #: chr  "Wasco" "South Umpqua" "Yamhill" "South Umpqua" ...
, "Station_Number"         #: num  30173 11316 28491 12248 11515 ...
, "Station_Description"    #: chr  "South Fork Mill Creek at Reservoir Road" "Myrtle Creek at mouth" "Salt Creek at River Mile 1.5" "Lookingglass Creek at Hwy 42 at Winston OR" ...
, "date"                   #: Date, format: "2010-06-08" ...
#$ "RESULT"                 #: chr  "ND" "ND" "ND" "ND" ...
#$ "MRL"                    #: num  10 0.1 10 0.1 10 10 10 10 10 10 ...
#$ "Units"                  #: chr  "µg/L" "µg/L" "µg/L" "µg/L" ...
,"Analyte"                 #: chr  "2,4-D" "2,4-D" "2,4-D" "2,4-D" ...
, "RESULT_clean.ug.l"      #: num  NA NA NA NA NA NA NA NA NA NA ...
, "dnd"                     #: num  0 0 0 0 0 0 0 0 0 0 ...
, "DQL"                    #: chr  "A+" "A" "A+" "A" ...
, "SampleType"             #: chr  "Sample" "Grab Sample::GS" "Sample" "Grab Sample::GS" ...
#, "Matrix"                 #: chr  "Surface water" "River/Stream" "Surface water" "River/Stream" ...
#$ "RESULT.raw"             #: chr  "<10" "ND" "<10" "ND" ...
#$ RESULT_clean.ug.l.subND #: num  0 0 0 0 0 0 0 0 0 0 ...
#$ RESULT_clean            #: num  NA NA NA NA NA NA NA NA NA NA ...
#$ "MRL_raw"                #: num  10 0.1 10 0.1 10 10 10 10 10 10 ...
#$ tResult                  #: num  10 0.1 10 0.1 10 10 10 10 10 10 ...
#$ Element.Name            #: chr  "2,4-D" "2,4-D" "2,4-D" "2,4-D" ...
#$ Has.benchmark           #: chr  "TRUE" "TRUE" "TRUE" "TRUE" ...
#$ benchmark.DEQ           #: num  NA NA NA NA NA NA NA NA NA NA ...
#$ benchmark.EPA           #: num  13.1 13.1 13.1 13.1 13.1 13.1 13.1 13.1 13.1 13.1 ...
, "relevant.AL.benchmark"   #: num  13.1 13.1 13.1 13.1 13.1 13.1 13.1 13.1 13.1 13.1 ...
#$ final_digress           #: num  NA NA NA NA NA NA NA NA NA NA ...
#$ percent.benchmark       #: num  NA NA NA NA NA NA NA NA NA NA ...
, "exceed.type"             #: chr  NA NA NA NA ...
#$ RESULT_clean.ug.l.neg   #: num  -999 -999 -999 -999 -999 -999 -999 -999 -999 -999 ...
, "year"                    #: int  2010 201415 2011 201415 2009 2011 2011 2011 2009 2011 ...
)]

public <- rename(public, c('dnd' = 'detect = 1', 
                           'RESULT_clean.ug.l' = 'RESULT in ug/L'
                         ))

public <- public[order(public$Basin), ]

write.csv(public, file = paste0('//deqhq1/PSP/DataRequests/PublicDataRequest_', Sys.Date(), '.csv'))


#Data Requests:

# #Gerco Hoogewegg  at Waterborne Env Linuron data request 4/14/2016: 
# write.csv(lasar[lasar$OrigAnalyte == "Linuron", ], file = "//deqhq1/PSP/DataRequests/lasarLinuron_DataRequest_GercoHoogewegg_at_Waterborne-Env_20160414.csv")
# write.csv(element[element$Analyte == "Linuron", ], file = "//deqhq1/PSP/DataRequests/elementLinuron_DataRequest_GercoHoogewegg_at_Waterborne-Env_20160414.csv")


# #Gerco Hoogewegg  at Waterborne Env Linuron data request 4/28/2016: 
# library(RODBC)
# ## connect to element and get data
# channel <- odbcConnect("element")
# 
# ##create the empty query
# myQuery <- c()
# ##populate the query
# ##All Linuron results from element
# qry <- paste0("SELECT * FROM dbo.Repo_Result WHERE  Analyte LIKE '%Linuron%' ")
# ## This line adds the query language to the empty query.
# myQuery <- append(myQuery, qry)
# 
# 
# ## Retrieve data.
# for(i in 1:length(myQuery)) {
#   print(myQuery[i])
#   data <- sqlQuery(channel,myQuery[i],stringsAsFactors = FALSE, na.strings = "NA")
#   ifelse(i==1,mydata <- data, mydata <- rbind(mydata,data))
#   rm(data)
# }
# write.csv(mydata, file = "//deqhq1/PSP/DataRequests/GercoHoogewegg/elementLinuron_DataRequest_GercoHoogewegg_at_Waterborne-Env_20160428.csv")


# #Gerco Hoogewegg  at Waterborne Env Carbaryl data request 4/28/2016: 
# library(RODBC)
# ## connect to element and get data
# channel <- odbcConnect("element")
# 
# ##create the empty query
# myQuery <- c()
# ##populate the query
# ##All Linuron results from element
# qry <- paste0("SELECT * FROM dbo.Repo_Result WHERE  Analyte LIKE '%Carbaryl%' ")
# ## This line adds the query language to the empty query.
# myQuery <- append(myQuery, qry)
# 
# 
# ## Retrieve data.
# for(i in 1:length(myQuery)) {
#   print(myQuery[i])
#   data <- sqlQuery(channel,myQuery[i],stringsAsFactors = FALSE, na.strings = "NA")
#   ifelse(i==1,mydata <- data, mydata <- rbind(mydata,data))
#   rm(data)
# }
# write.csv(mydata, file = "//deqhq1/PSP/DataRequests/element_Carbaryl_DataRequest_GercoHoogewegg_at_Waterborne-Env_20160428.csv")

# #Melanie Malone at Portland State University Glyphosate and AMPA data request 4/21/16:
# write.csv(element[element$Station_Description == 
# "Fifteenmile Creek Above Seufert Falls (AKA Cushing Falls)" & element$Analyte %in% c("Aminomethylphosphonic acid (AMPA)", "Glyphosate"),], 
# file = "//deqhq1/PSP/DataRequests/elementGlyphosate-AMPA_DataRequest_MelanieMalone_at_PSU_20160421.csv")

#Kirk V Cook  at Oregon Dept of Agriculture data request 5/09/2016: 
write.csv(element[element$Basin == "Pudding" & element$Year == "2015", ], 
          file = "//deqhq1/PSP/DataRequests/element_Pudding_2015_DataRequest_KirkCook_at_ODA_20160509.csv")
