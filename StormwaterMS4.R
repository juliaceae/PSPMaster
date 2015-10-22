#Stormwater MS4
#Julia Crown, Joey Peters
#Combine MS4 data

Gresham <- read.csv('//deqhq1/STORMWATER/Muni Stormwater Program/Program Planning & Admin/4 Science & Technology/Pesticide Monitoring/MS4_Pesticide_Data/City of Gresham pesticide data.csv', stringsAsFactors=FALSE)
Salem$Sample_Type <- as.character(NA)
Salem$Sample_Type <- as.character(NA)
Salem$Sample_Type <- as.character(NA)
Salem$Sample_Type <- as.character(NA)

Gresham <- rename(Gresham, c(
                         #'Sampling_Event' = 'Work_Order', 
                         #'Sampling.Subproject.Name' = 'Project',
                         'Station.ID' = 'Station_ID',
                         'Station.Name' = 'Station_Description',
                         'Sample.Date' = 'Sampled_Date',
                         'Analyte' = 'OrigAnalyte',
                         'QL' = 'MRL',
                         'Result' = 'Result',
                         ' Unit' = 'Units',
                         #'Sample_Type' = 'SampleType',
                         ' DL' = 'DQL',
                         #'Sample_Matrix' = 'Matrix',
                         'Analysis' = 'method_CODE'))

Salem <- read.csv('//deqhq1/STORMWATER/Muni Stormwater Program/Program Planning & Admin/4 Science & Technology/Pesticide Monitoring/MS4_Pesticide_Data/City of Salem Pesticide Data for DEQ_Combined.csv', stringsAsFactors=FALSE)
Salem$Sample_Type <- as.character(NA)
Salem[Salem$Station.Name %in% c("Hilfiker DUP", "Electric DUP", "Salem Industrial Dup"), ]$Sample_Type <- "Field Duplicate"
Salem[Salem$Station.Name %in% c("Hilfiker", "Electric", "Electric  ", "Salem Industrial"), ]$Sample_Type <- "Field Primary"
Salem <- rename(Salem, c(
  #'Sampling_Event' = 'Work_Order', 
  #'Sampling.Subproject.Name' = 'Project',
  'Station.ID ' = 'Station_ID',
  'Station.Name' = 'Station_Description',
  'Sample.Date' = 'Sampled_Date',
  'Analyte' = 'OrigAnalyte',
  'QL' = 'MRL',
  'Result' = 'Result',
  'Unit' = 'Units',
  #'Sample_Type' = 'SampleType',
  'DL' = 'DQL',
  #'Sample_Matrix' = 'Matrix',
  'Analysis' = 'method_CODE'))

MultCo <- read.csv('//deqhq1/STORMWATER/Muni Stormwater Program/Program Planning & Admin/4 Science & Technology/Pesticide Monitoring/MS4_Pesticide_Data/MultCo UIC Pesticide Data.csv', stringsAsFactors=FALSE)


Eugene <- read.csv('//deqhq1/STORMWATER/Muni Stormwater Program/Program Planning & Admin/4 Science & Technology/Pesticide Monitoring/MS4_Pesticide_Data/DEQ_Pesticides_083115_EUGENE.csv', stringsAsFactors=FALSE)

