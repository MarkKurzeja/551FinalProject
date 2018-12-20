#This section of code uses Google Translate to translate each of the 2600 KPIs from Swedish into English.

library(data.table)
library(magrittr)
library(googleLanguageR)

setwd("/Users/Desmond/Desktop/Work/551 data/Swedish")
temp = list.files(pattern="*.csv")
datalist = lapply(temp,fread)
FullData = datalist[[1]]
for(i in 2:5){
FullData = rbind(FullData,datalist[[i]])
}
TestData = FullData[!duplicated(FullData[,1])] %>%
  .[,c(1,6)]

#Use gl_translate to connect to Google through API and generate translations.
#In order to successfully run this code, you must obtain an API key from Google, which was obtained
#for this analysis using a free trial. 
translate_KPIs = gl_translate(TestData$kpi_desc,target='en',
                         source='sv')

#Generate dataset of translated KPIs.
EnglishKPIs = data.table(kpi = TestData$kpi,
                         KPI_desc = translate_KPIs$translatedText)

#Merge translated KPIs with original data and export file.
EnglishData = FullData[EnglishKPIs,on="kpi"]
write.csv(EnglishData,file='../DataWithEnglishKPIs.csv')
zip("../DataWithEnglishKPIs_ZIP","../DataWithEnglishKPIs.csv")
