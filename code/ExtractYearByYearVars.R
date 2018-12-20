#This code draws on the translated dataset to format and export a panel data file for selected predictors of interest. We focused specifically on a set of manually chosen variables of interest. For future work, data mining techniques could be explored to uncover more significant patterns. 



library(data.table)
library(magrittr)
library(readxl)
library(dplyr)
FullData = fread("/Users/Desmond/Desktop/Work/551 data/DataWithEnglishKPIs.csv")
listvars = unique(FullData$KPI_desc)
checkunem = grepl("foreign-born",listvars)
listvars[checkunem]


#Generate list of KPI names and IDs in English
KPI_English_Names = unique(FullData[,c("kpi","KPI_desc")])
write.csv(KPI_English_Names,file="./Documents/GitHub/551FinalProject/data/KPI_English_Names.csv")

otherdata = fread("/Users/Desmond/Documents/GitHub/551FinalProject/data/simplified_municipality_indicators.csv")

KPIs = c("Number of persons aged 16-24 in the municipality who are open unemployed or in programs with activity support,
divided by the number of residents 16-24 years in the municipality on 31/12 years T-1. Unemployment refers to statistics
from March month T. Source: Employment and Statistics Sweden.",
         "Number of unemployed unemployed and persons in programs with activity support between the ages of 18 and 64 divided
by the number of residents 18-64 years. Refers to statistics from March month T. Source: Employment Service and Statistics Sweden.",
         "Number of inhabitants 65-79 years divided by number of inhabitants total 31/12. Source: SCB.",
         "Total number of inhabitants on 31/12. Source: SCB.",
         "Number of votes cast in the last municipal elections (valid and invalid) divided by the number of eligible
voters multiplied by 100. Source: Valuation and SCB.",
         "Number of votes cast in the last parliamentary elections (valid and invalid) divided by the number of eligible
voters multiplied by 100. Source: Valuation and SCB.",
         "Total income earned between 20-64 years municipality (median), kr. Total earned income is the sum of income from
employment and income from business activities. The accumulated acquisition income consists of the total current taxable
income, which refers to income from employment, entrepreneurship, retirement, sickness benefit and other taxable transfers.
Total earned income does not include income from capital. Source: SCB.",
         "This is a development key figure, see questions and answers to kolada.se for more information. Municipal ranking
(1-290) of the complex business environment. The rankings contain a total of 18 factors weighted differently heavily.
The heaviest weighting in the ranking is the company's assessment of% u201DThe summary assessment of the business environment
in the municipality% u201D. Source: Swedish Enterprise",
         "The gin coefficient has a value between zero (0) and one hundred percent (1). 0 means that all individuals have
exactly equal assets (ie total equality) while 1 means total inequality. Based on total earned income. Source: SCB.",
         "Number of foreign-born members in the municipality divided by the total number of members in the municipality
multiplied by 100. The statistics are only published every four years, but in Kolada it is published in addition to the
year T, T + 1, T + 2 and T + 3. Source: SCB.")

KPIs = gsub("\r?\n|\r"," ", KPIs)

KPINames = c("Youth_Unemployment",
             "Unemployment",
             "Share_65+",
             "Population",
             "Share_Of_Voters_Who_Voted_Local",
             "Share_Of_Voters_Who_Voted_National",
             "Median_Income",
             "foretagsklimatRanking",
             "Gini_Coefficient",
             "Foreign_Born_Share")

KPINameData = data.table(KPI_desc = KPIs, Vars = KPINames)
 

#Subset to Year-by-Year Data of interest
YearlyData = FullData[KPI_desc %in% KPIs,-c("kpi_desc","V1"),with=FALSE] %>%
  .[KPINameData,on="KPI_desc"] %>%
  .[,-c("KPI_desc","kpi"),with=FALSE] %>%
  dcast(municipality_name + municipality_id + period ~ Vars,value.var='value') %>%
  .[order(municipality_name)] %>%
  .[,Year := as.numeric(period)] %>%
  .[,-c("period")]


#Load joint table of simplified indicators
other_data = fread("../data/untouched data/simplified_municipality_indicators.csv") %>%
  .[order(name)]

simpl_data = other_data
for(i in 1:16){
  simpl_data = rbind(simpl_data,other_data)
}
Year = rep(1998:2014,each=length(unique(YearlyData$municipality_name)))

simpl_data = cbind(simpl_data,Year) %>%
  .[order(name)] %>%
  .[,municipality_name := name] %>%
  .[,c("municipality_name","Year","urbanDegree","asylumCosts","municipalityType",
       "municipalityTypeBroad","governing","refugees","rentalApartments",
       "snowmobiles","cars","tractors","motorcycles","fokusRanking"),with=FALSE]


#Load table of fires
firedata = fread("../data/untouched data/school_fire_cases_1998_2014.csv") %>%
  .[,-c("Population"),with=FALSE]

missingdata = data.table(Municipality = c(rep("Knivsta",5),"Nykvarn"),Cases = rep(0,6),
                         Year = c(1998:2002,1998))

firedata = rbind(firedata,missingdata) %>%
  .[order(Municipality)]


# Join yearly data with simplified
agg_data = YearlyData[simpl_data,on=c("municipality_name","Year")] %>%
  cbind(Cases = firedata$Cases)

#Variable type conversions
agg_data[,3:12] = agg_data[,3:12] %>% mutate_if(is.character,as.numeric)
agg_data[,3:ncol(agg_data)] = agg_data[,3:ncol(agg_data)] %>% mutate_if(is.character,as.factor)


#Imputation of missing data
library(mice)

impdata = mice(agg_data[,3:26],m=5,maxit=20,method='pmm',ridge=.0001)
finaldata = complete(impdata)

finaldata = cbind(agg_data[,1:2],finaldata)

#Export final dataset as csv
write.csv(finaldata,"./Documents/GitHub/551FinalProject/data/yearly_joint_data.csv")



