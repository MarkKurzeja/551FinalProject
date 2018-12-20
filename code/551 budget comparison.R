library(ggplot2)
library(reshape2)
comparisondat = data.frame(percapita = 1000*59180.199, 
                           fires = 10^9/7.739237, 
                           edbudget = 49*10^9/7.739237/12) #adjusting to USD, monthly budget 
#2014 budget (Swedish State Budget 2014, http://www.regeringen.se/sb/d/16886/a/223709) 
#2014 fires
#2014 per capita GDP (World Bank, https://data.worldbank.org/indicator/NY.GDP.PCAP.CD?locations=SE)
compdat = melt(comparisondat)
ggplot(data = compdat,aes(x=variable, y=value,fill=variable!='fires')) + 
  geom_col() +
  labs(x='',y='Value in $USD') +
  scale_x_discrete(labels = c('percapita' = 'GDP / 1000 people', 
                              'fires' = 'Costs from school fires', 
                              'edbudget' = "National education budget,\n one month")) +
  guides(fill = FALSE)
ggsave('FireCosts.pdf',width = 6, height = 4, device = 'pdf')

library(xlsx)
dat = read.xlsx('/Users/askopelevich/Documents/GitHub/551FinalProject/data/joint_data_table.xlsx','Sheet1')
for(i in 35:39){ #a few columns are still factors
  dat[,i] = as.numeric(as.character(dat[,i]))
}
fire = dat
colnames(fire)[22] = 'Population'
colnames(fire)[2] = 'Municipality'
fire =melt(fire,id=1:34,variable.name = 'Year', value.name = 'Cases')
dat$fires = apply(dat[,35:51],1,sum) 

inputs = as.matrix(dat[,c(3,4,5,6,7,8,9,10,11,12,13,17,18,19,22,23,26,27,29,30,31,32,33,34)]) #quantvars
pc = princomp(scale(inputs))
summary(pc)
pc$loadings
plotdf = data.frame(pc$scores[,1:2], fires = dat$fires)
ggplot(dat = plotdf, aes(x=Comp.1,y=Comp.2)) + geom_point(aes(col=fires))

#lm(data = dat, fires ~ medianIncome + youthUnemployment2010 + unemployment2010 + )