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