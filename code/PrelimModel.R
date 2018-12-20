#This section of code runs a preliminary model on a set of simplified KPIs, using fires in the 2010-2014 period as the Poisson outcome.

#Final Code
library(magrittr)
library(stringr)
library(dplyr)
library(tidyr)
library(data.table)
library(rstanarm)
library(splines)
library(readxl)
library(magrittr)
library(BMA)
library(xtable)

#Load Data and Swap Muni Names
setwd('~/Documents/GitHub/551FinalProject/code')
joint_data = data.table(read_xlsx("../data/yearly_joint_data_clean.xlsx"))
getnames = fread("../data/yearly_joint_data.csv")
muni = unique(getnames$municipality_name)

#Function to force numeric scale output
numscale = function(x) {
  as.numeric(scale(x))
}

#Subset and Aggregate Years
TotalFires = joint_data[Year>= 2010,.(Fires = sum(Fires)),by=.(municipality_name)]$Fires

FactorVars = joint_data[Year >= 2010, lapply(.SD,min),
  by= municipality_name,
  .SDcols = c("municipalityType","municipalityTypeBroad","governing")] %>%
  .[,-c("municipality_name"),with=FALSE]

NumVars = joint_data[Year >= 2010,] %>%
  .[,lapply(.SD,mean,na.rm=TRUE),
    by=.(municipality_name),
    .SDcols = -c("municipalityType","municipalityTypeBroad","governing","Fires")] %>%
  .[,-c("municipality_name"),with=FALSE] %>%
  .[,lapply(.SD,numscale),.SDcols = -c("municipality_id")]

Prelim_Data = cbind(muni,NumVars,FactorVars,TotalFires) %>%
  .[,Share_65_Plus := `Share_65+`] %>%
  .[,-c("Year","Year_id","municipalityType_id","municipalityTypeBroad_id",
        "governing_id","Share_65+")]

Prelim_Data$municipalityTypeBroad = as.factor(Prelim_Data$municipalityTypeBroad)
Prelim_Data$governing = as.factor(Prelim_Data$governing)

#Generate formula for model
formnames = paste(names(Prelim_Data)[2:19],sep="")
modelform = as.formula(paste("TotalFires ~ ",
                             paste(formnames, collapse="+"),
                             "+ municipalityTypeBroad + governing"))

#Run model - stanreg
options(mc.cores=4)
set.seed(1)
prelimmodel = stan_glm(modelform,data=Prelim_Data,family=poisson(link="log"),prior=normal(),chains=4)
kprelim = kfold(prelimmodel,K=5,save_fits=TRUE)

posterior = as.array(prelimmodel)

#Run model - glmer
modelform = as.formula(paste("TotalFires ~ ",
                             paste(formnames, collapse="+"),
                             "+ (1 | municipalityTypeBroad) + (1 | governing)"))
prelim_lmer = stan_glmer(modelform,data=Prelim_Data,family=poisson(link='log'),
                         prior=normal(),adapt_delta=.99)
kprelim_lmer = kfold(prelim_lmer,K=5)

#Model comparison
lmer_lpd = kprelim_lmer$elpd_kfold
glm_lpd = kprelim$elpd_kfold
Comparison = data.frame(`No RE` = glm_lpd,`With RE` = lmer_lpd)
row.names(Comparison) = "ELPD"
xtable(Comparison,caption = "Comparison of Models with and without Random Effects")


#Visualizations/Model Checking
library(bayesplot)
library(ggplot2)

#Parameter Uncertainty
paramints = plot(prelimmodel) + 
  theme(axis.text = element_text(size=12))
paramints
ggsave("../fig/parameterintervals.pdf",plot=paramints,
       width=10.7,height=7.97,units='in')

#Model Fit
y = Prelim_Data$TotalFires
ydraws = t(replicate(6,sample(prelimmodel$fitted.values,length(y),replace=TRUE)))
errorhist = ppc_error_hist(y,ydraws,freq=FALSE,binwidth=20)
ggsave("../fig/errorhist.pdf",plot=errorhist)

#Boxplot
suburbplot = ggplot(data=Prelim_Data,aes(municipalityTypeBroad,TotalFires)) + 
  geom_boxplot() + 
  labs(x="Municipality Type",y="Total fires in 2010-2014")
ggsave("../fig/muni_type_fires.png",plot=suburbplot,
       width=6.26,height=7.04,units='in')


