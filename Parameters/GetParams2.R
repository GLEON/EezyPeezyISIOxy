###Generate oxygen distribution parameters from literature review
###Cayelan Carey
###originally created 19 January 2023

#Install the required googlesheets4 package
install.packages('googlesheets4')
#Load the library 
library(googlesheets4)
library(tidyverse)
library(fitdistrplus)
sites <- read.csv("data/MichaelisMentenOxygenParameters.csv")
str(sites)

sites$Fsed_oxy_value <- as.numeric(unlist(sites$Fsed_oxy_value)) #remove list from googlesheets
sites$Fsed_oxy_units <- as.character(unlist(sites$Fsed_oxy_units)) 

#focus on fsed_oxy values right now, assuming ksed values stay constant
sites_trim <- sites %>% 
  dplyr::select(LakeName, LakeTrophicState, Fsed_oxy_value, Fsed_oxy_units) %>% 
  drop_na()

#identify all units being used for Fsed_oxy values
unique(sites_trim$Fsed_oxy_units)

#convert all the different units to g/m2/day
for(i in 1:length(sites_trim$LakeName)){
  if(sites_trim$Fsed_oxy_units[i]=="mmol/m2/d"){
    sites_trim[i,5] = sites_trim$Fsed_oxy_value[i]*0.032 
  } #mmol/m2/day * (32 mg O2/1 mmol O2)* (1 g O2/1000 mg O2)
  if(sites_trim$Fsed_oxy_units[i]=="mg/L/d"){
    sites_trim[i,5] = sites_trim$Fsed_oxy_value[i]
  } #mg/L/day * (1 g O2/1000 mg O2)*(1000L/m3)*(1 m3/1 m2) assuming cubic m box of water on sediments
  if(sites_trim$Fsed_oxy_units[i]=="mol/L/s"){
    sites_trim[i,5] = sites_trim$Fsed_oxy_value[i]*2764800000
  } #mol/L/sec * (86400 sec/day)*(32 g/1 mol)*(1000L/1 m3)*(1 m3/1 m2)
  if(sites_trim$Fsed_oxy_units[i]=="NA"){
    sites_trim[i,5] = NA
  }
  if(sites_trim$Fsed_oxy_units[i]=="g/m2/d"){
    sites_trim[i,5] = sites_trim$Fsed_oxy_value[i]
  } #no need to convert, these are in correct units
  if(sites_trim$Fsed_oxy_units[i]=="mg/m2/d"){
    sites_trim[i,5] = sites_trim$Fsed_oxy_value[i]/1000
  } #mg/m2/d * (1 g/1000 mg)
}

#rename our new column
names(sites_trim)[(which(names(sites_trim)=='V5'))]<-'F_sed_oxy_gm2d'

sites_trim<-sites_trim %>% 
  drop_na()

#calculate oligotrophic normal distributions
Oligo <- fitdist(data = sites_trim$F_sed_oxy_gm2d[sites_trim$LakeTrophicState=="Oligotrophic"], 
                  distr = "norm", method = "mle")

#Oligo Parameters:
#estimate Std. Error
#mean 0.2731324 0.03099747
#sd   0.1669265 0.02191498

#calculate eutrophic normal distributions
Eutro <- fitdist(data = sites_trim$F_sed_oxy_gm2d[sites_trim$LakeTrophicState=="Eutrophic"], 
                 distr = "norm", method = "mle")

#Eutro Parameters:
#  estimate Std. Error
#mean -170.1525   154.3945
#sd    873.4926   109.1734

