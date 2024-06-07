# June 7, 2024

#As of 6/7/24 we have access only to an annual index. 
# Eventually include predators etc. 

library(tidyverse)
library(GPEDM)

#load data
s_annual <-read.csv("seine survey yoy total annual index.csv", head=T)%>%
  select(-X)%>%mutate(logmean=log(Stratified.Mean))

#plot
s_annual%>%
  ggplot(aes(Year, Stratified.Mean))+geom_line()+theme_classic()

#Fits across a grid of E and Tau
Ees <-seq(1,10,1)
taus <-seq(1,3,1)
var_pairs = expand.grid(Ees, taus) # Combinations of vars, 2 at a time
ETdf <-matrix(nrow=dim(var_pairs)[1],ncol=4)
ETdf[,1]<-var_pairs[,1]
ETdf[,2]<-var_pairs[,2]
r2matrixSB = array(NA, dim = c(length(Ees), length(taus)), dimnames = list(Ees,taus)) 
rmsematrixSB = array(NA, dim = c(length(Ees), length(taus)), dimnames = list(Ees,taus)) 
for (i in 1:nrow(var_pairs)) {
  try({
    fitSB <-fitGP(data = s_annual, y = "Stratified.Mean",scaling = "local", E=var_pairs[i,1], tau=var_pairs[i,2], predictmethod = "loo")
    fitSB_r2 <-fitSB$outsampfitstats[[1]]
    fitSB_rmse <-fitSB$outsampfitstats[[2]]
    r2matrixSB[var_pairs[i,1], var_pairs[i,2]] = fitSB_r2
    ETdf[i,3] <-fitSB_r2
    ETdf[i,4] <-fitSB_rmse
    rmsematrixSB[var_pairs[i,1], var_pairs[i,2]] = fitSB_rmse
  },silent=T)
}
r2matrixSB
rmsematrixSB