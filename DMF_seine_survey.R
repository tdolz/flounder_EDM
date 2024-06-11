# June 7, 2024

#As of 6/7/24 we have access only to an annual index. 
# Eventually include predators etc. 
library(rquery)
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
    #fitSB <-fitGP(data = sa_short, y = "Stratified.Mean",scaling = "local", E=var_pairs[i,1], tau=var_pairs[i,2], predictmethod = "loo")
    
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

ETdf <-as.data.frame(ETdf)
names(ETdf)<-c("E","tau","OOS_R2","OOS_RMSE")

#round(r2matrixSB,4) %>% 
# kbl()%>%kable_classic(full_width = F, html_font = "Cambria")

### Plot
#R- squared. 
ETdf %>%
  #filter(E != 10)%>%
  ggplot(aes(E,tau,label=round(OOS_R2,2)))+
  geom_tile(aes(fill=OOS_R2), show.legend = TRUE)+
  scale_fill_gradient(low="white",high="blue")+
  scale_x_discrete(limits=seq(1,10,1))+
  geom_text()+
  xlab("embedding dimension")+ylab("tau")+
  guides(fill=guide_colorbar(title="r-squared"))+
  theme_bw()+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.background = element_rect(fill = "transparent", colour = "black"),
        strip.background =element_rect(fill="white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

#RMSE. 
ETdf %>%
  #filter(E != 10)%>%
  ggplot(aes(E,tau,label=round(OOS_RMSE,2)))+
  geom_tile(aes(fill=OOS_RMSE), show.legend = TRUE)+
  scale_fill_gradient(low="blue",high="white")+
  scale_x_discrete(limits=seq(1,10,1))+
  geom_text()+
  xlab("embedding dimension")+ylab("tau")+
  guides(fill=guide_colorbar(title="RMSE"))+
  theme_bw()+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        panel.background = element_rect(fill = "transparent", colour = "black"),
        strip.background =element_rect(fill="white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

## Interesting. So the best is unequivocally a tau = 3 and E = 10! 
## If you're sticking with Tau =1, then there isn't much difference between E=1 and the rest. Maybe E=3 is best? 
## Let's try both Tau = 3, E = 10 and Tau = 1, E = 3. I don't think we have a long enough time series for Tau = 3. 

wfe3t1 <-fitGP(data = s_annual, y ="Stratified.Mean",scaling = "local", E=3, tau=1, predictmethod = "loo")

wfe10t3 <-fitGP(data = s_annual, y = "Stratified.Mean",scaling = "local", E=10, tau=3, predictmethod = "loo")

wfe10t3.res <-mutate(wfe10t3$outsampresults, model="E10tau3")
wfe3t1.res <-mutate(wfe3t1$outsampresults, model="E3tau1")
res = bind_rows(wfe3t1.res,wfe10t3.res)
res$year =s_annual$Year

res %>% ggplot() +
  facet_wrap(model~., scales = "free") +
  geom_line(aes(x=year,y=predmean)) + #out of sample
  #geom_line(aes(x=timestep,y=predmean.y),lty="dashed") + #insample
  geom_ribbon(aes(x=year,y=predmean,ymin=predmean-predfsd,ymax=predmean+predfsd), alpha=0.4,fill="#03C0C1") + #out of sample
  #geom_ribbon(aes(x=timestep,y=predmean.y, ymin=predmean.y-predfsd.y,ymax=predmean.y+predfsd.y), alpha=0.4,fill="#007F80") + #in sample
  geom_point(aes(x=year, y=obs)) +
  theme_classic()

#What we're really interested in, is whether we can predict ahead. 
# Let's run the models on data up to 2018 - so predicting the last 5 years. 
#if you re-run the grid of es and taus. the best is actually E9tau3 and E3tau3. 
sa_short <-filter(s_annual, Year < 2018)

#E3Tau1
sa_lagsE3 <-makelags(data=s_annual, y="Stratified.Mean", E=3, tau =1)
sa_lagsE3 <-cbind(s_annual, sa_lagsE3)%>%as.data.frame()
saE3_train <-filter(sa_lagsE3, Year <=(max(sa_lagsE3$Year)-5))
saE3_test <-filter(sa_lagsE3, Year >(max(sa_lagsE3$Year)-5))

#E10Tau3
sa_lagsE10 <-makelags(data=s_annual, y="Stratified.Mean", E=10, tau =3)
sa_lagsE10 <-cbind(s_annual, sa_lagsE10)%>%as.data.frame()
saE10_train <-filter(sa_lagsE10, Year <=(max(sa_lagsE10$Year)-5))
saE10_test <-filter(sa_lagsE10, Year >(max(sa_lagsE10$Year)-5))

fit_E3 <-fitGP(data = saE3_train, y = "Stratified.Mean", x=c("Stratified.Mean_1", "Stratified.Mean_2", "Stratified.Mean_3"),newdata=saE3_test,predictmethod = "loo")
fit_E10 <-fitGP(data = saE10_train, y = "Stratified.Mean", x=colnames(saE10_test)[7:16],newdata=saE10_test,predictmethod = "loo")

fit_E10.res <-mutate(fit_E10$outsampresults, Year = 2019:2023)%>%mutate(model="E10tau3",dataset="test")
fit_E3.res <-mutate(fit_E3$outsampresults, Year = 2019:2023)%>%mutate(model="E3tau1",dataset="test")
res2 = bind_rows(fit_E10.res,fit_E3.res)%>%mutate(Year=as.numeric(Year))%>%arrange(Year)
res_short = filter(res, year < 2019 )%>%rename(Year=year)%>%mutate(Year=as.numeric(Year), dataset="train")
res2_short = filter(res2,Year>=2019)
res3 =bind_rows(res_short,res2_short)%>%full_join(s_annual)%>%arrange(Year)

res3 %>% 
  ggplot() +
  facet_wrap(~model)+
  geom_line(aes(x=Year,y=predmean)) + #out of sample
  geom_ribbon(aes(x=Year,y=predmean,ymin=predmean-predfsd,ymax=predmean+predfsd, fill=dataset), alpha=0.4) + #out of sample
  geom_point(aes(x=Year, y=Stratified.Mean)) +
  theme_classic()

#fitstats
fit_E3$outsampfitstats #better RMSE on the test/train - but honestly not good still. 
fit_E10$outsampfitstats
wfe3t1$outsampfitstats
wfe10t3$outsampfitstats #best fit here. but it did not do well on the test/train. 
#It's having a hard time with the high flier at the end of the time series. 

#Is this much better than an arima or moving average? Probably not. 
#https://rpubs.com/JSHAH/481706 
ts <-select(s_annual, Year, Stratified.Mean)
acf(ts$Stratified.Mean)
AR1 <- arima(ts$Stratified.Mean, order = c(1,0,0))
ts.plot(ts$Stratified.Mean)
AR1_fit <- ts$Stratified.Mean - residuals(AR1)
points(AR1_fit, type = "l", col = 2, lty = 2)
summary(AR1)

AR2 <- arima(ts$Stratified.Mean, order = c(2,0,0))
ts.plot(ts$Stratified.Mean)
AR2_fit <- ts$Stratified.Mean - residuals(AR2)
points(AR2_fit, type = "l", col = 2, lty = 2)

AR3 <- arima(ts$Stratified.Mean, order = c(3,0,0))
ts.plot(ts$Stratified.Mean)
AR3_fit <- ts$Stratified.Mean - residuals(AR3)
points(AR3_fit, type = "l", col = 2, lty = 2)
