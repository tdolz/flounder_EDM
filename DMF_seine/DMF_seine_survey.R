#December 16, 2024
library(tidyverse)
library(GPEDM)
library(readxl)
library(gridExtra)
library(rquery)
library(Metrics)

sites <-read.csv("DMF_seine/Dolan/1060_catch_76-23.csv",header=T)%>%
  mutate(estuary=fct_recode(as.factor(estuary), "Great Pond"="1", "Waquoit Bay"="2","Cotuit Bay"="3","Lewis Bay"="4","Bass River"="5","Stage Harbor"="6"))


##BT data
bt <- read_excel("C:\\Users\\Tara.Dolan\\OneDrive - Commonwealth of Massachusetts\\Documents\\Winter Flounder\\Research Track\\MADMF Bottom Temperature data\\Tara_DataRequest.xlsx", sheet="Site Information")
#The only two locations that overlap with the seine survey are lewis bay and waquoit bay 
# actually that is not true we have temperature data for these other locations too. 
## see "C:\Users\Tara.Dolan\OneDrive - Commonwealth of Massachusetts\Documents\Winter Flounder\Research Track\Time series\Site Index_3.15.24.xlsx"
lb <-read_excel("C:\\Users\\Tara.Dolan\\OneDrive - Commonwealth of Massachusetts\\Documents\\Winter Flounder\\Research Track\\MADMF Bottom Temperature data\\Tara_DataRequest.xlsx", sheet="Lewis_Bay")
wb <-read_excel("C:\\Users\\Tara.Dolan\\OneDrive - Commonwealth of Massachusetts\\Documents\\Winter Flounder\\Research Track\\MADMF Bottom Temperature data\\Tara_DataRequest.xlsx", sheet="Waquoit_Bay")

#### Order of operations ####
## Look at the data by site individually and hierarchically ##

#is the area surveyed roughly the same year to year? 
p<-list()
for (i in 1:length(unique(sites$estuary))){
p[[i]]<- sites %>% filter(estuary== unique(sites$estuary)[i])%>%
    ggplot(aes(year,AREA, color=as.factor(station_id)))+
geom_line()+ ggtitle(unique(sites$estuary)[i])+
  facet_wrap(~station_id)+theme_bw()
}
#not really, but that is ok because density = N/area

#density
p<-list()
for (i in 1:length(unique(sites$estuary))){
  p[[i]]<- sites %>% filter(estuary== unique(sites$estuary)[i])%>%
    ggplot(aes(year,density, color=as.factor(station_id)))+
    geom_line()+ ggtitle(unique(sites$estuary)[i])+
    facet_wrap(~station_id)+theme_bw()
}

#mean and variance across stations within sites - so summarizing at the site level instead of station. 
by_site <-sites %>% group_by(year,estuary)%>% summarize(av_dens=mean(density, na.rm=T),var_dens=var(density, na.rm=T),sd_density=sd(density, na.rm=T), total_dens=sum(N, na.rm=T)/sum(AREA, na.rm=T),.groups="keep")
bysitepiv <-pivot_longer(by_site, 3:6, names_to = "metric", values_to = "value")

p<-list()
for (i in 1:length(unique(sites$estuary))){
  p[[i]]<- bysitepiv %>% filter(estuary== unique(sites$estuary)[i])%>%
    ggplot(aes(year,value))+ 
    geom_line()+ ggtitle(unique(sites$estuary)[i])+
    facet_wrap(~metric, scales="free")+theme_bw()
}

## facet for all 
by_site %>% 
  ggplot(aes(year,av_dens, color=as.factor(estuary)))+ 
  geom_line()+
  geom_ribbon(aes(ymin=av_dens-sd_density, ymax=av_dens+sd_density, fill=as.factor(estuary), alpha=0.2))+
  facet_wrap(~estuary, scales="free")+theme_bw()

# June 7, 2024

#As of 6/7/24 we have access only to an annual index. 
# Eventually include predators etc. 


#load data
s_annual <-read.csv("DMF_seine/seine survey yoy total annual index.csv", head=T)%>%
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
AR1_fit <- ts$Stratified.Mean - residuals(AR1)
summary(AR1)
AR2 <- arima(ts$Stratified.Mean, order = c(2,0,0))
AR2_fit <- ts$Stratified.Mean - residuals(AR2)
AR3 <- arima(ts$Stratified.Mean, order = c(3,0,0))
AR3_fit <- ts$Stratified.Mean - residuals(AR3)

ts.plot(ts$Stratified.Mean)
points(AR1_fit, type = "l", col = "red", lty = 2)
points(AR2_fit, type = "l", col = "blue", lty = 2)
points(AR3_fit, type = "l", col = "green", lty = 2)

#calculate R2 on the AR fits
getR2(obs=ts$Stratified.Mean, pred=AR1_fit)
getR2(obs=ts$Stratified.Mean, pred=AR2_fit)
getR2(obs=ts$Stratified.Mean, pred=AR3_fit)
rmse(actual=ts$Stratified.Mean,predicted=AR1_fit)
rmse(actual=ts$Stratified.Mean,predicted=AR2_fit)
rmse(actual=ts$Stratified.Mean,predicted=AR3_fit)

#forecast with ARIMA - let's do AR3 only. 
# for it to be comparable we have to shorten the time series. 
sa_pred = filter(s_annual, Year > 2018)
fAR3 <- arima(sa_short$Stratified.Mean, order = c(3,0,0))
fAR3_fit <- sa_short$Stratified.Mean - residuals(fAR3)
forecast3=predict(fAR3, 5)
getR2(obs=sa_pred$Stratified.Mean, pred=forecast3$pred)

#if we have time later, we can make a plot showing the best arima with forecasting compared to the best GP with forecasting. 

wb%>%ggplot(aes(Date,TempC))+geom_line()
wb%>%ggplot(aes(Date,MeanDailyTemp))+geom_line()
#looks like Waquoit bay is missing temperature data from 2007-2009 with no backup.
wb <-mutate(wb, Date= as.Date(Date), Time=format(Time, "%H:%M:%S"))

lb%>%ggplot(aes(Date,TempC))+geom_line()
lb%>%ggplot(aes(Date,MeanDailyTemp))+geom_line()
lb <-mutate(lb, Date= as.Date(Date), Time=format(Time, "%H:%M:%S"))%>%
  separate(Date,into=c("Year","Month","Day"),sep="-", remove=FALSE)

lb_annual <- lb %>% group_by(Year)%>%summarize(avTemp=mean(TempC))%>%filter(Year>2006)
lb_feb <-filter(lb,Month=="02")%>% group_by(Year)%>%summarize(FebTemp=mean(TempC)) 
lb_march <-filter(lb,Month=="03")%>% group_by(Year)%>%summarize(MarchTemp=mean(TempC)) 
lb_april <-filter(lb,Month=="04")%>% group_by(Year)%>%summarize(AprilTemp=mean(TempC)) 
lb_may <-filter(lb,Month=="05")%>% group_by(Year)%>%summarize(MayTemp=mean(TempC)) 
lb_june<-filter(lb,Month=="06")%>% group_by(Year)%>%summarize(JuneTemp=mean(TempC)) 
lb_july<-filter(lb,Month=="07")%>% group_by(Year)%>%summarize(JulyTemp=mean(TempC)) 

lbyoy <-filter(sites,estuary=="Lewis Bay")%>%group_by(year)%>%summarize(AvDens=mean(density))%>%
  filter(year>2006)%>%rename(Year=year)%>%bind_cols(lb_annual[,2],lb_feb[,2],lb_march[,2],lb_may[,2],lb_june[,2],lb_july[,2])
lbyoypiv <-pivot_longer(lbyoy,cols = 3:8)%>%mutate(name =factor(name, levels=c("avTemp","FebTemp","MarchTemp","AprilTemp","MayTemp","JuneTemp","JulyTemp")))

monthcolors =c("avTemp"="black","FebTemp"="#2166ac","MarchTemp"="#67a9cf","AprilTemp"="#d1e5f0","MayTemp"="#fddbc7","JuneTemp"="#ef8a62","JulyTemp"="#b2182b")
lbyoypiv %>%
  ggplot(aes(Year,value, color=name))+ geom_line()+ ylab("Temp C")+
  scale_color_manual(values=monthcolors)+theme_classic()

lbyoy%>%
  ggplot(aes(avTemp,AvDens))+
  geom_point(aes(avTemp,AvDens),color="black")+ 
  geom_smooth(method = "lm", se = FALSE, color = "black")
lbyoypiv%>%
  ggplot(aes(value,AvDens,color=name))+
  geom_point()+geom_smooth(method = "lm", se = FALSE)+
  facet_wrap(~name,scales="free_x")+
  scale_color_manual(values=monthcolors)+theme_classic()
 
###fitGP with YOY and then fit with YOY and BT. ###
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
    lags <-makelags(data=lbyoy,y="AvDens",E=var_pairs[i,1], tau=var_pairs[i,2])
    lags <-cbind(lbyoy,lags)
    fitSB <-fitGP(data = lags, y = "AvDens", x=names(lags)[3:ncol(lags)],predictmethod = "loo")
    #fitSB <-fitGP(data = lbyoy, y = "AvDens",scaling = "global", E=var_pairs[i,1], tau=var_pairs[i,2], predictmethod = "loo")
    #fitSB <-fitGP(data = sa_short, y = "Stratified.Mean",scaling = "local", E=var_pairs[i,1], tau=var_pairs[i,2], predictmethod = "loo")
    
    fitSB_r2 <-fitSB$outsampfitstats[[1]]
    fitSB_rmse <-fitSB$outsampfitstats[[2]]
    r2matrixSB[var_pairs[i,1], var_pairs[i,2]] = fitSB_r2
    ETdf[i,3] <-fitSB_r2
    ETdf[i,4] <-fitSB_rmse
    rmsematrixSB[var_pairs[i,1], var_pairs[i,2]] = fitSB_rmse
  },silent=T)
}
r2matrixSB #Best R2 is E=9 Tau=1; second best is E=5 Tau=2; E=7 Tau = 1 is third best
rmsematrixSB
ETdf <-as.data.frame(ETdf)
names(ETdf)<-c("E","tau","OOS_R2","OOS_RMSE")
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

### Fit the initial GP models ### E = 9, T= 1 and E = 5, T=2
ts <-lbyoy[,1:2]
lags91 <-makelags(data=ts, y="AvDens", E=9, tau =1)
lags91 <-cbind(ts,lags91)
lb91 <-fitGP(data = lags91, y = "AvDens", x=names(lags91)[3:ncol(lags91)],predictmethod = "loo")

lags52 <-makelags(data=ts, y="AvDens", E=5, tau =2)
lags52 <-cbind(ts,lags52)
lb52 <-fitGP(data = lags52, y = "AvDens", x=names(lags52)[3:ncol(lags52)],predictmethod = "loo")

lb52.res <-mutate(lb52$outsampresults, model="lb52")
lb91.res <-mutate(lb91$outsampresults, model="lb91")
res = bind_rows(lb52.res,lb91.res)
res$year =lbyoy$Year

res %>% ggplot() +
  facet_wrap(model~., scales = "free") +
  geom_line(aes(x=year,y=predmean)) + #out of sample
  #geom_line(aes(x=timestep,y=predmean.y),lty="dashed") + #insample
  geom_ribbon(aes(x=year,y=predmean,ymin=predmean-predfsd,ymax=predmean+predfsd), alpha=0.4,fill="#03C0C1") + #out of sample
  #geom_ribbon(aes(x=timestep,y=predmean.y, ymin=predmean.y-predfsd.y,ymax=predmean.y+predfsd.y), alpha=0.4,fill="#007F80") + #in sample
  geom_point(aes(x=year, y=obs)) +
  theme_classic()

lb52$outsampfitstats #R2 = 0.6
lb91$outsampfitstats #R2 = -0.15

# want to try this over newdata... 

# For now, let's look at average Bottom Temperature
ts <-lbyoy[,1:3]
#lags91 <-makelags(data=ts, y="AvDens", E=9, tau =1)
#here with lags of both temperature and density. 
lags91 <-makelags(data=ts, y=c("AvDens","avTemp"), E=9, tau =1)
lags91 <-cbind(ts,lags91)
avtemp91 <-fitGP(data = lags91, y = "AvDens", x=names(lags91)[3:ncol(lags91)],predictmethod = "loo")

#here with lags of both temperature and density. 
lags52 <-makelags(data=ts, y=c("AvDens","avTemp"), E=5, tau =2)
#lags52 <-makelags(data=ts, y="AvDens", E=5, tau =2)
lags52 <-cbind(ts,lags52)
avtemp52 <-fitGP(data = lags52, y = "AvDens", x=names(lags52)[3:ncol(lags52)],predictmethod = "loo")

at52.res <-mutate(avtemp52$outsampresults, model="at52")
at91.res <-mutate(avtemp91$outsampresults, model="at91")
res = bind_rows(at52.res,at91.res)
res$year =lbyoy$Year

res %>% ggplot() +
  facet_wrap(model~., scales = "free") +
  geom_line(aes(x=year,y=predmean)) + #out of sample
  #geom_line(aes(x=timestep,y=predmean.y),lty="dashed") + #insample
  geom_ribbon(aes(x=year,y=predmean,ymin=predmean-predfsd,ymax=predmean+predfsd), alpha=0.4,fill="purple") + #out of sample
  #geom_ribbon(aes(x=timestep,y=predmean.y, ymin=predmean.y-predfsd.y,ymax=predmean.y+predfsd.y), alpha=0.4,fill="#007F80") + #in sample
  geom_point(aes(x=year, y=obs)) +
  theme_classic()

avtemp52$outsampfitstats 
avtemp91$outsampfitstats


##########################################################################################################


vars_list <- names(lbyoy)[2:8]  # Extract variable names
plist <- list()  # Initialize an empty list for plots of full data models
ttlist <-list() #list for plots of test/train models
fsout <- data.frame(var = character(), fit52_r2 = numeric(), fit52_rmse = numeric(), fit91_r2 = numeric(), fit91_rmse = numeric(), stringsAsFactors = FALSE)
fsouttt <- data.frame(var = character(), fit52_r2 = numeric(), fit52_rmse = numeric(), fit91_r2 = numeric(), fit91_rmse = numeric(), stringsAsFactors = FALSE)

for (i in 1:length(vars_list)) {
  var_name <- vars_list[i]
  
  # Correct column selection
  ts <- lbyoy[, c("Year", "AvDens", var_name)]
  
  # Ensure correct variable selection for makelags
  #lags91 <- makelags(data = ts, y = "AvDens", E = 9, tau = 1)
  lags91 <- makelags(data = ts, y = c("AvDens",var_name), E = 9, tau = 1)
  lags91 <- cbind(ts, lags91)
  lags91 <- lags91[, !duplicated(names(lags91))]
  temp91 <- fitGP(data = lags91, y = "AvDens", x = names(lags91)[3:ncol(lags91)], predictmethod = "loo")
  
  #test/train version
  #lags91_train<-filter(lags91, Year <=(max(lags91$Year)-3))#predicting 3 years. 
  #lags91_test<-filter(lags91, Year >(max(lags91$Year)-3))
  lags91_train<-filter(lags91, Year <=(max(lags91$Year)-5))#predicting 5 years. 
  lags91_test<-filter(lags91, Year >(max(lags91$Year)-5))
  temp91tt <- fitGP(data = lags91_train, y = "AvDens", x = names(lags91_train)[3:ncol(lags91)], newdata=lags91_test,predictmethod = "loo")
  
  #lags52 <- makelags(data = ts, y = "AvDens", E = 5, tau = 2)
  lags52 <- makelags(data = ts, y = c("AvDens",var_name), E = 5, tau = 2)
  lags52 <- cbind(ts, lags52)
  lags52 <- lags52[, !duplicated(names(lags52))]
  temp52 <- fitGP(data = lags52, y = "AvDens", x = names(lags52)[3:ncol(lags52)], predictmethod = "loo")
  
  #test/train version
  #lags52_train<-filter(lags52, Year <=(max(lags52$Year)-3))#predicting 3 years. 
  #lags52_test<-filter(lags52, Year >(max(lags52$Year)-3))
  lags52_train<-filter(lags52, Year <=(max(lags52$Year)-5))#predicting 5 years. 
  lags52_test<-filter(lags52, Year >(max(lags52$Year)-5))
  temp52tt <- fitGP(data = lags52_train, y = "AvDens", x = names(lags52_train)[3:ncol(lags52)], newdata=lags52_test,predictmethod = "loo")
  
  # output and label results for straight model
  m52.res <- temp52$outsampresults %>% mutate(model = "m52")
  m91.res <- temp91$outsampresults %>% mutate(model = "m91")
  res <- bind_rows(m52.res, m91.res)
  res$Year <- lbyoy$Year
  
  #test/train version - 3 years. 
  #m52.restt <- temp52tt$outsampresults %>% mutate(Year = 2021:2023,dataset="test", model = "m52")
  #m91.restt <- temp91tt$outsampresults %>% mutate(Year = 2021:2023,dataset="test",model = "m91")
  #restt <- bind_rows(m52.restt, m91.restt)%>%mutate(Year=as.numeric(Year))%>%arrange(Year)
  #res_short = filter(lbyoy, Year < 2021 )%>%mutate(Year=as.numeric(Year), dataset="train")

  #test/train version - 5 years. 
  m52.restt <- temp52tt$outsampresults %>% mutate(Year = 2019:2023,dataset="test", model = "m52")
  m91.restt <- temp91tt$outsampresults %>% mutate(Year = 2019:2023,dataset="test",model = "m91")
  restt <- bind_rows(m52.restt, m91.restt)%>%mutate(Year=as.numeric(Year))%>%arrange(Year)
  res_short = filter(lbyoy, Year < 2019 )%>%mutate(Year=as.numeric(Year), dataset="train")
  
  
  #Plot results
  plot <- res %>% ggplot() +
    facet_wrap(~model, scales = "free") + geom_line(aes(x = Year, y = predmean)) +  # out-of-sample
    geom_ribbon(aes(x = Year, y = predmean, ymin = predmean - predfsd, ymax = predmean + predfsd), alpha = 0.4, fill = monthcolors[i]) +  # out-of-sample
    geom_point(aes(x = Year, y = obs)) +
    ggtitle(var_name) + theme_classic()
  plist[[i]] <- plot
  
  #test train version
  plot2 <- ggplot() +
    geom_point(data=res,aes(x = Year, y = obs)) + #observed data, entire time series 
    #pred test/train
    geom_point(data=restt,aes(x = Year, y = obs),color = monthcolors[i]) + #observed data, entire time series 
    geom_line(data=restt,aes(x = Year, y = predmean),color = monthcolors[i],linetype="dotted") +
    geom_ribbon(data=restt,aes(x = Year, y = predmean, ymin = predmean - predfsd, ymax = predmean + predfsd),alpha = 0.4, fill = monthcolors[i]) +  # out-of-sample
    facet_wrap(~model, scales = "free") +
    ggtitle(var_name) + theme_classic()
  ttlist[[i]] <- plot2
  
  # output fitstats for the full ts model. 
  fsout <- rbind(fsout, data.frame(
    var = var_name,
    fit52_r2 = temp52$outsampfitstats[1],
    fit52_rmse = temp52$outsampfitstats[2],
    fit91_r2 = temp91$outsampfitstats[1],
    fit91_rmse = temp91$outsampfitstats[2]
  ))
  #output fitstats for the test/train model
  fsouttt <- rbind(fsouttt, data.frame(
    var = var_name,
    fit52_r2 = temp52tt$outsampfitstats[1],
    fit52_rmse = temp52tt$outsampfitstats[2],
    fit91_r2 = temp91tt$outsampfitstats[1],
    fit91_rmse = temp91tt$outsampfitstats[2]
  ))
}

# View results
fsout <- fsout %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))
print(fsout)
fsouttt <- fsouttt %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))
print(fsouttt)


# Arrange multiple ggplots in a grid (adjust `ncol` as needed)
grid.arrange(grobs = plist, ncol = 3)
grid.arrange(grobs = ttlist, ncol = 3)

################################### Separately Plot A91 Febtemp and average temp.##################################################### 
ts <- lbyoy[, c("Year", "AvDens", "avTemp","FebTemp")]
lags91 <- makelags(data = ts, y = c("AvDens","avTemp","FebTemp"), E = 9, tau = 1)
lags91 <- cbind(ts, lags91)
lags91 <- lags91[, !duplicated(names(lags91))]
temp91 <- fitGP(data = lags91, y = "AvDens", x = names(lags91)[5:ncol(lags91)], predictmethod = "loo")

#fit to density only. 
dens91 <- fitGP(data = lags91, y = "AvDens", x = names(lags91)[5:13], predictmethod = "loo")

#test/train version
lags91_train<-filter(lags91, Year <=(max(lags91$Year)-5))#predicting 5 years. 
lags91_test<-filter(lags91, Year >(max(lags91$Year)-5))
temp91tt <- fitGP(data = lags91_train, y = "AvDens", x = names(lags91_train)[5:ncol(lags91)], newdata=lags91_test,predictmethod = "loo", time="Year")

#predict sequential version
temp91tt_update=predict_seq(temp91tt,newdata=lags91_test)
temp91tt_update$outsampfitstats

dens91$outsampfitstats
temp91$outsampfitstats
temp91tt$outsampfitstats

res <- temp91$outsampresults %>% mutate(model = "dens_febBT_avBT")
res2 <-dens91$outsampresults %>% mutate(model ="densonly")
res$Year <- lbyoy$Year
res2$Year <-lbyoy$Year
#test/train version - 5 years. 
restt <- temp91tt$outsampresults %>% mutate(Year = 2019:2023,model = "test")
res3 <-bind_rows(res,res2,restt)


#Is this much better than an arima or moving average? Probably not. 
#https://rpubs.com/JSHAH/481706 
ts <-select(lbyoy, Year, AvDens)
acf(ts$AvDens)
AR1 <- arima(ts$AvDens, order = c(1,0,0))
AR1_fit <- ts$AvDens - residuals(AR1)
summary(AR1)
AR2 <- arima(ts$AvDens, order = c(2,0,0))
AR2_fit <- ts$AvDens - residuals(AR2)
AR3 <- arima(ts$AvDens, order = c(3,0,0))
AR3_fit <- ts$AvDens - residuals(AR3)

ts.plot(ts$AvDens)
points(AR1_fit, type = "l", col = "red", lty = 2)
points(AR2_fit, type = "l", col = "blue", lty = 2)
points(AR3_fit, type = "l", col = "green", lty = 2)

#calculate R2 on the AR fits
getR2(obs=ts$AvDens, pred=AR1_fit)
getR2(obs=ts$AvDens, pred=AR2_fit)
getR2(obs=ts$AvDens, pred=AR3_fit)
rmse(actual=ts$AvDens,predicted=AR1_fit)
rmse(actual=ts$AvDens,predicted=AR2_fit)
rmse(actual=ts$AvDens,predicted=AR3_fit)

ts$AR3 <-AR3_fit

tsmean <-mean(res$obs)


ggplot() +
  geom_line(data=res2,aes(x = Year, y = predmean),color = "#555555")+
  geom_ribbon(data=res2,aes(x = Year, y = predmean, ymin = predmean - predfsd, ymax = predmean + predfsd),alpha = 0.4, fill = "gray") +  # out-of-sample
  geom_line(data=res,aes(x = Year, y = predmean),color = "#67a9cf")+
  geom_ribbon(data=res,aes(x = Year, y = predmean, ymin = predmean - predfsd, ymax = predmean + predfsd),alpha = 0.4, fill = "#67a9cf") +  # out-of-sample
  #pred test/train
  geom_hline(aes(yintercept=tsmean),linetype="dashed")+
  #geom_point(data=restt,aes(x = Year, y = obs),color = "#67a9cf") + 
  geom_line(data=ts,aes(x=Year,y=AR3,color="black",linetype="dotted"),color="black",linetype="dotted")+
  #geom_line(data=restt,aes(x = Year, y = predmean),color = "#67a9cf") +
  #geom_ribbon(data=restt,aes(x = Year, y = predmean, ymin = predmean - predfsd, ymax = predmean + predfsd),alpha = 0.4, fill = "#67a9cf") +  # out-of-sample
  geom_point(data=res,aes(x = Year, y = obs)) + #observed data, entire time series 
  #fitted to entire time series
  ylab("Average Density of YOY")+
  #ggtitle("Train BT and Density 2007:2018, Predict: Density 2019-2023")+
  theme_classic()






#########################Annual index and all BT data #################################################

##BT data
bt <- read_excel("C:\\Users\\Tara.Dolan\\OneDrive - Commonwealth of Massachusetts\\Documents\\Winter Flounder\\Research Track\\MADMF Bottom Temperature data\\Tara_DataRequest.xlsx", sheet="Site Information")
bb <-read_excel("C:\\Users\\Tara.Dolan\\OneDrive - Commonwealth of Massachusetts\\Documents\\Winter Flounder\\Research Track\\MADMF Bottom Temperature data\\Tara_DataRequest.xlsx", sheet="BB Barge_Tower")
bc <-read_excel("C:\\Users\\Tara.Dolan\\OneDrive - Commonwealth of Massachusetts\\Documents\\Winter Flounder\\Research Track\\MADMF Bottom Temperature data\\Tara_DataRequest.xlsx", sheet="Buz_Cleveland_Ledge")
mr <-read_excel("C:\\Users\\Tara.Dolan\\OneDrive - Commonwealth of Massachusetts\\Documents\\Winter Flounder\\Research Track\\MADMF Bottom Temperature data\\Tara_DataRequest.xlsx", sheet="CCB_WRECK_OF_MARS")
brm <-read_excel("C:\\Users\\Tara.Dolan\\OneDrive - Commonwealth of Massachusetts\\Documents\\Winter Flounder\\Research Track\\MADMF Bottom Temperature data\\Tara_DataRequest.xlsx", sheet="BOS_ROMANCE_MARTINS")
crp <-read_excel("C:\\Users\\Tara.Dolan\\OneDrive - Commonwealth of Massachusetts\\Documents\\Winter Flounder\\Research Track\\MADMF Bottom Temperature data\\Tara_DataRequest.xlsx", sheet="CCB_RCKY_POINT")
lb <-read_excel("C:\\Users\\Tara.Dolan\\OneDrive - Commonwealth of Massachusetts\\Documents\\Winter Flounder\\Research Track\\MADMF Bottom Temperature data\\Tara_DataRequest.xlsx", sheet="Lewis_Bay")
wb <-read_excel("C:\\Users\\Tara.Dolan\\OneDrive - Commonwealth of Massachusetts\\Documents\\Winter Flounder\\Research Track\\MADMF Bottom Temperature data\\Tara_DataRequest.xlsx", sheet="Waquoit_Bay")

temp_mon <-bind_rows(bb,bc,mr,brm,crp,lb,wb)%>% mutate(Date= as.Date(Date), Time=format(Time, "%H:%M:%S"))%>%
  separate(Date,into=c("Year","Month","Day"),sep="-", remove=FALSE)%>%mutate(TempC=ifelse(is.na(TempC),TempC_Backup,TempC))

#temp_mon %>%
 # ggplot(aes(Date,TempC))+geom_point()+
 # facet_wrap(~Location)+theme_classic()

#temp_mon cleaning 
thresholds <-data.frame(
  Location=unique(temp_mon$Location)[1:6],
  Year_min =c(1989,1992,1993,1990,1992,2007),
  Year_max =c(2022,2022,2022,2022,2023,2022))
#honestly there is a case for subsetting 1993-2022 and eliminating lewis bay as well. 

filtered_temp <- temp_mon %>%
  left_join(thresholds, by = "Location") %>%
  filter(Year >= Year_min & Year <= Year_max)

temps <- filtered_temp %>% group_by(Location, Date)%>% summarise(MeanDT=mean(TempC, na.omit=T),.groups="keep")%>%full_join(filtered_temp)%>%mutate(MeanDailyTemp=coalesce(MeanDailyTemp,MeanDT))

#temps %>%
 # ggplot(aes(Date,MeanDailyTemp))+geom_point()+
 # facet_wrap(~Location)+theme_classic()

#Create temperature indices
annual_temp <-temps %>%group_by(Location, Year)%>% summarise(MeanAT=mean(MeanDailyTemp, na.omit=T),.groups="keep")
annual_temp %>%
  ggplot(aes(Year,MeanAT))+geom_point()+
  facet_wrap(~Location)+theme_classic()

Monthly_temp <-temps %>%group_by(Location, Year, Month)%>% summarise(MeanMT=mean(MeanDailyTemp, na.omit=T),.groups="keep")
Monthly_temp %>%
  ggplot(aes(Year,MeanMT))+geom_point()+
  facet_grid(Month~Location)+theme_bw()

s_annual <-read.csv("DMF_seine/seine survey yoy total annual index.csv", head=T)%>%
  select(-X)%>%mutate(logmean=log(Stratified.Mean))

at<-s_annual[,1:2]%>%mutate(Year=as.character(Year))%>%left_join(annual_temp)

at%>%
  filter(!is.na(Location))%>%
  ggplot(aes(MeanAT,Stratified.Mean))+
  geom_point()+
  facet_wrap(~Location, scales="free_x")+
  geom_smooth(method = "lm", se = FALSE, color = "black")+
  theme_bw()

#South Shore Temp
south_temp <-filtered_temp%>%
  filter(Location %in% c("BB BARGE_TOWER", "BUZ CLEVELAND LEDGE"))%>% #leaving out Waquoit Bay and Lewis Bay for now so we get the longest time series.  
  filter(Year > 1991 & Year < 2023)%>% # 1992-2022
  left_join(at)

#a simplified south shore temp
sat <-at%>% filter(Location %in% c("BB BARGE_TOWER", "BUZ CLEVELAND LEDGE"))%>% #leaving out Waquoit Bay and Lewis Bay for now so we get the longest time series.  
               filter(Year > 1991 & Year < 2023)%>%pivot_wider(names_from = Location,values_from = MeanAT)


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
    #lagssb <-makelags(data=sat, y="Stratified.Mean",E=var_pairs[i,1], tau=var_pairs[i,2])
    lagssb <-makelags(data=sat, y=c("Stratified.Mean","BB BARGE_TOWER","BUZ CLEVELAND LEDGE"),E=var_pairs[i,1], tau=var_pairs[i,2])
    #lagssb <-makelags(data=sat, y=c("Stratified.Mean","BUZ CLEVELAND LEDGE"),E=var_pairs[i,1], tau=var_pairs[i,2])
    lagssb <- cbind(sat, lagssb)
    fitSB <-fitGP(data = lagssb, y = "Stratified.Mean",x=names(lagssb)[5:length(names(lagssb))],scaling = "local",predictmethod = "loo")
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

#let's try lags of both stations Tau = 1, E=7, better scores can be achieved.... and Tau = 3, e=8

mtempbb <-filter(Monthly_temp, Location=="BB BARGE_TOWER")%>%pivot_wider(names_from = Month,values_from = MeanMT)
names(mtempbb)[3:14]<-c("JanBB","FebBB","MarBB","AprBB","MayBB","JunBB","JulBB","AugBB","SepBB","OctBB","NovBB","DecBB")
mtempbuz  <-filter(Monthly_temp, Location=="BUZ CLEVELAND LEDGE")%>%pivot_wider(names_from = Month,values_from = MeanMT)
names(mtempbuz)[3:14]<-c("JanBZ","FebBZ","MarBZ","AprBZ","MayBZ","JunBZ","JulBZ","AugBZ","SepBZ","OctBZ","NovBZ","DecBZ")
mtempsouth <-full_join(mtempbb[,-1],mtempbuz[,-1])
#tbh i feel like the BZ dataset is unreliable... 

mtempbb<-right_join(sat,mtempbb)%>%select(-Location)%>%filter(Year>1992)%>%mutate(Year=as.numeric(Year))

vars_list <- names(mtempbb)[2:16]  # Extract variable names
plist <- list()  # Initialize an empty list for plots of full data models
ttlist <-list() #list for plots of test/train models
fsout <- data.frame(var = character(), fit71_r2 = numeric(), fit71_rmse = numeric(), fit83_r2 = numeric(), fit83_rmse = numeric(), stringsAsFactors = FALSE)
fsouttt <- data.frame(var = character(), fit71_r2 = numeric(), fit71_rmse = numeric(), fit83_r2 = numeric(), fit83_rmse = numeric(), stringsAsFactors = FALSE)

for (i in 1:length(vars_list)) {
  var_name <- vars_list[i]
  
  # Correct column selection
  ts <- mtempbb[, c("Year", "Stratified.Mean", var_name)]
  
  # Ensure correct variable selection for makelags
  #lags83 <- makelags(data = ts, y = "Stratified.Mean", E = 9, tau = 1)
  lags83 <- makelags(data = ts, y = c("Stratified.Mean",var_name), E = 8, tau = 3)
  lags83 <- cbind(ts, lags83)
  lags83 <- lags83[, !duplicated(names(lags83))]
  temp83 <- fitGP(data = lags83, y = "Stratified.Mean", x = names(lags83)[3:ncol(lags83)], predictmethod = "loo")
  
  #test/train version
  #lags83_train<-filter(lags83, Year <=(max(lags83$Year)-3))#predicting 3 years. 
  #lags83_test<-filter(lags83, Year >(max(lags83$Year)-3))
  lags83_train<-filter(lags83, Year <=(max(lags83$Year)-5))#predicting 5 years. 
  lags83_test<-filter(lags83, Year >(max(lags83$Year)-5))
  temp83tt <- fitGP(data = lags83, y = "Stratified.Mean", x = names(lags83)[3:ncol(lags83)], newdata=lags83_test,predictmethod = "loo")
  
  #lags71 <- makelags(data = ts, y = "Stratified.Mean", E = 5, tau = 2)
  lags71 <- makelags(data = ts, y = c("Stratified.Mean",var_name), E = 7, tau = 1)
  lags71 <- cbind(ts, lags71)
  lags71 <- lags71[, !duplicated(names(lags71))]
  temp71 <- fitGP(data = lags71, y = "Stratified.Mean", x = names(lags71)[3:ncol(lags71)], predictmethod = "loo")
  
  #test/train version
  #lags71_train<-filter(lags71, Year <=(max(lags71$Year)-3))#predicting 3 years. 
  #lags71_test<-filter(lags71, Year >(max(lags71$Year)-3))
  lags71_train<-filter(lags71, Year <=(max(lags71$Year)-5))#predicting 5 years. 
  lags71_test<-filter(lags71, Year >(max(lags71$Year)-5))
  temp71tt <- fitGP(data = lags71train, y = "Stratified.Mean", x = names(lags71)[3:ncol(lags71)], newdata=lags71_test,predictmethod = "loo")
  
  # output and label results for straight model
  m71.res <- temp71$outsampresults %>% mutate(model = "m71")
  m83.res <- temp83$outsampresults %>% mutate(model = "m83")
  res <- bind_rows(m71.res, m83.res)
  res$Year <- mtempbb$Year
  
  #test/train version - 3 years. 
  #m71.restt <- temp71tt$outsampresults %>% mutate(Year = 2021:2023,dataset="test", model = "m71")
  #m83.restt <- temp83tt$outsampresults %>% mutate(Year = 2021:2023,dataset="test",model = "m83")
  #restt <- bind_rows(m71.restt, m83.restt)%>%mutate(Year=as.numeric(Year))%>%arrange(Year)
  #res_short = filter(lbyoy, Year < 2021 )%>%mutate(Year=as.numeric(Year), dataset="train")
  
  #test/train version - 5 years. 
  m71.restt <- temp71tt$outsampresults %>% mutate(Year = 2019:2023,dataset="test", model = "m71")
  m83.restt <- temp83tt$outsampresults %>% mutate(Year = 2019:2023,dataset="test",model = "m83")
  restt <- bind_rows(m71.restt, m83.restt)%>%mutate(Year=as.numeric(Year))%>%arrange(Year)
  res_short = filter(mtempbb, Year < 2019 )%>%mutate(Year=as.numeric(Year), dataset="train")
  
  
  #Plot results
  plot <- res %>% ggplot() +
    facet_wrap(~model, scales = "free") + geom_line(aes(x = Year, y = predmean)) +  # out-of-sample
    geom_ribbon(aes(x = Year, y = predmean, ymin = predmean - predfsd, ymax = predmean + predfsd), alpha = 0.4) +  # out-of-sample
    geom_point(aes(x = Year, y = obs)) +
    ggtitle(var_name) + theme_classic()
  plist[[i]] <- plot
  
  #test train version
  plot2 <- ggplot() +
    geom_point(data=res,aes(x = Year, y = obs)) + #observed data, entire time series 
    #pred test/train
    geom_point(data=restt,aes(x = Year, y = obs)) + #observed data, entire time series 
    geom_line(data=restt,aes(x = Year, y = predmean),linetype="dotted") +
    geom_ribbon(data=restt,aes(x = Year, y = predmean, ymin = predmean - predfsd, ymax = predmean + predfsd),alpha = 0.4) +  # out-of-sample
    facet_wrap(~model, scales = "free") +
    ggtitle(var_name) + theme_classic()
  ttlist[[i]] <- plot2
  
  # output fitstats for the full ts model. 
  fsout <- rbind(fsout, data.frame(
    var = var_name,
    fit71_r2 = temp71$outsampfitstats[1],
    fit71_rmse = temp71$outsampfitstats[2],
    fit83_r2 = temp83$outsampfitstats[1],
    fit83_rmse = temp83$outsampfitstats[2]
  ))
  #output fitstats for the test/train model
  fsouttt <- rbind(fsouttt, data.frame(
    var = var_name,
    fit71_r2 = temp71tt$outsampfitstats[1],
    fit71_rmse = temp71tt$outsampfitstats[2],
    fit83_r2 = temp83tt$outsampfitstats[1],
    fit83_rmse = temp83tt$outsampfitstats[2]
  ))
}

# View results
fsout <- fsout %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))
print(fsout)
fsouttt <- fsouttt %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))
print(fsouttt)


# Arrange multiple ggplots in a grid (adjust `ncol` as needed)
grid.arrange(grobs = plist, ncol = 3)
grid.arrange(grobs = ttlist, ncol = 3)


###### instead of predicting the Seine survey, let's predict the NMFS DMF age 1 index... if i have it.... 
#### 



