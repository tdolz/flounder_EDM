### Predict Age 1 recruits in DMF survey for each stock with the relevant BT indices. 
## Created February 6, 2025

library(tidyverse)
library(GPEDM)
library(readxl)
library(gridExtra)
library(rquery)
library(Metrics)


####BT data####
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

#South Shore Temp
south_temp <-temps%>%
  filter(Location %in% c("BB BARGE_TOWER", "BUZ CLEVELAND LEDGE","LEWIS BAY"))%>% #leaving out Waquoit Bay, BB Barge Tower is the best data. 
  filter(Year > 1991 & Year < 2023) # 1992-2022
#south_temp%>% ggplot(aes(Date,MeanDT))+geom_point()+facet_wrap(~Location)+theme_classic()
smonth <-south_temp%>%group_by(Location, Year, Month)%>%summarize(MeanMT=mean(MeanDT),.groups="drop")%>%split(~Location)%>%
  map(~ pivot_wider(.x, names_from = Month, values_from = MeanMT))%>%map(~select(.x,-Location))
names(smonth$`BB BARGE_TOWER`)[2:13]<-c("JanBB","FebBB","MarBB","AprBB","MayBB","JunBB","JulBB","AugBB","SepBB","OctBB","NovBB","DecBB")
names(smonth$`BUZ CLEVELAND LEDGE`)[2:13]<-c("JanBZ","FebBZ","MarBZ","AprBZ","MayBZ","JunBZ","JulBZ","AugBZ","SepBZ","OctBZ","NovBZ","DecBZ")
names(smonth$`LEWIS BAY`)[2:13]<-c("JanLB","FebLB","MarLB","AprLB","MayLB","JunLB","JulLB","AugLB","SepLB","OctLB","NovLB","DecLB")
smonth <-bind_cols(smonth$`BB BARGE_TOWER`,smonth$`BUZ CLEVELAND LEDGE`)%>%select(-Year...14)%>%rename("Year"="Year...1")%>%full_join(smonth$`LEWIS BAY`)%>%
  select(-c("AprBZ","MayBZ","JunBZ","JulBZ","AugBZ","SepBZ","OctBZ","NovBZ"))
s_annual <-filter(annual_temp, Location %in% c("BB BARGE_TOWER", "BUZ CLEVELAND LEDGE","LEWIS BAY"))%>%pivot_wider(names_from = Location,values_from = MeanAT)
smonth <-left_join(smonth,s_annual)%>%mutate(Year=as.numeric(Year))
  
#North Shore Temp
north_temp <-temps%>%
  filter(Location %in% c("CCB RCKY POINT", "BOS ROMANCE_MARTINS"))%>% #CCB Rocky Point looks like the most complete data. #not including CCB wreck of mars because it looks unreliable.
  filter(Year > 1991 & Year < 2023) # 1992-2022
#north_temp%>% ggplot(aes(Date,MeanDT))+geom_point()+facet_wrap(~Location)+theme_classic()
nmonth <-north_temp%>%group_by(Location, Year, Month)%>%summarize(MeanMT=mean(MeanDT),.groups="drop")%>%split(~Location)%>%
  map(~ pivot_wider(.x, names_from = Month, values_from = MeanMT))%>%map(~select(.x,-Location))
names(nmonth$`BOS ROMANCE_MARTINS`)[2:13]<-c("JanBM","FebBM","MarBM","AprBM","MayBM","JunBM","JulBM","AugBM","SepBM","OctBM","NovBM","DecBM")
names(nmonth$`CCB RCKY POINT`)[2:13]<-c("MarRP","AprRP","MayRP","JunRP","JulRP","AugRP","SepRP","OctRP","NovRP","DecRP","JanRP","FebRP")
#names(nmonth$`CCB WRECK OF MARS`)[2:13]<-c("JanLB","FebLB","MarLB","AprLB","MayLB","JunLB","JulLB","AugLB","SepLB","OctLB","NovLB","DecLB") #not bothering with Wreck of Mars. 
nmonth <-full_join(nmonth$`BOS ROMANCE_MARTINS`,nmonth$`CCB RCKY POINT`)%>%mutate(Year=as.numeric(Year))%>%arrange(Year)
n_annual <-filter(annual_temp, Location %in% c("CCB RCKY POINT", "BOS ROMANCE_MARTINS"))%>%pivot_wider(names_from = Location,values_from = MeanAT)%>%mutate(Year=as.numeric(Year))
nmonth <-left_join(nmonth,n_annual)


##### Import the recruit data #####

#SNE#
SNEdmf_spring <- read.csv("data_WF_SNEMA/DMFSPRING_WFSNEMA.csv", header = TRUE)##MADMF Spring BTS mean numbers per tow, 7 is a plus group
SNEnmfs_fall <- read.csv("data_WF_SNEMA/fallBTS_WFSNEMA.csv", header = TRUE) #NMFS fall BTS mean numbers per tow, 7 is a plus group
SNEnmfs_spring <- read.csv("data_WF_SNEMA/springBTS_WFSNEMA.csv", header = TRUE)#NMFS Spring BTS mean numbers per tow, 7 is a plus group
SNEnemap_spring <-read.csv("data_WF_SNEMA/wfsurvey_NEMAP_SPRING.csv", header = TRUE) #nemap, 7 is a plus group
SNEtotal_catch <-read.csv("data_WF_SNEMA/total_catch_SNEMAWF.csv", header = TRUE) #total catch 
SNErecruits <-read.csv("data_WF_SNEMA/SNEMAWF_recruitment.csv", header = TRUE) #mean numbers per tow recruitmet (age 0) indices

#GOM#
plus30 <- read.csv("data_WF_GOM/GOM_WF_30plusBiomass.csv", header = TRUE)
survdat <-read.csv("data_WF_GOM/GOM_WF_survey.csv", header = TRUE)


#### Create the indices ##########

#DMF Age1 index
snedmfage1 <- SNEdmf_spring[,1:2]%>%mutate(Age0=lead(X1))%>%filter(Year>1991)%>%left_join(smonth)

#Fits across a grid of E and Tau

#choose your ts
#ts <-snedmfage1[,c(1,3)] #Age0 predicting itself. 
ts <-snedmfage1[,c("Year","Age0","BB BARGE_TOWER")]

#Fits across a grid of E and Tau
Ees <-seq(1,10,1)
taus <-seq(1,3,1)
var_pairs = expand.grid(Ees, taus) # Combinations of vars, 2 at a time
ETdf <-matrix(nrow=dim(var_pairs)[1],ncol=4)
ETdf[,1]<-var_pairs[,1]
ETdf[,2]<-var_pairs[,2]
r2matrixSB = array(NA, dim = c(length(Ees), length(taus)), dimnames = list(Ees,taus)) 
ttmatrixSB = array(NA, dim = c(length(Ees), length(taus)), dimnames = list(Ees,taus)) 
for (i in 1:nrow(var_pairs)) {
  try({
    #lagssb <-makelags(data=ts, y=names(ts)[-1],E=var_pairs[i,1], tau=var_pairs[i,2])
    lagssb <-makelags(data=ts, y="Age0",E=var_pairs[i,1], tau=var_pairs[i,2])
    lagssb <- cbind(ts, lagssb)
    #full data fit
    fitSB <-fitGP(data = lagssb, y = "Age0",x=names(lagssb)[length(names(ts)):length(names(lagssb))],scaling = "local",predictmethod = "loo")
    fitSB_r2 <-fitSB$outsampfitstats[[1]]
    #test train fit
    lags_train<-filter(lagssb, Year <=(max(lagssb$Year)-5))#predicting 5 years. 
    lags_test<-filter(lagssb, Year >(max(lagssb$Year)-5))
    fittt <- fitGP(data = lags_train, y = "Age0", x=names(lags_train)[length(names(ts)):length(names(lags_train))], newdata=lags_test,predictmethod = "loo")
    #collect_fitstats
    fitSB_r2 <-fitSB$outsampfitstats[[1]]
    fittt_r2 <-fittt$outsampfitstats[[1]]
    r2matrixSB[var_pairs[i,1], var_pairs[i,2]] = fitSB_r2
    ETdf[i,3] <-fitSB_r2
    ETdf[i,4] <-fittt_r2
    ttmatrixSB[var_pairs[i,1], var_pairs[i,2]] = fittt_r2
  },silent=T)
}
r2matrixSB
ttmatrixSB 

#Best e=9 tau=2

#################################loop through all the temperature options. ##################################################

tg<-snedmfage1[,c("Year", "X1","JanBB","FebBB","MarBB","AprBB","MayBB","JunBB","JulBB", "AugBB","SepBB","OctBB","NovBB","DecBB","BB BARGE_TOWER")]
vars_list <- names(tg)[3:length(names(tg))]  # Extract variable names
plist <- list()  # Initialize an empty list for plots of full data models
ttlist <-list() #list for plots of test/train models
fsout <- data.frame(var = character(), fit_r2 = numeric(), fit_tt = numeric())

for (i in 1:length(vars_list)) {
  var_name <- vars_list[i]
  
  # Correct column selection
  ts <- tg[, c("Year", "X1", var_name)]
  
  # Ensure correct variable selection for makelags
  lags92 <- makelags(data = ts, y = c("X1",var_name), E = 9, tau = 1)
  lags92 <- cbind(ts, lags92)
  lags92 <- lags92[, !duplicated(names(lags92))]
  temp92 <- fitGP(data = lags92, y = "X1", x = names(lags92)[3:ncol(lags92)], predictmethod = "loo")
  
  #test/train version
  lags92_train<-filter(lags92, Year <=(max(lags92$Year)-5))#predicting 5 years. 
  lags92_test<-filter(lags92, Year >(max(lags92$Year)-5))
  temp92tt <- fitGP(data = lags92_train, y = "X1", x = names(lags92_train)[3:ncol(lags92)], newdata=lags92_test,predictmethod = "loo")
  
  # output and label results
  m92.res <- temp92$outsampresults %>% mutate(model = "m92")  
  m92.res$Year <- ts$Year
  m92.restt <- temp92tt$outsampresults %>% mutate(Year = (max(lags92$Year)-4):max(lags92$Year),model = "m92tt")
  res <- bind_rows(m92.res, m92.restt)

  
  #Plot results
  plot <- res %>% ggplot() +
    geom_line(aes(x = Year, y = predmean, color=model)) +  # out-of-sample
    geom_ribbon(aes(x = Year, y = predmean, ymin = predmean - predfsd, ymax = predmean + predfsd,fill = model), alpha = 0.4) +  # out-of-sample
    geom_point(aes(x = Year, y = obs)) +
    ggtitle(var_name) + theme_classic()
  plist[[i]] <- plot

  # output fitstats for the full ts model. 
  fsout <- rbind(fsout, data.frame(
    var = var_name,
    fit_r2 = temp92$outsampfitstats[1],
    fit_tt = temp92tt$outsampfitstats[1]
  ))
}

# View results
fsout <- fsout %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))
print(fsout)

# Arrange multiple ggplots in a grid (adjust `ncol` as needed)
grid.arrange(grobs = plist, ncol = 3)



#Now plot one of the better models
ts <-snedmfage1[,c("Year","Age0","X1","MarBB","MayBB","OctBB","NovBB","JanBB")]

#Age0 only
lagssb <-makelags(data=ts, y="Age0",E=9, tau=2)
lagssb <- cbind(ts, lagssb)
fit0 <-fitGP(data = lagssb, y = "Age0",x=c("Age0_2","Age0_4","Age0_6","Age0_8","Age0_10","Age0_12","Age0_14","Age0_16","Age0_18"),scaling = "local",predictmethod = "loo")
fit0_r2 <-fit0$outsampfitstats[[1]]
res0 <-fit0$outsampresults %>% mutate(model="fit0")
res0$Year <-ts$Year

#Age0 & MarBB
lagssb <-makelags(data=ts, y=c("Age0","MarBB"),E=9, tau=2)
lagssb <- cbind(ts, lagssb)
fit0M <-fitGP(data = lagssb, y = "Age0",x=names(lagssb)[c(4,9:26)],scaling = "local",predictmethod = "loo")
fit0M_r2 <-fit0M$outsampfitstats[[1]]
res0M <-fit0M$outsampresults %>% mutate(model="fit0M")
res0M$Year <-ts$Year

#Age1tt only
lagssb <-makelags(data=ts, y="X1",E=9, tau=2)
lagssb <- cbind(ts, lagssb)
lags_train<-filter(lagssb, Year <=(max(lagssb$Year)-5))#predicting 5 years. 
lags_test<-filter(lagssb, Year >(max(lagssb$Year)-5))
fit1 <-fitGP(data = lags_train, y = "X1",x=c("X1_2","X1_4","X1_6","X1_8","X1_10","X1_12","X1_14","X1_16","X1_18"),newdata=lags_test,time="Year",predictmethod = "loo")
fit1_r2 <-fit1$outsampfitstats[[1]]
res1 <-fit1$outsampresults %>% mutate(model="fit1", Year=(max(ts$Year)-4):max(ts$Year))

#Age1predseq
fit1_update=predict_seq(fit1,newdata=lags_test)
fit1_update$outsampfitstats
res1u <-fit1_update$outsampresults %>% mutate(model = "fit1_update", Year=(max(ts$Year)-4):max(ts$Year))


#Age1tt with JanuaryBB
lagssb <-makelags(data=ts, y=c("X1","JanBB"),E=9, tau=2)
lagssb <- cbind(ts, lagssb)
lags_train<-filter(lagssb, Year <=(max(lagssb$Year)-5))#predicting 5 years. 
lags_test<-filter(lagssb, Year >(max(lagssb$Year)-5))
fit1J<- fitGP(data = lags_train, y = "X1", x=names(lags_train)[8:26], newdata=lags_test,predictmethod = "loo", time="Year")
res1J <-fit1J$outsampresults %>% mutate(model = "fit1J", Year=(max(ts$Year)-4):max(ts$Year))

#Age1Jpredseq
fit1J_update=predict_seq(fit1J,newdata=lags_test)
fit1J_update$outsampfitstats
res1Ju <-fit1J_update$outsampresults %>% mutate(model = "fit1J_update", Year=(max(ts$Year)-4):max(ts$Year))

#ARIMA fit -Age0
#Is this much better than an arima or moving average? Probably not. 
#https://rpubs.com/JSHAH/481706 
t2 <-select(ts, Year, Age0)%>%filter(Year<2019)
acf(t2$Age0)
AR1 <- arima(t2$Age0, order = c(1,0,0))
AR1_fit <- t2$Age0 - residuals(AR1)
summary(AR1)
AR2 <- arima(t2$Age0, order = c(2,0,0))
AR2_fit <- t2$Age0 - residuals(AR2)
AR3 <- arima(t2$Age0, order = c(3,0,0))
AR3_fit <- t2$Age0 - residuals(AR3)
#calculate R2 on the AR fits
getR2(obs=t2$Age0, pred=AR1_fit)
getR2(obs=t2$Age0, pred=AR2_fit)
getR2(obs=t2$Age0, pred=AR3_fit)
ts$AR3 <-c(NA,NA,NA,AR3_fit)

#ARIMA fit -Age1 with forecasting
t2 <-select(ts, Year, X1)%>%filter(Year<2017)
acf(t2$X1)
AR3 <- arima(t2$X1, order = c(3,0,0))
AR3_fit <- t2$X1 - residuals(AR3)
#calculate R2 on the AR fits
getR2(obs=t2$X1, pred=AR3_fit)
#AR3 is best
forecast_AR3 <- predict(AR3, n.ahead = 5)
ts$AR3 <-c(AR3_fit,forecast_AR3$pred)


#Age0 Plot
ggplot() +
  geom_line(data=res0,aes(x = Year, y = predmean),color = "#555555")+
  geom_ribbon(data=res0,aes(x = Year, y = predmean, ymin = predmean - predfsd, ymax = predmean + predfsd),alpha = 0.4, fill = "gray") +  # out-of-sample
  geom_line(data=res0M,aes(x = Year, y = predmean),color = "#67a9cf")+
  geom_ribbon(data=res0M,aes(x = Year, y = predmean, ymin = predmean - predfsd, ymax = predmean + predfsd),alpha = 0.4, fill = "#67a9cf") +  # out-of-sample
  geom_hline(aes(yintercept=mean(ts$Age0,na.rm=T)),linetype="dashed")+
  geom_point(data=ts,aes(x=Year,y=Age0))+
  geom_line(data=ts,aes(x=Year,y=AR3),linetype="dotted")+
  #geom_point(data=filter(ts,Year >2009),aes(x = Year, y = Age0)) + #observed data, entire time series 
  #scale_x_continuous(breaks = 2010:2020) +  
  #xlim(2010,2021)+
  ylab("DMF Spring Age0")+
  theme_classic()


#Age1 Test train
ggplot() +
  geom_line(data=res1,aes(x = Year, y = predmean),color = "#555555")+
  geom_ribbon(data=res1,aes(x = Year, y = predmean, ymin = predmean - predfsd, ymax = predmean + predfsd),alpha = 0.4, fill = "gray") +  # out-of-sample
  geom_line(data=res1J,aes(x = Year, y = predmean),color = "#67a9cf")+
  geom_ribbon(data=res1J,aes(x = Year, y = predmean, ymin = predmean - predfsd, ymax = predmean + predfsd),alpha = 0.4, fill = "#67a9cf") +  # out-of-sample
  geom_hline(aes(yintercept=mean(ts$X1,na.rm=T)),linetype="dashed")+
  geom_point(data=ts,aes(x=Year,y=X1))+
  ylab("DMF Spring Age1")+
  xlim(2010,2021)+
  theme_classic()


#Age1 pred_seq
ggplot() +
  geom_line(data=res1u,aes(x = Year, y = predmean),color = "#555555")+
  geom_ribbon(data=res1u,aes(x = Year, y = predmean, ymin = predmean - predfsd, ymax = predmean + predfsd),alpha = 0.4, fill = "gray") +  # out-of-sample
  geom_line(data=res1Ju,aes(x = Year, y = predmean),color = "#67a9cf")+
  geom_ribbon(data=res1Ju,aes(x = Year, y = predmean, ymin = predmean - predfsd, ymax = predmean + predfsd),alpha = 0.4, fill = "#67a9cf") +  # out-of-sample
  geom_hline(aes(yintercept=mean(ts$X1,na.rm=T)),linetype="dashed")+
  geom_line(data=ts,aes(x=Year,y=AR3),linetype="dotted")+
  geom_point(data=ts,aes(x=Year,y=X1))+
  ylab("DMF Spring Age1")+
  xlim(2010,2021)+
  theme_classic()

