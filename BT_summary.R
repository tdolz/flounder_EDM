#Plot BT locations & summarize #
# 2/7/2025

library(tidyverse)
library(readxl)
library(gridExtra)
library(Metrics)
library("ggmap")

### site information ####
site_info <-read_excel("C:\\Users\\Tara.Dolan\\OneDrive - Commonwealth of Massachusetts\\Documents\\Winter Flounder\\Research Track\\Time series\\Site Index_3.15.24.xlsx",sheet="Temp Monitors - Site Index")

### Create the site map ####
#REGISTER THE API KEY! 
register_google(key="AIzaSyAJnNWzhDwco0BIRpEgi9I_93dx6FZNMKA", account_type = "standard")

#get the basemap
MA <- "Boston"
#overview map
SNEmap <- get_map(MA, zoom = 8, source="google", maptype="terrain", color="bw",crop=FALSE)
ggmap(SNEmap)+
  geom_point(aes(x=LONDD, y=LATDD, color=`Depth (feet) at MLW unless otherwise noted`), data=site_info)

#get the basemap
#MA <- "Portland, ME"
#bbox1 <- c(left = -74.568967, bottom = 40.058769, right = -67.010374, top = 44.713204)
#SNEmap <-get_map(bbox1, source="google", maptype="terrain",zoom=6, color="bw",crop=TRUE) 



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

# CDF plots? & distributions of the data. 

filtered_temp%>%
  ggplot(aes(x = MeanDailyTemp)) +
  geom_histogram(binwidth = 0.001, color = "light grey", fill = "light grey") +
  facet_wrap(~Location)+
  #geom_vline(aes(xintercept = unrelated), color = "black", linetype = "dashed", size = 0.5) + #the estimated mean value for unrelated in the simulation
   labs(x = "Mean Daily Temperature C", y = "number of observations")+
  theme_bw()

#need to calculate a mean daily temperature for Lewis bay and reinput that back into filtered_temp


Monthly_extremes <-group_by(Month,Year,Location)

Annual_extremes




