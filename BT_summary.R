#Plot BT locations & summarize #
# 2/7/2025

library(tidyverse)
library(readxl)
library(gridExtra)
library(Metrics)
library("ggmap")

### site information ####
site_info <-read_excel("C:\\Users\\Tara.Dolan\\OneDrive - Commonwealth of Massachusetts\\Documents\\Winter Flounder\\Research Track\\Time series\\Site Index_3.15.24.xlsx",sheet="Temp Monitors - Site Index")

site_info %>% group_by(Status)%>%summarize(count=n_distinct(Name))

#site_info %>%( depth_bins = cut(`Depth (feet) at MLW unless otherwise noted`, breaks=seq(0,120,10)))%>%group_by(depth_bins)%>%summarize(count=n_distinct(Name))

### Create the site map ####
#REGISTER THE API KEY! 
register_google(key="AIzaSyAJnNWzhDwco0BIRpEgi9I_93dx6FZNMKA", account_type = "standard")

#get the basemap
MA <- "Plymouth, MA"
#overview map
SNEmap <- get_map(MA, zoom = 8, source="google", maptype="terrain", color="bw",crop=FALSE)
ggmap(SNEmap)+
  geom_point(aes(x=LONDD, y=LATDD, color=`Depth (feet) at MLW unless otherwise noted`), data=site_info)+
  #scale_color_viridis_b()
  scale_colour_binned(type = "viridis",
                      breaks=seq(0,120,10),
                      limits = c(0, 120),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE))

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

# Function to calculate mode
mode_value <- function(x) {
  tab <- table(x)
  as.numeric(names(tab)[which.max(tab)])}

# Summarize data
summary_temp <- temp_mon %>%
  group_by(Location) %>%
  summarize(
    min_Year = min(Year, na.rm = TRUE),  # Minimum Year
    max_Year = max(Year, na.rm = TRUE),  # Maximum Year
    na_count = sum(is.na(TempC)),  # Count of NA values in TempC
    mean_TempC = mean(TempC, na.rm = TRUE),  # Mean of TempC
    median_TempC = median(TempC, na.rm = TRUE),  # Median of TempC
    mode_TempC = mode_value(TempC)  # Mode of TempC
  )

print(summary_temp)

#temp_mon %>%
#ggplot(aes(Date,TempC))+geom_point()+
#facet_wrap(~Location, scales="free_y")+theme_classic()

temps <- temp_mon %>% group_by(Location, Date)%>% summarise(MeanDT=mean(TempC, na.omit=T),MedDT=median(TempC, na.omit=T).groups="keep")%>%
  full_join(temp_mon)%>%
  mutate(MeanDT_imputed=ifelse(is.na(MeanDailyTemp),MeanDT,MeanDailyTemp))

#Create temperature indices
annual_temp <-temps %>%group_by(Location, Year)%>% summarise(MeanAT=mean(MeanDailyTemp, na.omit=T),.groups="keep")
annual_temp %>%
  ggplot(aes(Year,MeanAT))+geom_point()+
  facet_wrap(~Location, scales="free_y")+theme_classic()

#temp_mon cleaning 
thresholds <-data.frame(
  Location=unique(temp_mon$Location)[1:6],
  Year_min =c(1989,1992,1993,1990,1992,2007),
  Year_max =c(2022,2022,2022,2022,2023,2022))

filtered_temp <- temps %>%
  left_join(thresholds, by = "Location") %>%
  filter(Year >= Year_min & Year <= Year_max)%>%
  mutate(TempC=ifelse(is.na(TempC),TempC_Backup,TempC))

### How many NA are present in TempC? 
count_NA <-filtered_temp%>%group_by(Location,Year)%>%summarize(na_num=sum(is.na(TempC)), val_count=sum(!is.na(TempC)),.groups="keep")
count_NA %>% ggplot(aes(x = Year, y = na_num, fill = Location)) + geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Number of NAs", title = "NA Count by Year and Location") + theme_minimal()

### How many 0 are present in TempC? 
count0 <-filtered_temp%>%group_by(Location,Year)%>%summarize(zero_num=sum(TempC==0),.groups="keep")
count0 %>% ggplot(aes(x = Year, y = zero_num, fill = Location)) + geom_bar(stat = "identity", position = "dodge") + 
  labs(x = "Year", y = "Number of 0s", title = "0 Count by Year and Location") +theme_minimal()


# CDF plots? & distributions of the data. 

#thresholds
egg_max = 10
larvae_min = 2
larvae_max = 20
YOY_max = 27
Juv_min=8
Juv_pref=18.5
Juv_max = 27
Adults_leave = 15
adult_pref = 13.5
adult_spring_prefmin_NEFSC = 4
adult_spring_prefmax_NEFSC = 6
adult_fall_prefmin_NEFSC = 10
adult_fall_prefmax_NEFSC = 15
adult_spring_prefmin_dmf = 5
adult_spring_prefmax_dmf = 13
adult_fall_prefmin_dmf = 9
adult_fall_prefmax_dmf = 13

filtered_temp2 <-filter(filtered_temp, Year> 1993 & Location %in% c("LEWIS BAY","BB BARGE_TOWER","BOS ROMANCE_MARTINS","CCB RCKY POINT"))

filtered_temp2%>%
  ggplot(aes(x = TempC)) +
  geom_histogram(binwidth = 0.001, color = "light grey", fill = "light grey") +
  #geom_vline(aes(xintercept = egg_max), color = "black", linetype = "dashed", linewidth = 0.5) + #the estimated mean value for unrelated in the simulation
  facet_wrap(~Location, scales="free_y")+
  labs(x = "Temperature C", y = "number of observations")+
  theme_bw()

filtered_temp2%>%
  ggplot(aes(x=))

# Summarize data
summary_temp <- filtered_temp2 %>%
  group_by(Location) %>%
  summarize(
    min_Year = min(Year, na.rm = TRUE),  # Minimum Year
    max_Year = max(Year, na.rm = TRUE),  # Maximum Year
    na_count = sum(is.na(TempC)),  # Count of NA values in TempC
    obs_count =sum(!is.na(TempC)),
    mean_TempC = mean(TempC, na.rm = TRUE),  # Mean of TempC
    median_TempC = median(TempC, na.rm = TRUE),  # Median of TempC
    mode_TempC = mode_value(TempC)  # Mode of TempC
  )
print(summary_temp)

#Egg temp
filtered_temp2%>%
  filter(Month %in% c("02","03","04","05"))%>%
  ggplot(aes(x = TempC)) +
  geom_histogram(binwidth = 0.001, color = "light grey", fill = "light grey") +
  geom_vline(aes(xintercept = egg_max), color = "black", linetype = "dashed", linewidth = 0.5) + #the estimated mean value for unrelated in the simulation
  facet_grid(Location~Month, scales="free_y")+
  #facet_wrap(~Location, scales="free_y")+
  labs(x = "Temperature C", y = "number of observations")+
  theme_bw()

#larvae temp
filtered_temp2%>%
  filter(Month %in% c("03","04","05","06","07","08"))%>%
  ggplot(aes(x = TempC)) +
  geom_histogram(binwidth = 0.001, color = "light grey", fill = "light grey") +
  geom_vline(aes(xintercept = larvae_min), color = "black", linetype = "dashed", linewidth = 0.5) + #the estimated mean value for unrelated in the simulation
  geom_vline(aes(xintercept = larvae_max), color = "black", linetype = "dashed", linewidth = 0.5) + #the estimated mean value for unrelated in the simulation
  facet_grid(Location~Month, scales="free_y")+
  #facet_wrap(~Location, scales="free_y")+
  labs(x = "Temperature C", y = "number of observations")+
  theme_bw()

#yoy temp
filtered_temp2%>%
  filter(Month %in% c("04","05","06","07","08","09","10"))%>%
  ggplot(aes(x = TempC)) +
  geom_histogram(binwidth = 0.001, color = "light grey", fill = "light grey") +
  geom_vline(aes(xintercept = YOY_max), color = "black", linetype = "dashed", linewidth = 0.5) + #the estimated mean value for unrelated in the simulation
  geom_vline(aes(xintercept = 25), color = "black", linetype = "dotted", linewidth = 0.5) + #the estimated mean value for unrelated in the simulation
  facet_grid(Location~Month, scales="free_y")+
  #facet_wrap(~Location, scales="free_y")+
  labs(x = "Temperature C", y = "number of observations")+
  theme_bw()

#juvenile temp
filtered_temp2%>%
  #filter(Month %in% c("04","05","06","07","08","09","10"))%>%
  ggplot(aes(x = TempC)) +
  geom_histogram(binwidth = 0.001, color = "light grey", fill = "light grey") +
  geom_vline(aes(xintercept = Juv_min), color = "black", linetype = "dotted", linewidth = 0.5) + #the estimated mean value for unrelated in the simulation
  geom_vline(aes(xintercept = Juv_pref), color = "black", linetype = "dashed", linewidth = 0.7) + #the estimated mean value for unrelated in the simulation
  geom_vline(aes(xintercept = Juv_max), color = "black", linetype = "dotted", linewidth = 0.5) + #the estimated mean value for unrelated in the simulation
  facet_grid(Location~Month, scales="free_y")+
  #facet_wrap(~Location, scales="free_y")+
  labs(x = "Temperature C", y = "number of observations")+
  theme_bw()

#adult temp
filtered_temp2%>%
  filter(Month %in% c("01","02","03","04","05"))%>%
  ggplot(aes(x = TempC)) +
  geom_histogram(binwidth = 0.001, color = "light grey", fill = "light grey") +
  geom_vline(aes(xintercept = 12), color = "black", linetype = "dotted", linewidth = 0.5) + #the estimated mean value for unrelated in the simulation
  geom_vline(aes(xintercept = 15), color = "black", linetype = "dotted", linewidth = 0.5) + #the estimated mean value for unrelated in the simulation
  facet_grid(Location~Month, scales="free_y")+
  #facet_wrap(~Location, scales="free_y")+
  labs(x = "Temperature C", y = "number of observations")+
  theme_bw()
